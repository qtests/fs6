{-# LANGUAGE OverloadedStrings #-}


-- | This module initializes the application's state and starts the warp server.
module Main where
-- import Control.Concurrent.STM
-- import Data.IntMap
import Yesod
import Database.Persist.Sql (ConnectionPool, SqlBackend, runSqlPool, runMigration)
import Data.Pool (Pool(..))

import Data.Text (pack)
import Data.Time (UTCTime(..), fromGregorian, getCurrentTime)
import Data.List (transpose, zipWith5, intercalate)
import Data.Maybe (fromJust, isJust)
 
import Control.Monad
import Control.Monad.Logger (LoggingT, runStderrLoggingT)
import Control.Monad.Trans.Resource (ResourceT, runResourceT)
import Control.Monad.Trans.Reader (ReaderT)
import Control.Concurrent (threadDelay, forkIO)

-- https://hackage.haskell.org/package/bytestring-0.10.8.2/docs/Data-ByteString.html
import qualified Data.ByteString as S

import Dispatch ()
import Foundation
import Config
import Model

import Yadata.LibAPI

import System.ReadEnvVar (readEnvDef)


-- DB links
-- https://github.com/agrafix/users
-- https://github.com/Daiver/HBlog


dbFunction :: ReaderT SqlBackend (LoggingT (ResourceT IO)) a -> Pool SqlBackend  -> IO a
dbFunction query pool = runResourceT $ runStderrLoggingT $ runSqlPool query pool


buildDb :: ReaderT SqlBackend (LoggingT (ResourceT IO)) ( )
buildDb = do

    maybeIBM <- getBy $ UniqueTicker "IBM" True
    case maybeIBM of
        Nothing -> do
                      liftIO $ putStrLn "Just kidding, IBM is not really there !"
                      insert_ $ Company  "IBM Inc" "www.ibm.com" "IBM" True
                      return ()
        Just (Entity companyId cpny) -> liftIO $ print cpny

    maybeMSFT <- getBy $ UniqueTicker "MSFT" True
    case maybeMSFT of
        Nothing -> do
                      liftIO $ putStrLn "Just kidding, Microsoft is not really there !"
                      insert_ $ Company  "Microsoft Inc" "www.microsoft.com" "MSFT" True
                      return ()
        Just (Entity companyId cpny) -> liftIO $ print cpny

-- *********************************************************************************************** -- 
-- Time Series DB Stuff
-- *********************************************************************************************** -- 

addTextFile2DB :: String -> String -> ReaderT SqlBackend (LoggingT (ResourceT IO)) ()
addTextFile2DB fileName filePath = do
    fileContents <- liftIO $ S.readFile $ filePath ++ fileName
    time <- liftIO getCurrentTime
    insert_ $ StoredFile (pack fileName) "text/plain" fileContents time


sendTS2DB :: String -> Either String [(UTCTime, [Double])] -> ReaderT SqlBackend (LoggingT (ResourceT IO)) ()
sendTS2DB _ (Right []) = return ()
sendTS2DB ticker timeSeries = 
    case timeSeries of 
        Left _    -> return ()
        Right ts  -> do 
                        maybeCpny <- getBy $ UniqueTicker (pack ticker) True
                        if (isJust maybeCpny) 
                            then
                                do 
                                    let (Entity companyId cpny) = fromJust maybeCpny
                                    let (index, dta) = unzip ts

                                    let [close, adjclose, volume] = transpose dta
                                    let dbts = zipWith5 TimeSeries (repeat companyId) index close adjclose volume 
                                    
                                    -- Insert data
                                    forM_ dbts insertUnique 

                                    -- ids <- forM dbts insert
                                    -- liftIO $ print ids
                            else
                                return ()


sendTS2File :: String -> Either String [(UTCTime, [Double])] -> IO ()
sendTS2File filePath timeSeries =
    case timeSeries of 
        Left _   -> writeFile filePath ""
        Right ts -> if length ts > 0 then
                        let (times, values) = unzip ts
                            tsString = mconcat $ ["Date,Value\n"] ++ 
                                    zipWith (\x y -> 
                                        mconcat [show x,",", intercalate ", " $ fmap show y,"\n"] ) times values
                        in writeFile filePath tsString
                    else
                        writeFile filePath ""


queryIdTSSendEm2File :: String -> String -> ReaderT SqlBackend (LoggingT (ResourceT IO)) ()
queryIdTSSendEm2File ticker filePath = do
    maybeCpny <- getBy $ UniqueTicker (pack ticker) True
    if (isJust maybeCpny) 
        then
            do 
                let (Entity companyId cpny) = fromJust maybeCpny
                tsRecords <- selectList [TimeSeriesTsid ==. companyId ] [ Asc TimeSeriesRefdate ]
                
                -- http://www.jakubkonka.com/2014/01/23/conduit-haskell.html
                -- Check !!
                let ts = fmap (\(Entity _ (TimeSeries _ refDate close adjclose vol)) -> 
                                                    (refDate, [close, adjclose, vol]) ) tsRecords

                liftIO $ sendTS2File filePath (Right ts)
                -- liftIO $ print $ take 5 ts
                -- return ()
        else
            liftIO $ sendTS2File filePath (Right [])


-- All batch job
tsDownloadJob :: [String] -> Int -> ConnectionPool -> IO ()
tsDownloadJob tickers timeDelay conpool = 
    forever $ do
        print ("TS Download Job: Downloading data!" :: String)

        -- Get the time series
        let ticker = tickers !! 0
        let  startDate = UTCTime (fromGregorian 2015 01 01) 0
        ts <- priceTimeSeriesWithDate ticker startDate
        
        -- Save time series to file
        -- sendTS2File "testFile_hts.csv" ts
        -- dbFunction (addTextFile2DB "testFile_hts.csv" "") conpool 

        -- Save to database
        dbFunction (sendTS2DB ticker ts) conpool 

        -- Query company's Id, time series and save the later two file
        dbFunction (queryIdTSSendEm2File ticker "testFile_htsdb.csv") conpool
        dbFunction (addTextFile2DB "testFile_htsdb.csv" "") conpool 

        print ("TS Download Job: Going to Sleep!" :: String)
        threadDelay timeDelay


main :: IO ()
main = do
    
    persistConfig <- perstConfig

    pool <- createPoolConfig persistConfig

    dbFunction (runMigration migrateAll) pool
    dbFunction buildDb pool 

    -- Download price time series
    -- let sleepTime = (10^6 * 60 * 5) :: Int
    let sleepTime = (10^6 * 3600 * 6) :: Int
    _ <- forkIO $ tsDownloadJob ["IBM"] sleepTime pool

    -- Initialize the filestore to an empty map.
    -- tstore <- atomically $ newTVar empty

    -- The first uploaded file should have an ID of 0.
    -- tident <- atomically $ newTVar 0

    -- warpEnv starts the Warp server over a port defined by an environment
    -- variable. To launch the app on a specific port use 'warp'.
    -- warpEnv $ App tident tstore

    port <- readEnvDef "PORT" 8080
    -- warp port $ App tident tstore pool persistConfig
    warp port $ App pool persistConfig
