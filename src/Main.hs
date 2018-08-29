{-# LANGUAGE OverloadedStrings #-}


-- | This module initializes the application's state and starts the warp server.
module Main where
-- import Control.Concurrent.STM
-- import Data.IntMap
import Yesod
import Database.Persist.Sql (ConnectionPool, SqlBackend, runSqlPool, runMigration)
import Data.Pool (Pool(..))
import Data.Maybe (fromJust, isJust)
import Data.Text (pack)
import Data.Time (UTCTime(..), fromGregorian)

 
import Control.Monad (forever, forM_)
import Control.Monad.Logger (LoggingT, runStderrLoggingT)
import Control.Monad.Trans.Resource (ResourceT, runResourceT)
import Control.Monad.Trans.Reader (ReaderT)
import Control.Concurrent (threadDelay, forkIO)


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


-- *********************************************************************************************** -- 
-- Time Series DB Stuff
-- *********************************************************************************************** -- 

queryIdTSSendEm2File :: String -> String -> ReaderT SqlBackend (LoggingT (ResourceT IO)) ()
queryIdTSSendEm2File ticker filePath = do
    maybeCpny <- getBy $ UniqueTicker (pack ticker) True
    if (isJust maybeCpny) 
        then
            do 
                let (Entity companyId cpny) = fromJust maybeCpny
                tsRecords <- selectList [TimeSeriesTsid ==. companyId ] [ Asc TimeSeriesRefdate ]
                
                -- http://www.jakubkonka.com/2014/01/23/conduit-haskell.html
-- ******** --
-- Check !! --
-- ******** --
                let ts = fmap (\(Entity _ (TimeSeries _ refDate close adjclose vol)) -> 
                                                    (refDate, [close, adjclose, vol]) ) tsRecords

                liftIO $ sendTS2File ticker filePath True (Right ts)
                -- liftIO $ print $ take 5 ts
        else
            liftIO $ sendTS2File ticker filePath True (Right [])


-- Batch job
tsDownloadJob :: [String] -> Int -> ConnectionPool -> IO ()
tsDownloadJob tickers timeDelay conpool = 
    forever $ do
        print ("TS Download Job: Downloading data!" :: String)

        -- Initializing the output file
        let outFile = "testFile_htsdb.csv"
        writeFile outFile "Date,Value\n"

        -- Get the time series
        let startDate = UTCTime (fromGregorian 2015 01 01) 0
        let jobTask ticker = do 
                print $ "Downloading: " ++ ticker

                -- *************************************************
                -- Determine the length of the series date1 vs date2
                -- *************************************************

                ts <- priceTimeSeriesWithDate ticker startDate
        
                -- Save time series to file
                -- sendTS2File "testFile_hts.csv" ts

                -- Save to database
                dbFunction (sendTS2DB ticker ts) conpool 

                dbFunction (queryIdTSSendEm2File ticker outFile) conpool
                
        forM_ tickers jobTask

                -- *************************************************
                -- Think about the report !
                -- *************************************************

        -- Query company's Id, time series and save the later two file
        dbFunction (addTextFile2DB outFile "") conpool 

        print ("TS Download Job: Going to Sleep!" :: String)
        threadDelay timeDelay


main :: IO ()
main = do
    
    persistConfig <- perstConfig

    pool <- createPoolConfig persistConfig

    dbFunction (runMigration migrateAll) pool
    flip dbFunction pool (buildDb "IBM Inc" "www.ibm.com" "IBM")
    flip dbFunction pool (buildDb "Microsoft Inc" "www.microsoft.com" "MSFT")

    -- Download price time series
    -- let sleepTime = (10^6 * 60 * 5) :: Int
    let sleepTime = (10^6 * 3600 * 6) :: Int
    _ <- forkIO $ tsDownloadJob ["IBM", "MSFT"] sleepTime pool

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


-- -- To be done
-- 2. Create DB - 3) DB update schedule
-- 2. 2 dates - functions?
-- 3. Update most recent values
-- 4. News

