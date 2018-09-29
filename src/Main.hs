{-# LANGUAGE OverloadedStrings #-}


-- | This module initializes the application's state and starts the warp server.
module Main where
-- import Control.Concurrent.STM
-- import Data.IntMap
import Yesod
import Database.Persist.Sql (ConnectionPool, SqlBackend, runSqlPool, runMigration)
import Data.Pool (Pool(..))
import Data.Maybe (fromJust, isJust, isNothing, fromMaybe)
import Data.Text (pack)
import Data.Time (UTCTime(..), getCurrentTime, addGregorianMonthsClip, formatTime, defaultTimeLocale)

 
import Control.Monad (forever, forM_, forM)
import Control.Monad.Logger (LoggingT, runStderrLoggingT)
import Control.Monad.Trans.Resource (ResourceT, runResourceT)
import Control.Monad.Trans.Reader (ReaderT)
import Control.Concurrent (threadDelay, forkIO)
import System.ReadEnvVar (readEnvDef)


import Dispatch ()
import Foundation
import Config
import Model

import Yadata.LibAPI
import Parsers.INIParser

-- News
import Text.NewsAPI


-- DB links
-- https://github.com/agrafix/users
-- https://github.com/Daiver/HBlog

-- *********************************************************************************************** -- 
-- Sector picture
-- *********************************************************************************************** -- 

getDBTS2XTS :: [String] -> [String] -> Int -> ConnectionPool -> IO (XTS Double)
getDBTS2XTS  [] _ _ _ = return $ XTS [] [] []
getDBTS2XTS  companyTickers companyNames colNum pool = do
    xtsList <- forM companyTickers $ \tk -> do 
        (timeId, dta, _) <- getDBTS2Tup tk pool
        if timeId == [] || dta == [] 
            then return $ TS [] []
            else return $ TS timeId (dta !! colNum)
    return $ foldl (\start (tk, ts) -> combineXTSnTS start tk ts) (XTS [] [] []) (zip companyNames xtsList)


updateMainImage :: [String] -> [String] -> Int -> Int -> ConnectionPool -> IO ()
updateMainImage companyTickers companyNames colNum timeDelay pool = do
    threadDelay ( (10^6 * 60 * 1) :: Int )
    let imgFile = "testFile_picture.png"
    forever $ do
            print ("Updatting Main Picture: Start" :: String)

            xts <- getDBTS2XTS  companyTickers companyNames colNum pool
            plotXTS  imgFile xts
            dbFunction (upsertStoredFile2DB imgFile "" "image/png" True False) pool 

            print ("Updatting Main Picture: End" :: String)
            threadDelay timeDelay


-- *********************************************************************************************** -- 
-- News
-- *********************************************************************************************** -- 

-- Add https://www.scmp.com/news/china/money-wealth

insertStoriesReuters :: Int -> ConnectionPool -> IO ()
insertStoriesReuters timeDelay pool = 
    forever $ do
        print ("News Job: Downloading stories!" :: String)
        now <- getCurrentTime
        topnews <- getTopStory
        fnews <- getFeatureStories
        snews <- getSideStories
        rssnews <- liftIO $ parseXml "http://feeds.reuters.com/reuters/businessNews"
        let topstories = mapM convertImageStory topnews now
            fstories = mapM convertImageStory fnews now
            sstories = mapM convertStory snews now
            rssstories = mapM convertRssFeed rssnews now
            allS = topstories <> fstories <> sstories <> rssstories
        mapM_ (\s -> dbFunction (checkStorySaved s) pool) allS
        
        print ("News Job: Going to Sleep!" :: String)
        threadDelay timeDelay


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
tsDownloadJob :: [String] -> Int -> UTCTime -> ConnectionPool -> IO ()
tsDownloadJob tickers timeDelay startDate conpool = 
    forever $ do
        print ("TS Download Job: Downloading data!" :: String)

        -- Initializing the output file
        let outFile = "testFile_htsdb.csv"
        writeFile outFile "Date,Value\n"

        -- Get the time series
        let jobTask ticker = do 
                print $ "Downloading: " ++ ticker

                ts <- priceTimeSeriesWithDate ticker startDate
        
                -- Save time series to file
                -- sendTS2File "testFile_hts.csv" ts

                -- Save to database
                dbFunction (sendTS2DB ticker ts) conpool 

                -- Save to file
                dbFunction (queryIdTSSendEm2File ticker outFile) conpool


        timeNow <- getCurrentTime
        let hourNow = (read $ formatTime defaultTimeLocale "%k" timeNow) :: Int
        if  (hourNow < 2 || (hourNow > 16 && hourNow < 23) )
                            then 
                                do 
                                    forM_ tickers jobTask

                                    -- Add the output file to the database
                                    dbFunction (upsertStoredFile2DB outFile "" "text/plain" True True) conpool 
                            else  return ()
    

        -- *************************************************
        -- Think about the report !
        -- *************************************************


        print ("TS Download Job: Going to Sleep!" :: String)
        threadDelay timeDelay


main :: IO ()
main = do
    
    persistConfig <- perstConfig

    pool <- createPoolConfig persistConfig

    -- Config.ini
    iniContents' <- readFile ("./config/config.ini")
    let iniContents = runParser ini iniContents'
    
    if (isNothing iniContents) 
        then 
            error "Could not parse INI file !"
        else
            do 
                print ("Config.ini: " :: String)
                print iniContents
    
    let env = fst $ fromJust iniContents

    -- Time Series start date
    let stDateString = fromMaybe "2015-01-01" $ lookupSectionVariable env "TimeSeries" "startDateBase"
    let startDate = fromJust $ read2MaybeUTCTime "%Y-%m-%d" stDateString
                    
    let varDateString = fromMaybe "1" $ lookupSectionVariable env "TimeSeries" "startDateVariable"
    let varDate = fromJust $ read2MaybeInteger varDateString
    timeNow <- getCurrentTime 
    let refreshPeriod = UTCTime (addGregorianMonthsClip (-varDate) (utctDay timeNow)) 0 
        
    
    -- Build the initial DB
    dbFunction (runMigration migrateAll) pool


    flip dbFunction pool (buildDb "SPDR Materials"      "http://www.spdrs.com" "XLB"   startDate)
    flip dbFunction pool (buildDb "SPDR Energy"         "http://www.spdrs.com" "XLE"   startDate)
    flip dbFunction pool (buildDb "SPDR Finance"        "http://www.spdrs.com" "XLF"   startDate)
    flip dbFunction pool (buildDb "SPDR Industrials"    "http://www.spdrs.com" "XLI"   startDate)
    flip dbFunction pool (buildDb "SPDR Consumer Staples" "http://www.spdrs.com" "XLP" startDate)
    flip dbFunction pool (buildDb "SPDR Utilities"      "http://www.spdrs.com" "XLU"   startDate)
    flip dbFunction pool (buildDb "SPDR Health Care"    "http://www.spdrs.com" "XLV"   startDate)
    flip dbFunction pool (buildDb "SPDR Consumer Discretionary" "http://www.spdrs.com" "XLY" startDate)
    flip dbFunction pool (buildDb "SPDR Technology"     "http://www.spdrs.com"         "XLK" startDate)


    -- Picture
    let sectorTks = ["XLB", "XLE", "XLF", "XLI", "XLP", "XLU", "XLV", "XLY", "XLK"]
    let sectorNames = ["Materials", "Energy", "Finance", "Industrials", "Consumer Staples", 
                        "Utilities", "Health Care",  "Consumer Discretionary", "Technology"]
   
    
    -- Plot picture
    let timeDelayMI = (10^6 * 3600 * 24) :: Int
    _ <- forkIO $ updateMainImage sectorTks sectorNames 0 timeDelayMI pool

    -- Download/Update price time series
    let sleepTimeTS = (10^6 * 3600 * 6) :: Int
    _ <- forkIO $ tsDownloadJob sectorTks sleepTimeTS refreshPeriod pool

    -- Stories
    let sleepTimeSR = (10^6 * 60 * 30) :: Int
    _ <- forkIO $ insertStoriesReuters sleepTimeSR pool
 
    -- Starting the web server
    port <- readEnvDef "PORT" 8080
    -- warp port $ App tident tstore pool persistConfig
    warp port $ App pool persistConfig
