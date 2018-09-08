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
import Data.Time (UTCTime(..), fromGregorian, getCurrentTime, addGregorianMonthsClip)
import Data.List (transpose)
 
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
import Parsers.INIParser

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

                dbFunction (queryIdTSSendEm2File ticker outFile) conpool
                
        forM_ tickers jobTask

        -- *************************************************
        -- Think about the report !
        -- *************************************************

        -- Add the output file to the database
        dbFunction (addTextFile2DB outFile "") conpool 

        print ("TS Download Job: Going to Sleep!" :: String)
        threadDelay timeDelay



getDBTS2XTS :: String -> ConnectionPool -> IO ( ([UTCTime], [[Double]], [String]) )
getDBTS2XTS ticker conpool = do
    cid <- dbFunction (getCompanyID ticker True) conpool
    case cid of 
        Nothing   -> return $ ([], [], [])
        Just id   -> do
            ts <- dbFunction (getCompanyRawTS id) conpool
            let (index, dta) = unzip ts
            return ( index,  (transpose dta),  ["Close", "Adjclose", "Volume"])


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
                print "Config.ini: "
                print iniContents
    
    let env = fst $ fromJust iniContents

    -- Time Series start date
    let stDateString = fromMaybe "2000-01-01" $ lookupSectionVariable env "TimeSeries" "startDateBase"
    let startDate = fromJust $ read2MaybeUTCTime "%Y-%m-%d" stDateString
                    
    let varDateString = fromMaybe "3" $ lookupSectionVariable env "TimeSeries" "startDateVariable"
    let varDate = fromJust $ read2MaybeInteger varDateString
    timeNow <- getCurrentTime 
    let refreshPeriod = UTCTime (addGregorianMonthsClip (-varDate) (utctDay timeNow)) 0 
        
    
    -- Build the initial DB
    dbFunction (runMigration migrateAll) pool

    flip dbFunction pool (buildDb "IBM Inc."        "www.ibm.com"       "IBM"  startDate)
    flip dbFunction pool (buildDb "Microsoft Inc."  "www.microsoft.com" "MSFT" startDate)

    flip dbFunction pool (buildDb "SPDR Materials"      "http://www.spdrs.com" "XLB"   startDate)
    flip dbFunction pool (buildDb "SPDR Energy"         "http://www.spdrs.com" "XLE"   startDate)
    flip dbFunction pool (buildDb "SPDR Finance"        "http://www.spdrs.com" "XLF"   startDate)
    flip dbFunction pool (buildDb "SPDR Industrials"    "http://www.spdrs.com" "XLI"   startDate)
    flip dbFunction pool (buildDb "SPDR Consumer Staples" "http://www.spdrs.com" "XLP" startDate)
    flip dbFunction pool (buildDb "SPDR Utilities"      "http://www.spdrs.com" "XLU"   startDate)
    flip dbFunction pool (buildDb "SPDR Health Care"    "http://www.spdrs.com" "XLV"   startDate)
    flip dbFunction pool (buildDb "SPDR Consumer Discretionary" "http://www.spdrs.com" "XLY" startDate)
    flip dbFunction pool (buildDb "SPDR Technology"     "http://www.spdrs.com"         "XLK" startDate)


    -- Download/Update price time series
    let sleepTime = (10^6 * 3600 * 6) :: Int
    _ <- forkIO $ tsDownloadJob ["IBM", "MSFT"] sleepTime refreshPeriod pool

    
    -- Picture
    let sectorTS = ["XLB", "XLE", "XLF", "XLI", "XLP", "XLU", "XLV", "XLY", "XLK"]
    let sTicker = "XLB"
    (timeId, dta, colNames) <- getDBTS2XTS sTicker pool
    let ts = TS timeId (dta !! 0)
    let xts = combineXTSnTS (XTS [] [] []) sTicker ts
    -- Do the same above for all tickers
    -- Plot picture
    plotXTS "testFile_picture.png" xts

    

    port <- readEnvDef "PORT" 8080
    -- warp port $ App tident tstore pool persistConfig
    warp port $ App pool persistConfig
