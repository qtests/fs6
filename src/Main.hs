{-# LANGUAGE OverloadedStrings #-}


-- | This module initializes the application's state and starts the warp server.
module Main where
-- import Control.Concurrent.STM
-- import Data.IntMap
import Yesod
import Database.Persist.Sql
import Data.Pool

import Data.Text (pack)
import Data.Time

import Control.Monad
import Control.Monad.Logger
import Control.Monad.Trans.Resource
import Control.Monad.Trans.Reader
import Control.Concurrent

-- https://hackage.haskell.org/package/bytestring-0.10.8.2/docs/Data-ByteString.html
import qualified Data.ByteString as S

import Dispatch
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


doMigrations :: ReaderT SqlBackend (LoggingT (ResourceT IO)) ()
doMigrations = do 
    runMigration migrateAll


doDbStuff :: ReaderT SqlBackend (LoggingT (ResourceT IO)) ()
doDbStuff = do

    maybeIBM <- getBy $ UniqueTicker "IBM" True
    case maybeIBM of
        Nothing -> do
                      liftIO $ putStrLn "Just kidding, IBM is not really there !"
                      insert_ $ Company  "IBM Inc" "www.ibm.com" "IBM" True
                      return ()
        Just (Entity companyId cpny) -> liftIO $ print cpny
 

addTextFile2DB :: String -> String -> ReaderT SqlBackend (LoggingT (ResourceT IO)) ()
addTextFile2DB fileName filePath = do
    fileContents <- liftIO $ S.readFile $ filePath ++ fileName
    time <- liftIO getCurrentTime
    insert_ $ StoredFile (pack fileName) "text/plain" fileContents time


tsDownloadJob :: [String] -> Int -> ConnectionPool -> IO ()
tsDownloadJob tickers timeDelay conpool = 
    forever $ do
        print "TS Download Job: Downloading data!"
        downloadH2File tickers

        let loadFile = addTextFile2DB "testFile_hd.csv" ""
        dbFunction loadFile conpool 

        print "TS Download Job: Going to Sleep!"
        threadDelay timeDelay


main :: IO ()
main = do
    
    persistConfig <- perstConfig

    pool <- createPoolConfig persistConfig

    dbFunction doMigrations pool
    dbFunction doDbStuff pool 

    -- Download price time series
    -- let sleepTime = (10^6 * 60 * 5) :: Int
    let sleepTime = (10^6 * 3600 * 4) :: Int
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
