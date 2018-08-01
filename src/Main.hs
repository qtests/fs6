{-# LANGUAGE OverloadedStrings #-}

-- | This module initializes the application's state and starts the warp server.
module Main where
-- import Control.Concurrent.STM
-- import Data.IntMap
import Yesod
import Database.Persist.Sql

import Control.Monad.Logger
import Control.Monad.Trans.Resource
import Control.Monad.Trans.Reader

import Dispatch ()
import Foundation
import Config
import Model (migrateAll)

import System.ReadEnvVar (lookupEnvDef, readEnvDef)

import Model
import Data.Pool


doMigrations :: ReaderT SqlBackend (LoggingT (ResourceT IO)) ()
doMigrations = do 
    runMigration migrateAll


doDbStuff :: ReaderT SqlBackend (LoggingT (ResourceT IO)) ()
doDbStuff = do

    maybeIBM <- getBy $ UniqueTicker "IBM" True
    case maybeIBM of
        Nothing -> do
                      liftIO $ putStrLn "Just kidding, not really there"
                      insert $ Company "IBM Inc" "www.ibm.com" "IBM" True
                      return ()
        Just (Entity companyId cpny) -> liftIO $ print cpny
 

dbFunction :: ReaderT SqlBackend (LoggingT (ResourceT IO)) a -> Pool SqlBackend  -> IO a
dbFunction query pool = runResourceT $ runStderrLoggingT $ runSqlPool query pool


main :: IO ()
main = do
    
    persistConfig <- perstConfig

    pool <- createPoolConfig persistConfig

    dbFunction doMigrations pool
    dbFunction doDbStuff pool 

    -- runResourceT $ runStderrLoggingT $ flip runSqlPool pool 
    --    cnyid <- liftIO $ insert $ Company "IBM Inc" "www.ibm.com" "IBM" True

    -- Initialize the filestore to an empty map.
    --tstore <- atomically $ newTVar empty

    -- The first uploaded file should have an ID of 0.
    -- tident <- atomically $ newTVar 0

    -- warpEnv starts the Warp server over a port defined by an environment
    -- variable. To launch the app on a specific port use 'warp'.
    -- warpEnv $ App tident tstore

    port <- readEnvDef "PORT" 8080
    -- warp port $ App tident tstore pool persistConfig
    warp port $ App pool persistConfig
