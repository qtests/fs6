
{-# LANGUAGE OverloadedStrings #-}

-- |This module defines how the 'persistent' library's back end is configured.
-- Other modules should not import anything Sqlite-specific
module Config where

-- import Database.Persist.Sqlite
import  Database.Persist.Postgresql
import  System.ReadEnvVar (lookupEnvDef, readEnvDef)


perstConfig :: IO PostgresConf
perstConfig  = do 
    dbConnNum <- readEnvDef "DATABASE_CONNECTION_NUM" 100
    dbConnectionString <- lookupEnvDef "DATABASE_URL" "postgres://mydbuser:mydbpass@localhost:5432/mydb"
    return $ PostgresConf dbConnectionString dbConnNum






