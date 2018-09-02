{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}



-- | This module is home to the foundation type, where all application state
-- is stored.
module Foundation where

import Control.Concurrent.STM ()
-- import Data.ByteString.Lazy (ByteString)
-- import Data.ByteString (ByteString)
import Data.Default
-- import qualified Data.IntMap as IntMap
-- import Data.Text (Text)
-- import qualified Data.Text as Text
import Database.Persist.Sql
import Text.Hamlet 
import Yesod
import Yesod.Default.Util

import  Database.Persist.Postgresql(PostgresConf)

-- import Config
import Model

-- | Extend this record to hold any information about uploaded files that you
-- need. Examples might be the time at which a file was uploaded, or the
-- identifier of the user account that added it.
--
-- All of these fields are initialized in the HomeR route's POST handler.
{- data StoredFile = StoredFile
    { -- | The file's name as it was supplied by the user's web browser.
      sfFilename :: !Text
    , -- | A MIME type. This is copied from the POST request verbatim. It is
      -- then supplied is the Content-Type header when the file is
      -- downloaded.
      sfContentType :: !Text
    , -- | This field contains the raw stream of bytes that were uploaded in
      -- the HomeR route's POST handler.
      sfFileBytes :: !ByteString
    } -}

-- | A collection of uploaded files keyed on a unique 'Int' identifier.
-- type Store = IntMap StoredFile

-- | This is the application\'s "foundation" type. The first argument to 'App'
-- is the next identifier to be used when a new file is uploaded. The second
-- is a mapping from identifiers to files.

-- data App = App (TVar Int) (TVar Store)
{- data App = App
    { tnextId :: TVar Int
    , tstore :: TVar Store
    , connPool :: ConnectionPool
    , persistConfig ::  PostgresConf
    } -}
data App = App
    {   connPool :: ConnectionPool
      , persistConfig ::  PostgresConf
    }

instance Yesod App where
  -- This method is customized so that global CSS styling can be defined in
  -- "templates/default-layout.cassius". This code is very similar to Yesod's
  -- scaffolding website.
  defaultLayout widget = do
    pc <- widgetToPageContent $ $(widgetFileNoReload def "default-layout")
    withUrlRenderer $(hamletFile "templates/default-layout-wrapper.hamlet")

-- Making 'App' an instance of RenderMessage is required for Yesod's form
-- controls.
instance RenderMessage App FormMessage where
  renderMessage _ _ = defaultFormMessage


instance YesodPersist App where
    type YesodPersistBackend App = SqlBackend
    -- runDB = defaultRunDB (const perstConfig) connPool 
    runDB = defaultRunDB persistConfig connPool 

instance YesodPersistRunner App where
    getDBRunner = defaultGetDBRunner connPool
-- Calling 'mkYesodData' generates boilerplate code and type aliases for
-- interfacing our foundation type with Yesod. The "Dispatch" module contains
-- a call to 'mkYesodDispatch', which performs the other half of boilerplate
-- generation.
mkYesodData "App" $(parseRoutesFile "config/routes")

-- | Use this function to generate a unique identifier for a newly uploaded
-- file. This should be the only function that manipulates the value stored in
-- the 'App' data constructor\'s first argument.
-- getNextId :: App -> STM Int
-- getNextId app = do
--     nextId <- readTVar $ tnextId app
--     writeTVar (tnextId app) $ nextId + 1
--     return nextId

-- | Generate a list of file's and identifiers. This is used on the main page
-- to generate links to preview pages.
-- getList :: Handler [(Int, StoredFile)]
-- getList = do
--     app <- getYesod
--     store <- liftIO . readTVarIO $ tstore app
--     return $ IntMap.toList store
getList :: Handler [Entity StoredFile]
getList = runDB $ selectList [] []

-- | Add a new file to the 'Store'.
addFile :: StoredFile -> Handler ()
-- addFile file = do
--     app <- getYesod
--     liftIO . atomically $ do
--         ident <- getNextId app
--         modifyTVar (tstore app) $ IntMap.insert ident file
addFile file = runDB $ insert_ file

-- | Retrieve a file from the application\'s 'Store'. In the case where the
-- file does not exist a 404 error will be returned.
-- getById :: Int -> Handler StoredFile
-- getById ident = do
--     app <- getYesod
--     store <- liftIO . readTVarIO $ tstore app
--     case IntMap.lookup ident store of
--       Nothing -> notFound
--       Just file -> return file
getById :: Key StoredFile -> Handler StoredFile
getById ident = do
    mfile <- runDB $ get ident
    case mfile of
      Nothing -> notFound
      Just file -> return file

-- ******************************************************************************
-- News
-- ******************************************************************************
getStoryList :: Handler [Entity Story]
getStoryList = runDB $ selectList [] [] 