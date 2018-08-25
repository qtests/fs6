{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses      #-}

module Model 
where

import Data.ByteString (ByteString)
import Data.Text (Text)
import Database.Persist.Quasi
import Yesod
import Data.Typeable ()
import Data.Time

import Control.Monad.Trans.Reader (ReaderT)
import Database.Persist.Sql (SqlBackend)
import Control.Monad.Logger (LoggingT)
import Control.Monad.Trans.Resource (ResourceT)
-- https://hackage.haskell.org/package/bytestring-0.10.8.2/docs/Data-ByteString.html
import qualified Data.ByteString as S
import Data.Text (pack)
import Data.List (intercalate)

-- import Config


share [mkPersist sqlSettings, mkMigrate "migrateAll"]
    $(persistFileWith lowerCaseSettings "config/models")


addTextFile2DB :: String -> String -> ReaderT SqlBackend (LoggingT (ResourceT IO)) ()
addTextFile2DB fileName filePath = do
    fileContents <- liftIO $ S.readFile $ filePath ++ fileName
    time <- liftIO getCurrentTime
    insert_ $ StoredFile (pack fileName) "text/plain" fileContents time


sendTS2File :: String -> Either String [(UTCTime, [Double])] -> IO ()
sendTS2File filePath (Left _) = writeFile filePath ""
sendTS2File filePath (Right []) = writeFile filePath ""
sendTS2File filePath (Right timeSeries) =
    let (times, values) = unzip timeSeries
        tsString = mconcat $ ["Date,Value\n"] ++ 
            zipWith (\x y -> mconcat [show x,",", intercalate ", " $ fmap show y,"\n"] ) times values
    in writeFile filePath tsString

