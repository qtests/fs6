{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
-- News
{-# LANGUAGE FlexibleInstances          #-}

module Model 

where

import Data.ByteString (ByteString)
import Data.Text (Text, concat, unpack)
import Database.Persist.Quasi
import qualified Database.Persist as P
import Yesod


import Control.Monad.Trans.Reader (ReaderT)
import Database.Persist.Sql (SqlBackend, ConnectionPool, runSqlPool)
import Control.Monad.Logger (LoggingT, runStderrLoggingT)
import Control.Monad.Trans.Resource (ResourceT, runResourceT)
-- https://hackage.haskell.org/package/bytestring-0.10.8.2/docs/Data-ByteString.html
import qualified Data.ByteString as S
import Data.Text (pack)
import Data.List (intercalate)
import Data.Maybe (fromJust, isJust)
import Data.List (transpose, zipWith5)
import Data.Typeable ()
import Data.Time
import Data.Pool (Pool(..))

import Control.Monad (forM_)

-- News
import Data.Aeson
import Data.Hashable

import Yadata.LibAPI (priceTimeSeriesWithDate)

share [mkPersist sqlSettings, mkMigrate "migrateAll"]
    $(persistFileWith lowerCaseSettings "config/models")


upsertStoredFile2DB :: String -> String -> String -> Bool -> Bool -> ReaderT SqlBackend (LoggingT (ResourceT IO)) ()
upsertStoredFile2DB fileName filePath fileType isItInternal published = do
    fileContents <- liftIO $ S.readFile $ filePath ++ fileName
    time <- liftIO getCurrentTime
    fid <- selectFirst [StoredFileName ==. (pack fileName), StoredFileInternal ==. isItInternal] [Desc StoredFileCreated]
    case fid of 
        Nothing                -> insert_ $ 
                    StoredFile (pack fileName) (pack fileType) fileContents isItInternal published time
        Just (Entity fileId _) -> replace fileId $ 
                    StoredFile (pack fileName) (pack fileType) fileContents isItInternal published time


sendTS2File :: String -> String -> Bool -> Either String [(UTCTime, [Double])] -> IO ()
sendTS2File _ filePath _ (Left _) = writeFile filePath ""
sendTS2File _ filePath _ (Right []) = writeFile filePath ""
sendTS2File ticker filePath append (Right timeSeries) =
    let (times, values) = unzip timeSeries
        (fileWriter, beginingString) = if append then (appendFile, "") else (writeFile ,"Date,Value\n")        
        tsString = mconcat $ [beginingString] ++ 
            zipWith3 (\x y z -> mconcat 
                [show x,",", intercalate ", " (fmap show y), ", ", z, "\n"] ) times values (repeat ticker)
        
    in fileWriter filePath tsString

-- Maybe one could take out: maybeCpny <- getBy $ UniqueTicker (pack ticker) True 
sendTS2DB :: String -> Either String [(UTCTime, [Double])] -> ReaderT SqlBackend (LoggingT (ResourceT IO)) ()
sendTS2DB _ (Left _ ) = return ()
sendTS2DB _ (Right []) = return ()
sendTS2DB ticker (Right timeSeries) = do
    maybeCpny <- getBy $ UniqueTicker (pack ticker) True 
    if (isJust maybeCpny)   
        then
                let (Entity companyId cpny) = fromJust maybeCpny
                    (index, dta) = unzip timeSeries

                    [close, adjclose, volume] = transpose dta
                    dbts = zipWith5 TimeSeries (repeat companyId) index close adjclose volume 
                                    
                -- Insert data
                in putMany dbts -- forM_ dbts insertUnique 

        else
            return () 


buildDb :: Text -> Text -> Text -> UTCTime -> ReaderT SqlBackend (LoggingT (ResourceT IO)) ( )
buildDb name website ticker startDate = do

    maybeIBM <- getBy $ UniqueTicker ticker True
    case maybeIBM of
        Nothing -> do
                        liftIO $ putStrLn $ unpack $ Data.Text.concat ["Just kidding, ", name, " is not really there !"]
                        companyId <- insert $ Company name website ticker True

                        ts' <- liftIO $ priceTimeSeriesWithDate (unpack ticker) startDate
                        let ts = either (\_ -> []) (id) ts'
                        let (index, dta) = unzip ts
                        let [close, adjclose, volume] = transpose dta
                        let dbts = zipWith5 TimeSeries (repeat companyId) index close adjclose volume 
                        
                        forM_ dbts insert_ 

        Just (Entity companyId cpny) -> liftIO $ print cpny


-- Get company id from the Company table
getCompanyID :: String -> Bool ->  ReaderT SqlBackend (LoggingT (ResourceT IO)) ( Maybe (Key Company) )
getCompanyID ticker active = do 
    maybeCpny <- getBy $ UniqueTicker (pack ticker) active
    if (isJust maybeCpny)   
        then
            return $ fmap(\(Entity companyId _) -> companyId) maybeCpny
        else
            return Nothing



-- Query time series from TimeSeries table
getCompanyRawTS :: Key Company -> ReaderT SqlBackend (LoggingT (ResourceT IO)) ( [(UTCTime, [Double])] )
getCompanyRawTS companyId = do
    tsRecords <- selectList [TimeSeriesTsid ==. companyId ] [ Asc TimeSeriesRefdate ]
    return $ fmap (\(Entity _ (TimeSeries _ refDate close adjclose vol)) -> 
                                                        (refDate, [close, adjclose, vol]) ) tsRecords



dbFunction :: ReaderT SqlBackend (LoggingT (ResourceT IO)) a -> Pool SqlBackend  -> IO a
dbFunction query pool = runResourceT $ runStderrLoggingT $ runSqlPool query pool



getDBTS2Tup :: String -> ConnectionPool -> IO ( ([UTCTime], [[Double]], [String]) )
getDBTS2Tup ticker conpool = do
    cid <- dbFunction (getCompanyID ticker True) conpool
    case cid of 
        Nothing   -> return $ ([], [], [])
        Just id   -> do
            ts <- dbFunction (getCompanyRawTS id) conpool
            let (index, dta) = unzip ts
            return ( index,  (transpose dta),  ["Close", "Adjclose", "Volume"])

-- **********************************************************************
-- News
-- **********************************************************************

instance ToJSON (P.Entity Story) where
    toJSON (P.Entity _ p) = object
        [ "title"   .= storyTitle p
        , "link"    .= storyLink p
        , "content" .= storyContent p
        , "image"   .= storyImage p
        ]


makeHash
  :: Hashable a
  => a -> Int
makeHash = hash


checkStorySaved :: Story -> ReaderT SqlBackend (LoggingT (ResourceT IO)) ()
checkStorySaved story = do
    insertedStory <- selectFirst [StoryHashId ==. storyHashId story] []
    case insertedStory of
        Nothing ->  do
                        _ <- insertUnique story
                        return ()
        Just _  ->  return ()

-- checkStorySaved :: Story -> IO (Maybe (Entity Story))
-- checkStorySaved story = do
--   insertedStory <- runDb $ selectFirst [StoryHashId ==. storyHashId story] []
--   case insertedStory of
--     Nothing -> do
--       _ <- runDb $ insert story
--       return Nothing
--     Just s -> return $ Just s