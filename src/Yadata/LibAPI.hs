{-# LANGUAGE OverloadedStrings #-}


module Yadata.LibAPI
(
       downloadH2File
    ,  priceTimeSeriesWithDate
)
where

import Yadata.LibCSV
import Yadata.LibTS
import Yadata.LibYahoo

-- import CSV related functions
import qualified Data.ByteString.Lazy.Char8 as C
import qualified Data.ByteString.Lazy.UTF8 as DBLU
import Text.CSV

import Data.Time
import Data.Maybe
import Data.Either
import Data.List


priceTimeSeries :: String -> IO (Either String [(UTCTime, Double)] )
priceTimeSeries ticker = do
    let  startDate = UTCTime  (fromGregorian 2015 01 01) 0
    res <- priceTSWithSource "yahoo" ticker startDate
    case res of
       Right p  ->  do 
                    let (res1, res2) = unzip p
                    return $ Right $ zip res1 ((transpose res2) !! 0)
       Left p   ->  return $ Left p

        
priceTimeSeriesWithDate :: String -> UTCTime -> IO (Either String [(UTCTime, [Double])] )
priceTimeSeriesWithDate ticker startDate = priceTSWithSource "yahoo" ticker startDate


transformData :: Either String [String] ->  
                    String -> 
                        Either String [[String]] -> 
                            Either String [(UTCTime, [Double])]
transformData (Left a) _ _ = Left a
transformData _ _ (Left a)  = Left a
transformData (Right []) _ _ = Right []
transformData _ _ (Right []) = Right []
transformData indexes dateFormat values = do 
    ids <- indexes
    vals <- values
    let dates = readClean2UTCTime dateFormat ids
    let numbers = (fmap . fmap) read2DoubleMaybe vals
    let nidx = nub . concat $ fmap (findIndices isNothing) numbers 
    let rmAtIndLst = removeAtIndexList nidx
    return $ zip ( rmAtIndLst dates) ( transpose $ fmap (catMaybes . rmAtIndLst) numbers ) 
                 

priceTSWithSource :: String -> String -> UTCTime -> IO (Either String [(UTCTime, [Double])] )
priceTSWithSource source ticker startDate
   | source == "yahoo" = do endDate <- getCurrentTime
                            ydata <- getYahooHistoData ticker startDate endDate :: IO (Either YahooException C.ByteString)
                            let dcsv = either (\_ -> Left YStatusCodeException) id
                                     (mapM (\x -> parseCSV "Ticker" (DBLU.toString x )) ydata)
                            -- Get the right stuff
                            let dates = getColumnInCSVEither dcsv "Date"
                            let closep = getColumnInCSVEither dcsv "Close"
                            let adjclosep = getColumnInCSVEither dcsv "Adj Close"
                            let volume = getColumnInCSVEither dcsv "Volume"
                            -- return
                            return $ transformData dates "%Y-%m-%d"( sequenceA [closep, adjclosep, volume] )

   | otherwise         =    return $ Left "priceTSWithSource: Unknown source!"
-- ------------------------------------------
-- API
---------------------------------------------


downDataExt :: [String] -> [String] -> XTS Double -> IO (XTS Double, [String])
downDataExt [] tf accum = return (accum, tf)
downDataExt (tk:rest) tkFailed accum = do
    ts <- priceTimeSeries tk
    ts <- if (isLeft ts || (fmap length ts) == Right 0)
                then priceTimeSeries tk
                else return ts

    let tkf = if (isLeft ts || (fmap length ts) == Right 0)
                    then [tk]
                    else []

    let allD = combineXTSnTS accum tk (createTSEither ts)
    if (rest == []) then return (allD, tkFailed ++ tkf)
                    else downDataExt rest (tkFailed ++ tkf) allD


downloadH2File :: [String] -> IO ()
downloadH2File tickers = do
    print tickers
    (xts, tks) <- downDataExt tickers [] (createXTSRaw [] [] [])
    (xts, tks) <- if (length tks > 0)
                        then downDataExt tks [] xts
                        else return (xts, tks)
    -- print $ takeRowXTS 2 xts
    -- print tks
    writeFileXTS "testFile_hd.csv" xts
    writeFile "testFile_hd_errors.csv" $ show tks
    return ()

-- downloadH2File ["IBM", "MSFT", "AAPL", "KO" ]