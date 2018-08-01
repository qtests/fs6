{-# LANGUAGE OverloadedStrings #-}


module Yadata.LibAPI
(
    downloadH2File
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
priceTimeSeries ticker = priceTSWithSource "yahoo" ticker


transformData :: Either String [String] -> String -> Either String [String] -> Either String [(UTCTime, Double)]
transformData (Left a) _ _ = Left a
transformData _ _ (Left a) = Left a
transformData (Right []) _ _ = Right []
transformData _ _ (Right []) = Right []
transformData indexes dateFormat values = do 
    ids <- indexes
    vals <- values
    let dates = readClean2UTCTime dateFormat ids
    let numbers = fmap read2DoubleMaybe vals
    let nidx = findIndices isNothing numbers
    return $ zip (removeAtIndexList nidx dates) (catMaybes numbers ) 
                 

priceTSWithSource :: String -> String -> IO (Either String [(UTCTime, Double)] )
priceTSWithSource source ticker
   | source == "yahoo" = do ydata <- getYahooData ticker :: IO (Either YahooException C.ByteString)
                            let dcsv = either (\_ -> Left YStatusCodeException) id
                                     (mapM (\x -> parseCSV "Ticker" (DBLU.toString x )) ydata)
                            let dates = getColumnInCSVEither dcsv "Date"
                            let closep = getColumnInCSVEither dcsv "Adj Close"
                            return $ transformData dates "%Y-%m-%d" closep

   | otherwise                    =     return $ Left "priceTSWithSource: Unknown source!"
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