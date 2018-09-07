{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

-- | This module contains the GET handler for files' preview pages.
module Handler.Preview where

import Control.Exception hiding (Handler)
-- import qualified Data.ByteString.Lazy as LB
import qualified Data.ByteString as SB
import Data.Default
import Data.Text (Text)
import qualified Data.Text as Text
-- import qualified Data.Text.Lazy as LT
-- import qualified Data.Text.Lazy.Encoding as LT
import qualified Data.Text.Encoding as Text
import Text.Blaze
import Yesod
import Yesod.Default.Util

import Foundation
import Model


-- | The preview page display's the file's name, a hyperlink to download it, and a preview.
-- getPreviewR :: Int -> Handler Html
getPreviewR :: Key StoredFile -> Handler Html
getPreviewR ident = do
    StoredFile filename contentType bytes timeCreated <- getById ident
    defaultLayout $ do
        setTitle . toMarkup $ "Dashboard - " `Text.append` filename
        previewBlock <- liftIO $ preview ident contentType bytes
        $(widgetFileNoReload def "preview")

-- | Generate a block of HTML to include in the preview section.
-- preview :: Int -> Text -> LB.ByteString -> IO Widget
-- preview :: Int -> Text -> SB.ByteString -> IO Widget
preview :: Key StoredFile -> Text -> SB.ByteString -> IO Widget
preview ident contentType bytes
  -- Image files have a MIME type starting with "image" by convention.
  | "image/" `Text.isPrefixOf` contentType =
    return [whamlet|<img src=@{DownloadR ident}>|]
  -- Other preview handlers should be inserted here.
  | otherwise = do
    -- Try to display the file as raw text as a last resort. Only files that
    -- are encoded as UTF-8 are displayed. The 'decodeUtf8' function is pure,
    -- but throws an exception in the IO monad when an error is
    -- encountered.
    -- eText <- try . evaluate $ LT.decodeUtf8 bytes :: IO (Either SomeException LT.Text)
    eText <- try . evaluate $ Text.decodeUtf8 bytes :: IO (Either SomeException Text)
    return $ case eText of
      Left _ -> errorMessage
      Right text -> [whamlet|<pre>#{text}|]
  where
    errorMessage = [whamlet|<pre>Unable to display file contents.|]
