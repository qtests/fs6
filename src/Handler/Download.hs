{-# LANGUAGE OverloadedStrings #-}

-- | This module contains the GET handler for hyperlinks to files.
module Handler.Download where

import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text

import Yesod

import Foundation
import Model

-- | Search the application\'s 'Store' for a file keyed on ident. Return a 404
-- HTTP response if no file is found. In the case that the file exists, return
-- it along with appropriate HTTP headers so that web browsers will download
-- it as a file.
-- getDownloadR :: Int -> Handler TypedContent
getDownloadR :: Key StoredFile -> Handler TypedContent
getDownloadR ident = do
    -- Attempt to retrieve the file, failing with a 404.
    StoredFile filename contentType bytes <- getById ident
    -- The Content-Distribution header hints that the resource should be
    -- downloaded as a file.
    addHeader "Content-Disposition" $ Text.concat
        [ "attachment; filename=\"", filename, "\""]
    -- contentType will be checked by web browsers that choose to ignore the
    -- Content-Distribution header. Calling 'encodeUtf8' is safe because
    -- contentType was originally populated by an HTTP Request.
    sendResponse (Text.encodeUtf8 contentType, toContent bytes)
