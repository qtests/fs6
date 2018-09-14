{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

-- | This module contains the GET and POST handler for the main page.
module Handler.Home where

import Control.Monad.Trans.Resource
import qualified Data.ByteString as S
import qualified Data.ByteString.Lazy as L
import Data.Conduit
import Data.Conduit.Binary
import Data.Default
import Yesod
import Yesod.Default.Util


import Data.Time

import Foundation
import Model


getHomeR :: Handler Html
getHomeR = do
    -- generateFormPost will create a block of HTML, 'formWidget' to include
    -- within a form block. We will still need to write minimal markup for the
    -- form. The 'formEncType' binding is to be used in the <form> tag's
    -- "enctype" attribute.
    (formWidget, formEncType) <- generateFormPost uploadForm
    -- getList returns a list of identifiers and their associated files. We
    -- use this to generate hyperlinks to summary pages.
    storedFiles <- getList
    
    defaultLayout $ do
        setTitle "File Processor"
        -- Load the widget defined in "templates/home.hamlet".
        $(widgetFileNoReload def "home")

postHomeR :: Handler Html
postHomeR = do
    -- runFormPost searches through the HTTP request for form fields, and then
    -- creates a corresponding Haskell type, a 'FileInfo' in this case. The
    -- return values I've ignored are used if we want this handler to return
    -- useful HTML markup in response to the form.
    ((result, _), _) <- runFormPost uploadForm
    -- Form data can be successfully parsed, invalid, or missing.
    case result of
      -- fi is a 'FileInfo'
      FormSuccess fi -> do
        -- Extract the file's raw contents into a lazy bytestring. Despite the
        -- type being lazy, calling 'sinkLbs' will fully evaluate the file
        -- contents.
        time <- liftIO getCurrentTime
        fileBytes <- runResourceT $ fileSource fi $$ sinkLbs
        addFile $ StoredFile (fileName fi) (fileContentType fi)
--                             fileBytes
                            (S.pack . L.unpack $ fileBytes) time
      _ -> return ()
    -- Users will see an updated file listing in the case of a successful
    -- upload. Should an error occur or invalid form data be supplied the list
    -- will remain unchanged.
    redirect HomeR

-- | Define a form with a single required field.
uploadForm :: Html -> MForm Handler (FormResult FileInfo, Widget)
uploadForm = renderDivs $ fileAFormReq "file"
