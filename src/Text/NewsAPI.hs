
module Text.NewsAPI
(
       R.parseXml
    ,  getTopStory
    ,  getFeatureStories
    ,  getSideStories
    ,  convertImageStory
    ,  convertStory
    ,  convertRssFeed
)
where

import qualified Text.HTML.Freader as R
import qualified Text.HTML.Fscraper as F

import Data.Pool (Pool(..))
import Database.Persist.Sql (ConnectionPool, SqlBackend)

import Control.Monad.IO.Class (MonadIO, liftIO) 
import Control.Monad.Logger (LoggingT)
import Control.Monad.Trans.Resource (ResourceT)
import Control.Monad.Trans.Reader (ReaderT)

import Data.Time (UTCTime(..), getCurrentTime)
import Data.Text (pack)

import Model


getTopStory :: MonadIO m => m [F.News]
getTopStory = do
  headStory <- liftIO $ F.topStory "olympics-topStory" F.reutersUrl
  case headStory of
    Nothing -> return []
    Just a -> return a


getFeatureStories :: MonadIO m => m [F.News]
getFeatureStories = do
  stories <- liftIO $ F.featureNews "column1" F.reutersUrl
  case stories of
    Nothing -> return []
    Just a -> return a


getSideStories :: MonadIO m => m [F.News]
getSideStories = do
  stories <- liftIO $ F.leftColumnNews "more-headlines" F.reutersUrl
  case stories of
    Nothing -> return []
    Just a -> return a

convertImageStory :: F.News -> UTCTime -> Story
convertImageStory news now =
  Story
  { storyHashId = makeHash (F.newstitle news)
  , storyTitle = pack $ F.newstitle news
  , storyLink = pack $ F.newslink news
  , storyContent = Just (pack $ F.newstext news)
  , storyImage = Just (pack $ F.newsimage news)
  , storyCreated = now
  }


convertStory :: F.News -> UTCTime -> Story
convertStory news now =
  Story
  { storyHashId = makeHash (F.newstitle news)
  , storyTitle = pack $ F.newstitle news
  , storyLink = pack $ F.newslink news
  , storyContent = Just (pack $ F.newstext news)
  , storyImage = Nothing
  , storyCreated = now
  }

convertRssFeed :: R.RssFeed -> UTCTime -> Story
convertRssFeed feed now =
  Story
  { storyHashId = makeHash (R.rssTitle feed)
  , storyTitle = R.rssTitle feed
  , storyLink = R.rssUrl feed
  , storyContent = Nothing
  , storyImage = Nothing
  , storyCreated = now
  }
