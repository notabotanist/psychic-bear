module Import
    ( module Import
    ) where

import           Prelude              as Import hiding (head, init, last,
                                                 readFile, tail, writeFile)
import           Yesod                as Import hiding (Route (..))
import          Yesod.Form.Bootstrap3 as Import

import           Control.Applicative  as Import (pure, (<$>), (<*>), (<*))
import           Data.Text            as Import (Text, pack)

import           Foundation           as Import
import           Model                as Import
import           Settings             as Import
import           Settings.Development as Import
import           Settings.StaticFiles as Import

#if __GLASGOW_HASKELL__ >= 704
import           Data.Monoid          as Import
                                                 (Monoid (mappend, mempty, mconcat),
                                                 (<>))
#else
import           Data.Monoid          as Import
                                                 (Monoid (mappend, mempty, mconcat))

infixr 5 <>
(<>) :: Monoid m => m -> m -> m
(<>) = mappend
#endif

import           Model.ScrumBet       as Import

-- helper functions
import Data.Text (append)
import Yesod.Core (Route)
import Data.Time.Format.Human (humanReadableTime')
import Data.Time.Clock (getCurrentTime)

getHandList :: Handler [Entity Hand]
getHandList =
  runDB $ selectList [] [ Desc HandCreatedDate, LimitTo 5 ]

getBidList :: HandId -> Handler [Entity ScrumBet]
getBidList handId =
  runDB $ selectList [ ScrumBetHand ==. handId ] []

getUnanimous :: [ScrumBet] -> Maybe FibonacciSubset
getUnanimous [] = Nothing
getUnanimous ((ScrumBet _ _ v _):bs)
  | all ((v==).scrumBetValue) bs = Just v
  | otherwise                    = Nothing

makeHandTitleText :: HandId -> Text
makeHandTitleText handId = append
  ("Hand " :: Text)
  (((either pack id).fromPersistValueText.unKey) handId)

-- Global Widgets

appBarWidget :: Text -> Maybe (Route App) -> Widget
appBarWidget appBarTitle mbackUrl = do
  primaryColor <- fmap extraPrimaryColor $ handlerToWidget getExtra
  $(widgetFile "appbar")

mondoButtonWidget :: Text -> Widget
mondoButtonWidget buttonText = do
  $(widgetFile "mondobutton")

handListWidget :: (HandId -> Route App) -> Widget
handListWidget handView = do
  handEntities <- handlerToWidget getHandList
  curTime <- liftIO getCurrentTime
  $(widgetFile "handlist")
