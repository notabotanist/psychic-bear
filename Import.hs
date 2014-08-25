module Import
    ( module Import
    ) where

import           Prelude              as Import hiding (head, init, last,
                                                 readFile, tail, writeFile)
import           Yesod                as Import hiding (Route (..))

import           Control.Applicative  as Import (pure, (<$>), (<*>))
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

getHandList :: Handler [Entity Hand]
getHandList =
  runDB $ selectList [] [ Desc HandCreatedDate, LimitTo 5 ]

getBidList :: HandId -> Handler [Entity ScrumBet]
getBidList handId =
  runDB $ selectList [ ScrumBetHand ==. handId ] [ Asc ScrumBetLastUpdated ]

getUnanimous :: [ScrumBet] -> Maybe FibonacciSubset
getUnanimous [] = Nothing
getUnanimous ((ScrumBet _ _ v _):bs)
  | all ((v==).scrumBetValue) bs = Just v
  | otherwise                    = Nothing

makeHandTitleText :: HandId -> Text
makeHandTitleText handId = append
  ("Hand " :: Text)
  (((either pack id).fromPersistValueText.unKey) handId)

appBarWidget :: Text -> Widget
appBarWidget appBarTitle = do
  primaryColor <- fmap extraPrimaryColor $ handlerToWidget getExtra
  $(widgetFile "appbar")
