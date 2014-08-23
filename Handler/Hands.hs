module Handler.Hands where

import Import
import Control.Arrow ((&&&))
import Data.Time (getCurrentTime)
import Safe (readMay)
import Data.Text (unpack)
import System.Random (randomRIO)

import Model.ScrumBet

userIdSessionKey :: Text
userIdSessionKey = "scrumuserid"

-- These temporary anonymous user ids only need to be unique among the
-- participants of a hand for the duration of their sessions' timeouts.
generateUserId :: Handler Int
generateUserId = lift (liftIO $ randomRIO (0, 9999))

getOrGenerateUserId :: Handler Int
getOrGenerateUserId = do
  maybeTextUserId <- lookupSession userIdSessionKey
  case maybeTextUserId >>= (readMay . unpack) of
    Just userId -> return userId
    Nothing     -> do
      userId <- generateUserId
      setSession userIdSessionKey (pack . show $ userId)
      return userId

scrumBetForm :: HandId -> Int -> Form ScrumBet
scrumBetForm handId userId = renderDivs $ ScrumBet
  <$> pure handId
  <*> pure userId
  <*> areq (selectFieldList fibs) "Value" (Just F0)
  <*> lift (liftIO getCurrentTime)
  where
    fibs = map (toText &&& id) [(F0)..]

getHandsR :: Handler Html
getHandsR = do
  hands <- fmap (map (\(Entity handid _) -> handid)) (runDB $ selectList [] [ Desc HandCreatedDate, LimitTo 5 ])
  let handView = BettorViewR
  defaultLayout $ do
    $(widgetFile "handlist")

getBettorViewR :: HandId -> Handler Html
getBettorViewR handId = do
  userId <- getOrGenerateUserId
  (formWidget, _) <- generateFormPost $ scrumBetForm handId userId
  defaultLayout $ do
    $(widgetFile "bettorview")

postBetsR :: HandId -> Int -> Handler Value
postBetsR = undefined
