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

insertOrUpdateBet :: ScrumBet -> YesodDB App ScrumBetId
insertOrUpdateBet bet@(ScrumBet handId userId _ _) = do
  maybeOldScrumBet <- getBy $ UniqueBet handId userId
  case maybeOldScrumBet of
    Nothing -> insert bet
    Just (Entity oldBetId _) -> replace oldBetId bet >> return oldBetId

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
  _ <- runDB $ get404 handId
  userId <- getOrGenerateUserId
  ((_, formWidget), _) <- runFormPost $ scrumBetForm handId userId
  mmesg <- getMessage
  defaultLayout $ do
    $(widgetFile "bettorview")

postBetsR :: HandId -> Int -> Handler ()
postBetsR handId userId = do
  _ <- runDB $ get404 handId
  ((formResult, _),_) <- runFormPost $ scrumBetForm handId userId
  case formResult of
    FormSuccess bet -> do
      _ <- runDB $ insertOrUpdateBet bet
      setMessage $ "Vote submitted."
      redirect $ BettorViewR handId
    _ -> do
      setMessage $ "Vote rejected."
      redirect $ BettorViewR handId
