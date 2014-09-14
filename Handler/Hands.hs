module Handler.Hands where

import Import
import Control.Arrow ((&&&))
import Data.Time (getCurrentTime, diffUTCTime)
import Safe (readMay)
import Data.Text (unpack, append)
import System.Random (randomRIO)

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
scrumBetForm handId userId = renderBootstrap3 BootstrapBasicForm $ ScrumBet
  <$> pure handId
  <*> pure userId
  <*> areq (selectFieldList fibs) (bfs ("Value" :: Text)) (Just F0)
  <*> lift (liftIO getCurrentTime)
  where
    fibs = map (toText &&& id) [(F0)..]

generalUnanimousWidget :: HandId -> Hand -> [Entity ScrumBet] -> Widget
generalUnanimousWidget handId hand betEntities = do
  curTime <- liftIO $ getCurrentTime
  case ((curTime<) &&& id) <$> handShowdownTime hand of
    (Just (True, showdownTime)) -> do
      let secondsToConsensus = ceiling $ diffUTCTime showdownTime curTime :: Int
      unanimousWidget handId betEntities
      toWidget [julius|startCountdown(#{toJSON secondsToConsensus});|]
    (Just (False, _))           -> unanimousStaticWidget betEntities
    Nothing                     -> mempty

getHandsR :: Handler Html
getHandsR = do
  defaultLayout $ do
    appBarWidget "Hands" Nothing
    handListWidget BettorViewR

getBettorViewR :: HandId -> Handler Html
getBettorViewR handId = do
  hand <- runDB $ get404 handId
  userId <- getOrGenerateUserId
  ((_, formWidget), _) <- runFormPost $ scrumBetForm handId userId
  mmesg <- getMessage
  betEntities <- getBidList handId
  defaultLayout $ do
    appBarWidget (makeHandTitleText handId) (Just HandsR)
    generalUnanimousWidget handId hand betEntities
    $(widgetFile "bettorview")

postBetsR :: HandId -> Int -> Handler ()
postBetsR handId userId = do
  hand <- runDB $ get404 handId
  curTime <- liftIO $ getCurrentTime
  case ((<) curTime) <$> handShowdownTime hand of
    Just True -> do
      ((formResult, _),_) <- runFormPost $ scrumBetForm handId userId
      case formResult of
        FormSuccess bet -> do
          _ <- runDB $ insertOrUpdateBet bet
          setMessage . toHtml $ prependFS bet " - Bet submitted."
          redirect $ BettorViewR handId
        _ -> reject "Bet rejected."
    Just False -> reject "Bet rejected - round over"
    Nothing -> reject "Betting not yet open"
  where
    prependFS fs = append (toText . scrumBetValue $ fs)
    reject msg = do
      setMessage msg
      redirect $ BettorViewR handId
