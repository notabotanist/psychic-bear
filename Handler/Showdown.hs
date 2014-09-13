module Handler.Showdown where

import Import
import Data.Time.Clock (getCurrentTime, addUTCTime)
import Data.Aeson.Types (emptyObject)
import Control.Arrow ((&&&))

getShowdownR :: HandId -> Handler Value
getShowdownR handId = do
  hand <- runDB $ get404 handId
  curTime <- liftIO getCurrentTime
  case (curTime >=) <$> handShowdownTime hand of
    Nothing    -> return emptyObject
    Just False -> return emptyObject
    Just True  -> do
      bidEntities <- getBidList handId
      let mConsensus = getUnanimous (map entityVal bidEntities)
      return . toJSON $ object [ "consensus" .= (toText <$> mConsensus) ]

postShowdownR :: HandId -> Handler Value
postShowdownR handId = do
  hand <- runDB $ get404 handId
  votingWindow <- extraVotingWindow <$> getExtra
  curTime <- liftIO getCurrentTime
  case ((curTime <) &&& id) <$> (handShowdownTime hand) of
    (Just (True, stime)) -> do
      return . toJSON $ object [ "showdown_time" .= stime ]
    _                    -> do
      let showdownTime = addUTCTime (fromIntegral votingWindow) curTime
      runDB $ update handId [ HandShowdownTime =. Just showdownTime ]
      return . toJSON $ object [ "showdown_time" .= showdownTime ]

