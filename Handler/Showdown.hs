module Handler.Showdown where

import Import
import Data.Time.Clock (getCurrentTime, addUTCTime)

getShowdownR :: HandId -> Handler Value
getShowdownR handId = undefined

postShowdownR :: HandId -> Handler Value
postShowdownR handId = do
  hand <- runDB $ get404 handId
  votingWindow <- extraVotingWindow <$> getExtra
  showdownTime <- addUTCTime (fromIntegral votingWindow) <$> (lift $ liftIO getCurrentTime)
  runDB $ update handId [ HandShowdownTime =. Just showdownTime ]
  return . toJSON $ object [ "showdown_time" .= showdownTime ]
