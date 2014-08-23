module Handler.Hands where

import Import
import Control.Arrow ((&&&))
import Data.Time (getCurrentTime)

import Model.ScrumBet

userIdSessionKey :: Text
userIdSessionKey = "scrumuserid"

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
  let hands = []
      handView = BettorViewR
  defaultLayout $ do
    $(widgetFile "handlist")

getBettorViewR :: HandId -> Handler Html
getBettorViewR handId = do
  (formWidget, _) <- generateFormPost $ scrumBetForm handId 0
  defaultLayout $ [whamlet|
    <form method=post action=@{BetsR handId 0}>
      ^{formWidget}
      <input type=submit value="Place your bet">
    |]

postBetsR :: HandId -> Int -> Handler Value
postBetsR = undefined
