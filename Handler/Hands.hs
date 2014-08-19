module Handler.Hands where

import Import

getHandsR :: Handler Html
getHandsR = do
  let hands = []
  defaultLayout $ do
    $(widgetFile "handlist")

getBettorViewR :: HandId -> Handler Html
getBettorViewR = undefined

postBetsR :: HandId -> Int -> Handler Value
postBetsR = undefined
