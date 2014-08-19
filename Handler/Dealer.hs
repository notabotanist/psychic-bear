module Handler.Dealer where

import Import
import Safe (maximumDef)

getHandList =
  runDB $ selectList [] [ Desc HandCreatedDate, LimitTo 5 ]

getDealerR :: Handler Html
getDealerR = do
  hands <- fmap (map (\(Entity handid _) -> handid)) getHandList
  let handlistWidget = $(widgetFile "handlist")
  defaultLayout $ do
    $(widgetFile "dealer")

postDealerR :: Handler Html
postDealerR = undefined

getDealerViewR :: HandId -> Handler Html
getDealerViewR = undefined
