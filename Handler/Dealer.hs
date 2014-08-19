module Handler.Dealer where

import Import
import Safe (maximumDef)
import Data.Time (getCurrentTime)

getHandList =
  runDB $ selectList [] [ Desc HandCreatedDate, LimitTo 5 ]

handForm :: Form Hand
handForm = renderDivs $ Hand
  <$> lift (liftIO getCurrentTime)

getDealerR :: Handler Html
getDealerR = do
  hands <- fmap (map (\(Entity handid _) -> handid)) getHandList
  (formWidget, _) <- generateFormPost $ handForm
  let handView = DealerViewR
      handlistWidget = $(widgetFile "handlist")
  defaultLayout $ do
    $(widgetFile "dealer")

postDealerR :: Handler Html
postDealerR = do
  ((result, _), _) <- runFormPost $ handForm
  case result of
    FormSuccess hand -> do
      handId <- runDB $ insert hand
      redirect $ DealerViewR handId
    _ -> getDealerR

getDealerViewR :: HandId -> Handler Html
getDealerViewR = undefined
