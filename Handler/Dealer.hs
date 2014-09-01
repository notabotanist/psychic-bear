module Handler.Dealer where

import Import
import Data.Time (getCurrentTime)

data BetGraphBar = BetGraphBar
  { label :: FibonacciSubset
  , barCount :: Int
  }
  deriving Show

-- Perhaps also include total votes for bar scaling
data BetGraph = BetGraph
  { numVotes :: Int
  , bars :: [BetGraphBar]
  }

mkBetGraph :: [FibonacciSubset] -> BetGraph
mkBetGraph [] = BetGraph 0 []
mkBetGraph flatVotes@(f:_) = BetGraph (length flatVotes) (foldBars f flatVotes)
  where
    foldBars _ [] = []
    foldBars targetF votesTail =
        let (targetVotes, remainingVotes) = span (== targetF) votesTail in
        (BetGraphBar targetF (length targetVotes)) : foldBars (bSucc targetF) remainingVotes
    -- bounded successor
    bSucc F55 = F55
    bSucc x   = succ x
    
-- End pure util functions.

handForm :: Form Hand
handForm = renderBootstrap3 BootstrapBasicForm $ Hand
  <$> lift (liftIO getCurrentTime)

getDealerR :: Handler Html
getDealerR = do
  hands <- fmap (map entityKey) getHandList
  (formWidget, _) <- generateFormPost $ handForm
  let handView = DealerViewR
      handlistWidget = $(widgetFile "handlist")
      newHandButton = mondoButtonWidget "Create New Hand"
  defaultLayout $ do
    appBarWidget "Dealer"
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
getDealerViewR handId = do
  _ <- runDB $ get404 handId
  betEntities <- getBidList handId
  let betGraph = mkBetGraph . (map $ scrumBetValue . entityVal) $ betEntities
  defaultLayout $ do
    appBarWidget $ makeHandTitleText handId
    $(widgetFile "unanimous")
    $(widgetFile "dealerview")
