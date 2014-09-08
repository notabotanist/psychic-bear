module Handler.Dealer where

import Import
import Data.Time (getCurrentTime)
import Data.List (sort)

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

-- |Make sure you call with a sorted list, or else INFINITE RECURSION will happen
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

-- Widgets
callVoteWidget :: HandId -> Widget
callVoteWidget handId = do
  timerId <- newIdent
  timerStart <- handlerToWidget $ extraVotingWindow <$> getExtra
  $(widgetFile "callvote")
-- End Widgets

handForm :: Form Hand
handForm = renderBootstrap3 BootstrapBasicForm $ Hand
  <$> lift (liftIO getCurrentTime)
  <*> pure Nothing

getDealerR :: Handler Html
getDealerR = do
  (formWidget, _) <- generateFormPost $ handForm
  let handlistWidget = handListWidget DealerViewR
      newHandButton = mondoButtonWidget "Create New Hand"
  defaultLayout $ do
    appBarWidget "Dealer" Nothing
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
  let betGraph = mkBetGraph . sort . (map $ scrumBetValue . entityVal) $ betEntities
  defaultLayout $ do
    appBarWidget (makeHandTitleText handId) (Just DealerR)
    unanimousWidget handId betEntities
    $(widgetFile "dealerview")
