module Handler.Dealer where

import Import
import Safe (maximumDef)
import Data.Time (getCurrentTime)
import Data.List (groupBy)

import Data.Function (on)
import Model.ScrumBet

getHandList =
  runDB $ selectList [] [ Desc HandCreatedDate, LimitTo 5 ]

getBidList handId =
  runDB $ selectList [ ScrumBetHand ==. handId ] [ Asc ScrumBetLastUpdated ]

compareBetEntitiesByVal :: Entity ScrumBet -> Entity ScrumBet -> Bool
compareBetEntitiesByVal = (==) `on` scrumBetValue . entityVal

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

--mkBetGraphBar :: FibonacciSubset -> [FibonacciSubset] -> BetGraphBar
--mkBetGraphBar def []        = BetGraphBar def 0
--mkBetGraphBar _   l@(fib:_) = BetGraphBar fib (length l)

--mkBetGraph :: [[FibonacciSubset]] -> BetGraph
--mkBetGraph [] = BetGraph 0 []
--mkBetGraph betGroups = BetGraph fillDomain (head.head betGroups) betGroups

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
    
  
-- End pure util functions.  Move to shared module later

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
getDealerViewR handId = do
  _ <- runDB $ get404 handId
  betEntities <- getBidList handId
  let betGraph = mkBetGraph . (map $ scrumBetValue . entityVal) $ betEntities
  defaultLayout $ do
    $(widgetFile "dealerview")
