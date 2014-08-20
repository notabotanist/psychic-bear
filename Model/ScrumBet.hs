module Model.ScrumBet where

import Prelude
import Database.Persist.TH
import Data.Text (Text)

data FibonacciSubset = F0 | F1 | F2 | F3 | F5 | F8 | F13 | F21 | F34 | F55
  deriving (Show, Read, Eq, Enum)

toText :: FibonacciSubset -> Text
toText f = fibTextList !! (fromEnum f)
  where fibTextList = ["0","1","2","3","5","8","13","21","34","55"]

derivePersistField "FibonacciSubset"
