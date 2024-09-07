module Money (Money (..)) where

import Data.Int (Int64)

newtype Money = Money {amount :: Int64}
  deriving newtype (Show, Eq, Num)
