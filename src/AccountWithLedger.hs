module AccountWithLedger (
  AccountWithLedger (..),
) where

import Account (Account)
import Data.Aeson (ToJSON)
import GHC.Generics (Generic)
import Ledger (LedgerEntry)

data AccountWithLedger = AccountWithLedger
  { account :: Account
  , ledger :: [LedgerEntry]
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON)