module Ledger (
  LedgerEntry (..),
  mkCredit,
  mkDebit,
  sumLedger,
) where

import Account (Account (..))
import Data.Aeson (FromJSON, ToJSON)
import Data.Int (Int64)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Time (UTCTime)
import Data.UUID (UUID)
import GHC.Generics (Generic)

data LedgerEntry = LedgerEntry
  { uuid :: UUID
  , amount :: Int64
  , datetime :: UTCTime
  , accountId :: UUID
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

mkCredit :: Account -> UUID -> Int64 -> UTCTime -> Either Text LedgerEntry
mkCredit account uuid amount datetime
  | amount > 0 = pure $ LedgerEntry uuid amount datetime account.uuid
  | otherwise = Left $ "invalid credit amount: " <> (T.pack . show) amount

mkDebit :: Account -> UUID -> Int64 -> UTCTime -> Either Text LedgerEntry
mkDebit account uuid amount datetime
  | amount < 0 = pure $ LedgerEntry uuid amount datetime account.uuid
  | otherwise = Left $ "invalid debit amount: " <> (T.pack . show) amount

sumLedger :: (Traversable t) => t LedgerEntry -> Int64
sumLedger = sum . fmap amount
