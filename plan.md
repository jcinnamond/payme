# The plan

Ledger : collect of credits & debits
  account id
  type: credit / debit
  amount
  source (payment, interest)
  destination (payment, interest, charges)
  datetime

Account : collect of ledger entries, ownership, account number (routing), amount?

API - receive payment, make payment, list accounts, show account

Metrics: how many transactions, how much money, how long does it take to credit/debit, how often are debits declined due to lack of funds,...

Logs: given an id, tell me everything that happened and which system/component did it
      tell me if anything goes wrong

Error handling (queuing): ability to retry transient errors; deadletter & ability to retry

Liveness & readiness

Migrations

Config & env vars