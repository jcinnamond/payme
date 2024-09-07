CREATE TABLE ledger_entries (
  id UUID PRIMARY KEY NOT NULL,
  amount BIGINT NOT NULL,
  datetime TIMESTAMP WITH TIME ZONE NOT NULL
);