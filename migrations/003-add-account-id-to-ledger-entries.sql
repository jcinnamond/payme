ALTER TABLE ledger_entries ADD COLUMN account_id UUID NOT NULL
  CONSTRAINT ledger_entries_account_id_fk REFERENCES accounts (id)   
  ON UPDATE CASCADE ON DELETE CASCADE; 