# simplefinr (development version)

* `sfin_set_access_url()`, `sfin_get_access_url()`, `sfin_delete_access_url()`,
  and `sfin_list_keys()` store and retrieve Access URLs from the OS credential
  store via the optional `keyring` package (macOS Keychain, Windows Credential
  Store, Secret Service on Linux).
* `sfin_accounts()` and `sfin_transactions()` now accept `key` as an
  alternative to `access_url`, resolving the credential from the OS store
  directly (e.g. `sfin_accounts(key = "personal")`).
* `sfin_claim_token()` claims a Base64-encoded SimpleFIN Token and returns a
  persistent Access URL.
* `sfin_transactions()` is a convenience wrapper around `sfin_accounts()` that
  returns just the transactions tibble. Set `join_accounts = TRUE` to
  left-join account name, connection, and currency onto each row.
* `sfin_accounts()` queries a SimpleFIN server's `/accounts` endpoint and
  returns a named list of four flattened tibbles: `accounts`, `transactions`,
  `connections`, and `errors`. Supports all v2 query parameters (`start_date`,
  `end_date`, `pending`, `account`, `balances_only`).
