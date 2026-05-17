------------------------------------------------------------------------

editor_options: markdown: wrap: 72 ---

<img src="simplefinr_hex.png" align="right" width="150"/>

# simplefinr

An R package for querying a [SimpleFIN](https://www.simplefin.org) server or [Bridge](https://bridge.simplefin.org) and parsing the response into tidy tibbles. Implements the [SimpleFIN Protocol v2.0](https://www.simplefin.org/protocol.html).

## Installation

``` r
# install.packages("pak")
pak::pak("andtheWings/simplefinr")
```

## Overview

SimpleFIN gives read-only programmatic access to your bank account balances and transactions. Three steps are needed the first time:

1.  **Get a token** — visit the [SimpleFIN Bridge](https://bridge.simplefin.org/simplefin/create) and follow the prompts.
2.  **Claim it** — exchange the one-time token for a persistent Access URL.
3.  **Store it** — save the Access URL to your OS credential store.

After that, every subsequent query is a single function call.

``` r
library(simplefinr)

# --- One-time setup ----------------------------------------------------------

token     <- readline("Paste your SimpleFIN token: ")
access_url <- sfin_claim_token(token)
sfin_set_access_url(access_url, key = "personal")  # requires keyring package

# --- Query -------------------------------------------------------------------

# Account balances
sfin_accounts(key = "personal", balances_only = TRUE)$accounts

# Transactions for the last 90 days
txns <- sfin_transactions(
  key          = "personal",
  start_date   = Sys.Date() - 90,
  join_accounts = TRUE           # adds account_name, currency, etc.
)
```

## Functions

### Authentication

| Function | Description |
|------------------------------------|------------------------------------|
| `sfin_claim_token(token)` | Exchange a Base64-encoded SimpleFIN Token for a persistent Access URL |

### Credential store (requires [`keyring`](https://r-lib.github.io/keyring/))

| Function | Description |
|------------------------------------|------------------------------------|
| `sfin_set_access_url(access_url, key)` | Save an Access URL to the OS credential store |
| `sfin_get_access_url(key)` | Retrieve a stored Access URL |
| `sfin_delete_access_url(key)` | Remove a stored Access URL |
| `sfin_list_keys()` | List all stored simplefinr key names |

The `key` argument (default `"default"`) lets you manage multiple accounts, e.g. `key = "personal"` and `key = "business"`.

### Querying

| Function | Description |
|------------------------------------|------------------------------------|
| `sfin_accounts(...)` | Full response as a named list of four tibbles |
| `sfin_transactions(...)` | Transactions tibble, optionally joined with account metadata |

Both functions accept either `access_url` (a literal URL string) or `key` (a keyring key name) — not both.

**Common parameters:**

| Parameter | Description |
|------------------------------------|------------------------------------|
| `start_date` | Transactions on or after this date (`Date`, `POSIXct`, or UNIX timestamp) |
| `end_date` | Transactions before this date (exclusive) |
| `pending` | Include pending transactions; default `FALSE` |
| `account` | Character vector of account IDs to filter to |
| `balances_only` | Skip transaction data and return balances only |

### Return value of `sfin_accounts()`

A named list with four tibbles:

| Element | Grain | Key columns |
|------------------------|------------------------|------------------------|
| `$accounts` | one row per account | `account_id`, `balance`, `balance_date`, `currency` |
| `$transactions` | one row per transaction | `account_id`, `amount`, `posted`, `description`, `pending` |
| `$connections` | one row per institution connection | `conn_id`, `name`, `org_url` |
| `$errors` | one row per server-reported error | `code`, `msg`, `conn_id` |

Timestamps (`balance_date`, `posted`, `transacted_at`) are `POSIXct` in UTC. Balance and amount fields are `numeric` (the API returns them as strings). A `posted` value of `NA` indicates a pending transaction.

## Example workflow

``` r
library(simplefinr)
library(dplyr)

result <- sfin_accounts(key = "personal", start_date = Sys.Date() - 30)

# Check for any connection-level errors
result$errors

# Summarise spending by account
result$transactions |>
  filter(amount < 0) |>
  left_join(result$accounts |> select(account_id, account_name), by = "account_id") |>
  summarise(total_spent = sum(amount), .by = account_name)
```

## Security notes

- Access URLs contain embedded credentials. Treat them like passwords.
- `sfin_set_access_url()` stores them in the OS credential store (macOS Keychain, Windows Credential Store, or the Secret Service on Linux) via the [`keyring`](https://r-lib.github.io/keyring/) package, which is safer than environment variables or `.Renviron`.
- If you choose not to use `keyring`, the Access URL can be passed directly as `access_url = Sys.getenv("SIMPLEFIN_ACCESS_URL")`.
- simplefinr only ever makes `GET` requests after the initial claim — the SimpleFIN protocol is strictly read-only.

## Credits

MIT © 2026 Daniel Riggins. See [LICENSE.md](LICENSE.md) for details.

Hex badge designed in collaboration with Gemini (Google AI), mixing the R logo concept with the original SimpleFIN fin aesthetic.

## Related

- [SimpleFIN Protocol specification](https://www.simplefin.org/protocol.html)
- [SimpleFIN Bridge](https://bridge.simplefin.org) — aggregates accounts from institutions that don't yet run their own SimpleFIN server
