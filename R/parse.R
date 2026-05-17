# Internal parsers: JSON → tibbles

# Parse a full Account Set response (parsed list from jsonlite) into a named
# list of tibbles: accounts, transactions, connections, errors.
parse_account_set <- function(x) {
  list(
    accounts = parse_accounts(x[["accounts"]]),
    transactions = parse_transactions(x[["accounts"]]),
    connections = parse_connections(x[["connections"]]),
    errors = parse_errors(x[["errlist"]])
  )
}

# Flatten the accounts array into a one-row-per-account tibble.
# Transactions are excluded here; use parse_transactions() for those.
parse_accounts <- function(accounts) {
  if (is.null(accounts) || length(accounts) == 0) {
    return(empty_accounts_tbl())
  }

  rows <- lapply(accounts, function(a) {
    tibble::tibble(
      account_id = null_to_na_chr(a[["id"]]),
      account_name = null_to_na_chr(a[["name"]]),
      conn_id = null_to_na_chr(a[["conn_id"]]),
      conn_name = null_to_na_chr(a[["conn_name"]]),
      currency = null_to_na_chr(a[["currency"]]),
      balance = null_to_na_dbl(a[["balance"]]),
      available_balance = null_to_na_dbl(a[["available-balance"]]),
      balance_date = unix_to_datetime(a[["balance-date"]])
    )
  })

  dplyr::bind_rows(rows)
}

# Flatten all transactions across all accounts into a single tibble.
# Each row includes the parent account_id for joining back to accounts.
parse_transactions <- function(accounts) {
  if (is.null(accounts) || length(accounts) == 0) {
    return(empty_transactions_tbl())
  }

  rows <- lapply(accounts, function(a) {
    txns <- a[["transactions"]]
    if (is.null(txns) || length(txns) == 0) {
      return(NULL)
    }
    account_id <- null_to_na_chr(a[["id"]])

    txn_rows <- lapply(txns, function(t) {
      tibble::tibble(
        account_id = account_id,
        transaction_id = null_to_na_chr(t[["id"]]),
        posted = unix_to_datetime(t[["posted"]]),
        transacted_at = unix_to_datetime(t[["transacted_at"]]),
        amount = null_to_na_dbl(t[["amount"]]),
        description = null_to_na_chr(t[["description"]]),
        pending = null_to_na_lgl(t[["pending"]])
      )
    })

    dplyr::bind_rows(txn_rows)
  })

  result <- dplyr::bind_rows(rows)
  if (nrow(result) == 0) empty_transactions_tbl() else result
}

# Flatten the connections array into a one-row-per-connection tibble.
parse_connections <- function(connections) {
  if (is.null(connections) || length(connections) == 0) {
    return(empty_connections_tbl())
  }

  rows <- lapply(connections, function(c) {
    tibble::tibble(
      conn_id = null_to_na_chr(c[["conn_id"]]),
      name = null_to_na_chr(c[["name"]]),
      org_id = null_to_na_chr(c[["org_id"]]),
      org_url = null_to_na_chr(c[["org_url"]]),
      sfin_url = null_to_na_chr(c[["sfin_url"]])
    )
  })

  dplyr::bind_rows(rows)
}

# Flatten the errlist array into a one-row-per-error tibble.
parse_errors <- function(errlist) {
  if (is.null(errlist) || length(errlist) == 0) {
    return(empty_errors_tbl())
  }

  rows <- lapply(errlist, function(e) {
    tibble::tibble(
      code = null_to_na_chr(e[["code"]]),
      msg = null_to_na_chr(e[["msg"]]),
      conn_id = null_to_na_chr(e[["conn_id"]]),
      account_id = null_to_na_chr(e[["account_id"]])
    )
  })

  dplyr::bind_rows(rows)
}

# --- Empty tibble constructors (correct types, zero rows) ---

empty_accounts_tbl <- function() {
  tibble::tibble(
    account_id = character(),
    account_name = character(),
    conn_id = character(),
    conn_name = character(),
    currency = character(),
    balance = double(),
    available_balance = double(),
    balance_date = as.POSIXct(character(), tz = "UTC")
  )
}

empty_transactions_tbl <- function() {
  tibble::tibble(
    account_id = character(),
    transaction_id = character(),
    posted = as.POSIXct(character(), tz = "UTC"),
    transacted_at = as.POSIXct(character(), tz = "UTC"),
    amount = double(),
    description = character(),
    pending = logical()
  )
}

empty_connections_tbl <- function() {
  tibble::tibble(
    conn_id = character(),
    name = character(),
    org_id = character(),
    org_url = character(),
    sfin_url = character()
  )
}

empty_errors_tbl <- function() {
  tibble::tibble(
    code = character(),
    msg = character(),
    conn_id = character(),
    account_id = character()
  )
}
