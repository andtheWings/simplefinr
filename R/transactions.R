#' Retrieve transactions from a SimpleFIN server
#'
#' A convenience wrapper around [sfin_accounts()] that returns only the
#' transactions tibble. Optionally left-joins account-level metadata
#' (name, currency, connection) onto each transaction row.
#'
#' @inheritParams sfin_accounts
#' @param join_accounts Logical. If `TRUE`, left-join the `accounts` tibble
#'   onto the transactions by `account_id`, adding columns `account_name`,
#'   `conn_id`, `conn_name`, and `currency` to every transaction row.
#'   Default `FALSE`.
#'
#' @return A tibble with one row per transaction. Columns when
#'   `join_accounts = FALSE`:
#'
#' | Column | Type | Description |
#' |---|---|---|
#' | `account_id` | chr | Account the transaction belongs to |
#' | `transaction_id` | chr | Unique transaction ID within the account |
#' | `posted` | POSIXct (UTC) | Post date; `NA` for pending transactions |
#' | `transacted_at` | POSIXct (UTC) | Transaction date, if provided |
#' | `amount` | dbl | Positive = deposit, negative = withdrawal |
#' | `description` | chr | Human-readable description |
#' | `pending` | lgl | `TRUE` if not yet posted |
#'
#' When `join_accounts = TRUE`, four additional columns are prepended after
#' `account_id`: `account_name`, `conn_id`, `conn_name`, `currency`.
#'
#' @details
#' Any server-level errors are surfaced as warnings (same as [sfin_accounts()]).
#' Inspect them by calling [sfin_accounts()] directly and checking
#' `result$errors`.
#'
#' @examples
#' \dontrun{
#' # Using a literal Access URL
#' access_url <- Sys.getenv("SIMPLEFIN_ACCESS_URL")
#' sfin_transactions(access_url, start_date = Sys.Date() - 30)
#'
#' # Using a keyring key (requires the keyring package)
#' sfin_transactions(key = "personal", start_date = Sys.Date() - 30)
#'
#' # With account metadata joined in
#' sfin_transactions(key = "personal", start_date = Sys.Date() - 30,
#'                   join_accounts = TRUE)
#' }
#'
#' @export
sfin_transactions <- function(
    access_url = NULL,
    key = NULL,
    start_date = NULL,
    end_date = NULL,
    pending = FALSE,
    account = NULL,
    join_accounts = FALSE) {
  result <- sfin_accounts(
    access_url = access_url,
    key = key,
    start_date = start_date,
    end_date = end_date,
    pending = pending,
    account = account
  )

  txns <- result$transactions

  if (!isTRUE(join_accounts)) {
    return(txns)
  }

  acct_meta <- result$accounts[, c(
    "account_id", "account_name", "conn_id", "conn_name", "currency"
  )]

  # Left-join and reorder so account context columns sit next to account_id
  merged <- dplyr::left_join(txns, acct_meta, by = "account_id")

  merged[, c(
    "account_id", "account_name", "conn_id", "conn_name", "currency",
    "transaction_id", "posted", "transacted_at", "amount", "description",
    "pending"
  )]
}
