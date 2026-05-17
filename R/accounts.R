#' Retrieve accounts and transactions from a SimpleFIN server
#'
#' Issues a `GET /accounts` request to the SimpleFIN Access URL and returns
#' the response as a named list of four flattened tibbles: `accounts`,
#' `transactions`, `connections`, and `errors`.
#'
#' @param access_url Character. The Access URL returned by [sfin_claim_token()].
#'   Must be an HTTPS URL with embedded Basic Auth credentials in the form
#'   `https://username:password@host/path`. Provide either this or `key`,
#'   not both.
#' @param key Character. A keyring key name set via [sfin_set_access_url()].
#'   When supplied the Access URL is retrieved from the OS credential store
#'   and `access_url` must be `NULL`. Requires the \pkg{keyring} package.
#' @param start_date Optional. Restrict transactions to those posted on or
#'   after this date. Accepts a `Date`, `POSIXct`, or a numeric UNIX timestamp.
#' @param end_date Optional. Restrict transactions to those posted before
#'   (**not including**) this date. Accepts a `Date`, `POSIXct`, or a numeric
#'   UNIX timestamp.
#' @param pending Logical. If `TRUE`, include pending transactions (if
#'   supported by the server). Default `FALSE`.
#' @param account Optional character vector of account IDs. If provided, only
#'   data for those accounts is returned. May list multiple IDs.
#' @param balances_only Logical. If `TRUE`, skip transaction data and return
#'   only account balances. Default `FALSE`.
#'
#' @return A named list with four tibbles:
#'
#' - **`accounts`** — one row per account with columns:
#'   `account_id`, `account_name`, `conn_id`, `conn_name`, `currency`,
#'   `balance` (numeric), `available_balance` (numeric),
#'   `balance_date` (POSIXct UTC).
#'
#' - **`transactions`** — one row per transaction with columns:
#'   `account_id`, `transaction_id`, `posted` (POSIXct UTC),
#'   `transacted_at` (POSIXct UTC), `amount` (numeric), `description`,
#'   `pending` (logical). `posted` is `NA` for pending transactions.
#'
#' - **`connections`** — one row per connection with columns:
#'   `conn_id`, `name`, `org_id`, `org_url`, `sfin_url`.
#'
#' - **`errors`** — one row per structured error with columns:
#'   `code`, `msg`, `conn_id`, `account_id`.
#'
#' @details
#' The function raises an error on HTTP 403 (access revoked or bad credentials)
#' and HTTP 402 (payment required). A non-empty `errors` tibble in the return
#' value indicates partial failures at the connection or account level; inspect
#' these alongside the other tibbles rather than treating the call as failed.
#'
#' Timestamps from the API are UNIX epoch integers and are converted to
#' `POSIXct` in UTC. Balance and amount fields are returned as numeric
#' (the API represents them as strings).
#'
#' @examples
#' \dontrun{
#' # Using a literal Access URL
#' access_url <- Sys.getenv("SIMPLEFIN_ACCESS_URL")
#' result <- sfin_accounts(access_url, start_date = Sys.Date() - 30)
#'
#' # Using a keyring key (requires the keyring package)
#' sfin_set_access_url(access_url, key = "personal")
#' result <- sfin_accounts(key = "personal", start_date = Sys.Date() - 30)
#'
#' result$accounts
#' result$transactions
#' result$errors
#' }
#'
#' @export
sfin_accounts <- function(
    access_url = NULL,
    key = NULL,
    start_date = NULL,
    end_date = NULL,
    pending = FALSE,
    account = NULL,
    balances_only = FALSE) {
  access_url <- resolve_access_url(access_url, key)
  parsed_url <- parse_access_url(access_url)

  req <- httr2::request(parsed_url$base_url) |>
    httr2::req_url_path_append("accounts") |>
    httr2::req_auth_basic(parsed_url$username, parsed_url$password) |>
    httr2::req_url_query(version = "2")

  if (!is.null(start_date)) {
    req <- httr2::req_url_query(req, `start-date` = to_unix(start_date))
  }
  if (!is.null(end_date)) {
    req <- httr2::req_url_query(req, `end-date` = to_unix(end_date))
  }
  if (isTRUE(pending)) {
    req <- httr2::req_url_query(req, pending = "1")
  }
  if (!is.null(account)) {
    for (acct_id in account) {
      req <- httr2::req_url_query(req, account = acct_id)
    }
  }
  if (isTRUE(balances_only)) {
    req <- httr2::req_url_query(req, `balances-only` = "1")
  }

  resp <- req |>
    httr2::req_error(is_error = \(r) FALSE) |>
    httr2::req_perform()

  status <- httr2::resp_status(resp)

  if (status == 403L) {
    stop(
      "HTTP 403: Access denied. Credentials may be invalid or access has ",
      "been revoked. The user should visit their SimpleFIN Bridge to ",
      "reconnect."
    )
  }
  if (status == 402L) {
    stop("HTTP 402: Payment required to access this SimpleFIN server.")
  }
  if (status != 200L) {
    stop("Unexpected HTTP ", status, " response from the SimpleFIN server.")
  }

  body <- httr2::resp_body_string(resp)
  parsed <- jsonlite::fromJSON(body, simplifyVector = FALSE)

  result <- parse_account_set(parsed)

  if (nrow(result$errors) > 0L) {
    warning(
      nrow(result$errors),
      " error(s) returned by the server. ",
      "Check result$errors for details."
    )
  }

  result
}
