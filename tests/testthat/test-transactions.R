# Reuse helpers from test-accounts.R via testthat's helper loading.
# These are redefined here to keep the file self-contained.

account_set_json_txn <- function(accounts = list(), connections = list(), errlist = list()) {
  jsonlite::toJSON(
    list(errlist = errlist, connections = connections, accounts = accounts),
    auto_unbox = TRUE
  )
}

json_response_txn <- function(json, status = 200L) {
  httr2::response(
    status_code = status,
    headers = list(`content-type` = "application/json; charset=utf-8"),
    body = charToRaw(as.character(json))
  )
}

two_account_payload <- function() {
  accounts <- list(
    list(
      id = "ACT-001", name = "Checking", conn_id = "CON-001",
      conn_name = "My Bank", currency = "USD",
      balance = "1000.00", `available-balance` = "950.00",
      `balance-date` = 1700000000L,
      transactions = list(
        list(id = "TXN-001", posted = 1699900000L, amount = "-10.00",
             description = "Coffee", pending = FALSE),
        list(id = "TXN-002", posted = 1699800000L, amount = "-50.00",
             description = "Groceries", pending = FALSE)
      )
    ),
    list(
      id = "ACT-002", name = "Savings", conn_id = "CON-001",
      conn_name = "My Bank", currency = "USD",
      balance = "5000.00", `available-balance` = "5000.00",
      `balance-date` = 1700000000L,
      transactions = list(
        list(id = "TXN-003", posted = 1699700000L, amount = "200.00",
             description = "Transfer in", pending = FALSE)
      )
    )
  )
  account_set_json_txn(accounts = accounts)
}

demo_url <- "https://user:s3cr3t@bridge.simplefin.org/simplefin"

# --- join_accounts = FALSE (default) ---

test_that("sfin_transactions returns a tibble without account columns by default", {
  mock_fn <- \(req) json_response_txn(two_account_payload())

  result <- httr2::with_mocked_responses(mock_fn, sfin_transactions(demo_url))

  expect_s3_class(result, "tbl_df")
  expect_equal(nrow(result), 3L)
  expect_named(
    result,
    c("account_id", "transaction_id", "posted", "transacted_at",
      "amount", "description", "pending")
  )
})

test_that("sfin_transactions aggregates transactions from multiple accounts", {
  mock_fn <- \(req) json_response_txn(two_account_payload())

  result <- httr2::with_mocked_responses(mock_fn, sfin_transactions(demo_url))

  expect_equal(sort(unique(result$account_id)), c("ACT-001", "ACT-002"))
  expect_equal(nrow(result[result$account_id == "ACT-001", ]), 2L)
  expect_equal(nrow(result[result$account_id == "ACT-002", ]), 1L)
})

# --- join_accounts = TRUE ---

test_that("sfin_transactions with join_accounts = TRUE adds account metadata columns", {
  mock_fn <- \(req) json_response_txn(two_account_payload())

  result <- httr2::with_mocked_responses(
    mock_fn,
    sfin_transactions(demo_url, join_accounts = TRUE)
  )

  expect_named(
    result,
    c("account_id", "account_name", "conn_id", "conn_name", "currency",
      "transaction_id", "posted", "transacted_at", "amount", "description",
      "pending")
  )
})

test_that("sfin_transactions join correctly populates account metadata", {
  mock_fn <- \(req) json_response_txn(two_account_payload())

  result <- httr2::with_mocked_responses(
    mock_fn,
    sfin_transactions(demo_url, join_accounts = TRUE)
  )

  checking_txns <- result[result$account_id == "ACT-001", ]
  expect_equal(unique(checking_txns$account_name), "Checking")
  expect_equal(unique(checking_txns$currency), "USD")

  savings_txns <- result[result$account_id == "ACT-002", ]
  expect_equal(unique(savings_txns$account_name), "Savings")
})

test_that("sfin_transactions preserves all rows with join_accounts = TRUE", {
  mock_fn <- \(req) json_response_txn(two_account_payload())

  result <- httr2::with_mocked_responses(
    mock_fn,
    sfin_transactions(demo_url, join_accounts = TRUE)
  )

  expect_equal(nrow(result), 3L)
})

# --- empty response ---

test_that("sfin_transactions returns empty tibble when no transactions exist", {
  empty_accounts <- list(list(
    id = "ACT-001", name = "Checking", conn_id = "CON-001",
    conn_name = "My Bank", currency = "USD",
    balance = "0.00", `available-balance` = "0.00",
    `balance-date` = 1700000000L,
    transactions = list()
  ))
  mock_fn <- \(req) json_response_txn(account_set_json_txn(accounts = empty_accounts))

  result <- httr2::with_mocked_responses(mock_fn, sfin_transactions(demo_url))

  expect_s3_class(result, "tbl_df")
  expect_equal(nrow(result), 0L)
})

# --- query params are passed through ---

test_that("sfin_transactions forwards start_date to the underlying request", {
  captured_req <- NULL
  mock_fn <- function(req) {
    captured_req <<- req
    json_response_txn(account_set_json_txn())
  }

  httr2::with_mocked_responses(
    mock_fn,
    sfin_transactions(demo_url, start_date = as.Date("2024-06-01"))
  )

  expect_match(captured_req$url, "start-date=")
})

test_that("sfin_transactions does not send balances-only param", {
  captured_req <- NULL
  mock_fn <- function(req) {
    captured_req <<- req
    json_response_txn(account_set_json_txn())
  }

  httr2::with_mocked_responses(mock_fn, sfin_transactions(demo_url))

  expect_false(grepl("balances-only", captured_req$url))
})
