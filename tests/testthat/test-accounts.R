# Helpers ------------------------------------------------------------------

# Build a minimal valid Account Set JSON body for mocking.
account_set_json <- function(
    accounts = list(),
    connections = list(),
    errlist = list()) {
  jsonlite::toJSON(
    list(errlist = errlist, connections = connections, accounts = accounts),
    auto_unbox = TRUE
  )
}

# A single account with one transaction.
one_account <- function(n_transactions = 1L) {
  txns <- if (n_transactions > 0) {
    lapply(seq_len(n_transactions), function(i) {
      list(
        id = paste0("TXN-00", i),
        posted = 1699900000L + i * 86400L,
        amount = "-25.00",
        description = paste("Transaction", i),
        pending = FALSE
      )
    })
  } else {
    list()
  }

  list(list(
    id = "ACT-001",
    name = "Checking",
    conn_id = "CON-001",
    conn_name = "Test Bank",
    currency = "USD",
    balance = "500.00",
    `available-balance` = "475.00",
    `balance-date` = 1700000000L,
    transactions = txns
  ))
}

# Build a mock httr2 response from a JSON string.
json_response <- function(json, status = 200L) {
  httr2::response(
    status_code = status,
    headers = list(`content-type` = "application/json; charset=utf-8"),
    body = charToRaw(as.character(json))
  )
}

demo_access_url <- "https://user:s3cr3t@bridge.simplefin.org/simplefin"

# Mocked integration tests --------------------------------------------------

test_that("sfin_accounts sends request to /accounts path with auth", {
  captured_req <- NULL

  mock_fn <- function(req) {
    captured_req <<- req
    json_response(account_set_json())
  }

  httr2::with_mocked_responses(
    mock_fn,
    sfin_accounts(demo_access_url)
  )

  expect_match(captured_req$url, "/accounts")
  # Basic auth header should be present
  expect_true("authorization" %in% tolower(names(captured_req$headers)))
})

test_that("sfin_accounts returns correct list structure", {
  mock_fn <- \(req) json_response(account_set_json(one_account()))

  result <- httr2::with_mocked_responses(mock_fn, sfin_accounts(demo_access_url))

  expect_type(result, "list")
  expect_named(result, c("accounts", "transactions", "connections", "errors"))
  expect_s3_class(result$accounts, "tbl_df")
  expect_s3_class(result$transactions, "tbl_df")
  expect_s3_class(result$connections, "tbl_df")
  expect_s3_class(result$errors, "tbl_df")
})

test_that("sfin_accounts parses accounts tibble correctly", {
  mock_fn <- \(req) json_response(account_set_json(one_account()))

  result <- httr2::with_mocked_responses(mock_fn, sfin_accounts(demo_access_url))

  accts <- result$accounts
  expect_equal(nrow(accts), 1L)
  expect_equal(accts$account_id, "ACT-001")
  expect_equal(accts$account_name, "Checking")
  expect_equal(accts$currency, "USD")
  expect_equal(accts$balance, 500.00)
  expect_equal(accts$available_balance, 475.00)
  expect_s3_class(accts$balance_date, "POSIXct")
  expect_equal(
    as.numeric(accts$balance_date),
    as.numeric(as.POSIXct(1700000000, origin = "1970-01-01", tz = "UTC"))
  )
})

test_that("sfin_accounts parses transactions tibble correctly", {
  mock_fn <- \(req) json_response(account_set_json(one_account(n_transactions = 2L)))

  result <- httr2::with_mocked_responses(mock_fn, sfin_accounts(demo_access_url))

  txns <- result$transactions
  expect_equal(nrow(txns), 2L)
  expect_equal(unique(txns$account_id), "ACT-001")
  expect_equal(txns$amount, c(-25.00, -25.00))
  expect_type(txns$pending, "logical")
  expect_s3_class(txns$posted, "POSIXct")
})

test_that("sfin_accounts returns empty transactions when balances_only = TRUE", {
  mock_fn <- \(req) json_response(account_set_json(one_account(n_transactions = 0L)))

  result <- httr2::with_mocked_responses(
    mock_fn,
    sfin_accounts(demo_access_url, balances_only = TRUE)
  )

  expect_equal(nrow(result$transactions), 0L)
  expect_equal(nrow(result$accounts), 1L)
})

test_that("sfin_accounts issues start-date query param as unix timestamp", {
  captured_req <- NULL
  mock_fn <- function(req) {
    captured_req <<- req
    json_response(account_set_json())
  }

  httr2::with_mocked_responses(
    mock_fn,
    sfin_accounts(demo_access_url, start_date = as.Date("2024-01-01"))
  )

  expect_match(captured_req$url, "start-date=")
})

test_that("sfin_accounts warns when the server returns errors", {
  # account_id is optional per spec; omit rather than setting NULL to avoid
  # jsonlite serializing NULL as {} (empty object)
  errlist <- list(list(code = "con.auth", msg = "Authentication failed", conn_id = "CON-999"))
  mock_fn <- \(req) json_response(account_set_json(errlist = errlist))

  expect_warning(
    httr2::with_mocked_responses(mock_fn, sfin_accounts(demo_access_url)),
    regexp = "1 error"
  )
})

test_that("sfin_accounts returns structured errors in the errors tibble", {
  errlist <- list(list(code = "con.auth", msg = "Authentication failed", conn_id = "CON-999"))
  mock_fn <- \(req) json_response(account_set_json(errlist = errlist))

  result <- suppressWarnings(
    httr2::with_mocked_responses(mock_fn, sfin_accounts(demo_access_url))
  )

  expect_equal(nrow(result$errors), 1L)
  expect_equal(result$errors$code, "con.auth")
  expect_equal(result$errors$conn_id, "CON-999")
  expect_true(is.na(result$errors$account_id))
})

test_that("sfin_accounts stops on HTTP 403", {
  mock_fn <- \(req) json_response("{}", status = 403L)

  expect_snapshot(
    error = TRUE,
    httr2::with_mocked_responses(mock_fn, sfin_accounts(demo_access_url))
  )
})

test_that("sfin_accounts stops on HTTP 402", {
  mock_fn <- \(req) json_response("{}", status = 402L)

  expect_snapshot(
    error = TRUE,
    httr2::with_mocked_responses(mock_fn, sfin_accounts(demo_access_url))
  )
})

# Live integration test (skipped unless SIMPLEFIN_ACCESS_URL is set) ---------

test_that("sfin_accounts works against a real SimpleFIN server", {
  access_url <- Sys.getenv("SIMPLEFIN_ACCESS_URL")
  skip_if(
    nchar(access_url) == 0,
    "SIMPLEFIN_ACCESS_URL not set — skipping live integration test"
  )

  result <- sfin_accounts(access_url, start_date = Sys.Date() - 30L)

  expect_named(result, c("accounts", "transactions", "connections", "errors"))
  expect_s3_class(result$accounts, "tbl_df")
  expect_s3_class(result$transactions, "tbl_df")
  expect_true(nrow(result$accounts) > 0L)
  expect_true(all(c("account_id", "balance", "currency") %in% names(result$accounts)))
  expect_true(all(c("transaction_id", "amount", "posted") %in% names(result$transactions)))
})
