sample_account_set <- function() {
  jsonlite::fromJSON(
    '{
      "errlist": [],
      "connections": [
        {
          "conn_id": "CON-001",
          "name": "My Bank - Alice",
          "org_id": "ORG-001",
          "org_url": "https://mybank.com",
          "sfin_url": "https://sfin.mybank.com"
        }
      ],
      "accounts": [
        {
          "id": "ACT-001",
          "name": "Checking",
          "conn_id": "CON-001",
          "conn_name": "My Bank - Alice",
          "currency": "USD",
          "balance": "1234.56",
          "available-balance": "1000.00",
          "balance-date": 978366153,
          "transactions": [
            {
              "id": "TXN-001",
              "posted": 793090572,
              "amount": "-42.00",
              "description": "Coffee Shop",
              "pending": false
            },
            {
              "id": "TXN-002",
              "posted": 0,
              "amount": "-10.00",
              "description": "Pending charge",
              "pending": true
            }
          ]
        }
      ]
    }',
    simplifyVector = FALSE
  )
}

# --- parse_accounts ---

test_that("parse_accounts returns correct columns and types", {
  result <- parse_accounts(sample_account_set()[["accounts"]])
  expect_s3_class(result, "tbl_df")
  expect_named(
    result,
    c(
      "account_id", "account_name", "conn_id", "conn_name",
      "currency", "balance", "available_balance", "balance_date"
    )
  )
  expect_type(result$balance, "double")
  expect_type(result$available_balance, "double")
  expect_s3_class(result$balance_date, "POSIXct")
})

test_that("parse_accounts parses values correctly", {
  result <- parse_accounts(sample_account_set()[["accounts"]])
  expect_equal(result$account_id, "ACT-001")
  expect_equal(result$account_name, "Checking")
  expect_equal(result$balance, 1234.56)
  expect_equal(result$available_balance, 1000.00)
  expect_equal(result$currency, "USD")
})

test_that("parse_accounts returns empty tibble for NULL input", {
  result <- parse_accounts(NULL)
  expect_s3_class(result, "tbl_df")
  expect_equal(nrow(result), 0L)
})

# --- parse_transactions ---

test_that("parse_transactions returns correct columns and types", {
  result <- parse_transactions(sample_account_set()[["accounts"]])
  expect_s3_class(result, "tbl_df")
  expect_named(
    result,
    c(
      "account_id", "transaction_id", "posted", "transacted_at",
      "amount", "description", "pending"
    )
  )
  expect_type(result$amount, "double")
  expect_type(result$pending, "logical")
  expect_s3_class(result$posted, "POSIXct")
})

test_that("parse_transactions parses values correctly", {
  result <- parse_transactions(sample_account_set()[["accounts"]])
  expect_equal(nrow(result), 2L)
  expect_equal(result$account_id, c("ACT-001", "ACT-001"))
  expect_equal(result$amount, c(-42.00, -10.00))
  expect_equal(result$pending, c(FALSE, TRUE))
})

test_that("posted == 0 becomes NA for pending transactions", {
  result <- parse_transactions(sample_account_set()[["accounts"]])
  expect_true(is.na(result$posted[result$pending == TRUE]))
})

test_that("parse_transactions returns empty tibble for NULL input", {
  result <- parse_transactions(NULL)
  expect_s3_class(result, "tbl_df")
  expect_equal(nrow(result), 0L)
})

# --- parse_connections ---

test_that("parse_connections returns correct columns", {
  result <- parse_connections(sample_account_set()[["connections"]])
  expect_named(result, c("conn_id", "name", "org_id", "org_url", "sfin_url"))
  expect_equal(result$conn_id, "CON-001")
})

# --- parse_errors ---

test_that("parse_errors returns empty tibble when errlist is empty", {
  result <- parse_errors(list())
  expect_s3_class(result, "tbl_df")
  expect_equal(nrow(result), 0L)
  expect_named(result, c("code", "msg", "conn_id", "account_id"))
})

test_that("parse_errors parses structured errors", {
  errlist <- list(
    list(
      code = "con.auth",
      msg = "Auth failed",
      conn_id = "CON-001",
      account_id = NULL
    )
  )
  result <- parse_errors(errlist)
  expect_equal(nrow(result), 1L)
  expect_equal(result$code, "con.auth")
  expect_equal(result$conn_id, "CON-001")
  expect_true(is.na(result$account_id))
})

# --- parse_account_set (integration) ---

test_that("parse_account_set returns list with four expected names", {
  result <- parse_account_set(sample_account_set())
  expect_named(result, c("accounts", "transactions", "connections", "errors"))
  expect_s3_class(result$accounts, "tbl_df")
  expect_s3_class(result$transactions, "tbl_df")
  expect_s3_class(result$connections, "tbl_df")
  expect_s3_class(result$errors, "tbl_df")
})

# --- unix_to_datetime ---

test_that("unix_to_datetime converts correctly to POSIXct UTC", {
  result <- unix_to_datetime(0)
  expect_s3_class(result, "POSIXct")
  expect_true(is.na(result))
})

test_that("unix_to_datetime handles a valid timestamp", {
  result <- unix_to_datetime(978366153)
  expect_s3_class(result, "POSIXct")
  expect_equal(format(result, tz = "UTC"), "2001-01-01 16:22:33")
})

test_that("unix_to_datetime returns NA for NULL input", {
  result <- unix_to_datetime(NULL)
  expect_s3_class(result, "POSIXct")
  expect_true(is.na(result))
})

test_that("unix_to_datetime returns NA for empty numeric vector", {
  result <- unix_to_datetime(numeric(0))
  expect_s3_class(result, "POSIXct")
  expect_true(is.na(result))
})

# --- to_unix ---

test_that("to_unix converts POSIXct to numeric", {
  ts <- as.POSIXct("2024-01-15 12:00:00", tz = "UTC")
  expect_equal(to_unix(ts), as.numeric(ts))
})

test_that("to_unix converts Date to numeric", {
  d <- as.Date("2024-01-15")
  expected <- as.numeric(as.POSIXct(d, tz = "UTC"))
  expect_equal(to_unix(d), expected)
})

test_that("to_unix passes through numeric as-is", {
  expect_equal(to_unix(1705316400), 1705316400)
})

# --- null_to_na helpers ---

test_that("null_to_na_dbl handles NULL, empty, NA, and valid input", {
  expect_equal(null_to_na_dbl(NULL), NA_real_)
  expect_equal(null_to_na_dbl(list()), NA_real_)
  expect_equal(null_to_na_dbl(list(NA_real_)), NA_real_)
  expect_equal(null_to_na_dbl(list(42)), 42)
  expect_equal(null_to_na_dbl(list("123.45")), 123.45)
})

test_that("null_to_na_chr handles NULL, empty, NA, and valid input", {
  expect_equal(null_to_na_chr(NULL), NA_character_)
  expect_equal(null_to_na_chr(list()), NA_character_)
  expect_equal(null_to_na_chr(list(NA_character_)), NA_character_)
  expect_equal(null_to_na_chr(list("hello")), "hello")
})

test_that("null_to_na_lgl handles NULL, empty, NA, and valid input", {
  expect_equal(null_to_na_lgl(NULL), NA)
  expect_equal(null_to_na_lgl(list()), NA)
  expect_equal(null_to_na_lgl(list(NA)), NA)
  expect_equal(null_to_na_lgl(list(TRUE)), TRUE)
  expect_equal(null_to_na_lgl(list(FALSE)), FALSE)
})

# --- parse_connections edge cases ---

test_that("parse_connections returns empty tibble for NULL input", {
  result <- parse_connections(NULL)
  expect_s3_class(result, "tbl_df")
  expect_equal(nrow(result), 0L)
  expect_named(result, c("conn_id", "name", "org_id", "org_url", "sfin_url"))
})

test_that("parse_connections returns empty tibble for empty list", {
  result <- parse_connections(list())
  expect_s3_class(result, "tbl_df")
  expect_equal(nrow(result), 0L)
})

# --- parse_errors edge cases ---

test_that("parse_errors returns empty tibble for NULL input", {
  result <- parse_errors(NULL)
  expect_s3_class(result, "tbl_df")
  expect_equal(nrow(result), 0L)
  expect_named(result, c("code", "msg", "conn_id", "account_id"))
})

# --- empty tibble constructors ---

test_that("empty_accounts_tbl returns zero-row tibble with correct columns", {
  tbl <- empty_accounts_tbl()
  expect_s3_class(tbl, "tbl_df")
  expect_equal(nrow(tbl), 0L)
  expect_named(
    tbl,
    c("account_id", "account_name", "conn_id", "conn_name",
      "currency", "balance", "available_balance", "balance_date")
  )
  expect_type(tbl$balance, "double")
  expect_type(tbl$currency, "character")
})

test_that("empty_transactions_tbl returns zero-row tibble with correct columns", {
  tbl <- empty_transactions_tbl()
  expect_s3_class(tbl, "tbl_df")
  expect_equal(nrow(tbl), 0L)
  expect_named(
    tbl,
    c("account_id", "transaction_id", "posted", "transacted_at",
      "amount", "description", "pending")
  )
  expect_type(tbl$amount, "double")
  expect_type(tbl$pending, "logical")
})

test_that("empty_connections_tbl returns zero-row tibble with correct columns", {
  tbl <- empty_connections_tbl()
  expect_s3_class(tbl, "tbl_df")
  expect_equal(nrow(tbl), 0L)
  expect_named(tbl, c("conn_id", "name", "org_id", "org_url", "sfin_url"))
})

test_that("empty_errors_tbl returns zero-row tibble with correct columns", {
  tbl <- empty_errors_tbl()
  expect_s3_class(tbl, "tbl_df")
  expect_equal(nrow(tbl), 0L)
  expect_named(tbl, c("code", "msg", "conn_id", "account_id"))
})

# --- parse_account_set edge case ---

test_that("parse_account_set returns four empty tibbles for empty input", {
  result <- parse_account_set(list(
    accounts = NULL,
    connections = NULL,
    errlist = NULL
  ))
  expect_named(result, c("accounts", "transactions", "connections", "errors"))
  expect_equal(nrow(result$accounts), 0L)
  expect_equal(nrow(result$transactions), 0L)
  expect_equal(nrow(result$connections), 0L)
  expect_equal(nrow(result$errors), 0L)
})
