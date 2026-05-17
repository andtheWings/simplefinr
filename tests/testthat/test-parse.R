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
