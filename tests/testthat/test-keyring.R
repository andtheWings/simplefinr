fake_url <- "https://user:s3cr3t@bridge.simplefin.org/simplefin"

# --- check_keyring() --------------------------------------------------------

test_that("check_keyring errors with install hint when keyring is absent", {
  local_mocked_bindings(
    check_keyring = function() stop("The keyring package is required", call. = FALSE)
  )
  expect_snapshot(error = TRUE, sfin_set_access_url(fake_url))
})

# --- sfin_set_access_url() --------------------------------------------------

test_that("sfin_set_access_url calls keyring with correct service and username", {
  recorded <- list()
  local_mocked_bindings(
    check_keyring = function() invisible(NULL),
    keyring_key_set = function(service, username, password) {
      recorded <<- list(service = service, username = username, password = password)
    }
  )
  sfin_set_access_url(fake_url, key = "personal")
  expect_equal(recorded$service, "simplefinr")
  expect_equal(recorded$username, "personal")
  expect_equal(recorded$password, fake_url)
})

test_that("sfin_set_access_url returns the access_url invisibly", {
  local_mocked_bindings(
    check_keyring = function() invisible(NULL),
    keyring_key_set = function(service, username, password) invisible(NULL)
  )
  result <- sfin_set_access_url(fake_url)
  expect_equal(result, fake_url)
})

test_that("sfin_set_access_url uses 'default' key when key is not specified", {
  recorded <- list()
  local_mocked_bindings(
    check_keyring = function() invisible(NULL),
    keyring_key_set = function(service, username, password) {
      recorded <<- list(username = username)
    }
  )
  sfin_set_access_url(fake_url)
  expect_equal(recorded$username, "default")
})

# --- sfin_get_access_url() --------------------------------------------------

test_that("sfin_get_access_url retrieves from the correct service and username", {
  local_mocked_bindings(
    check_keyring = function() invisible(NULL),
    keyring_key_get = function(service, username) {
      expect_equal(service, "simplefinr")
      expect_equal(username, "personal")
      fake_url
    }
  )
  result <- sfin_get_access_url("personal")
  expect_equal(result, fake_url)
})

test_that("sfin_get_access_url uses 'default' key when key is not specified", {
  local_mocked_bindings(
    check_keyring = function() invisible(NULL),
    keyring_key_get = function(service, username) {
      expect_equal(username, "default")
      fake_url
    }
  )
  sfin_get_access_url()
})

# --- sfin_delete_access_url() -----------------------------------------------

test_that("sfin_delete_access_url calls keyring with correct service and username", {
  recorded <- list()
  local_mocked_bindings(
    check_keyring = function() invisible(NULL),
    keyring_key_delete = function(service, username) {
      recorded <<- list(service = service, username = username)
    }
  )
  sfin_delete_access_url("personal")
  expect_equal(recorded$service, "simplefinr")
  expect_equal(recorded$username, "personal")
})

test_that("sfin_delete_access_url returns NULL invisibly", {
  local_mocked_bindings(
    check_keyring = function() invisible(NULL),
    keyring_key_delete = function(service, username) invisible(NULL)
  )
  expect_null(sfin_delete_access_url())
})

# --- sfin_list_keys() -------------------------------------------------------

test_that("sfin_list_keys returns a tibble with a key column", {
  local_mocked_bindings(
    check_keyring = function() invisible(NULL),
    keyring_key_list = function(service) {
      expect_equal(service, "simplefinr")
      data.frame(username = c("default", "personal"), stringsAsFactors = FALSE)
    }
  )
  result <- sfin_list_keys()
  expect_s3_class(result, "tbl_df")
  expect_named(result, "key")
  expect_equal(result$key, c("default", "personal"))
})

test_that("sfin_list_keys returns zero-row tibble when no keys are stored", {
  local_mocked_bindings(
    check_keyring = function() invisible(NULL),
    keyring_key_list = function(service) data.frame(username = character(0))
  )
  result <- sfin_list_keys()
  expect_equal(nrow(result), 0L)
})

# --- resolve_access_url() ---------------------------------------------------

test_that("resolve_access_url errors when both access_url and key are provided", {
  expect_snapshot(
    error = TRUE,
    resolve_access_url("https://x:y@host/sfin", "mykey")
  )
})

test_that("resolve_access_url errors when neither access_url nor key is provided", {
  expect_snapshot(error = TRUE, resolve_access_url(NULL, NULL))
})

test_that("resolve_access_url returns access_url when only access_url is given", {
  result <- resolve_access_url(fake_url, NULL)
  expect_equal(result, fake_url)
})

test_that("resolve_access_url calls sfin_get_access_url when key is given", {
  local_mocked_bindings(
    sfin_get_access_url = function(key) {
      expect_equal(key, "personal")
      fake_url
    }
  )
  result <- resolve_access_url(NULL, "personal")
  expect_equal(result, fake_url)
})

# --- sfin_accounts() / sfin_transactions() integration ---------------------

test_that("sfin_accounts accepts key and resolves to access_url", {
  mock_body <- jsonlite::toJSON(
    list(errlist = list(), connections = list(), accounts = list()),
    auto_unbox = TRUE
  )
  local_mocked_bindings(
    sfin_get_access_url = function(key) fake_url
  )
  result <- httr2::with_mocked_responses(
    \(req) httr2::response(
      200L,
      headers = list(`content-type` = "application/json"),
      body = charToRaw(as.character(mock_body))
    ),
    sfin_accounts(key = "personal")
  )
  expect_named(result, c("accounts", "transactions", "connections", "errors"))
})

test_that("sfin_transactions accepts key and resolves to access_url", {
  mock_body <- jsonlite::toJSON(
    list(errlist = list(), connections = list(), accounts = list()),
    auto_unbox = TRUE
  )
  local_mocked_bindings(
    sfin_get_access_url = function(key) fake_url
  )
  result <- httr2::with_mocked_responses(
    \(req) httr2::response(
      200L,
      headers = list(`content-type` = "application/json"),
      body = charToRaw(as.character(mock_body))
    ),
    sfin_transactions(key = "personal")
  )
  expect_s3_class(result, "tbl_df")
})
