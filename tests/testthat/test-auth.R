test_that("sfin_claim_token errors on non-base64 input", {
  expect_snapshot(error = TRUE, sfin_claim_token("not_valid_base64!!!"))
})

test_that("sfin_claim_token errors when decoded URL is not HTTPS", {
  # base64 of "http://example.com/claim/token"
  http_token <- jsonlite::base64_enc(charToRaw("http://example.com/claim/token"))
  expect_snapshot(error = TRUE, sfin_claim_token(http_token))
})

test_that("parse_access_url extracts credentials correctly", {
  result <- parse_access_url("https://alice:s3cr3t@bridge.simplefin.org/simplefin")
  expect_equal(result$username, "alice")
  expect_equal(result$password, "s3cr3t")
  expect_equal(result$base_url, "https://bridge.simplefin.org/simplefin")
})

test_that("parse_access_url errors on malformed URL", {
  expect_snapshot(error = TRUE, parse_access_url("https://no-credentials-here.com/path"))
})
