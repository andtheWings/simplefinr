# sfin_claim_token errors on non-base64 input

    Code
      sfin_claim_token("not_valid_base64!!!")
    Condition
      Error in `value[[3L]]()`:
      ! Failed to Base64-decode the SimpleFIN token: Error in base64 decode

# sfin_claim_token errors when decoded URL is not HTTPS

    Code
      sfin_claim_token(http_token)
    Condition
      Error in `sfin_claim_token()`:
      ! The decoded SimpleFIN token must point to an HTTPS URL. Only HTTPS connections are permitted.

# parse_access_url errors on malformed URL

    Code
      parse_access_url("https://no-credentials-here.com/path")
    Condition
      Error in `parse_access_url()`:
      ! Could not parse the Access URL. Expected format: https://username:password@host/path

# sfin_claim_token errors on unexpected HTTP status

    Code
      httr2::with_mocked_responses(function(req) httr2::response(status_code = 500L),
      sfin_claim_token(token))
    Condition
      Error in `sfin_claim_token()`:
      ! Unexpected HTTP 500 response while claiming the token.

