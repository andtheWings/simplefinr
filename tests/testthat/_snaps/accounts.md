# sfin_accounts stops on HTTP 403

    Code
      httr2::with_mocked_responses(mock_fn, sfin_accounts(demo_access_url))
    Condition
      Error in `sfin_accounts()`:
      ! HTTP 403: Access denied. Credentials may be invalid or access has been revoked. The user should visit their SimpleFIN Bridge to reconnect.

# sfin_accounts stops on HTTP 402

    Code
      httr2::with_mocked_responses(mock_fn, sfin_accounts(demo_access_url))
    Condition
      Error in `sfin_accounts()`:
      ! HTTP 402: Payment required to access this SimpleFIN server.

