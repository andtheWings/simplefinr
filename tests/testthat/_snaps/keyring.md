# check_keyring errors with install hint when keyring is absent

    Code
      sfin_set_access_url(fake_url)
    Condition
      Error:
      ! The keyring package is required

# resolve_access_url errors when both access_url and key are provided

    Code
      resolve_access_url("https://x:y@host/sfin", "mykey")
    Condition
      Error:
      ! Provide `access_url` or `key`, not both.

# resolve_access_url errors when neither access_url nor key is provided

    Code
      resolve_access_url(NULL, NULL)
    Condition
      Error:
      ! Provide an `access_url` or a keyring `key` (e.g. key = "default"). Store an Access URL first with sfin_set_access_url().

