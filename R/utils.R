# Internal utility helpers for simplefinr

# Convert a UNIX epoch integer/numeric to a POSIXct value (UTC).
# Returns NA_POSIXct_ for missing or zero values (pending transactions use 0).
unix_to_datetime <- function(x) {
  if (is.null(x) || length(x) == 0) {
    return(as.POSIXct(NA_real_, origin = "1970-01-01", tz = "UTC"))
  }
  result <- as.POSIXct(as.numeric(x), origin = "1970-01-01", tz = "UTC")
  # posted == 0 signals a pending transaction with no post date
  result[x == 0] <- NA
  result
}

# Convert a date-like value (Date, POSIXct, or numeric) to a UNIX timestamp.
to_unix <- function(x) {
  if (inherits(x, "POSIXct")) {
    return(as.numeric(x))
  }
  if (inherits(x, "Date")) {
    return(as.numeric(as.POSIXct(x, tz = "UTC")))
  }
  as.numeric(x)
}

# Resolve an access URL from either the literal value or a keyring key.
# Exactly one of access_url / key must be non-NULL.
resolve_access_url <- function(access_url, key) {
  if (!is.null(access_url) && !is.null(key)) {
    stop(
      "Provide `access_url` or `key`, not both.",
      call. = FALSE
    )
  }
  if (!is.null(key)) {
    return(sfin_get_access_url(key))
  }
  if (is.null(access_url)) {
    stop(
      "Provide an `access_url` or a keyring `key` (e.g. key = \"default\"). ",
      "Store an Access URL first with sfin_set_access_url().",
      call. = FALSE
    )
  }
  access_url
}

# Parse embedded Basic Auth credentials from an Access URL.
# Access URLs are of the form: https://user:password@host/path
# Returns a list with username, password, and the sanitized base URL.
parse_access_url <- function(access_url) {
  m <- regexec(
    "^(https?)://([^:@/]+):([^@/]+)@(.+)$",
    access_url,
    perl = TRUE
  )
  parts <- regmatches(access_url, m)[[1]]

  if (length(parts) == 0) {
    stop(
      "Could not parse the Access URL. ",
      "Expected format: https://username:password@host/path"
    )
  }

  list(
    username = parts[3],
    password = parts[4],
    base_url = paste0(parts[2], "://", parts[5])
  )
}

# Coerce a value to numeric, returning NA_real_ if the input is NULL, empty,
# or NA. Empty lists (e.g. JSON `{}`) have length 0 and are treated as NA.
null_to_na_dbl <- function(x) {
  if (is.null(x) || length(x) == 0L || (length(x) == 1L && is.na(x[[1L]]))) {
    NA_real_
  } else {
    as.numeric(x[[1L]])
  }
}

# Coerce a value to character, returning NA_character_ if NULL, empty, or NA.
null_to_na_chr <- function(x) {
  if (is.null(x) || length(x) == 0L || (length(x) == 1L && is.na(x[[1L]]))) {
    NA_character_
  } else {
    as.character(x[[1L]])
  }
}

# Coerce a value to logical, returning NA if NULL, empty, or NA.
null_to_na_lgl <- function(x) {
  if (is.null(x) || length(x) == 0L || (length(x) == 1L && is.na(x[[1L]]))) {
    NA
  } else {
    as.logical(x[[1L]])
  }
}
