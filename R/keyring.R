## Keyring integration
##
## Access URLs are stored in the OS credential store under a fixed service
## name ("simplefinr") with a user-chosen key as the account/username field.
## This keeps multiple connections (e.g. personal vs. business) cleanly
## namespaced without colliding with other apps.

.sfin_service <- "simplefinr"

# Check that the keyring package is installed, with a helpful error if not.
check_keyring <- function() {
  if (!requireNamespace("keyring", quietly = TRUE)) {
    stop(
      "The keyring package is required to use credential store functions. ",
      "Install it with: install.packages(\"keyring\")",
      call. = FALSE
    )
  }
}

# Thin internal wrappers — kept separate so tests can mock them without
# touching the OS credential store.
keyring_key_set <- function(service, username, password) {
  keyring::key_set_with_value(service, username = username, password = password)
}

keyring_key_get <- function(service, username) {
  keyring::key_get(service, username = username)
}

keyring_key_delete <- function(service, username) {
  keyring::key_delete(service, username = username)
}

keyring_key_list <- function(service) {
  keyring::key_list(service = service)
}

#' Store a SimpleFIN Access URL in the OS credential store
#'
#' Saves a SimpleFIN Access URL securely using the system keyring (macOS
#' Keychain, Windows Credential Store, or the Secret Service on Linux).
#' Requires the \pkg{keyring} package.
#'
#' @param access_url Character. The Access URL returned by
#'   [sfin_claim_token()].
#' @param key Character. A name that identifies this credential, e.g.
#'   `"personal"` or `"business"`. Defaults to `"default"`. Use the same
#'   value when retrieving with [sfin_get_access_url()].
#'
#' @return `access_url`, invisibly.
#'
#' @seealso [sfin_get_access_url()], [sfin_delete_access_url()],
#'   [sfin_list_keys()]
#'
#' @examples
#' \dontrun{
#' token <- readline("Paste your SimpleFIN token: ")
#' access_url <- sfin_claim_token(token)
#' sfin_set_access_url(access_url, key = "personal")
#' }
#'
#' @export
sfin_set_access_url <- function(access_url, key = "default") {
  check_keyring()
  keyring_key_set(.sfin_service, username = key, password = access_url)
  invisible(access_url)
}

#' Retrieve a SimpleFIN Access URL from the OS credential store
#'
#' Fetches a previously stored Access URL from the system keyring. Requires
#' the \pkg{keyring} package.
#'
#' @param key Character. The name used when the credential was stored with
#'   [sfin_set_access_url()]. Defaults to `"default"`.
#'
#' @return The Access URL as a character string.
#'
#' @seealso [sfin_set_access_url()], [sfin_delete_access_url()],
#'   [sfin_list_keys()]
#'
#' @examples
#' \dontrun{
#' access_url <- sfin_get_access_url("personal")
#' sfin_accounts(access_url)
#'
#' # Or pass the key directly to sfin_accounts() / sfin_transactions()
#' sfin_accounts(key = "personal")
#' }
#'
#' @export
sfin_get_access_url <- function(key = "default") {
  check_keyring()
  keyring_key_get(.sfin_service, username = key)
}

#' Delete a SimpleFIN Access URL from the OS credential store
#'
#' Removes a stored Access URL from the system keyring. This does **not**
#' revoke the credential on the SimpleFIN server — visit your SimpleFIN
#' Bridge account to do that. Requires the \pkg{keyring} package.
#'
#' @param key Character. The name of the credential to remove. Defaults to
#'   `"default"`.
#'
#' @return `NULL`, invisibly.
#'
#' @seealso [sfin_set_access_url()], [sfin_get_access_url()]
#'
#' @examples
#' \dontrun{
#' sfin_delete_access_url("personal")
#' }
#'
#' @export
sfin_delete_access_url <- function(key = "default") {
  check_keyring()
  keyring_key_delete(.sfin_service, username = key)
  invisible(NULL)
}

#' List SimpleFIN Access URL keys stored in the OS credential store
#'
#' Returns a tibble of key names that have been stored via
#' [sfin_set_access_url()]. Requires the \pkg{keyring} package.
#'
#' @return A tibble with one row per stored key and a single column `key`.
#'
#' @seealso [sfin_set_access_url()], [sfin_delete_access_url()]
#'
#' @examples
#' \dontrun{
#' sfin_list_keys()
#' }
#'
#' @export
sfin_list_keys <- function() {
  check_keyring()
  result <- keyring_key_list(.sfin_service)
  tibble::tibble(key = result$username)
}
