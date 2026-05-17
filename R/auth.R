#' Claim a SimpleFIN token to obtain an Access URL
#'
#' Takes a Base64-encoded SimpleFIN Token and exchanges it for a permanent
#' Access URL by making a one-time POST request to the decoded claim URL.
#'
#' @param token A Base64-encoded SimpleFIN Token string. Users obtain this by
#'   visiting their institution's `/create` endpoint or the
#'   [SimpleFIN Bridge](https://bridge.simplefin.org/simplefin/create).
#'
#' @return A character string containing the Access URL. The URL embeds HTTP
#'   Basic Auth credentials in the form `https://user:password@host/path`.
#'   Store this value securely — treat it like a password.
#'
#' @details
#' This function performs the token-claim step of the SimpleFIN flow. Each
#' token can only be claimed once; call this function once and persist the
#' returned Access URL for all future use with [sfin_accounts()].
#'
#' A 403 response means the token has already been claimed or is invalid.
#' In that case, the user should be notified to revoke and regenerate their
#' token because it may have been compromised.
#'
#' Only HTTPS claim URLs are accepted.
#'
#' @examples
#' \dontrun{
#' # Demo token from the SimpleFIN Bridge (reusable for testing)
#' token <- "aHR0cHM6Ly9icmlkZ2Uuc2ltcGxlZmluLm9yZy9zaW1wbGVmaW4vY2xhaW0vZGVtbw=="
#' access_url <- sfin_claim_token(token)
#' # Store access_url securely, e.g. in keyring or .Renviron
#' }
#'
#' @export
sfin_claim_token <- function(token) {
  claim_url <- tryCatch(
    rawToChar(jsonlite::base64_dec(trimws(token))),
    error = function(e) {
      stop("Failed to Base64-decode the SimpleFIN token: ", conditionMessage(e))
    }
  )

  if (!grepl("^https://", claim_url)) {
    stop(
      "The decoded SimpleFIN token must point to an HTTPS URL. ",
      "Only HTTPS connections are permitted."
    )
  }

  resp <- httr2::request(claim_url) |>
    httr2::req_method("POST") |>
    httr2::req_error(is_error = \(resp) FALSE) |>
    httr2::req_perform()

  status <- httr2::resp_status(resp)

  if (status == 403L) {
    stop(
      "HTTP 403: The SimpleFIN token has already been claimed or is invalid. ",
      "Notify the user — their token may be compromised."
    )
  }

  if (status != 200L) {
    stop("Unexpected HTTP ", status, " response while claiming the token.")
  }

  trimws(httr2::resp_body_string(resp))
}
