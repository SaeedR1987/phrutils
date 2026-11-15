#' Get the translations stored in the phrutils environment
#'
#' Lazily loads translations from the default location if none have been
#' loaded yet.
#'
#' @return Named list: `list(<lang> = list(<key> = text))`.
#' @export
get_phr_translations <- function() {
  if (is.null(.phr_env$translations)) {
    phr_load_translations()
  }
  .phr_env$translations
}
