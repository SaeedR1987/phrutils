#' Initialize (replace) the stored translations
#'
#' Clears what is stored and loads from the given source. Takes the same
#' arguments as `phr_load_translations()`.
#'
#' @inheritParams phr_load_translations
#' @export
phr_init_translations <- function(...) {
  phr_load_translations(..., merge = FALSE)
}
