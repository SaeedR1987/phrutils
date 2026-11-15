#' Translate text using a translations list
#'
#' Looks up `key` in `translations`, falling back to English, then `default`,
#' then the marked key. Glue expressions are evaluated in the caller's
#' environment.
#'
#' @param key Character text (English) used to look up the translation. It is
#'   converted to a lookup key with `phr_text_to_key()`.
#' @param lang Optional language code. Defaults to "en".
#' @param default Optional fallback text.
#' @param translations Named list of translations, `list(<lang> = list(<key> = text))`.
#'   Defaults to the translations loaded by the app via `get_phr_translations()`.
#' @param .envir Environment in which glue expressions are evaluated.
#'   Defaults to the caller's environment.
#' @return Character string.
#' @export
phr_txt <- function(
    key,
    lang = NULL,
    default = NULL,
    translations = phrutils::get_phr_translations(),
    .envir = parent.frame()
) {

  if (is.null(lang) || !lang %in% names(translations)) {
    lang <- "en"
  }

  lookup_key <- phr_text_to_key(key)

  value <- translations[[lang]][[lookup_key]]

  if (is.null(value) && lang != "en") {
    value <- translations[["en"]][[lookup_key]]
  }

  if (is.null(value) || value == "") {
    value <- if (is.null(default)) paste0("\u29EB", key, "\u29EB") else default
  }

  as.character(glue::glue(value, .envir = .envir))
}
