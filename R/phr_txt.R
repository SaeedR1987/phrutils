#' Translate text using a translations list
#'
#' Looks up `key` in `translations`, falling back to English, then `default`,
#' then the marked key. Glue expressions are evaluated in `.envir`.
#'
#' @param key Character text (English) used to look up the translation.
#'   Converted to a lookup key with `phr_text_to_key()`.
#' @param lang Optional language code. Defaults to the language stored in the
#'   phrutils environment (`.phr_env$current_lang`), then "en".
#' @param default Optional fallback text.
#' @param translations Named list `list(<lang> = list(<key> = text))`.
#'   Defaults to the store in the phrutils environment.
#' @param .envir Environment in which glue expressions are evaluated.
#'   Defaults to the caller's environment.
#' @return Character string.
#' @export
phr_txt <- function(
    key,
    lang = NULL,
    default = NULL,
    translations = get_phr_translations(),
    .envir = parent.frame()
) {

  lang <- lang %||% .phr_env$current_lang %||% "en"

  if (!lang %in% names(translations)) {
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
