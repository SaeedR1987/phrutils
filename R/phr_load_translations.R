#' Load translation dictionaries from JSON files
#'
#' Reads translations from either per-language files (`en.json` containing
#' flat `key: text` pairs) or a combined file (`{"en": {...}, "fr": {...}}`),
#' and stores them in the phrutils environment so `phr_txt()` and
#' `get_phr_translations()` can use them.
#'
#' @param package Optional package name. If given, `folder` is resolved with
#'   `system.file()` (i.e. relative to the installed package root, so
#'   `inst/` is omitted).
#' @param folder Folder containing the JSON file(s). Default
#'   `"app/www/i18n"`.
#' @param file Optional file name inside `folder` (e.g. `"translations.json"`).
#'   If NULL, every `*.json` file in `folder` is read.
#' @param path Optional explicit file or directory path. Overrides
#'   `package`/`folder`/`file`.
#' @param merge If TRUE (default), merge into translations already loaded
#'   (later loads override earlier keys). If FALSE, replace them.
#' @param store If TRUE (default), save the result in the phrutils
#'   environment. If FALSE, only return it.
#' @return Invisibly, the full translations list.
#' @export
phr_load_translations <- function(
    package = NULL,
    folder = file.path("app", "www", "i18n"),
    file = NULL,
    path = NULL,
    merge = TRUE,
    store = TRUE
) {

  # Resolve target ---------------------------------------------------------
  if (!is.null(path)) {
    target <- path
  } else if (!is.null(package)) {
    target <- system.file(folder, package = package)
    if (!is.null(file) && nzchar(target)) target <- file.path(target, file)
  } else {
    candidates <- c(
      file.path(getwd(), "inst", folder),
      file.path(getwd(), folder)
    )
    target <- candidates[file.exists(candidates)][1]
    if (!is.null(file) && !is.na(target)) target <- file.path(target, file)
  }

  if (length(target) == 0 || is.na(target) || !nzchar(target) ||
      !file.exists(target)) {
    warning("Translation file or folder not found; no translations loaded.")
    loaded <- list()
  } else {
    json_files <- if (dir.exists(target)) {
      list.files(target, pattern = "\\.json$", full.names = TRUE)
    } else {
      target
    }

    loaded <- list()
    for (json_file in json_files) {
      tryCatch({
        content <- jsonlite::fromJSON(json_file, simplifyVector = FALSE)
        lang_code <- tools::file_path_sans_ext(basename(json_file))
        # Combined file: top-level entries are languages (each a list of
        # key/text). Per-language file: flat key/text pairs.
        is_combined <- length(content) > 0 && all(vapply(content, is.list, TRUE))
        parsed <- if (is_combined) content else stats::setNames(list(content), lang_code)
        loaded <- utils::modifyList(loaded, parsed)
      }, error = function(e) {
        warning("Failed to load translation file: ", json_file, " - ", e$message)
      })
    }
  }

  result <- if (merge) {
    utils::modifyList(.phr_env$translations %||% list(), loaded)
  } else {
    loaded
  }

  if (store) .phr_env$translations <- result
  invisible(result)
}
