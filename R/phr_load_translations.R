#' Load translation dictionaries from JSON files into the phrutils store
#'
#' Reads per-language files (`en.json` with flat `key: text` pairs) or a
#' combined file (`{"en": {...}, "fr": {...}}`) and stores the result in the
#' package-private environment used by `phr_txt()`.
#'
#' @param package Optional package name; `folder` is then resolved with
#'   `system.file()` (installed layout, so without `inst/`).
#' @param folder Folder containing the JSON file(s).
#' @param file Optional file name inside `folder`. If NULL, all `*.json`
#'   files in `folder` are read.
#' @param path Optional explicit file or directory path. Overrides
#'   `package`, `folder` and `file`.
#' @param merge If TRUE (default), merge into translations already loaded
#'   (later loads override earlier keys). If FALSE, replace them.
#' @return Invisibly, the full stored translations list.
#' @export
phr_load_translations <- function(
    package = NULL,
    folder = file.path("app", "www", "i18n"),
    file = NULL,
    path = NULL,
    merge = TRUE
) {

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
    target <- candidates[dir.exists(candidates)][1]
    if (!is.null(file) && !is.na(target)) target <- file.path(target, file)
  }

  loaded <- list()

  if (length(target) == 0 || is.na(target) || !nzchar(target) ||
      !file.exists(target)) {
    warning("Translation file or folder not found; no translations loaded.")
  } else {
    json_files <- if (dir.exists(target)) {
      list.files(target, pattern = "\\.json$", full.names = TRUE)
    } else {
      target
    }

    for (json_file in json_files) {
      tryCatch({
        content <- jsonlite::fromJSON(json_file, simplifyVector = FALSE)
        # Combined file: every top-level entry is a language list.
        # Per-language file: flat key/text pairs named by the file name.
        is_combined <- length(content) > 0 && all(vapply(content, is.list, TRUE))
        parsed <- if (is_combined) {
          content
        } else {
          stats::setNames(list(content), tools::file_path_sans_ext(basename(json_file)))
        }
        loaded <- utils::modifyList(loaded, parsed)
      }, error = function(e) {
        warning("Failed to load translation file: ", json_file, " - ", e$message)
      })
    }
  }

  current <- if (merge) .phr_env$translations %||% list() else list()
  .phr_env$translations <- utils::modifyList(current, loaded)

  invisible(.phr_env$translations)
}
