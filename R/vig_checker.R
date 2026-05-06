# =============================================================================
# Simplified Interactive Vignette Preview Generator
# =============================================================================

#' Preview vignettes in an interactive HTML file
#'
#' @param vig_data Output from `build_vignette_data()` with `design_data` and
#'   `texts`.
#' @param file Path to save the HTML file.
#' @param title Title for the preview page.
#' @param open_browser Logical, whether to open in a browser after creation.
#'
#' @return Path to the created HTML file, invisibly.
#'
#' @details Creates a self-contained HTML page where users can:
#'   - select a deck number to see a live preview
#'   - switch between languages
#'   - inspect highlighted factor levels
#'
#' @export
preview_vignettes <- function(
  vig_data,
  file = "vignette_preview.html",
  title = "Vignette Preview",
  open_browser = FALSE
) {
  if (is.null(vig_data$design_data)) {
    stop(
      "vig_data must have design_data. Set store_design_data = TRUE in build_vignette_data()."
    )
  }

  if (is.null(vig_data$texts)) {
    stop("vig_data must have texts.")
  }

  preview_assets <- load_preview_assets()
  langs <- get_preview_languages(vig_data$design_data)
  vignette_data <- build_preview_vignette_data(vig_data, langs)
  vignette_json <- jsonlite::toJSON(
    vignette_data,
    auto_unbox = TRUE,
    pretty = FALSE,
    null = "null"
  )

  design_df <- get_preview_design_df(vig_data$design_data, langs[[1]])

  deck_options <- paste(
    sprintf(
      '<option value="%d">%d</option>',
      seq_len(vig_data$decks),
      seq_len(vig_data$decks)
    ),
    collapse = "\n"
  )

  lang_options <- paste(
    sprintf('<option value="%s">%s</option>', langs, toupper(langs)),
    collapse = "\n"
  )

  lang_control_html <- ""
  if (length(langs) > 1) {
    lang_control_html <- paste(
      '<div class="control-group">',
      '<label for="lang-select">Language</label>',
      sprintf('<select id="lang-select">%s</select>', lang_options),
      "</div>",
      sep = "\n"
    )
  }

  info_badge_html <- sprintf(
    "<span><strong>Vignettes:</strong> %d</span><span class=\"info-separator\">|</span><span><strong>Decks:</strong> %d</span>",
    vig_data$vigs,
    vig_data$decks
  )

  html_content <- render_preview_template(
    preview_assets$template,
    list(
      TITLE = html_escape(title),
      CSS = preview_assets$css,
      DECK_OPTIONS = deck_options,
      LANG_CONTROL = lang_control_html,
      INFO_BADGE = info_badge_html,
      VIGNETTE_JSON = vignette_json,
      HAS_MULTIPLE_LANGUAGES = tolower(as.character(length(langs) > 1)),
      DEFAULT_LANGUAGE = jsonlite::toJSON(langs[[1]], auto_unbox = TRUE),
      JS = preview_assets$js
    )
  )

  output_dir <- dirname(file)
  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
  }

  writeLines(html_content, file, useBytes = TRUE)
  message("Vignette preview created: ", file)

  if (isTRUE(open_browser)) {
    utils::browseURL(normalizePath(file, winslash = "/", mustWork = FALSE))
  }

  invisible(file)
}


#' Build preview vignette data structure from design_data
#' @keywords internal
build_preview_vignette_data <- function(vig_data, langs) {
  vignette_data <- list()

  for (deck in seq_len(vig_data$decks)) {
    deck_data <- list()

    for (vig in seq_len(vig_data$vigs)) {
      vig_name <- paste0(vig_data$prefix, vig)
      vig_info <- list()

      for (lang in langs) {
        preview_df <- get_preview_design_df(vig_data$design_data, lang)
        row_data <- preview_df[preview_df$deck == deck & preview_df$vig == vig, ]

        if (nrow(row_data) == 0) {
          next
        }

        var_values <- as.list(row_data[1, vig_data$vig_vars, drop = FALSE])
        rendered_text <- render_vignette_text(row_data$text[[1]], var_values)

        vig_info[[lang]] <- list(
          id = vig_name,
          rendered = rendered_text,
          variables = var_values
        )
      }

      deck_data[[as.character(vig)]] <- vig_info
    }

    vignette_data[[as.character(deck)]] <- deck_data
  }

  vignette_data
}


#' Render vignette text with actual variable values
#' @keywords internal
render_vignette_text <- function(text, var_values) {
  rendered <- text

  for (value in unlist(var_values, use.names = FALSE)) {
    value <- as.character(value)
    rendered <- stringr::str_replace_all(
      rendered,
      stringr::fixed(value),
      sprintf('<span class="variable-highlight">%s</span>', value)
    )
  }

  rendered
}


#' @keywords internal
get_preview_languages <- function(design_data) {
  if (is.data.frame(design_data)) {
    return("default")
  }

  langs <- base::names(design_data)
  if (is.null(langs) || length(langs) == 0) {
    stop("design_data must be a data frame or a named list of data frames.")
  }

  langs
}


#' @keywords internal
get_preview_design_df <- function(design_data, lang) {
  if (lang == "default") {
    return(design_data)
  }

  design_data[[lang]]
}


#' @keywords internal
html_escape <- function(x) {
  x <- gsub("&", "&amp;", x, fixed = TRUE)
  x <- gsub("<", "&lt;", x, fixed = TRUE)
  x <- gsub(">", "&gt;", x, fixed = TRUE)
  gsub('"', "&quot;", x, fixed = TRUE)
}


#' @keywords internal
resolve_preview_resource_dir <- function() {
  local_dir <- file.path("inst", "preview")
  if (dir.exists(local_dir)) {
    return(local_dir)
  }

  package_dir <- system.file("preview", package = "LSExperiment")
  if (nzchar(package_dir) && dir.exists(package_dir)) {
    return(package_dir)
  }

  stop("Could not locate bundled preview resources.")
}


#' @keywords internal
load_preview_assets <- function() {
  dir_path <- resolve_preview_resource_dir()
  file_names <- c(
    template = "template.html",
    css = "style.css",
    js = "script.js"
  )

  file_paths <- file.path(dir_path, unname(file_names))
  missing_files <- file_paths[!file.exists(file_paths)]

  if (length(missing_files) > 0) {
    stop(
      "Missing preview resource file(s): ",
      paste(basename(missing_files), collapse = ", ")
    )
  }

  contents <- lapply(file_paths, function(path) {
    paste(readLines(path, warn = FALSE, encoding = "UTF-8"), collapse = "\n")
  })

  stats::setNames(contents, names(file_names))
}


#' @keywords internal
replace_fixed <- function(x, token, value) {
  pieces <- strsplit(x, token, fixed = TRUE)[[1]]
  paste(pieces, collapse = value)
}


#' @keywords internal
render_preview_template <- function(template, replacements) {
  output <- template

  for (name in base::names(replacements)) {
    output <- replace_fixed(output, paste0("{{", name, "}}"), replacements[[name]])
  }

  output
}
