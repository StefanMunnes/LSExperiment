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
#'   - inspect the labelled design table
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

  langs <- get_preview_languages(vig_data$design_data)
  vignette_data <- build_preview_vignette_data(vig_data, langs)
  vignette_json <- jsonlite::toJSON(
    vignette_data,
    auto_unbox = TRUE,
    pretty = FALSE,
    null = "null"
  )

  design_df <- get_preview_design_df(vig_data$design_data, langs[[1]])
  design_table_html <- build_design_table_html(design_df)

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

  html_content <- sprintf(
    paste(
      "<!DOCTYPE html>",
      "<html lang=\"en\">",
      "<head>",
      "<meta charset=\"UTF-8\">",
      "<meta name=\"viewport\" content=\"width=device-width, initial-scale=1.0\">",
      "<title>%s</title>",
      "<style>%s</style>",
      "</head>",
      "<body class=\"no-variable-highlights\">",
      "<div class=\"container\">",
      "<div class=\"header\">",
      "<h1>%s</h1>",
      "<p>Interactive preview of survey vignettes with live deck selection.</p>",
      "</div>",
      "<div class=\"controls\">",
      "<div class=\"control-group\">",
      "<label for=\"deck-select\">Deck</label>",
      "<select id=\"deck-select\">%s</select>",
      "</div>",
      "<div class=\"control-group %s\">",
      "<label for=\"lang-select\">Language</label>",
      "<select id=\"lang-select\">%s</select>",
      "</div>",
      "<div class=\"info-badge\"><strong>Vignettes:</strong> %d | <strong>Decks:</strong> %d</div>",
      "<button class=\"btn\" id=\"toggle-highlights\" type=\"button\">Toggle highlights</button>",
      "<button class=\"btn\" id=\"toggle-table\" type=\"button\">Toggle level table</button>",
      "<button class=\"btn\" id=\"print-preview\" type=\"button\">Print preview</button>",
      "</div>",
      "<div id=\"vignettes-container\" class=\"vignettes-grid\"></div>",
      "<div id=\"design-table-wrapper\" class=\"table-wrapper is-hidden\">",
      "<h2>Level table</h2>",
      "%s",
      "</div>",
      "</div>",
      "<script>",
      "const vignetteData = %s;",
      "const hasMultipleLanguages = %s;",
      "%s",
      "</script>",
      "</body>",
      "</html>",
      sep = "\n"
    ),
    title,
    preview_css(),
    title,
    deck_options,
    if (length(langs) == 1) "is-hidden" else "",
    lang_options,
    vig_data$vigs,
    vig_data$decks,
    design_table_html,
    vignette_json,
    tolower(as.character(length(langs) > 1)),
    preview_js()
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


#' Build HTML table from preview data
#' @keywords internal
build_design_table_html <- function(preview_df) {
  display_cols <- c(
    "deck",
    "vig",
    setdiff(base::names(preview_df), c("deck", "vig"))
  )
  display_cols <- intersect(display_cols, base::names(preview_df))
  df_display <- preview_df[, display_cols, drop = FALSE]

  header_html <- paste(
    sprintf("<th>%s</th>", html_escape(base::names(df_display))),
    collapse = ""
  )

  row_html <- vapply(seq_len(nrow(df_display)), function(i) {
    deck_val <- df_display[i, "deck"][[1]]
    cell_values <- vapply(df_display[i, , drop = FALSE], as.character, character(1))
    cells <- paste(sprintf("<td>%s</td>", html_escape(cell_values)), collapse = "")
    sprintf('<tr data-deck="%s">%s</tr>', deck_val, cells)
  }, character(1))

  paste0(
    '<table class="design-table"><thead><tr>',
    header_html,
    "</tr></thead><tbody>",
    paste(row_html, collapse = ""),
    "</tbody></table>"
  )
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
preview_css <- function() {
  paste(
    "body { font-family: Arial, sans-serif; margin: 0; background: #f4f6f8; color: #18212b; }",
    ".container { max-width: 1200px; margin: 0 auto; padding: 24px; }",
    ".header { margin-bottom: 20px; }",
    ".header h1 { margin: 0 0 8px; }",
    ".controls { display: flex; gap: 12px; flex-wrap: wrap; align-items: end; margin-bottom: 20px; }",
    ".control-group { display: flex; flex-direction: column; gap: 6px; min-width: 140px; }",
    ".control-group label { font-size: 0.9rem; font-weight: 700; }",
    ".control-group select, .btn { border: 1px solid #c3ced8; border-radius: 8px; padding: 10px 12px; font-size: 0.95rem; background: #fff; }",
    ".btn { cursor: pointer; }",
    ".info-badge { padding: 10px 12px; background: #fff; border: 1px solid #c3ced8; border-radius: 8px; }",
    ".vignettes-grid { display: grid; grid-template-columns: repeat(auto-fit, minmax(260px, 1fr)); gap: 16px; }",
    ".vignette-card { background: #fff; border: 1px solid #d6dde4; border-radius: 12px; padding: 18px; box-shadow: 0 6px 18px rgba(0, 0, 0, 0.06); }",
    ".vignette-card h2 { margin-top: 0; font-size: 1rem; }",
    ".variables { margin-top: 14px; border-top: 1px solid #e5ebf0; padding-top: 10px; }",
    ".variables dl { margin: 0; display: grid; grid-template-columns: auto 1fr; gap: 6px 10px; }",
    ".variables dt { font-weight: 700; }",
    ".table-wrapper { margin-top: 24px; background: #fff; border: 1px solid #d6dde4; border-radius: 12px; padding: 18px; overflow-x: auto; }",
    ".design-table { width: 100%; border-collapse: collapse; }",
    ".design-table th, .design-table td { border: 1px solid #d6dde4; padding: 8px 10px; text-align: left; vertical-align: top; }",
    ".design-table th { background: #eef2f6; }",
    ".is-hidden { display: none !important; }",
    ".no-variable-highlights .variable-highlight { background: transparent; padding: 0; font-weight: inherit; }",
    ".variable-highlight { background: #fff0b3; padding: 0 2px; border-radius: 3px; font-weight: 700; }",
    "@media print { .controls { display: none; } body { background: #fff; } .table-wrapper.is-hidden { display: block !important; } .vignette-card { break-inside: avoid; box-shadow: none; } }",
    sep = "\n"
  )
}


#' @keywords internal
preview_js <- function() {
  paste(
    "const deckSelect = document.getElementById('deck-select');",
    "const langSelect = document.getElementById('lang-select');",
    "const container = document.getElementById('vignettes-container');",
    "const tableWrapper = document.getElementById('design-table-wrapper');",
    "const highlightButton = document.getElementById('toggle-highlights');",
    "const tableButton = document.getElementById('toggle-table');",
    "const printButton = document.getElementById('print-preview');",
    "",
    "function currentLang() {",
    "  return hasMultipleLanguages ? langSelect.value : 'default';",
    "}",
    "",
    "function renderDeck() {",
    "  const deck = deckSelect.value;",
    "  const lang = currentLang();",
    "  const deckData = vignetteData[deck] || {};",
    "  const cards = Object.keys(deckData).sort((a, b) => Number(a) - Number(b)).map(function(vig) {",
    "    const entry = deckData[vig][lang] || deckData[vig].default || deckData[vig][Object.keys(deckData[vig])[0]];",
    "    if (!entry) { return ''; }",
    "    const variableRows = Object.keys(entry.variables).map(function(name) {",
    "      return '<dt>' + name + '</dt><dd>' + entry.variables[name] + '</dd>';",
    "    }).join('');",
    "    return [",
    "      '<article class=\"vignette-card\">',",
    "      '<h2>Vignette ' + vig + '</h2>',",
    "      '<div class=\"vignette-text\">' + entry.rendered + '</div>',",
    "      '<div class=\"variables\"><dl>' + variableRows + '</dl></div>',",
    "      '</article>'",
    "    ].join('');",
    "  });",
    "  container.innerHTML = cards.join('');",
    "  filterTable(deck);",
    "}",
    "",
    "function filterTable(deck) {",
    "  const rows = tableWrapper.querySelectorAll('tbody tr');",
    "  rows.forEach(function(row) {",
    "    row.classList.toggle('is-hidden', row.dataset.deck !== deck);",
    "  });",
    "}",
    "",
    "deckSelect.addEventListener('change', renderDeck);",
    "langSelect.addEventListener('change', renderDeck);",
    "highlightButton.addEventListener('click', function() {",
    "  document.body.classList.toggle('no-variable-highlights');",
    "});",
    "tableButton.addEventListener('click', function() {",
    "  tableWrapper.classList.toggle('is-hidden');",
    "});",
    "printButton.addEventListener('click', function() {",
    "  window.print();",
    "});",
    "renderDeck();",
    sep = "\n"
  )
}
