# =============================================================================
# Auto-detecting Multi-Language Vignette Code Generator
# =============================================================================

#' Helper: Get language-specific text from multilingual object
#' @keywords internal
get_lang <- function(x, lang, fallback = NA_character_) {
  if (is.null(x)) {
    return(fallback)
  }

  # Named list
  if (is.list(x) && !is.null(names(x)) && lang %in% names(x)) {
    return(as.character(x[[lang]]))
  }

  # Named atomic vector
  if (!is.list(x) && !is.null(names(x)) && lang %in% names(x)) {
    return(as.character(x[[lang]]))
  }

  # Fallback: plain value as label
  as.character(x)[1]
}


#' Detect available languages from factors structure
#'
#' @param factors Named list of factor definitions
#' @return Character vector of detected language codes
detect_languages <- function(factors) {
  factors |>
    purrr::map(\(lvl1) purrr::map(lvl1, names)) |>
    purrr::list_flatten() |>
    purrr::discard(is.null) |>
    unlist(use.names = FALSE) |>
    unique() |>
    sort()
}


#' Generate vignette codes for a single language
#'
#' @keywords internal
prepare_vignettes_single_lang <- function(
  factors,
  design_df = NULL,
  text = NULL,
  vig_n = NULL,
  lang = NULL,
  prefix = "vig",
  names = NULL,
  store_design_data = TRUE
) {
  vig_vars <- base::names(factors)

  if (!is.null(design_df)) {
    required_cols <- c("deck", "vig", vig_vars)
    missing_cols <- setdiff(required_cols, base::names(design_df))

    if (length(missing_cols) > 0) {
      stop(
        "design_df is missing required columns: ",
        paste(missing_cols, collapse = ", ")
      )
    }
  }

  if (is.null(design_df)) {
    # Generate full factorial design
    design_lab <- factors |>
      lapply(function(x) names(x)) |>
      expand.grid(stringsAsFactors = FALSE) |>
      dplyr::slice_sample(prop = 1)

    if (is.null(vig_n)) {
      stop("Either 'design_df' or 'vig_n' must be provided")
    }

    decks <- ceiling(nrow(design_lab) / vig_n)

    # Adjust to exact number of rows needed
    total_rows <- decks * vig_n
    if (nrow(design_lab) < total_rows) {
      # Recycle rows if needed
      design_lab <- design_lab[
        rep(seq_len(nrow(design_lab)), length.out = total_rows),
      ]
    } else {
      design_lab <- design_lab[1:total_rows, ]
    }

    design_lab <- design_lab |>
      dplyr::mutate(
        deck = rep(1:decks, each = vig_n),
        vig = rep(1:vig_n, decks),
        .before = 1
      )

    # Convert factor levels to labels
    for (var in vig_vars) {
      design_lab[[var]] <- sapply(design_lab[[var]], function(level) {
        lookup <- factors[[var]][[as.character(level)]]
        if (is.null(lang)) {
          as.character(lookup)
        } else {
          get_lang(lookup, lang, fallback = as.character(level))
        }
      })
    }
  } else {
    # 1. LABEL MAPPING: Convert numeric IDs into human-readable text labels
    design_lab <- design_df |>
      dplyr::mutate(dplyr::across(
        dplyr::all_of(vig_vars),
        ~ purrr::map_chr(.x, \(v) {
          # Access the nested 'factors' list using the current column name and the numeric value 'v'
          lookup <- factors[[dplyr::cur_column()]][[as.character(v)]]
          if (is.null(lang)) {
            # If no language is specified, return the label as a simple character
            as.character(lookup)
          } else {
            # If language exists, use a custom helper to fetch the correct translation
            get_lang(lookup, lang, fallback = as.character(v))
          }
        }),
        .names = "{.col}_lbl"
      ))

    # Identify the maximum number of vignettes and decks (for output and name randomization)
    vig_n <- max(design_df$vig)
    decks <- max(design_df$deck)

    # 2. NAME RANDOMIZATION: Assign names based factor variables
    if (!is.null(names)) {
      # Helper function: Creates a shuffled vector of names across all experimental decks
      sample_for_all_decks <- function(names, decks_max = decks, vigs = vig_n) {
        replicate(decks_max, sample(names, size = vigs, replace = FALSE)) |>
          as.vector()
      }

      # Recursively apply the shuffling function to the nested names list
      names_shuffled <- rapply(names, sample_for_all_decks, how = "list")

      n_rows <- nrow(design_lab)
      name_vec <- character(n_rows)

      # Loop through rows to pull names that match the specific Gender/Age of that vignette
      for (i in 1:n_rows) {
        g_idx <- as.numeric(design_lab$gender[i])
        a_idx <- as.numeric(design_lab$age[i])

        # Extract name from the shuffled list based on demographics and (optionally) language
        if (!is.null(lang)) {
          name_vec[i] <- names_shuffled[[g_idx]][[a_idx]][[lang]][i]
        } else {
          name_vec[i] <- names_shuffled[[g_idx]][[a_idx]][i]
        }
      }

      design_lab$name <- name_vec
    }

    # 3. Fill in {...} placeholders (ifelse functions or factor variables) in factor labels
    design_lab <- design_lab |>
      dplyr::rowwise() |>
      dplyr::mutate(dplyr::across(
        # Evaluate any code inside {} within the label strings
        dplyr::ends_with("_lbl"),
        ~ glue::glue_data(dplyr::pick(dplyr::everything()), .x)
      )) |>
      dplyr::ungroup() |>
      # Replace the original numeric columns with labelled factors
      dplyr::select(-dplyr::all_of(vig_vars)) |>
      dplyr::rename_with(
        ~ sub("_lbl$", "", .x),
        dplyr::all_of(paste0(vig_vars, "_lbl"))
      )
  }

  # Add name to variable list if present
  if (!is.null(names)) {
    vig_vars <- c("name", vig_vars)
  }

  # Pivot to wide format
  design_wide <- design_lab |>
    # remove curly brackets from values (error in LS expression manager if nested)
    dplyr::mutate(dplyr::across(
      dplyr::all_of(vig_vars),
      ~ gsub("\\{|\\}", "", .x)
    )) |>
    tidyr::pivot_wider(
      id_cols = "deck",
      names_from = "vig",
      values_from = dplyr::all_of(vig_vars),
      names_glue = paste0(prefix, "{vig}{.value}")
    )

  deck_values <- design_wide[["deck"]]

  design_wide <- design_wide |>
    dplyr::mutate(dplyr::across(
      dplyr::starts_with(prefix),
      ~ {
        ifelse(
          grepl("^if\\(", .x),
          paste0("{if(", prefix, "deck==", deck_values, ", ", .x, ")}"),
          paste0("{if(", prefix, "deck==", deck_values, ", '", .x, "')}")
        )
      }
    )) |>
    dplyr::select(-"deck")

  # Create conditions as character vector
  conditions <- design_wide |>
    apply(2, function(x) paste(x, collapse = ""))

  result <- list(
    conditions = conditions,
    vigs = vig_n,
    decks = decks
  )

  if (store_design_data) {
    # Add vignette specific text (fill ifelse conditions with values)
    if (!is.null(text)) {
      design_lab <- design_lab |>
        dplyr::mutate(
          text = get_lang(text, lang) |>
            stringr::str_replace_all("\\{if\\(", "\\{ifelse\\(")
        ) |>
        dplyr::rowwise() |>
        dplyr::mutate(
          text = glue::glue_data(dplyr::pick(dplyr::everything()), text)
        ) |>
        dplyr::ungroup()
    }

    result$design_data <- design_lab
  }

  return(result)
}


#' Build vignette data with automatic multi-language detection
#'
#' @param factors Named list of factor definitions with multilingual labels
#' @param design_df Optional data frame with design matrix (columns: deck, vig,
#'   factor variables coded as integers)
#' @param vig_n Number of vignettes per deck (required if design_df is NULL)
#' @param lang Single language code or NULL for auto-detection of all languages
#' @param prefix Prefix for vignette variable names (default: "vig")
#' @param text Optional named list of text templates by language
#' @param names Optional nested list structure for person names
#' @param store_design_data Logical, wheter to store labeled design data for preview
#'
#' @return List with structure:
#'   - vigs: number of vignettes
#'   - decks: number of decks
#'   - prefix: variable prefix
#'   - vig_vars: character vector of variable names
#'   - conditions: named list where each element is a list of language codes
#'     (e.g., conditions$vigper1name$de, conditions$vigper1name$en)
#'   - texts: named list by language code (e.g., text$de, text$en)
#'   - design_data: (if store_design_data=TRUE) nested list by language with
#'     labeled design data for creating previews
#'
#' @examples
#' \dontrun{
#' # Auto-detect all languages
#' result <- build_vignette_data(
#'   factors = fct_per,
#'   design_df = design_sas,
#'   text = text_templates,
#'   names = names_5,
#' )
#'
#' # Access structure:
#' result$conditions$vigper1name$de  # German condition string
#' result$conditions$vigper1name$en  # English condition string
#' result$texts$vigper1$de            # German vignette text for vignette 1
#' result$texts$vigper1$en            # English vignette text for vignette 1
#' }
#'
#' @export
build_vignette_data <- function(
  factors,
  design_df = NULL,
  vig_n = NULL,
  lang = NULL,
  prefix = "vig",
  text = NULL,
  names = NULL,
  store_design_data = TRUE
) {
  # Validate inputs
  if (is.null(factors) || !is.list(factors) || is.null(base::names(factors))) {
    stop("'factors' must be a named list")
  }

  if (is.null(design_df) && is.null(vig_n)) {
    stop("Either 'design_df' or 'vig_n' must be provided")
  }

  vig_vars <- base::names(factors)
  if (!is.null(names)) {
    vig_vars <- c("name", vig_vars)
  }

  # Single language mode
  if (!is.null(lang)) {
    if (!is.character(lang) || length(lang) != 1) {
      stop("'lang' must be a single character string or NULL")
    }

    result <- prepare_vignettes_single_lang(
      factors = factors,
      design_df = design_df,
      vig_n = vig_n,
      text = text,
      lang = lang,
      prefix = prefix,
      names = names,
      store_design_data = store_design_data
    )

    result$prefix <- prefix
    result$vig_vars <- vig_vars

    # Add text if provided
    if (!is.null(text)) {
      if (is.list(text) && lang %in% names(text)) {
        result$texts <- build_vignette_texts(result, text[[lang]])
      } else {
        result$texts <- build_vignette_texts(result, text)
      }
    }

    return(result)
  }

  # Multi-language mode: auto-detect languages
  langs <- detect_languages(factors)

  if (length(langs) == 0) {
    message("No languages detected in factors. Using single-language mode.")
    result <- prepare_vignettes_single_lang(
      factors = factors,
      design_df = design_df,
      vig_n = vig_n,
      text = text,
      lang = NULL,
      prefix = prefix,
      names = names,
      store_design_data = store_design_data
    )

    result$prefix <- prefix
    result$vig_vars <- vig_vars

    if (!is.null(text)) {
      result$texts <- build_vignette_texts(result, text)
    }

    return(result)
  }

  message("Detected languages: ", paste(langs, collapse = ", "))

  # Generate for each language
  lang_results <- lapply(langs, function(l) {
    message("Processing language: ", l)

    prepare_vignettes_single_lang(
      factors = factors,
      design_df = design_df,
      vig_n = vig_n,
      text = text,
      lang = l,
      prefix = prefix,
      names = names,
      store_design_data = store_design_data
    )
  })

  names(lang_results) <- langs

  # Restructure to YAML format: conditions$varname$lang
  # Get variable names from first language
  var_names <- names(lang_results[[langs[1]]]$conditions)

  # Build nested structure: conditions$varname$lang = "STRING"
  conditions_nested <- lapply(var_names, function(var) {
    lang_list <- lapply(langs, function(l) {
      lang_results[[l]]$conditions[[var]]
    })
    names(lang_list) <- langs
    lang_list
  })
  names(conditions_nested) <- var_names

  # Build text structure: text$lang = "STRING"
  # text_nested <- NULL
  # if (!is.null(text)) {
  #   text_nested <- lapply(langs, function(l) {
  #     if (l %in% names(text)) {
  #       text[[l]]
  #     } else {
  #       NULL
  #     }
  #   })
  #   names(text_nested) <- langs
  #   text_nested <- purrr::compact(text_nested) # Remove NULL entries
  # }

  # Build design_data structure: design_data$lang = data.frame
  design_data_nested <- NULL
  if (store_design_data) {
    design_data_nested <- lapply(langs, function(l) {
      lang_results[[l]]$design_data
    })
    names(design_data_nested) <- langs
  }

  # Return unified structure
  result <- list(
    vigs = lang_results[[langs[1]]]$vigs,
    decks = lang_results[[langs[1]]]$decks,
    prefix = prefix,
    vig_vars = vig_vars,
    conditions = conditions_nested
  )

  if (!is.null(text)) {
    result$texts <- build_vignette_texts(result, text)
  }

  # if (!is.null(text_nested) && length(text_nested) > 0) {
  #   result$texts <- text_nested
  # }

  if (!is.null(design_data_nested)) {
    result$design_data <- design_data_nested
  }

  return(result)
}


#' Prefix vignette placeholders in text
#'
#' @param texts Character vector of texts with placeholders in `\\{...\\}`.
#' @param keywords Character vector of variable names to prefix
#' @param prefix Prefix string (default: "vig")
#' @param num Vignette number to use in prefix
#' @return Character vector with prefixed variable names
#'
#' @examples
#' txt <- "{name} is {age}"
#' prefix_vignette_placeholders(txt, c("name", "age"), prefix = "vig", num = 1)
#'
#' @keywords internal
prefix_vignette_placeholders <- function(texts, keywords, prefix = "vig", num) {
  if (!is.character(texts)) {
    stop("'texts' must be a character vector")
  }
  if (!is.character(keywords) || length(keywords) == 0) {
    stop("'keywords' must be a non-empty character vector")
  }
  if (!is.numeric(num) || length(num) != 1) {
    stop("'num' must be a single number")
  }

  # Only replace keywords inside {...} blocks
  stringr::str_replace_all(
    texts,
    "\\{.*?\\}",
    function(match) {
      stringr::str_replace_all(
        match,
        paste0("\\b(", paste(keywords, collapse = "|"), ")\\b"),
        paste0(prefix, num, "\\1")
      )
    }
  )
}


#' Build vignette texts for all vignettes and languages
#'
#' @param vig_data List from build_vignette_data with multi-language structure
#' @param text Single text or named list of texts by language
#' @return List with processed_text for each vignette (and language)
#'
#' @details Processed_text element with structure:
#'   processed_text$vigN$lang = "processed string"
#'
#' @keywords internal
build_vignette_texts <- function(vig_data, text) {
  n_vigs <- vig_data$vigs
  keywords <- vig_data$vig_vars
  prefix <- vig_data$prefix
  langs <- names(text)

  # Build structure: nested list with text for each vignette (& language)
  processed_texts <- list()

  for (vig in 1:n_vigs) {
    vig_name <- paste0(prefix, vig)
    processed_texts[[vig_name]] <- list()

    # process text (depending if multi-language or not)
    if (is.null(langs)) {
      processed_texts[[vig_name]] <- prefix_vignette_placeholders(
        texts = text,
        keywords = keywords,
        prefix = prefix,
        num = vig
      )
    } else {
      for (lang in langs) {
        processed_texts[[vig_name]][[lang]] <- prefix_vignette_placeholders(
          texts = text[[lang]],
          keywords = keywords,
          prefix = prefix,
          num = vig
        )
      }
    }
  }

  return(processed_texts)
}


#' Add vignettes to a seed structure
#'
#' @param vig_data Output from build_vignette_data (with YAML format)
#' @param seed LimeSeed seed object
#' @param deckgroupsuffix Suffix of the group containing the deck randomizer.
#'   Set to `NULL` if the deck variable is managed elsewhere.
#'
#' @return Modified seed object with vignette defaults and hidden variables
#'   added to the corresponding groups.
#'
#' @export
vignettes_to_seed <- function(
  vig_data,
  seed,
  deckgroupsuffix = "intro"
) {
  prefix <- vig_data$prefix
  n_decks <- vig_data$decks
  n_vigs <- vig_data$vigs
  vig_vars <- vig_data$vig_vars

  # Fill deck randomizer
  if (!is.null(deckgroupsuffix)) {
    deckgroup <- paste0(prefix, deckgroupsuffix)
    deckquest <- paste0(prefix, "deck")

    if (
      is.null(seed$structure[[deckgroup]]) ||
        is.null(seed$structure[[deckgroup]][[deckquest]])
    ) {
      stop(
        sprintf(
          "Vignette deck question %s not properly defined in question group %s.",
          deckquest,
          deckgroup
        )
      )
    }

    deckrand <- paste0("{rand(1,", n_decks, ")}")

    seed$structure[[deckgroup]][[deckquest]]$default <- deckrand
  }

  # Process each vignette
  for (vig in 1:n_vigs) {
    vig_name <- paste0(prefix, vig)

    if (!vig_name %in% names(seed$structure)) {
      warning("Vignette '", vig_name, "' not found in seed structure")
      next
    }

    # Add vignette text (if available)
    text_field <- paste0(vig_name, "text")
    if (
      text_field %in%
        names(seed$structure[[vig_name]]) &&
        !is.null(vig_data$texts)
    ) {
      seed$structure[[vig_name]][[text_field]]$questionTexts <-
        vig_data$texts[[vig_name]]
    }

    # Modify questions
    question_pattern <- paste0("^", vig_name, "q[0-9]+$")
    question_names <- grep(
      question_pattern,
      names(seed$structure[[vig_name]]),
      value = TRUE
    )

    fields_to_prefix <- c(
      "questionTexts",
      "subquestions",
      "answerOptions",
      "default"
    )

    for (quest in question_names) {
      elems <- intersect(
        fields_to_prefix,
        names(seed$structure[[vig_name]][[quest]])
      )

      for (elem in elems) {
        # Get the current content
        current_content <- seed$structure[[vig_name]][[quest]][[elem]]

        # Only process if it's character data
        if (is.null(current_content)) {
          next
        }

        if (elem == "subquestions") {
          # Handle subquestions (list of items)
          seed$structure[[vig_name]][[quest]][[elem]] <- lapply(
            current_content,
            function(subq) {
              if (is.character(subq)) {
                prefix_vignette_placeholders(subq, vig_vars, prefix, vig)
              } else if (is.list(subq)) {
                # Handle nested list structure
                lapply(subq, function(x) {
                  if (is.character(x)) {
                    prefix_vignette_placeholders(x, vig_vars, prefix, vig)
                  } else {
                    x
                  }
                })
              } else {
                subq
              }
            }
          )
        } else {
          # Handle other fields
          if (is.character(current_content)) {
            seed$structure[[vig_name]][[quest]][[elem]] <-
              prefix_vignette_placeholders(
                current_content,
                vig_vars,
                prefix,
                vig
              )
          } else if (is.list(current_content)) {
            # Handle list structure (multi-language)
            seed$structure[[vig_name]][[quest]][[elem]] <- lapply(
              current_content,
              function(x) {
                if (is.character(x)) {
                  prefix_vignette_placeholders(x, vig_vars, prefix, vig)
                } else {
                  x
                }
              }
            )
          }
        }
      }
    }
  }

  # Add condition variables
  for (var_name in names(vig_data$conditions)) {
    vig_match <- stringr::str_match(var_name, paste0("^(", prefix, "[0-9]+)"))
    if (is.na(vig_match[1, 2])) {
      next
    }

    vig_name <- vig_match[1, 2]

    if (vig_name %in% names(seed$structure)) {
      seed$structure[[vig_name]][[var_name]] <- list(
        type = "short text",
        hidden = 1,
        default = vig_data$conditions[[var_name]]
      )
    }
  }

  return(seed)
}
