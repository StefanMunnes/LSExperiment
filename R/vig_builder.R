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


#' Normalize dependent covariable definitions
#'
#' @keywords internal
normalize_covariables <- function(covariables = NULL) {
  if (is.null(covariables)) {
    return(NULL)
  }

  if (!is.list(covariables) || is.null(base::names(covariables))) {
    stop("'covariables' must be a named list")
  }

  normalize_covariable_rule <- function(rule, var_name) {
    if (!is.list(rule)) {
      stop("Each covariable rule for '", var_name, "' must be a list")
    }

    if (!is.null(rule$when)) {
      when <- rule$when
    } else {
      reserved <- c("values", "value", "labels")
      when <- rule[setdiff(base::names(rule), reserved)]
    }

    value_keys <- intersect(c("values", "value", "labels"), base::names(rule))

    if (length(value_keys) == 0) {
      stop(
        "Each covariable rule for '", var_name,
        "' must define one of: values, value, labels"
      )
    }

    if (length(when) == 0 || is.null(base::names(when))) {
      stop("Each covariable rule for '", var_name, "' must define at least one condition")
    }

    list(
      when = purrr::map_chr(when, as.character),
      values = rule[[value_keys[[1]]]]
    )
  }

  normalize_covariable_spec <- function(spec, var_name) {
    if (!is.list(spec)) {
      stop("Each covariable definition must be a list. Problem in '", var_name, "'")
    }

    if (is.null(base::names(spec)) || !("conditions" %in% base::names(spec))) {
      conditions <- purrr::map(spec, normalize_covariable_rule, var_name = var_name)
      return(list(
        randomize = TRUE,
        unique_within = "deck",
        conditions = conditions
      ))
    }

    conditions <- purrr::map(
      spec$conditions,
      normalize_covariable_rule,
      var_name = var_name
    )

    list(
      randomize = if (is.null(spec$randomize)) TRUE else isTRUE(spec$randomize),
      unique_within = if (is.null(spec$unique_within)) "deck" else spec$unique_within,
      conditions = conditions
    )
  }

  purrr::imap(covariables, normalize_covariable_spec)
}


#' Assign dependent covariables to a design table
#'
#' @keywords internal
assign_covariables <- function(design_lab, design_keys, covariables, lang = NULL) {
  if (is.null(covariables)) {
    return(design_lab)
  }

  draw_values <- function(pool, n_needed, randomize = TRUE) {
    if (length(pool) == 0) {
      stop("Covariable rule has no values to assign")
    }

    if (n_needed > length(pool)) {
      stop(
        "Not enough unique values for covariable assignment: need ", n_needed,
        ", but only ", length(pool), " available. Add more values or reduce matches."
      )
    }

    if (randomize) {
      sample(pool, size = n_needed, replace = FALSE)
    } else {
      pool[seq_len(n_needed)]
    }
  }

  for (var_name in base::names(covariables)) {
    spec <- covariables[[var_name]]
    assigned <- rep(FALSE, nrow(design_lab))
    output <- rep(NA_character_, nrow(design_lab))

    group_ids <- if (is.null(spec$unique_within)) {
      rep("all", nrow(design_keys))
    } else {
      group_var <- as.character(spec$unique_within[[1]])
      if (!group_var %in% base::names(design_keys)) {
        stop(
          "Covariable '", var_name, "' uses unknown unique_within column: ",
          group_var
        )
      }
      as.character(design_keys[[group_var]])
    }

    for (rule in spec$conditions) {
      required_keys <- base::names(rule$when)
      missing_keys <- setdiff(required_keys, base::names(design_keys))
      if (length(missing_keys) > 0) {
        stop(
          "Covariable '", var_name, "' references missing factor(s): ",
          paste(missing_keys, collapse = ", ")
        )
      }

      matched <- rep(TRUE, nrow(design_keys))
      for (key in required_keys) {
        matched <- matched & as.character(design_keys[[key]]) == rule$when[[key]]
      }

      if (any(matched & assigned)) {
        stop("Covariable '", var_name, "' has overlapping conditions")
      }

      if (!any(matched)) {
        next
      }

      values <- rule$values
      if (!is.null(lang)) {
        values <- get_lang(values, lang, fallback = values)
      }
      values <- as.character(values)

      for (group in unique(group_ids[matched])) {
        idx <- which(matched & group_ids == group)
        output[idx] <- draw_values(
          pool = values,
          n_needed = length(idx),
          randomize = spec$randomize
        )
        assigned[idx] <- TRUE
      }
    }

    if (any(!assigned)) {
      stop(
        "Covariable '", var_name, "' is missing assignments for ",
        sum(!assigned), " row(s)"
      )
    }

    design_lab[[var_name]] <- output
  }

  design_lab
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
  covariables = NULL,
  store_design_data = TRUE
) {
  vig_vars <- base::names(factors)
  covariables <- normalize_covariables(covariables = covariables)

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
    design_keys <- factors |>
      lapply(function(x) names(x)) |>
      expand.grid(stringsAsFactors = FALSE) |>
      dplyr::slice_sample(prop = 1)

    if (is.null(vig_n)) {
      stop("Either 'design_df' or 'vig_n' must be provided")
    }

    decks <- ceiling(nrow(design_keys) / vig_n)

    # Adjust to exact number of rows needed
    total_rows <- decks * vig_n
    if (nrow(design_keys) < total_rows) {
      # Recycle rows if needed
      design_keys <- design_keys[
        rep(seq_len(nrow(design_keys)), length.out = total_rows),
      ]
    } else {
      design_keys <- design_keys[1:total_rows, ]
    }

    design_keys <- design_keys |>
      dplyr::mutate(
        deck = rep(1:decks, each = vig_n),
        vig = rep(1:vig_n, decks),
        .before = 1
      )

    design_lab <- design_keys

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
    design_keys <- design_df

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

    # Identify the maximum number of vignettes and decks
    vig_n <- max(design_df$vig)
    decks <- max(design_df$deck)
  }

  # 2. Assign dependent covariables from factor-based conditions
  design_lab <- assign_covariables(
    design_lab = design_lab,
    design_keys = design_keys,
    covariables = covariables,
    lang = lang
  )

  if (!is.null(design_df)) {
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

  # Add covariables to variable list if present
  if (!is.null(covariables)) {
    vig_vars <- c(base::names(covariables), vig_vars)
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
#' @param covariables Optional named list of dependent vignette attributes.
#'   Each entry defines conditions over one or more factor variables and a value
#'   pool to assign. By default, values are randomized without replacement
#'   within each deck.
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
#'   covariables = covariables_per,
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
  covariables = NULL,
  store_design_data = TRUE
) {
  # Validate inputs
  if (is.null(factors) || !is.list(factors) || is.null(base::names(factors))) {
    stop("'factors' must be a named list")
  }

  if (is.null(design_df) && is.null(vig_n)) {
    stop("Either 'design_df' or 'vig_n' must be provided")
  }

  covariables <- normalize_covariables(covariables = covariables)
  vig_vars <- base::names(factors)
  if (!is.null(covariables)) {
    vig_vars <- c(base::names(covariables), vig_vars)
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
      covariables = covariables,
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
      covariables = covariables,
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
      covariables = covariables,
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
