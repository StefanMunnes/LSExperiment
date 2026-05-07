test_that("covariables assign dependent randomized values without duplicates within deck", {
  set.seed(123)

  factors <- list(
    gender = list(
      `1` = c(en = "male", de = "mannlich"),
      `2` = c(en = "female", de = "weiblich")
    ),
    age = list(
      `1` = c(en = "young", de = "jung"),
      `2` = c(en = "old", de = "alt")
    )
  )

  design_df <- data.frame(
    deck = c(1, 1, 1, 1, 2, 2, 2, 2),
    vig = c(1, 2, 3, 4, 1, 2, 3, 4),
    gender = c(1, 1, 2, 2, 1, 1, 2, 2),
    age = c(1, 1, 2, 2, 1, 1, 2, 2)
  )

  covariables <- list(
    name = list(
      male_young = list(
        gender = 1,
        age = 1,
        pool = list(
          en = c("Jack", "Joshua"),
          de = c("Leon", "Lukas")
        )
      ),
      female_old = list(
        gender = 2,
        age = 2,
        pool = list(
          en = c("Sarah", "Helen"),
          de = c("Sandra", "Katrin")
        )
      )
    ),
    specific_age = list(
      younger = list(
        age = 1,
        pool = list(
          en = c("24", "27"),
          de = c("24", "27")
        )
      ),
      older = list(
        age = 2,
        pool = list(
          en = c("56", "59"),
          de = c("56", "59")
        )
      )
    ),
    tenure = list(
      men = list(
        gender = 1,
        pool = list(
          en = c("2 years", "4 years"),
          de = c("2 Jahre", "4 Jahre")
        )
      ),
      women = list(
        gender = 2,
        pool = list(
          en = c("3 years", "5 years"),
          de = c("3 Jahre", "5 Jahre")
        )
      )
    )
  )

  result <- build_vignette_data(
    design = design_df,
    content = list(
      factors = factors,
      text = "{name} is {specific_age} and has {tenure}.",
      covariables = covariables
    ),
    lang = "en",
    prefix = "vigper"
  )

  expect_true(all(c("name", "specific_age", "tenure") %in% result$vig_vars))

  design_data <- result$design_data

  male_young <- subset(design_data, gender == "male" & age == "young")
  female_old <- subset(design_data, gender == "female" & age == "old")

  expect_equal(length(unique(male_young$name[male_young$deck == 1])), 2)
  expect_equal(length(unique(male_young$name[male_young$deck == 2])), 2)
  expect_equal(length(unique(female_old$name[female_old$deck == 1])), 2)
  expect_equal(length(unique(female_old$name[female_old$deck == 2])), 2)

  expect_equal(length(unique(design_data$specific_age[design_data$deck == 1 & design_data$age == "young"])), 2)
  expect_equal(length(unique(design_data$tenure[design_data$deck == 1 & design_data$gender == "male"])), 2)

  expect_match(result$conditions[["vigper1name"]], "Jack|Joshua|Sarah|Helen")
  expect_match(result$texts$vigper1, "vigper1specific_age")
})

test_that("named YAML covariable conditions can be read from file", {
  skip_if_not_installed("yaml")
  set.seed(321)

  factors <- list(
    gender = list(`1` = "male", `2` = "female"),
    age = list(`1` = "young", `2` = "old")
  )

  design_df <- data.frame(
    deck = c(1, 1),
    vig = c(1, 2),
    gender = c(1, 2),
    age = c(1, 2)
  )

  yaml_file <- tempfile(fileext = ".yml")
  writeLines(
    c(
      "covariables:",
      "  name:",
      "    male_young:",
      "      gender: 1",
      "      age: 1",
      "      pool:",
      "        en: ['Jack']",
      "    female_old:",
      "      gender: 2",
      "      age: 2",
      "      pool:",
      "        en: ['Sarah']"
    ),
    con = yaml_file
  )

  config <- yaml::read_yaml(yaml_file)

  result <- build_vignette_data(
    design = design_df,
    content = list(
      factors = factors,
      text = "{name}",
      covariables = config$covariables
    ),
    lang = "en",
  )

  expect_equal(result$design_data$name, c("Jack", "Sarah"))
})

test_that("covariable values can be reused across decks", {
  set.seed(999)

  factors <- list(
    age = list(`1` = "young")
  )

  design_df <- data.frame(
    deck = c(1, 2, 3, 4),
    vig = c(1, 1, 1, 1),
    age = c(1, 1, 1, 1)
  )

  covariables <- list(
    specific_age = list(
      younger = list(
        age = 1,
        pool = c("24", "26")
      )
    )
  )

  result <- build_vignette_data(
    design = design_df,
    content = list(
      factors = factors,
      text = "{specific_age}",
      covariables = covariables
    )
  )

  expect_true(all(result$design_data$specific_age %in% c("24", "26")))
  expect_lte(length(unique(result$design_data$specific_age)), 2)
})

test_that("covariables use replacement within a deck when the pool is too short", {
  set.seed(1234)

  factors <- list(
    age = list(`1` = "young")
  )

  design_df <- data.frame(
    deck = c(1, 1, 1, 1),
    vig = c(1, 2, 3, 4),
    age = c(1, 1, 1, 1)
  )

  covariables <- list(
    specific_age = list(
      randomize = FALSE,
      younger = list(
        age = 1,
        pool = c("24", "26")
      )
    )
  )

  result <- build_vignette_data(
    design = design_df,
    content = list(
      factors = factors,
      text = "{specific_age}",
      covariables = covariables
    )
  )

  expect_equal(nrow(result$design_data), 4)
  expect_equal(result$design_data$specific_age, c("24", "26", "24", "26"))
})

test_that("build_vignette_data accepts shared content from a config list via content", {
  factors <- list(
    age = list(`1` = "young")
  )

  design_df <- data.frame(
    deck = c(1, 1),
    vig = c(1, 2),
    age = c(1, 1)
  )

  config <- list(
    factors = factors,
    text = "{specific_age}",
    covariables = list(
      specific_age = list(
        younger = list(
          age = 1,
          pool = c("24", "26")
        )
      )
    )
  )

  result <- build_vignette_data(
    design = design_df,
    content = config
  )

  expect_equal(nrow(result$design_data), 2)
  expect_true(all(result$design_data$specific_age %in% c("24", "26")))
})

test_that("build_vignette_data accepts shared content from a YAML config file via content", {
  skip_if_not_installed("yaml")

  yaml_file <- tempfile(fileext = ".yml")
  writeLines(
    c(
      "factors:",
      "  age:",
      "    1: young",
      "text: '{specific_age}'",
      "covariables:",
      "  specific_age:",
      "    younger:",
      "      age: 1",
      "      pool: ['24', '26']"
    ),
    con = yaml_file
  )

  design_df <- data.frame(
    deck = c(1, 1),
    vig = c(1, 2),
    age = c(1, 1)
  )

  result <- build_vignette_data(
    design = design_df,
    content = yaml_file
  )

  expect_equal(nrow(result$design_data), 2)
  expect_true(all(result$design_data$specific_age %in% c('24', '26')))
})

test_that("build_vignette_data requires text after resolving inputs", {
  design_df <- data.frame(
    deck = 1,
    vig = 1,
    age = 1
  )

  expect_error(
    build_vignette_data(
      design = design_df,
      content = list(
        factors = list(
          age = list(`1` = "young")
        )
      )
    ),
    "'content' must contain 'text'"
  )
})

test_that("build_vignette_data rejects invalid content paths clearly", {
  expect_error(
    build_vignette_data(
      design = data.frame(deck = 1, vig = 1, age = 1),
      content = "does-not-exist.yml"
    ),
    "'content' must be a readable YAML file path or a named list"
  )
})

test_that("build_vignette_data rejects unsupported keys in content", {
  expect_error(
    build_vignette_data(
      design = data.frame(deck = 1, vig = 1, age = 1),
      content = list(
        factors = list(age = list(`1` = "young")),
        text = "{age}",
        prefix = "bad"
      )
    ),
    "Unexpected entries: prefix"
  )
})
