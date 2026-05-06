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
      conditions = list(
        list(
          when = list(gender = 1, age = 1),
          values = list(
            en = c("Jack", "Joshua"),
            de = c("Leon", "Lukas")
          )
        ),
        list(
          when = list(gender = 2, age = 2),
          values = list(
            en = c("Sarah", "Helen"),
            de = c("Sandra", "Katrin")
          )
        )
      )
    ),
    specific_age = list(
      conditions = list(
        list(
          when = list(age = 1),
          values = list(
            en = c("24", "27"),
            de = c("24", "27")
          )
        ),
        list(
          when = list(age = 2),
          values = list(
            en = c("56", "59"),
            de = c("56", "59")
          )
        )
      )
    ),
    tenure = list(
      conditions = list(
        list(
          when = list(gender = 1),
          values = list(
            en = c("2 years", "4 years"),
            de = c("2 Jahre", "4 Jahre")
          )
        ),
        list(
          when = list(gender = 2),
          values = list(
            en = c("3 years", "5 years"),
            de = c("3 Jahre", "5 Jahre")
          )
        )
      )
    )
  )

  result <- build_vignette_data(
    factors = factors,
    design_df = design_df,
    lang = "en",
    prefix = "vigper",
    text = "{name} is {specific_age} and has {tenure}.",
    covariables = covariables
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

test_that("covariables fail when a deck would require duplicate values", {
  factors <- list(
    gender = list(`1` = "male"),
    age = list(`1` = "young")
  )

  design_df <- data.frame(
    deck = c(1, 1),
    vig = c(1, 2),
    gender = c(1, 1),
    age = c(1, 1)
  )

  covariables <- list(
    name = list(
      conditions = list(
        list(
          when = list(gender = 1, age = 1),
          values = c("Jack")
        )
      )
    )
  )

  expect_error(
    build_vignette_data(
      factors = factors,
      design_df = design_df,
      lang = NULL,
      covariables = covariables
    ),
    "Not enough unique values"
  )
})
