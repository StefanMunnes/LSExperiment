# Minimal LSExperiment example

factors <- list(
  gender = list(
    `1` = "male",
    `2` = "female"
  ),
  contract = list(
    `1` = c(en = "permanent contract", de = "unbefristeter Vertrag"),
    `2` = c(en = "fixed-term contract", de = "befristeter Vertrag")
  )
)

design_df <- data.frame(
  deck = c(1, 1, 2, 2),
  vig = c(1, 2, 1, 2),
  gender = c(1, 2, 2, 1),
  contract = c(1, 2, 1, 2)
)

text <- list(
  en = "{if(gender == 'female', 'She', 'He')} has a {contract}.",
  de = "{if(gender == 'female', 'Sie', 'Er')} hat einen {contract}."
)

vig <- build_vignette_data(
  factors = factors,
  design_df = design_df,
  prefix = "jobvig",
  text = text
)

preview_vignettes(
  vig,
  file = "jobvig-preview.html",
  title = "Job Vignettes"
)
