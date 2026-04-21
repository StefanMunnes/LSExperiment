# LSExperiment

`LSExperiment` is an R package for building factorial survey experiments for
LimeSurvey in a reproducible way. It complements `LimeSeed`: you keep the
survey structure in a LimeSeed, and use `LSExperiment` to generate
the vignette texts, hidden condition variables, deck randomization, and HTML
previews from structured R objects or YAML-based factor definitions.

## What the package does

- stores vignette dimensions and labelled levels in a structured format
- turns a design table into labelled vignette data
- supports multi-language labels and text templates
- creates LimeSurvey-ready condition strings for hidden variables
- injects vignette content into an existing LimeSeed seed object
- writes a self-contained HTML preview for review before export

## Workflow

1. Define factors and text templates in YAML or as R lists.
2. Provide a design table with at least `deck`, `vig`, and one numeric column 
   per factor.
3. Run `build_vignette_data()` to generate a vignette object.
4. Optionally inspect the result with `preview_vignettes()`.
5. Add the vignette object to a LimeSeed seed with
   `vignettes_to_seed()`.
6. Export the final survey with `LimeSeed::seed_to_tsv()`.

## Minimal example

```r
library(LSExperiment)

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

preview_vignettes(vig, "jobvig-preview.html")
```

## LimeSeed integration

`vignettes_to_seed()` expects a LimeSeed seed/list that already
contains the vignette groups and question shells. The function then:

- fills the deck randomizer question (for example `vigperdeck`)
- injects vignette text into `vigXtext`
- prefixes placeholders in question texts, subquestions, answer options, and
  defaults
- appends hidden variables with the generated condition strings

Typical usage:

```r
seed <- LimeSeed::prime_seed("path/to/seed-folder")
seed <- vignettes_to_seed(vig, seed, deckgroupsuffix = "intro")
LimeSeed::seed_to_tsv(seed, "survey.tsv")
```

## Advice

- Keep factor names short and stable because they become placeholder names in
  your text templates.
- Validate the design table early. It should always contain `deck`, `vig`, and
  all factor columns expected by `build_vignette_data()`.
- Use `preview_vignettes()` as a review step before export, especially in
  multi-language projects.
- Keep all generated vignette variables grouped under a consistent prefix such
  as `vigper` or `vigjob`.
- Treat name randomization as experiment-specific logic. In the current code,
  this part is still tailored to a gender/age-style setup and should be made
  more generic before publishing to CRAN or reusing across many studies.
