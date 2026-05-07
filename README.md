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

1. Provide a design table with at least `deck`, `vig`, and one numeric column 
   per factor.
2. Define factors and text templates in YAML or as R lists.
3. Run `build_vignette_data()` to generate a vignette object.
4. Optionally inspect the result with `preview_vignettes()`.
5. Add the vignette object to a LimeSeed seed with
   `vignettes_to_seed()`.
6. Export the final survey with `LimeSeed::seed_to_tsv()`.

## Minimal example

```r
library(LSExperiment)

design_df <- data.frame(
  deck = c(1, 1, 2, 2),
  vig = c(1, 2, 1, 2),
  gender = c(1, 2, 2, 1),
  contract = c(1, 2, 1, 2)
)

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

text <- list(
  en = "{if(gender == 'female', 'She', 'He')} has a {contract}.",
  de = "{if(gender == 'female', 'Sie', 'Er')} hat einen {contract}."
)

vig <- build_vignette_data(
  design = design_df,
  content = list(
    factors = factors,
    text = text
  ),
  prefix = "jobvig"
)

preview_vignettes(vig, "jobvig-preview.html")
```

## Dependent covariables

Use `covariables` for attributes that depend on one or more factor values, such
as names, exact ages, tenure, or departments.

`content` is the only input for `factors`, `text`, and optional
`covariables`. It must be a named list or YAML file with exactly those
top-level entries.

The rule format is flat:
- use named rules directly under each covariable
- use factor variable names directly inside each rule
- use `pool` for the candidate values

`randomize` defaults to `TRUE`, so you only need to set it when you want
ordered, non-random assignment.

```r
covariables <- list(
  name = list(
    male_young = list(
      gender = 1,
      age = 1,
      pool = list(
        en = c("Jack", "Joshua", "Thomas"),
        de = c("Leon", "Lukas", "Jonas")
      )
    ),
    female_old = list(
      gender = 2,
      age = 2,
      pool = list(
        en = c("Sarah", "Helen", "Claire"),
        de = c("Sandra", "Katrin", "Claudia")
      )
    )
  ),
  specific_age = list(
    younger = list(age = 1, pool = c("24", "26", "28")),
    older = list(age = 2, pool = c("56", "58", "60"))
  )
)

vig <- build_vignette_data(
  design = design_df,
  content = list(
    factors = factors,
    text = "{name} is {specific_age} years old.",
    covariables = covariables
  )
)
```

You can bundle `factors`, `text`, and `covariables` into one top-level
config object or YAML file through `content`. `design` stays separate because
it is tabular:

```r
config <- list(
  factors = factors,
  text = text,
  covariables = covariables
)

vig <- build_vignette_data(
  design = design_df,
  content = config
)
```

Equivalent YAML:

```yaml
factors:
  gender:
    1: male
    2: female
  age:
    1: young
    2: old
text:
  en: "{name} is {specific_age} years old."
covariables:
  name:
    male_young:
      gender: 1
      age: 1
      pool:
        en: ["Jack", "Joshua", "Thomas"]
        de: ["Leon", "Lukas", "Jonas"]
    female_old:
      gender: 2
      age: 2
      pool:
        en: ["Sarah", "Helen", "Claire"]
        de: ["Sandra", "Katrin", "Claudia"]
```

Then load it like this:

```r
vig <- build_vignette_data(
  design = design_df,
  content = "path/to/vig-config.yml"
)
```

Values are randomized without replacement within each deck as long as the pool
is large enough. If a deck needs more draws than available values, sampling
automatically falls back to replacement, so two available values can also be
used for four vignettes in the same deck.

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
seed <- LimeSeed::load_seed("path/to/seed-folder")
seed <- vignettes_to_seed(vig, seed)
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
- Use `covariables` for all dependent attributes, including person names.
