### ---------------------------------------------------------------------
## Tests voor propagate_scores()
##
## Vereist: testthat, dplyr, rlang
## Uitvoeren met: testthat::test_file("test-propagate_scores.R")
## (of plaats dit bestand in tests/testthat/ van je package en run
## devtools::test())
## ---------------------------------------------------------------------

library(testthat)
library(dplyr)
library(rlang)

#source("R/propagate_scores.R")

## item_limits voor de tests --------------------------------------------
item_limits <- data.frame(
  item    = c("ddi_b0", "ddi_f1", "ddi_lim"),
  min_age = c(2,  2,  5),
  max_age = c(30, 30, 23)
)

## ------------------------------------------------------------------
## 1. id is verplicht
## ------------------------------------------------------------------

test_that("error when id is missing", {
  df <- data.frame(
    child_id = "A",
    age      = c(4, 8, 12),
    ddi_b0   = c(NA, NA, 0)
  )

  expect_error(
    propagate_scores(
      df,
      age   = age,
      items = starts_with("ddi"),
      item_limits = item_limits
    ),
    "id"
  )
})

test_that("error when id is explicitly NULL", {
  df <- data.frame(
    child_id = "A",
    age      = c(4, 8, 12),
    ddi_b0   = c(NA, NA, 0)
  )

  expect_error(
    propagate_scores(
      df,
      age   = age,
      items = starts_with("ddi"),
      id    = NULL,
      item_limits = item_limits
    ),
    "id"
  )
})

## ------------------------------------------------------------------
## 2. Backward propagation van 0 - alleen binnen persoon
## ------------------------------------------------------------------

test_that("0 propageert terug, en blijft binnen een persoon", {
  ages <- c(4, 8, 12, 16, 20, 24)

  df <- data.frame(
    child_id = rep(c("A", "B"), each = length(ages)),
    age      = rep(ages, 2),
    ddi_b0   = c(
      # kind A: 0 gescoord op leeftijd 12
      NA, NA, 0, NA, NA, NA,
      # kind B: nooit een 0 gescoord
      NA, NA, NA, NA, NA, NA
    )
  )

  res <- propagate_scores(
    df,
    age   = age,
    items = ddi_b0,
    id    = child_id,
    item_limits = item_limits
  )

  res_A <- res$ddi_b0[res$child_id == "A"]
  res_B <- res$ddi_b0[res$child_id == "B"]

  # Kind A: 0 op leeftijd 12 propageert terug naar 4 en 8
  # (min_age = 2 voor ddi_b0)
  expect_equal(res_A, c(0, 0, 0, NA, NA, NA))

  # Kind B: geen 0 geobserveerd -> niets mag worden ingevuld,
  # ook al ligt de 0 van kind A in dezelfde leeftijdsrange
  expect_true(all(is.na(res_B)))
})

## ------------------------------------------------------------------
## 3. Forward propagation van 1 - alleen binnen persoon
## ------------------------------------------------------------------

test_that("1 propageert vooruit, en blijft binnen een persoon", {
  ages <- c(4, 8, 12, 16, 20, 24)

  df <- data.frame(
    child_id = rep(c("A", "B", "C"), each = length(ages)),
    age      = rep(ages, 3),
    ddi_f1   = c(
      # kind A: 1 gescoord op leeftijd 8
      NA, 1, NA, NA, NA, NA,
      # kind B: 1 gescoord op leeftijd 16
      NA, NA, NA, 1, NA, NA,
      # kind C: nooit een 1 gescoord
      NA, NA, NA, NA, NA, NA
    )
  )

  res <- propagate_scores(
    df,
    age   = age,
    items = ddi_f1,
    id    = child_id,
    item_limits = item_limits
  )

  res_A <- res$ddi_f1[res$child_id == "A"]
  res_B <- res$ddi_f1[res$child_id == "B"]
  res_C <- res$ddi_f1[res$child_id == "C"]

  # Kind A: 1 op leeftijd 8 propageert vooruit t/m max_age = 30
  expect_equal(res_A, c(NA, 1, 1, 1, 1, 1))

  # Kind B: 1 op leeftijd 16 propageert vooruit naar 20 en 24
  expect_equal(res_B, c(NA, NA, NA, 1, 1, 1))

  # Kind C: niets geobserveerd -> niets propageert
  expect_true(all(is.na(res_C)))
})

## ------------------------------------------------------------------
## 4. min_age / max_age grenzen worden gerespecteerd
## ------------------------------------------------------------------

test_that("propagatie respecteert min_age en max_age", {
  ages <- c(3, 6, 9, 12, 15, 18, 21, 24, 27)

  df <- data.frame(
    child_id = "A",
    age      = ages,
    ddi_lim  = c(NA, NA, 0, NA, NA, NA, NA, 1, NA)
  )

  res <- propagate_scores(
    df,
    age   = age,
    items = ddi_lim,
    id    = child_id,
    item_limits = item_limits
  )

  # ddi_lim: min_age = 5, max_age = 23
  # - 0 op leeftijd 9 propageert terug naar 6 (6 > 5),
  #   maar NIET naar 3 (3 < min_age = 5)
  # - 1 op leeftijd 24 propageert NIET vooruit, want
  #   24 ligt al boven max_age = 23
  expect_equal(res$ddi_lim, c(NA, 0, 0, NA, NA, NA, NA, 1, NA))
})

## ------------------------------------------------------------------
## 5. Geobserveerde scores worden niet overschreven
##
## NB: deze test slaagt met de optionele "CHANGE 2" fix
## (& is.na(score)) in limit_propagated_scores(). Zonder die fix
## faalt deze test, omdat de geobserveerde 1 op leeftijd 8 wordt
## overschreven door de backward-propagatie van de 0 op leeftijd 12.
## ------------------------------------------------------------------

test_that("geobserveerde scores blijven ongewijzigd", {
  ages <- c(4, 8, 12, 16, 20)

  df <- data.frame(
    child_id = "A",
    age      = ages,
    ddi_b0   = c(0, 1, 0, NA, NA)
  )

  res <- propagate_scores(
    df,
    age   = age,
    items = ddi_b0,
    id    = child_id,
    item_limits = item_limits
  )

  # De geobserveerde 1 op leeftijd 8 mag niet veranderd worden,
  # ook al ligt deze tussen twee geobserveerde 0's.
  expect_equal(res$ddi_b0, c(0, 1, 0, NA, NA))
})

## ------------------------------------------------------------------
## 6. Meerdere items en meerdere personen tegelijk
## ------------------------------------------------------------------

test_that("meerdere items en personen worden onafhankelijk verwerkt", {
  ages <- c(4, 8, 12, 16, 20, 24)

  df <- data.frame(
    child_id = rep(c("A", "B", "C"), each = length(ages)),
    age      = rep(ages, 3),
    ddi_b0 = c(
      NA, NA, 0, NA, NA, NA,   # A
      NA, NA, NA, NA, NA, NA,  # B
      NA, 0, NA, NA, NA, NA    # C
    ),
    ddi_f1 = c(
      NA, 1, NA, NA, NA, NA,   # A
      NA, NA, NA, 1, NA, NA,   # B
      NA, NA, NA, NA, NA, NA   # C
    )
  )

  res <- propagate_scores(
    df,
    age   = age,
    items = c(ddi_b0, ddi_f1),
    id    = child_id,
    item_limits = item_limits
  )

  expect_equal(res$ddi_b0[res$child_id == "A"], c(0, 0, 0, NA, NA, NA))
  expect_equal(res$ddi_b0[res$child_id == "C"], c(0, 0, NA, NA, NA, NA))
  expect_true(all(is.na(res$ddi_b0[res$child_id == "B"])))

  expect_equal(res$ddi_f1[res$child_id == "A"], c(NA, 1, 1, 1, 1, 1))
  expect_equal(res$ddi_f1[res$child_id == "B"], c(NA, NA, NA, 1, 1, 1))
  expect_true(all(is.na(res$ddi_f1[res$child_id == "C"])))
})

## ------------------------------------------------------------------
## 7. Output volgorde / rijen blijven behouden
## ------------------------------------------------------------------

test_that("aantal rijen en kolomnamen blijven gelijk", {
  ages <- c(4, 8, 12)

  df <- data.frame(
    child_id = rep(c("A", "B"), each = length(ages)),
    age      = rep(ages, 2),
    ddi_b0   = c(NA, NA, 0, NA, NA, NA)
  )

  res <- propagate_scores(
    df,
    age   = age,
    items = ddi_b0,
    id    = child_id,
    item_limits = item_limits
  )

  expect_equal(nrow(res), nrow(df))
  expect_equal(names(res), names(df))
})
