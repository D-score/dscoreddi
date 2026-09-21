test_that("calculate_child_dscore agrees with dscore", {
  skip_if_not_installed("dscore")
  x <- dscore::gsample
  items <- dscore::get_itemnames(x)
  x$visit <- seq_len(nrow(x))
  got <- calculate_child_dscore(x, items, id = "subjid", visit = "visit",
                                age = "agedays", age_unit = "days",
                                key = "gsed2510", min_items = 1)
  expected <- dscore::dscore(x, items = items, key = "gsed2510",
                             xname = "agedays", xunit = "days")
  expect_equal(got$d, expected$d)
  expect_equal(nrow(got), nrow(x))
})

test_that("missing required columns produce an informative error", {
  expect_error(calculate_child_dscore(data.frame(ID = 1, age = 1), "ddi_item"),
               "Missing columns")
})
