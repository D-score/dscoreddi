test_that("prepare_child_items makes correct long outcomes", {
  x <- data.frame(ID = c(1, 1), ContactmomentID = 1:2, age = c(.5, 1),
                  ddi001 = c(0, 1), ddi002 = c(NA, 1))
  z <- prepare_child_items(x, c("ddi001", "ddi002"))
  expect_equal(nrow(z), 4L)
  expect_equal(sum(z$observed), 3L)
  expect_true(all(c("Behaald", "Nog niet behaald", "Niet afgenomen") %in% as.character(z$result)))
})

test_that("only children with repeated measurements enter selector", {
  fake <- structure(list(
    visits = tibble::tibble(ID = c(1, 1, 2), ContactmomentID = 1:3, a = 1, n = 3, d = 20),
    items = tibble::tibble(), domains = NULL,
    children = tibble::tibble(ID = 1, n_measurements = 2),
    settings = list(id = "ID", visit = "ContactmomentID", age = "age")
  ), class = "dfeedback_data")
  expect_equal(fake$children$ID, 1)
})
