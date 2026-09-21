test_that("basic growth module creates a plotly object", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("plotly")
  fd <- structure(list(
    visits = tibble::tibble(ID = c("A", "A"), ContactmomentID = 1:2,
      a = c(.5, 1), d = c(25, 35), daz = c(0, .2), sem = c(1, 1), n = c(5, 6)),
    items = tibble::tibble(), domains = NULL,
    children = tibble::tibble(ID = "A", n_measurements = 2),
    settings = list(id = "ID")
  ), class = "dfeedback_data")

  shiny::testServer(mod_growth_basic_server,
    args = list(feedback_data = fd, child_id = shiny::reactive("A")), {
      expect_s3_class(session$returned(), "plotly")
    })
})
