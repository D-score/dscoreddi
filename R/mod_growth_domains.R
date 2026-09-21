#' Domain growth-chart module UI
#' @param id Module id.
#' @export
mod_growth_domains_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::checkboxInput(ns("show_points"), "Toon meetpunten", FALSE),
    plotly::plotlyOutput(ns("plot"), height = "520px")
  )
}

#' Domain growth-chart module server
#' @export
mod_growth_domains_server <- function(id, feedback_data, child_id) {
  shiny::moduleServer(id, function(input, output, session) {
    output$plot <- plotly::renderPlotly({
      x <- if (shiny::is.reactive(feedback_data)) feedback_data() else feedback_data
      shiny::req(child_id())
      shiny::validate(shiny::need(!is.null(x$domains), "Domeinscores zijn niet berekend."))
      key <- x$settings$id
      dat <- x$domains[as.character(x$domains[[key]]) == as.character(child_id()), , drop = FALSE]
      dat$hover <- sprintf("Domein: %s<br>Leeftijd: %.1f maanden<br>Score: %.1f<br>Beschikbare items: %s",
                           dat$domain, dat$a * 12, dat$d, dat$n)
      g <- ggplot2::ggplot(dat, ggplot2::aes(.data$a * 12, .data$d, colour = .data$domain,
                                             group = .data$domain, text = .data$hover)) +
        ggplot2::geom_line(linewidth = 0.8) +
        ggplot2::labs(x = "Leeftijd (maanden)", y = "Domeinscore", colour = "Domein",
                      title = "Ontwikkeling per domein") +
        ggplot2::theme_minimal(base_size = 13)
      if (isTRUE(input$show_points)) g <- g + ggplot2::geom_point(size = 2.5)
      plotly::ggplotly(g, tooltip = "text") |>
        plotly::config(displaylogo = FALSE)
    })
  })
}
