#' Child-selection Shiny module UI
#' @param id Module id.
#' @export
mod_child_select_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::selectInput(ns("child"), "Voorbeeldkind", choices = NULL),
    shiny::uiOutput(ns("summary"))
  )
}

#' Child-selection Shiny module server
#' @param id Module id.
#' @param feedback_data A `dfeedback_data` object or reactive returning one.
#' @return A reactive containing the selected child id.
#' @export
mod_child_select_server <- function(id, feedback_data) {
  shiny::moduleServer(id, function(input, output, session) {
    fd <- shiny::reactive(if (shiny::is.reactive(feedback_data)) feedback_data() else feedback_data)
    shiny::observeEvent(fd(), {
      x <- fd(); key <- x$settings$id
      ids <- as.character(x$children[[key]])
      labels <- paste0(ids, " (", x$children$n_measurements, " metingen)")
      shiny::updateSelectInput(session, "child", choices = stats::setNames(ids, labels),
                               selected = ids[1])
    }, ignoreNULL = FALSE)
    selected <- shiny::reactive({ shiny::req(input$child); input$child })
    output$summary <- shiny::renderUI({
      x <- fd(); shiny::req(selected())
      row <- x$children[as.character(x$children[[x$settings$id]]) == selected(), , drop = FALSE]
      shiny::tags$p(class = "child-summary",
                    paste(row$n_measurements, "beschikbare meetmomenten"))
    })
    selected
  })
}
