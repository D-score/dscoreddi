library(shiny)
#library(dscoreddi)

feedback_data <- getOption("dscoreddi.feedback_data")
if (is.null(feedback_data)) {
  data("jgz_example_feedback", package = "dscoreddi")
  feedback_data <- prepare_feedback_data(
    jgz_example_feedback,
    id = "ID", visit = "ContactmomentID", age = "age",
    key = "gsed", population = "dutch", min_items = 1,
    # Add for domain prototype:
    include_domains = TRUE, domain_set = "VWO"
  )
}

ui <- shiny::fluidPage(

  shiny::titlePanel(
    "D-score terugkoppeling"
  ),

  shiny::sidebarLayout(

    shiny::sidebarPanel(
      mod_child_select_ui(
        "child_select"
      ),
      width = 3
    ),

    shiny::mainPanel(

      shiny::tabsetPanel(

        shiny::tabPanel(
          "Groeidiagram",
          mod_growth_basic_ui(
            "growth_basic"
          )
        ),

        shiny::tabPanel(
          "Domeinen",
          mod_growth_domains_ui(
            "growth_domains"
          )
        ),

        shiny::tabPanel(
          "Itemtijdlijn",
          mod_item_timeline_ui(
            "item_timeline"
          )
        ),

        shiny::tabPanel(
          "Klikbare D-score",
          mod_growth_item_drilldown_ui(
            "growth_item_drilldown"
          )
        ),

        shiny::tabPanel(
          "D-score met kenmerken",
          mod_growth_item_overlay_ui(
            "growth_item_overlay"
          )
        )
      ),

      width = 9
    )
  )
)

server <- function(input, output, session) {

  selected_child <- mod_child_select_server(
    id = "child_select",
    feedback_data = feedback_data
  )

  mod_growth_basic_server(
    id = "growth_basic",
    feedback_data = feedback_data,
    child_id = selected_child
  )

  mod_growth_domains_server(
    id = "growth_domains",
    feedback_data = feedback_data,
    child_id = selected_child
  )

  mod_item_timeline_server(
    id = "item_timeline",
    feedback_data = feedback_data,
    child_id = selected_child
  )

  mod_growth_item_drilldown_server(
    id = "growth_item_drilldown",
    feedback_data = feedback_data,
    child_id = selected_child
  )

  mod_growth_item_overlay_server(
    id = "growth_item_overlay",
    feedback_data = feedback_data,
    child_id = selected_child
  )
}

shinyApp(ui, server)
