library(shiny)
library(dplyr)
library(dscoreddi)

# Laad de meest recente functies uit het lokale package
devtools::load_all()


# ------------------------------------------------------------------------------
# 1. Feedbackdata maken
# ------------------------------------------------------------------------------

feedback_data <- prepare_feedback_data(
  data = jgz_example_feedback,
  id = "ID",
  visit = "ContactmomentID",
  age = "age",
  age_unit = "decimal",
  key = "gsed",
  population = "dutch",
  min_items = 1L,
  include_domains = FALSE
)


# ------------------------------------------------------------------------------
# 2. Kies één kind om te testen
# ------------------------------------------------------------------------------

selected_child <- feedback_data$children$ID[1]

print(selected_child)

# Controleer de data voor dit kind
feedback_data$visits |>
  filter(ID == selected_child) |>
  select(
    ID,
    ContactmomentID,
    age,
    a,
    d,
    daz,
    sem,
    n
  ) |>
  print()


# ------------------------------------------------------------------------------
# 3. Minimale test-app
# ------------------------------------------------------------------------------

ui <- fluidPage(

  h2("Test van mod_growth_basic"),

  p(
    paste(
      "Getoond voorbeeldkind:",
      selected_child
    )
  ),

  mod_growth_basic_ui("growth")
)


server <- function(input, output, session) {

  mod_growth_basic_server(
    id = "growth",
    feedback_data = feedback_data,
    child_id = reactive(selected_child)
    )
}


# ------------------------------------------------------------------------------
# 4. App starten
# ------------------------------------------------------------------------------

shinyApp(
  ui = ui,
  server = server
)
