## think aloud app

library(shiny)
library(dplyr)
library(plotly)
library(dscoreddi)

# -----------------------------------------------------------------------------
# Data and complete feedback object
# -----------------------------------------------------------------------------

data("jgz_example_feedback", package = "dscoreddi")

# -----------------------------------------------------------------------------
# Koppeling tussen kind-ID en fictieve naam
# -----------------------------------------------------------------------------

if (!"name" %in% names(jgz_example_feedback)) {
  stop(
    "De kolom 'name' ontbreekt in jgz_example_feedback.",
    call. = FALSE
  )
}

child_lookup <- jgz_example_feedback |>
  dplyr::select(
    ID,
    name
  ) |>
  dplyr::filter(
    !is.na(.data$name),
    .data$name != ""
  ) |>
  dplyr::distinct(
    ID,
    .keep_all = TRUE
  ) |>
  dplyr::mutate(
    child_key = as.character(.data$ID),
    display_name = as.character(.data$name)
  )

full_feedback <- prepare_feedback_data(
  data = jgz_example_feedback,
  id = "ID",
  visit = "ContactmomentID",
  age = "age",
  age_unit = "decimal",
  key = "gsed2510",
  population = "GSED-NLD",
  min_items = 1L,
  item_metadata = dscoreddi::itemtableVWO,
  include_domains = TRUE,
  domain_set = "VWO"
)

id_column <- full_feedback$settings$id
visit_column <- full_feedback$settings$visit
age_column <- full_feedback$settings$age

# Only children with at least two valid D-score occasions can be selected.
scenario_occasions <- full_feedback$visits |>
  dplyr::filter(!is.na(.data$d)) |>
  dplyr::mutate(
    child_key = as.character(.data[[id_column]]),
    visit_key = as.character(.data[[visit_column]]),
    age_months = .data$a * 12
  ) |>
  dplyr::group_by(.data$child_key) |>
  dplyr::filter(dplyr::n() >= 2L) |>
  dplyr::ungroup() |>
  dplyr::arrange(.data$child_key, .data$age_months)

# -----------------------------------------------------------------------------
# Kinderen die in de scenario-app geselecteerd kunnen worden
# -----------------------------------------------------------------------------

selectable_children <- scenario_occasions |>
  dplyr::distinct(
    child_key
  ) |>
  dplyr::left_join(
    child_lookup |>
      dplyr::select(
        child_key,
        display_name
      ),
    by = "child_key"
  ) |>
  dplyr::mutate(
    # Gebruik het ID als terugvaloptie als een naam ontbreekt.
    display_name = dplyr::if_else(
      is.na(.data$display_name) |
        .data$display_name == "",
      paste(
        "Voorbeeldkind",
        .data$child_key
      ),
      .data$display_name
    )
  )

child_choices <- selectable_children$child_key

# Named list:
# - names zijn zichtbaar in de selector;
# - values zijn de onderliggende ID's.
child_choice_list <- stats::setNames(
  as.list(selectable_children$child_key),
  selectable_children$display_name
)

if (length(child_choices) == 0L) {
  stop(
    paste0(
      "Er zijn geen voorbeeldkinderen met minimaal ",
      "twee geldige meetmomenten."
    ),
    call. = FALSE
  )
}

if (length(child_choices) == 0L) {
  stop("Er zijn geen voorbeeldkinderen met minimaal twee geldige meetmomenten.")
}

# -----------------------------------------------------------------------------
# Helpers
# -----------------------------------------------------------------------------

format_age <- function(months) {
  years <- floor(months / 12)
  remaining_months <- round(months - years * 12)

  if (years == 0L) {
    return(paste0(remaining_months, " maanden"))
  }

  if (remaining_months == 0L) {
    return(paste0(years, if (years == 1L) " jaar" else " jaar"))
  }

  paste0(years, " jaar en ", remaining_months, " maanden")
}

# -----------------------------------------------------------------------------
# UI
# -----------------------------------------------------------------------------

ui <- shiny::fluidPage(
  shiny::tags$head(
    shiny::tags$link(rel = "stylesheet", type = "text/css", href = "custom.css")
  ),

  shiny::titlePanel("D-score terugkoppeling: think-aloudsessie"),

  shiny::sidebarLayout(
    shiny::sidebarPanel(
      width = 3,

      shiny::h4("Scenario-instellingen"),

      shiny::selectInput(
        inputId = "scenario_child",
        label = "Kind",
        choices = child_choice_list,
        selected = child_choices[1]
      ),

      shiny::selectInput(
        inputId = "scenario_visit",
        label = "Leeftijdsmoment in het scenario",
        choices = NULL
      ),

      shiny::helpText(
        "De visualisatie gebruikt alleen dit meetmoment en eerdere meetmomenten. Latere gegevens worden niet getoond."
      ),

      shiny::hr(),

      shiny::strong("Huidige stap"),
      shiny::textOutput("current_step")
    ),

    shiny::mainPanel(
      width = 9,

      shiny::tabsetPanel(
        id = "scenario_tabs",
        type = "hidden",

        shiny::tabPanel(
          title = "Kindinformatie",
          value = "child_info",

          shiny::div(
            class = "scenario-card",
            shiny::h2("1. Kennismaken met het kind"),
            shiny::p(
              "Bekijk eerst de achtergrondinformatie. Vertel hardop welke informatie opvalt en welke verwachtingen deze informatie oproept."
            ),
            shiny::uiOutput("child_information")
          ),

          shiny::div(
            class = "scenario-navigation",
            shiny::actionButton("to_items", "Volgende: ingevulde kenmerken", class = "btn-primary")
          )
        ),

        shiny::tabPanel(
          title = "Ingevulde kenmerken",
          value = "items",

          shiny::div(
            class = "scenario-card",
            shiny::h2("2. Ingevulde ontwikkelingskenmerken"),
            shiny::p(
              "Bekijk de antwoorden van het huidige contactmoment. Vertel hardop welke informatie u gebruikt om een beeld van de ontwikkeling te vormen."
            ),
            shiny::uiOutput("current_items")
          ),

          shiny::div(
            class = "scenario-navigation",
            shiny::actionButton("back_to_child", "Terug"),
            shiny::actionButton("to_feedback", "Volgende: terugkoppeling", class = "btn-primary")
          )
        ),

        shiny::tabPanel(
          title = "Terugkoppeling",
          value = "feedback",

          shiny::div(
            class = "scenario-card scenario-card-wide",
            shiny::h2("3. Terugkoppeling over de ontwikkeling"),
            shiny::p(
              "Bekijk de visualisatie en vertel hardop wat u ziet, hoe u de informatie interpreteert en welke vragen de terugkoppeling oproept. Klik op een meetpunt voor de bijbehorende itemdetails."
            ),
            mod_growth_integrated_ui("scenario_growth")
          ),

          shiny::div(
            class = "scenario-navigation",
            shiny::actionButton("back_to_items", "Terug naar kenmerken")
          )
        )
      )
    )
  )
)

# -----------------------------------------------------------------------------
# Server
# -----------------------------------------------------------------------------

server <- function(input, output, session) {
  selected_child_name <- shiny::reactive({

    shiny::req(
      input$scenario_child
    )

    selected_name <- child_lookup |>
      dplyr::filter(
        .data$child_key ==
          as.character(input$scenario_child)
      ) |>
      dplyr::pull(
        .data$display_name
      )

    if (
      length(selected_name) == 0L ||
      is.na(selected_name[1]) ||
      selected_name[1] == ""
    ) {
      return(
        paste(
          "Voorbeeldkind",
          input$scenario_child
        )
      )
    }

    selected_name[1]
  })
  # Available scenario occasions for the selected child.
  child_occasions <- shiny::reactive({
    shiny::req(input$scenario_child)

    scenario_occasions |>
      dplyr::filter(.data$child_key == input$scenario_child) |>
      dplyr::arrange(.data$age_months)
  })

  shiny::observeEvent(
    input$scenario_child,
    {

      shiny::req(
        input$scenario_child
      )

      occasions <- scenario_occasions |>
        dplyr::filter(
          .data$child_key ==
            as.character(input$scenario_child)
        ) |>
        dplyr::arrange(
          .data$age_months
        )

      shiny::req(
        nrow(occasions) > 0L
      )

      labels <- paste0(
        vapply(
          occasions$age_months,
          format_age,
          character(1)
        ),
        " (",
        round(occasions$age_months, 1),
        " maanden)"
      )

      # Een named list voorkomt de jsonlite-waarschuwing over named vectors.
      choices <- as.list(
        stats::setNames(
          occasions$visit_key,
          labels
        )
      )

      shiny::updateSelectInput(
        session = session,
        inputId = "scenario_visit",
        choices = choices,
        selected = unname(
          tail(
            occasions$visit_key,
            1
          )
        )
      )
    },
    ignoreInit = FALSE
  )

  selected_occasion <- shiny::reactive({

    shiny::req(
      input$scenario_child,
      input$scenario_visit
    )

    occasions <- child_occasions()

    shiny::req(
      nrow(occasions) > 0L
    )

    selected <- occasions |>
      dplyr::filter(
        .data$visit_key ==
          as.character(input$scenario_visit)
      )

    # Tijdens het wisselen van kind kan scenario_visit heel kort nog
    # verwijzen naar het vorige kind. Wacht dan op de nieuwe selectie.
    shiny::req(
      nrow(selected) > 0L
    )

    selected |>
      dplyr::slice(1)
  })

  cutoff_age <- shiny::reactive({

    occasion <- selected_occasion()

    shiny::req(
      nrow(occasion) == 1L,
      "a" %in% names(occasion),
      !is.na(occasion$a[1])
    )

    as.numeric(
      occasion$a[1]
    )
  })

  # Truncate all three feedback components at the selected scenario age.
  scenario_feedback <- shiny::reactive({

    shiny::req(
      input$scenario_child,
      input$scenario_visit
    )

    x <- full_feedback
    child <- as.character(
      input$scenario_child
    )
    cutoff <- cutoff_age()

    shiny::req(
      length(child) == 1L,
      length(cutoff) == 1L,
      !is.na(cutoff)
    )

    visits <- x$visits |>
      dplyr::filter(
        as.character(.data[[id_column]]) == child,
        .data$a <= cutoff + 1e-8
      )

    shiny::req(
      nrow(visits) > 0L
    )

    items <- x$items |>
      dplyr::filter(
        as.character(.data[[id_column]]) == child,
        .data[[age_column]] <= cutoff + 1e-8
      )

    domains <- x$domains

    if (!is.null(domains)) {

      domains <- domains |>
        dplyr::filter(
          as.character(.data[[id_column]]) == child,
          .data$a <= cutoff + 1e-8
        )
    }

    children <- x$children |>
      dplyr::filter(
        as.character(.data[[id_column]]) == child
      )

    structure(
      list(
        visits = visits,
        items = items,
        domains = domains,
        children = children,
        settings = x$settings
      ),
      class = "dfeedback_data"
    )
  })

  selected_child_reactive <- shiny::reactive(input$scenario_child)

  mod_growth_integrated_server(
    id = "scenario_growth",
    feedback_data = scenario_feedback,
    child_id = selected_child_reactive,
    reference_key = "gsed2510",
    reference_population = "dutch",
    domain_set = "VWO"
  )

  # ---------------------------------------------------------------------------
  # Tab 1: child information
  # ---------------------------------------------------------------------------

  output$child_information <- shiny::renderUI({

    shiny::req(
      input$scenario_child,
      input$scenario_visit
    )

    occ <- selected_occasion()

    shiny::req(
      nrow(occ) == 1L,
      "age_months" %in% names(occ)
    )

    raw_child <- jgz_example_feedback |>
      dplyr::filter(
        as.character(.data[[id_column]]) ==
          as.character(input$scenario_child)
      )

    shiny::req(
      nrow(raw_child) > 0L
    )

    # De naam wordt apart weergegeven en daarom niet meer in deze lijst opgenomen.
    optional_fields <- intersect(
      c(
        "Geslachtkind",
        "geslacht",
        "Gebied",
        "gebied"
      ),
      names(raw_child)
    )

    optional_rows <- lapply(
      optional_fields,
      function(field) {

        available_values <- raw_child[[field]][
          !is.na(raw_child[[field]]) &
            raw_child[[field]] != ""
        ]

        if (length(available_values) == 0L) {
          return(NULL)
        }

        field_label <- dplyr::case_when(
          field %in% c("Geslachtkind", "geslacht") ~ "Geslacht:",
          field %in% c("Gebied", "gebied") ~ "Gebied:",
          TRUE ~ paste0(field, ":")
        )

        shiny::tags$div(
          class = "info-row",

          shiny::tags$span(
            class = "info-label",
            field_label
          ),

          shiny::tags$span(
            as.character(available_values[1])
          )
        )
      }
    )

    shiny::tagList(

      shiny::div(
        class = "child-profile",

        shiny::tags$div(
          class = "info-row",

          shiny::tags$span(
            class = "info-label",
            "Naam:"
          ),

          shiny::tags$span(
            selected_child_name()
          )
        ),

        shiny::tags$div(
          class = "info-row",

          shiny::tags$span(
            class = "info-label",
            "Huidige leeftijd:"
          ),

          shiny::tags$span(
            format_age(
              occ$age_months[1]
            )
          )
        ),

        shiny::tags$div(
          class = "info-row",

          shiny::tags$span(
            class = "info-label",
            "Contactmoment:"
          ),

          shiny::tags$span(
            as.character(
              input$scenario_visit
            )
          )
        ),

        optional_rows
      )
    )
  })

  # ---------------------------------------------------------------------------
  # Tab 2: items from exactly the selected occasion
  # ---------------------------------------------------------------------------

  current_item_data <- shiny::reactive({

    shiny::req(
      input$scenario_child,
      input$scenario_visit
    )

    # ---------------------------------------------------------------------------
    # Itemmetadata
    # ---------------------------------------------------------------------------

    metadata <- dscoreddi::itemtableVWO

    shiny::validate(
      shiny::need(
        all(c("item", "labelNL") %in% names(metadata)),
        paste0(
          "itemtableVWO moet minimaal de kolommen ",
          "'item' en 'labelNL' bevatten."
        )
      )
    )

    # Voeg month toe als deze onverwacht niet in itemtableVWO staat.
    if (!"month" %in% names(metadata)) {
      metadata$month <- NA_real_
    }

    metadata <- metadata |>
      dplyr::select(
        "item",
        "labelNL",
        "month"
      ) |>
      dplyr::distinct(
        item,
        .keep_all = TRUE
      )

    # ---------------------------------------------------------------------------
    # Domeinmetadata
    # ---------------------------------------------------------------------------

    domains <- dscoreddi::domaintableddi

    shiny::validate(
      shiny::need(
        all(c("item", "domain") %in% names(domains)),
        paste0(
          "domaintableddi moet de kolommen ",
          "'item' en 'domain' bevatten."
        )
      )
    )

    if (
      "set" %in% names(domains) &&
      "VWO" %in% domains$set
    ) {

      domains <- domains |>
        dplyr::filter(
          .data$set == "VWO"
        )
    }

    domains <- domains |>
      dplyr::select(
        "item",
        "domain"
      ) |>
      dplyr::distinct(
        item,
        .keep_all = TRUE
      )

    # ---------------------------------------------------------------------------
    # Items van het gekozen meetmoment
    # ---------------------------------------------------------------------------

    dat <- full_feedback$items |>
      dplyr::filter(
        as.character(.data[[id_column]]) ==
          as.character(input$scenario_child),

        as.character(.data[[visit_column]]) ==
          as.character(input$scenario_visit),

        !is.na(.data$score)
      )

    shiny::validate(
      shiny::need(
        nrow(dat) > 0L,
        paste0(
          "Voor dit contactmoment zijn geen ",
          "afgenomen kenmerken beschikbaar."
        )
      )
    )

    # Verwijder reeds aanwezige metadata vóór de joins.
    # Anders ontstaan labelNL.x, labelNL.y, month.x, month.y enzovoort.
    existing_metadata_columns <- intersect(
      c(
        "label",
        "labelNL",
        "month",
        "domain"
      ),
      names(dat)
    )

    if (length(existing_metadata_columns) > 0L) {

      dat <- dat |>
        dplyr::select(
          -dplyr::all_of(
            existing_metadata_columns
          )
        )
    }

    dat |>
      dplyr::left_join(
        metadata,
        by = "item"
      ) |>
      dplyr::left_join(
        domains,
        by = "item"
      ) |>
      dplyr::mutate(
        labelNL = dplyr::if_else(
          is.na(.data$labelNL) |
            .data$labelNL == "",
          .data$item,
          .data$labelNL
        ),

        domain = dplyr::if_else(
          is.na(.data$domain) |
            as.character(.data$domain) == "",
          "Overig",
          as.character(.data$domain)
        ),

        # Gebruik Inf voor ontbrekende afnamemaanden.
        # Die items komen daardoor onderaan binnen het domein.
        month_order = dplyr::if_else(
          is.na(.data$month),
          Inf,
          as.numeric(.data$month)
        ),

        answer = dplyr::case_when(
          .data$score == 1 ~ "(+) Ja, behaald",
          .data$score == 0 ~ "(-) Nee, nog niet behaald",
          TRUE ~ "Niet bekend"
        )
      ) |>
      dplyr::arrange(
        .data$domain,
        .data$month_order,
        .data$labelNL
      )
  })

  output$current_items <- shiny::renderUI({
    dat <- current_item_data()

    shiny::validate(
      shiny::need(nrow(dat) > 0L, "Voor dit contactmoment zijn geen afgenomen kenmerken beschikbaar.")
    )

    split_items <- split(dat, dat$domain, drop = TRUE)

    blocks <- lapply(names(split_items), function(domain_name) {
      domain_dat <- split_items[[domain_name]]

      rows <- lapply(seq_len(nrow(domain_dat)), function(i) {
        achieved <- domain_dat$score[i] == 1

        shiny::tags$div(
          class = "scenario-item-row",
          shiny::tags$span(class = "scenario-item-label", domain_dat$labelNL[i]),
          shiny::tags$span(
            class = if (achieved) "scenario-answer achieved" else "scenario-answer not-achieved",
            domain_dat$answer[i]
          )
        )
      })

      shiny::tags$section(
        class = "scenario-domain-block",
        shiny::tags$h4(domain_name),
        do.call(shiny::tagList, rows)
      )
    })

    do.call(shiny::tagList, blocks)
  })

  # ---------------------------------------------------------------------------
  # Navigation
  # ---------------------------------------------------------------------------

  shiny::observeEvent(input$to_items, {
    shiny::updateTabsetPanel(session, "scenario_tabs", selected = "items")
  })

  shiny::observeEvent(input$back_to_child, {
    shiny::updateTabsetPanel(session, "scenario_tabs", selected = "child_info")
  })

  shiny::observeEvent(input$to_feedback, {
    shiny::updateTabsetPanel(session, "scenario_tabs", selected = "feedback")
  })

  shiny::observeEvent(input$back_to_items, {
    shiny::updateTabsetPanel(session, "scenario_tabs", selected = "items")
  })

  # Start a newly selected scenario at step 1.
  shiny::observeEvent(
    list(input$scenario_child, input$scenario_visit),
    {
      shiny::updateTabsetPanel(session, "scenario_tabs", selected = "child_info")
    },
    ignoreInit = TRUE
  )

  output$current_step <- shiny::renderText({
    switch(
      input$scenario_tabs,
      child_info = "1. Kennismaken met het kind",
      items = "2. Ingevulde kenmerken",
      feedback = "3. Terugkoppeling",
      "1. Kennismaken met het kind"
    )
  })
}

shiny::shinyApp(ui = ui, server = server)
