#' Item timeline module UI
#'
#' @param id Module id.
#'
#' @return Shiny UI elements.
#' @export
mod_item_timeline_ui <- function(id) {

  ns <- shiny::NS(id)

  shiny::tagList(

    shiny::fluidRow(

      shiny::column(
        width = 4,

        shiny::checkboxInput(
          inputId = ns("observed_only"),
          label = "Alleen afgenomen kenmerken",
          value = TRUE
        )
      ),

      shiny::column(
        width = 4,

        shiny::checkboxInput(
          inputId = ns("show_y_labels"),
          label = "Toon itemlabels op de y-as",
          value = TRUE
        )
      )
    ),

    plotly::plotlyOutput(
      outputId = ns("plot"),
      height = "760px"
    )
  )
}


#' Item timeline module server
#'
#' @param id Module id.
#' @param feedback_data A `dfeedback_data` object or reactive returning one.
#' @param child_id Selected child identifier or reactive returning one.
#'
#' @return Reactive Plotly object.
#' @export
mod_item_timeline_server <- function(
    id,
    feedback_data,
    child_id) {

  shiny::moduleServer(id, function(input, output, session) {

    # --------------------------------------------------------------------------
    # Feedback data
    # --------------------------------------------------------------------------

    fd <- shiny::reactive({

      if (shiny::is.reactive(feedback_data)) {
        feedback_data()
      } else {
        feedback_data
      }
    })


    # --------------------------------------------------------------------------
    # Selected child
    # --------------------------------------------------------------------------

    selected_child <- shiny::reactive({

      if (shiny::is.reactive(child_id)) {
        child_id()
      } else {
        child_id
      }
    })


    # --------------------------------------------------------------------------
    # Item metadata
    # --------------------------------------------------------------------------

    item_metadata <- shiny::reactive({

      metadata <- dscoreddi::itemtableVWO

      shiny::validate(
        shiny::need(
          all(
            c(
              "item",
              "labelNL",
              "month"
            ) %in% names(metadata)
          ),
          paste0(
            "dscoreddi::itemtableVWO moet de kolommen ",
            "'item', 'labelNL' en 'month' bevatten."
          )
        )
      )

      metadata |>
        dplyr::select(
          .data$item,
          .data$labelNL,
          .data$month
        ) |>
        dplyr::distinct(
          .data$item,
          .keep_all = TRUE
        )
    })


    # --------------------------------------------------------------------------
    # Item data for selected child
    # --------------------------------------------------------------------------

    child_items <- shiny::reactive({

      x <- fd()
      child <- selected_child()

      shiny::req(x)
      shiny::req(child)

      id_column <- x$settings$id
      age_column <- x$settings$age

      shiny::validate(

        shiny::need(
          !is.null(x$items),
          "Er zijn geen itemgegevens beschikbaar."
        ),

        shiny::need(
          !is.null(id_column),
          paste0(
            "De naam van de ID-kolom ontbreekt in ",
            "feedback_data$settings."
          )
        ),

        shiny::need(
          !is.null(age_column),
          paste0(
            "De naam van de leeftijdskolom ontbreekt in ",
            "feedback_data$settings."
          )
        ),

        shiny::need(
          id_column %in% names(x$items),
          paste0(
            "De ID-kolom '",
            id_column,
            "' ontbreekt in feedback_data$items."
          )
        ),

        shiny::need(
          age_column %in% names(x$items),
          paste0(
            "De leeftijdskolom '",
            age_column,
            "' ontbreekt in feedback_data$items."
          )
        ),

        shiny::need(
          "item" %in% names(x$items),
          "De kolom 'item' ontbreekt in feedback_data$items."
        ),

        shiny::need(
          "score" %in% names(x$items),
          "De kolom 'score' ontbreekt in feedback_data$items."
        )
      )


      # ------------------------------------------------------------------------
      # Selecteer het gekozen kind
      # ------------------------------------------------------------------------

      dat <- x$items[
        as.character(x$items[[id_column]]) ==
          as.character(child),
        ,
        drop = FALSE
      ]

      shiny::validate(
        shiny::need(
          nrow(dat) > 0L,
          "Geen itemgegevens beschikbaar voor dit kind."
        )
      )


      # ------------------------------------------------------------------------
      # Verwijder reeds aanwezige metadata
      # ------------------------------------------------------------------------

      metadata_columns <- intersect(
        c(
          "label",
          "labelNL",
          "month"
        ),
        names(dat)
      )

      if (length(metadata_columns) > 0L) {

        dat <- dat |>
          dplyr::select(
            -dplyr::all_of(metadata_columns)
          )
      }


      # ------------------------------------------------------------------------
      # Voeg Nederlands label en gebruikelijke afnamemaand toe
      # ------------------------------------------------------------------------

      dat <- dat |>
        dplyr::left_join(
          item_metadata(),
          by = "item"
        ) |>
        dplyr::mutate(

          labelNL = dplyr::if_else(
            is.na(.data$labelNL) |
              .data$labelNL == "",
            .data$item,
            .data$labelNL
          ),

          month = suppressWarnings(
            as.numeric(.data$month)
          )
        )


      # ------------------------------------------------------------------------
      # Geobserveerd en resultaat
      # ------------------------------------------------------------------------

      if (!"observed" %in% names(dat)) {
        dat$observed <- !is.na(dat$score)
      }

      if (!"result" %in% names(dat)) {

        dat <- dat |>
          dplyr::mutate(
            result = dplyr::case_when(
              .data$score == 1 ~ "Behaald",
              .data$score == 0 ~ "Nog niet behaald",
              TRUE ~ "Niet afgenomen"
            )
          )
      }

      dat <- dat |>
        dplyr::mutate(
          result = factor(
            as.character(.data$result),
            levels = c(
              "Behaald",
              "Nog niet behaald",
              "Niet afgenomen"
            )
          )
        )


      # ------------------------------------------------------------------------
      # Alleen afgenomen kenmerken
      # ------------------------------------------------------------------------

      if (isTRUE(input$observed_only)) {

        dat <- dat[
          dat$observed,
          ,
          drop = FALSE
        ]
      }

      shiny::validate(
        shiny::need(
          nrow(dat) > 0L,
          "Geen itemgegevens beschikbaar voor dit kind."
        )
      )


      # ------------------------------------------------------------------------
      # Leeftijd op het meetmoment
      # ------------------------------------------------------------------------

      dat <- dat |>
        dplyr::mutate(
          age_months = .data[[age_column]] * 12
        )


      # ------------------------------------------------------------------------
      # Eén y-aspositie per unieke itemcode
      #
      # De itemcode wordt intern gebruikt als y-waarde. Daardoor worden items
      # met hetzelfde Nederlandse label niet samengevoegd.
      #
      # Oplopende volgorde van month:
      #   - eerste maanden onderaan;
      #   - latere maanden bovenaan;
      #   - onbekende maanden helemaal bovenaan.
      # ------------------------------------------------------------------------

      item_order <- dat |>
        dplyr::distinct(
          .data$item,
          .data$labelNL,
          .data$month
        ) |>
        dplyr::mutate(
          month_order = dplyr::if_else(
            is.na(.data$month),
            Inf,
            .data$month
          )
        ) |>
        dplyr::arrange(
          .data$month_order,
          .data$labelNL,
          .data$item
        ) |>
        dplyr::mutate(
          y_position = dplyr::row_number()
        )


      # ------------------------------------------------------------------------
      # Maak zichtbare labels uniek
      #
      # Alleen wanneer labels identiek zijn, wordt de itemcode toegevoegd.
      # ------------------------------------------------------------------------

      item_order <- item_order |>
        dplyr::group_by(
          .data$labelNL
        ) |>
        dplyr::mutate(
          n_same_label = dplyr::n()
        ) |>
        dplyr::ungroup() |>
        dplyr::mutate(
          axis_label = dplyr::if_else(
            .data$n_same_label > 1L,
            paste0(
              .data$labelNL,
              " [",
              .data$item,
              "]"
            ),
            .data$labelNL
          )
        )


      # ------------------------------------------------------------------------
      # Voeg de y-positie toe aan iedere itemobservatie
      # ------------------------------------------------------------------------

      dat <- dat |>
        dplyr::left_join(
          item_order |>
            dplyr::select(
              .data$item,
              .data$y_position,
              .data$axis_label
            ),
          by = "item"
        )


      # ------------------------------------------------------------------------
      # Hovertekst
      # ------------------------------------------------------------------------

      dat$tooltip <- paste0(
        "<b>",
        dat$labelNL,
        "</b>",
        "<br><b>Itemcode:</b> ",
        dat$item,
        "<br><b>Gebruikelijke afnamemaand:</b> ",
        ifelse(
          is.na(dat$month),
          "niet bekend",
          as.character(dat$month)
        ),
        "<br><b>Leeftijd kind:</b> ",
        sprintf(
          "%.1f",
          dat$age_months
        ),
        " maanden",
        "<br><b>Uitkomst:</b> ",
        as.character(dat$result)
      )


      # ------------------------------------------------------------------------
      # Output
      # ------------------------------------------------------------------------

      list(
        observations = dat |>
          dplyr::arrange(
            .data$y_position,
            .data$age_months
          ),

        item_order = item_order |>
          dplyr::arrange(
            .data$y_position
          )
      )
    })


    # --------------------------------------------------------------------------
    # Item timeline plot
    # --------------------------------------------------------------------------

    p <- shiny::reactive({

      plot_data <- child_items()

      dat <- plot_data$observations
      item_order <- plot_data$item_order

      # Bepaal of y-aslabels zichtbaar moeten zijn.
      show_tick_labels <- isTRUE(
        input$show_y_labels
      )

      plotly::plot_ly(
        data = dat,
        x = ~age_months,
        y = ~y_position,
        type = "scatter",
        mode = "markers",
        color = ~result,
        colors = c(
          "Behaald" = "#218739",
          "Nog niet behaald" = "#D55E00",
          "Niet afgenomen" = "#BDBDBD"
        ),
        text = ~tooltip,
        hoverinfo = "text",
        marker = list(
          size = 10,
          line = list(
            width = 0.5,
            color = "#FFFFFF"
          )
        ),
        source = session$ns("item_timeline")
      ) |>
        plotly::layout(

          title = list(
            text = "Afgenomen ontwikkelingskenmerken"
          ),

          xaxis = list(
            title = "Leeftijd van het kind (maanden)",
            zeroline = FALSE
          ),

          yaxis = list(
            title = "",
            type = "linear",

            # De laagste y-positie staat standaard onderaan.
            # Omdat y_position oploopt met month, staan de eerste
            # afnamemaanden onderaan en de latere maanden bovenaan.
            autorange = TRUE,

            tickmode = "array",
            tickvals = if (show_tick_labels) {
              item_order$y_position
            } else {
              numeric(0)
            },
            ticktext = if (show_tick_labels) {
              item_order$axis_label
            } else {
              character(0)
            },
            showticklabels = show_tick_labels,
            ticks = if (show_tick_labels) {
              "outside"
            } else {
              ""
            },
            automargin = TRUE,
            range = c(
              0.5,
              nrow(item_order) + 0.5
            )
          ),

          legend = list(
            title = list(
              text = "Uitkomst"
            ),
            orientation = "h",
            x = 0,
            y = -0.15
          ),

          hovermode = "closest",

          margin = list(
            l = if (show_tick_labels) {
              300
            } else {
              60
            },
            r = 30,
            b = 90,
            t = 70
          )
        ) |>
        plotly::config(
          displaylogo = FALSE,
          modeBarButtonsToRemove = c(
            "lasso2d",
            "select2d"
          )
        )
    })


    # --------------------------------------------------------------------------
    # Render output
    # --------------------------------------------------------------------------

    output$plot <- plotly::renderPlotly({
      p()
    })

    p
  })
}
