#' D-score growth curve with clickable item details
#'
#' @param id Module id.
#'
#' @return Shiny UI elements.
#' @export
mod_growth_item_drilldown_ui <- function(id) {

  ns <- shiny::NS(id)

  shiny::tagList(

    shiny::fluidRow(

      shiny::column(
        width = 4,
        shiny::checkboxInput(
          inputId = ns("show_line"),
          label = "Verbind de meetpunten",
          value = TRUE
        )
      ),

      shiny::column(
        width = 4,
        shiny::checkboxInput(
          inputId = ns("show_uncertainty"),
          label = "Toon onzekerheid rond de D-score",
          value = TRUE
        )
      )
    ),

    plotly::plotlyOutput(
      outputId = ns("plot"),
      height = "480px"
    ),

    shiny::hr(),

    shiny::uiOutput(
      outputId = ns("selected_occasion_title")
    ),

    shiny::uiOutput(
      outputId = ns("item_details")
    )
  )
}


#' D-score growth curve with clickable item details
#'
#' @param id Module id.
#' @param feedback_data A `dfeedback_data` object or reactive returning one.
#' @param child_id Selected child identifier or reactive returning one.
#'
#' @return A reactive containing the selected contact moment.
#' @export
mod_growth_item_drilldown_server <- function(
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
          all(c("item", "labelNL") %in% names(metadata)),
          paste0(
            "dscoreddi::itemtableVWO moet de kolommen ",
            "'item' en 'labelNL' bevatten."
          )
        )
      )

      metadata |>
        dplyr::select(
          .data$item,
          .data$labelNL
        ) |>
        dplyr::distinct(
          .data$item,
          .keep_all = TRUE
        )
    })


    # --------------------------------------------------------------------------
    # D-score data for selected child
    # --------------------------------------------------------------------------

    child_scores <- shiny::reactive({

      x <- fd()
      child <- selected_child()

      shiny::req(x)
      shiny::req(child)

      id_column <- x$settings$id
      visit_column <- x$settings$visit

      shiny::validate(

        shiny::need(
          !is.null(x$visits),
          "Er zijn geen D-scoregegevens beschikbaar."
        ),

        shiny::need(
          id_column %in% names(x$visits),
          paste0(
            "De ID-kolom '",
            id_column,
            "' ontbreekt in feedback_data$visits."
          )
        ),

        shiny::need(
          visit_column %in% names(x$visits),
          paste0(
            "De contactmomentkolom '",
            visit_column,
            "' ontbreekt in feedback_data$visits."
          )
        ),

        shiny::need(
          all(c("a", "d") %in% names(x$visits)),
          "De kolommen 'a' en 'd' ontbreken in feedback_data$visits."
        )
      )

      dat <- x$visits[
        as.character(x$visits[[id_column]]) ==
          as.character(child),
        ,
        drop = FALSE
      ]

      dat <- dat[
        !is.na(dat$d),
        ,
        drop = FALSE
      ]

      shiny::validate(
        shiny::need(
          nrow(dat) > 0L,
          "Geen geldige D-scores beschikbaar voor dit kind."
        )
      )

      if (!"daz" %in% names(dat)) {
        dat$daz <- NA_real_
      }

      if (!"sem" %in% names(dat)) {
        dat$sem <- NA_real_
      }

      if (!"n" %in% names(dat)) {
        dat$n <- NA_integer_
      }

      dat <- dat |>
        dplyr::mutate(
          age_months = .data$a * 12,
          visit_key = as.character(.data[[visit_column]]),
          tooltip = paste0(
            "<b>Leeftijd:</b> ",
            sprintf("%.1f", .data$age_months),
            " maanden",
            "<br><b>D-score:</b> ",
            sprintf("%.1f", .data$d),
            "<br><b>DAZ:</b> ",
            dplyr::if_else(
              is.na(.data$daz),
              "niet beschikbaar",
              sprintf("%.2f", .data$daz)
            ),
            "<br><b>Aantal items:</b> ",
            dplyr::if_else(
              is.na(.data$n),
              "niet beschikbaar",
              as.character(.data$n)
            ),
            "<br><br><b>Klik voor itemdetails</b>"
          )
        ) |>
        dplyr::arrange(
          .data$a
        )

      dat
    })


    # --------------------------------------------------------------------------
    # D-score plot
    # --------------------------------------------------------------------------

    output$plot <- plotly::renderPlotly({

      dat <- child_scores()

      plot_mode <- if (isTRUE(input$show_line)) {
        "lines+markers"
      } else {
        "markers"
      }

      uncertainty <- NULL

      if (
        isTRUE(input$show_uncertainty) &&
        any(!is.na(dat$sem))
      ) {

        uncertainty <- list(
          type = "data",
          array = 1.96 * dat$sem,
          symmetric = TRUE,
          visible = TRUE,
          color = "#1261A0",
          thickness = 1,
          width = 4
        )
      }

      plotly::plot_ly(
        data = dat,
        x = ~age_months,
        y = ~d,
        type = "scatter",
        mode = plot_mode,
        customdata = ~visit_key,
        text = ~tooltip,
        hoverinfo = "text",
        source = session$ns("growth_click"),
        line = list(
          color = "#1261A0",
          width = 2
        ),
        marker = list(
          color = "#1261A0",
          size = 11,
          line = list(
            color = "#FFFFFF",
            width = 1.5
          )
        ),
        error_y = uncertainty,
        name = "D-score"
      ) |>
        plotly::layout(
          title = list(
            text = "Klik op een meetpunt voor de afgenomen kenmerken"
          ),
          xaxis = list(
            title = "Leeftijd van het kind (maanden)",
            zeroline = FALSE
          ),
          yaxis = list(
            title = "D-score",
            zeroline = FALSE
          ),
          hovermode = "closest",
          showlegend = FALSE,
          margin = list(
            l = 70,
            r = 30,
            b = 70,
            t = 70
          )
        ) |>
        plotly::config(
          displaylogo = FALSE
        )
    })


    # --------------------------------------------------------------------------
    # Click event
    # --------------------------------------------------------------------------

    selected_visit <- shiny::reactive({

      click <- plotly::event_data(
        event = "plotly_click",
        source = session$ns("growth_click"),
        priority = "event"
      )

      if (is.null(click)) {
        return(NULL)
      }

      if (
        !"customdata" %in% names(click) ||
        length(click$customdata) == 0L
      ) {
        return(NULL)
      }

      as.character(
        click$customdata[[1]]
      )
    })


    # --------------------------------------------------------------------------
    # Items for selected contact moment
    # --------------------------------------------------------------------------

    selected_items <- shiny::reactive({

      x <- fd()
      child <- selected_child()
      visit <- selected_visit()

      shiny::req(child)
      shiny::req(visit)

      id_column <- x$settings$id
      visit_column <- x$settings$visit
      age_column <- x$settings$age

      dat <- x$items[
        as.character(x$items[[id_column]]) ==
          as.character(child) &
          as.character(x$items[[visit_column]]) ==
          as.character(visit),
        ,
        drop = FALSE
      ]

      # Alleen daadwerkelijk afgenomen items
      dat <- dat[
        !is.na(dat$score),
        ,
        drop = FALSE
      ]

      shiny::validate(
        shiny::need(
          nrow(dat) > 0L,
          "Op dit meetmoment zijn geen afgenomen items beschikbaar."
        )
      )

      metadata_columns <- intersect(
        c("label", "labelNL"),
        names(dat)
      )

      if (length(metadata_columns) > 0L) {
        dat <- dat |>
          dplyr::select(
            -dplyr::all_of(metadata_columns)
          )
      }

      dat |>
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
          result = dplyr::case_when(
            .data$score == 1 ~ "Behaald",
            .data$score == 0 ~ "Nog niet behaald",
            TRUE ~ "Niet afgenomen"
          ),
          age_months = .data[[age_column]] * 12
        ) |>
        dplyr::arrange(
          dplyr::desc(.data$score),
          .data$labelNL
        )
    })


    # --------------------------------------------------------------------------
    # Selected occasion title
    # --------------------------------------------------------------------------

    output$selected_occasion_title <- shiny::renderUI({

      visit <- selected_visit()

      if (is.null(visit)) {
        return(
          shiny::div(
            class = "alert alert-info",
            "Klik op een D-scorepunt om de afgenomen kenmerken te bekijken."
          )
        )
      }

      dat <- selected_items()

      shiny::tags$h4(
        paste0(
          "Afgenomen kenmerken bij ",
          sprintf("%.1f", dat$age_months[1]),
          " maanden"
        )
      )
    })


    # --------------------------------------------------------------------------
    # Item details
    # --------------------------------------------------------------------------

    output$item_details <- shiny::renderUI({

      visit <- selected_visit()

      if (is.null(visit)) {
        return(NULL)
      }

      dat <- selected_items()

      item_rows <- lapply(
        seq_len(nrow(dat)),
        function(i) {

          passed <- identical(
            dat$result[i],
            "Behaald"
          )

          icon <- if (passed) {
            "\u2713"
          } else {
            "\u2717"
          }

          colour <- if (passed) {
            "#218739"
          } else {
            "#D55E00"
          }

          shiny::tags$div(
            style = paste0(
              "display:flex;",
              "align-items:flex-start;",
              "gap:10px;",
              "padding:8px 4px;",
              "border-bottom:1px solid #eeeeee;"
            ),

            shiny::tags$span(
              style = paste0(
                "font-weight:bold;",
                "font-size:18px;",
                "color:",
                colour,
                ";"
              ),
              icon
            ),

            shiny::tags$div(
              shiny::tags$strong(
                dat$labelNL[i]
              ),
              shiny::tags$br(),
              shiny::tags$small(
                paste0(
                  dat$item[i],
                  " | ",
                  dat$result[i]
                )
              )
            )
          )
        }
      )

      do.call(
        shiny::tagList,
        item_rows
      )
    })

    selected_visit
  })
}
