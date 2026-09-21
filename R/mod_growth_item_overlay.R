#' D-score curve with item-level observations around each score
#'
#' @param id Module id.
#'
#' @return Shiny UI elements.
#' @export
mod_growth_item_overlay_ui <- function(id) {

  ns <- shiny::NS(id)

  shiny::tagList(

    shiny::fluidRow(

      shiny::column(
        width = 4,
        shiny::checkboxInput(
          inputId = ns("show_line"),
          label = "Verbind de D-scores",
          value = TRUE
        )
      ),

      shiny::column(
        width = 4,
        shiny::checkboxInput(
          inputId = ns("show_dscore_points"),
          label = "Toon D-scorepunten",
          value = TRUE
        )
      ),

      shiny::column(
        width = 4,
        shiny::sliderInput(
          inputId = ns("item_spread"),
          label = "Spreiding van items rond de D-score",
          min = 0.5,
          max = 5,
          value = 2,
          step = 0.5
        )
      )
    ),

    plotly::plotlyOutput(
      outputId = ns("plot"),
      height = "620px"
    )
  )
}


#' D-score curve with item-level observations around each score
#'
#' @param id Module id.
#' @param feedback_data A `dfeedback_data` object or reactive returning one.
#' @param child_id Selected child identifier or reactive returning one.
#'
#' @return Reactive Plotly object.
#' @export
mod_growth_item_overlay_server <- function(
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
          all(c("item", "labelNL", "month") %in% names(metadata)),
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
    # Combined D-score and item data
    # --------------------------------------------------------------------------

    plot_data <- shiny::reactive({

      x <- fd()
      child <- selected_child()

      shiny::req(x)
      shiny::req(child)

      id_column <- x$settings$id
      visit_column <- x$settings$visit
      age_column <- x$settings$age

      shiny::validate(

        shiny::need(
          !is.null(x$visits),
          "Er zijn geen D-scoregegevens beschikbaar."
        ),

        shiny::need(
          !is.null(x$items),
          "Er zijn geen itemgegevens beschikbaar."
        ),

        shiny::need(
          all(
            c(
              id_column,
              visit_column,
              "a",
              "d"
            ) %in% names(x$visits)
          ),
          paste0(
            "Niet alle benodigde kolommen zijn in ",
            "feedback_data$visits aanwezig."
          )
        ),

        shiny::need(
          all(
            c(
              id_column,
              visit_column,
              age_column,
              "item",
              "score"
            ) %in% names(x$items)
          ),
          paste0(
            "Niet alle benodigde kolommen zijn in ",
            "feedback_data$items aanwezig."
          )
        )
      )


      # ------------------------------------------------------------------------
      # D-scores
      # ------------------------------------------------------------------------

      scores <- x$visits[
        as.character(x$visits[[id_column]]) ==
          as.character(child),
        ,
        drop = FALSE
      ] |>
        dplyr::filter(
          !is.na(.data$d)
        ) |>
        dplyr::mutate(
          visit_key = as.character(.data[[visit_column]]),
          age_months = .data$a * 12,
          score_tooltip = paste0(
            "<b>D-score</b>",
            "<br><b>Leeftijd:</b> ",
            sprintf("%.1f", .data$age_months),
            " maanden",
            "<br><b>D-score:</b> ",
            sprintf("%.1f", .data$d)
          )
        ) |>
        dplyr::arrange(
          .data$a
        )

      shiny::validate(
        shiny::need(
          nrow(scores) > 0L,
          "Geen geldige D-scores beschikbaar voor dit kind."
        )
      )


      # ------------------------------------------------------------------------
      # Items
      # ------------------------------------------------------------------------

      items <- x$items[
        as.character(x$items[[id_column]]) ==
          as.character(child),
        ,
        drop = FALSE
      ] |>
        dplyr::filter(
          !is.na(.data$score)
        )

      metadata_columns <- intersect(
        c("label", "labelNL", "month"),
        names(items)
      )

      if (length(metadata_columns) > 0L) {
        items <- items |>
          dplyr::select(
            -dplyr::all_of(metadata_columns)
          )
      }

      items <- items |>
        dplyr::left_join(
          item_metadata(),
          by = "item"
        ) |>
        dplyr::mutate(
          visit_key = as.character(.data[[visit_column]]),
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
          )
        )


      # ------------------------------------------------------------------------
      # Add D-score to every item observation
      # ------------------------------------------------------------------------

      score_lookup <- scores |>
        dplyr::select(
          .data$visit_key,
          .data$age_months,
          .data$d
        ) |>
        dplyr::distinct(
          .data$visit_key,
          .keep_all = TRUE
        )

      items <- items |>
        dplyr::left_join(
          score_lookup,
          by = "visit_key"
        ) |>
        dplyr::filter(
          !is.na(.data$d)
        )


      # ------------------------------------------------------------------------
      # Position items around the corresponding D-score
      #
      # Items are ordered by the planned administration month and then spread
      # symmetrically around the D-score. The vertical displacement is only a
      # display position and is not itself a score.
      # ------------------------------------------------------------------------

      items <- items |>
        dplyr::group_by(
          .data$visit_key
        ) |>
        dplyr::arrange(
          .data$month,
          .data$labelNL,
          .by_group = TRUE
        ) |>
        dplyr::mutate(
          n_items_at_visit = dplyr::n(),
          item_sequence = dplyr::row_number(),
          relative_position = dplyr::if_else(
            .data$n_items_at_visit == 1L,
            0,
            (
              .data$item_sequence -
                1
            ) /
              (
                .data$n_items_at_visit -
                  1
              ) *
              2 -
              1
          ),
          item_y = .data$d +
            .data$relative_position *
            input$item_spread
        ) |>
        dplyr::ungroup()


      # ------------------------------------------------------------------------
      # Hover text
      # ------------------------------------------------------------------------

      items$tooltip <- paste0(
        "<b>",
        items$labelNL,
        "</b>",
        "<br><b>Itemcode:</b> ",
        items$item,
        "<br><b>Leeftijd:</b> ",
        sprintf("%.1f", items$age_months),
        " maanden",
        "<br><b>Uitkomst:</b> ",
        items$result,
        "<br><b>D-score meetmoment:</b> ",
        sprintf("%.1f", items$d)
      )

      list(
        scores = scores,
        items = items
      )
    })


    # --------------------------------------------------------------------------
    # Plot
    # --------------------------------------------------------------------------

    p <- shiny::reactive({

      dat <- plot_data()

      scores <- dat$scores
      items <- dat$items

      plot_mode <- if (isTRUE(input$show_line)) {
        "lines"
      } else {
        "markers"
      }

      p <- plotly::plot_ly(
        source = session$ns("growth_item_overlay")
      )


      # D-score curve
      if (isTRUE(input$show_line)) {

        p <- p |>
          plotly::add_lines(
            data = scores,
            x = ~age_months,
            y = ~d,
            line = list(
              color = "#1261A0",
              width = 2
            ),
            name = "D-score",
            hoverinfo = "skip"
          )
      }


      # D-score points
      if (isTRUE(input$show_dscore_points)) {

        p <- p |>
          plotly::add_markers(
            data = scores,
            x = ~age_months,
            y = ~d,
            text = ~score_tooltip,
            hoverinfo = "text",
            marker = list(
              color = "#1261A0",
              size = 12,
              symbol = "circle",
              line = list(
                color = "#FFFFFF",
                width = 1.5
              )
            ),
            name = "D-score"
          )
      }


      # Item-level points
      p <- p |>
        plotly::add_markers(
          data = items,
          x = ~age_months,
          y = ~item_y,
          color = ~result,
          colors = c(
            "Behaald" = "#218739",
            "Nog niet behaald" = "#D55E00"
          ),
          text = ~tooltip,
          hoverinfo = "text",
          marker = list(
            size = 8,
            opacity = 0.85,
            line = list(
              color = "#FFFFFF",
              width = 0.5
            )
          ),
          name = ~result
        ) |>
        plotly::layout(
          title = list(
            text = paste0(
              "D-score met afgenomen ontwikkelingskenmerken ",
              "per meetmoment"
            )
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
          legend = list(
            orientation = "h",
            x = 0,
            y = -0.15
          ),
          margin = list(
            l = 70,
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

      p
    })


    # --------------------------------------------------------------------------
    # Render plot
    # --------------------------------------------------------------------------

    output$plot <- plotly::renderPlotly({
      p()
    })

    p
  })
}
