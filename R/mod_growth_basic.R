#' Basic growth-chart module UI
#'
#' @param id Module id.
#'
#' @return Shiny UI elements.
#' @export
mod_growth_basic_ui <- function(id) {

  ns <- shiny::NS(id)

  shiny::tagList(

    shiny::fluidRow(

      shiny::column(
        width = 3,
        shiny::checkboxInput(
          inputId = ns("show_line"),
          label = "Verbind de meetpunten",
          value = TRUE
        )
      ),

      shiny::column(
        width = 3,
        shiny::checkboxInput(
          inputId = ns("show_uncertainty"),
          label = "Toon onzekerheid rond de D-score",
          value = FALSE
        )
      ),

      shiny::column(
        width = 3,
        shiny::checkboxInput(
          inputId = ns("show_reference"),
          label = "Toon Nederlandse referentie",
          value = FALSE
        )
      ),

      shiny::column(
        width = 3,
        shiny::checkboxInput(
          inputId = ns("show_reference_band"),
          label = "Toon referentie als band",
          value = TRUE
        )
      )
    ),

    shiny::conditionalPanel(
      condition = sprintf(
        "input['%s'] === true",
        ns("show_reference")
      ),

      shiny::fluidRow(

        shiny::column(
          width = 4,
          shiny::selectInput(
            inputId = ns("reference_interval"),
            label = "Referentiegebied",
            choices = c(
              "P3 tot P97" = "P3_P97",
              "P10 tot P90" = "P10_P90",
              "Min en plus 1 SD" = "SD1",
              "Min en plus 2 SD" = "SD2"
            ),
            selected = "P3_P97"
          )
        ),

        shiny::column(
          width = 4,
          shiny::checkboxInput(
            inputId = ns("show_reference_median"),
            label = "Toon mediane referentielijn",
            value = FALSE
          )
        ),

        shiny::column(
          width = 4,
          shiny::checkboxInput(
            inputId = ns("show_reference_boundaries"),
            label = "Toon grenzen van referentiegebied",
            value = FALSE
          )
        )
      )
    ),

    plotly::plotlyOutput(
      outputId = ns("plot"),
      height = "520px"
    )
  )
}


#' Basic growth-chart module server
#'
#' @param id Module id.
#' @param feedback_data A `dfeedback_data` object or reactive returning one.
#' @param child_id Selected child identifier or reactive returning one.
#' @param reference_key Optional D-score key for the reference data. If `NULL`,
#'   the key in `feedback_data$settings$key` is used.
#' @param reference_population Optional reference population. If `NULL`,
#'   the population in `feedback_data$settings$population` is used.
#'
#' @return Reactive Plotly object.
#' @export
mod_growth_basic_server <- function(
    id,
    feedback_data,
    child_id,
    reference_key = NULL,
    reference_population = NULL) {

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
    # Reference key
    # --------------------------------------------------------------------------

    current_reference_key <- shiny::reactive({

      x <- fd()

      if (!is.null(reference_key)) {
        return(reference_key)
      }

      if (
        !is.null(x$settings$key) &&
        length(x$settings$key) == 1L &&
        !is.na(x$settings$key) &&
        nzchar(x$settings$key)
      ) {
        return(x$settings$key)
      }

      "gsed2510"
    })


    # --------------------------------------------------------------------------
    # Reference population
    # --------------------------------------------------------------------------

    current_reference_population <- shiny::reactive({

      x <- fd()

      if (!is.null(reference_population)) {
        return(reference_population)
      }

      if (
        !is.null(x$settings$population) &&
        length(x$settings$population) == 1L &&
        !is.na(x$settings$population) &&
        nzchar(x$settings$population)
      ) {
        return(x$settings$population)
      }

      "dutch"
    })


    # --------------------------------------------------------------------------
    # Data for selected child
    # --------------------------------------------------------------------------

    child_data <- shiny::reactive({

      x <- fd()
      child <- selected_child()

      shiny::req(x)
      shiny::req(child)

      id_column <- x$settings$id

      shiny::validate(
        shiny::need(
          !is.null(id_column),
          paste0(
            "De naam van de ID-kolom ontbreekt in ",
            "feedback_data$settings."
          )
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
          all(c("a", "d") %in% names(x$visits)),
          paste0(
            "De kolommen 'a' en 'd' ontbreken in ",
            "feedback_data$visits."
          )
        )
      )

      dat <- x$visits[
        as.character(x$visits[[id_column]]) ==
          as.character(child),
        ,
        drop = FALSE
      ]

      shiny::validate(
        shiny::need(
          nrow(dat) > 0L,
          "Geen metingen beschikbaar voor dit kind."
        ),
        shiny::need(
          any(!is.na(dat$d)),
          "Er zijn geen geldige D-scores beschikbaar voor dit kind."
        )
      )

      dat |>
        dplyr::arrange(.data$a)
    })


    # --------------------------------------------------------------------------
    # Reference data from dscore::builtin_references
    # --------------------------------------------------------------------------

    reference_data <- shiny::reactive({

      ref_key <- "gsed2510" #current_reference_key()
      ref_population <- current_reference_population()

      reference <- dscore::builtin_references |>
        dplyr::filter(
          .data$key == ref_key,
          .data$population == ref_population
        ) |>
        dplyr::arrange(.data$age)

      shiny::validate(
        shiny::need(
          nrow(reference) > 0L,
          paste0(
            "Geen referentiegegevens beschikbaar voor key = '",
            ref_key,
            "' en population = '",
            ref_population,
            "'."
          )
        )
      )

      reference
    })


    # --------------------------------------------------------------------------
    # Selected reference limits
    # --------------------------------------------------------------------------

    reference_limits <- shiny::reactive({

      interval <- input$reference_interval

      switch(
        interval,

        "P10_P90" = list(
          lower = "P10",
          middle = "P50",
          upper = "P90",
          label = "P10 tot P90"
        ),

        "P3_P97" = list(
          lower = "P3",
          middle = "P50",
          upper = "P97",
          label = "P3 tot P97"
        ),

        "SD1" = list(
          lower = "SDM1",
          middle = "SD0",
          upper = "SDP1",
          label = "min en plus 1 SD"
        ),

        "SD2" = list(
          lower = "SDM2",
          middle = "SD0",
          upper = "SDP2",
          label = "min en plus 2 SD"
        ),

        list(
          lower = "P10",
          middle = "P50",
          upper = "P90",
          label = "P10 tot P90"
        )
      )
    })


    # --------------------------------------------------------------------------
    # Plot
    # --------------------------------------------------------------------------

    p <- shiny::reactive({

      dat <- child_data()

      # Add optional variables if absent
      if (!"daz" %in% names(dat)) {
        dat$daz <- NA_real_
      }

      if (!"n" %in% names(dat)) {
        dat$n <- NA_integer_
      }

      if (!"sem" %in% names(dat)) {
        dat$sem <- NA_real_
      }

      # Hover information for individual scores
      dat$hover <- paste0(
        "<b>Leeftijd:</b> ",
        sprintf("%.1f", dat$a * 12),
        " maanden",
        "<br><b>D-score:</b> ",
        sprintf("%.1f", dat$d),
        "<br><b>DAZ:</b> ",
        ifelse(
          is.na(dat$daz),
          "niet beschikbaar",
          sprintf("%.2f", dat$daz)
        ),
        "<br><b>Aantal items:</b> ",
        ifelse(
          is.na(dat$n),
          "niet beschikbaar",
          as.character(dat$n)
        )
      )

      # Start plot
      g <- ggplot2::ggplot(
        data = dat,
        mapping = ggplot2::aes(
          x = .data$a * 12,
          y = .data$d,
          text = .data$hover,
          group = 1
        )
      )


      # ------------------------------------------------------------------------
      # Reference information
      # ------------------------------------------------------------------------

      if (isTRUE(input$show_reference)) {

        reference <- reference_data()
        limits <- reference_limits()

        required_reference_columns <- c(
          "age",
          limits$lower,
          limits$middle,
          limits$upper
        )

        missing_reference_columns <- setdiff(
          required_reference_columns,
          names(reference)
        )

        shiny::validate(
          shiny::need(
            length(missing_reference_columns) == 0L,
            paste0(
              "De volgende referentiekolommen ontbreken: ",
              paste(missing_reference_columns, collapse = ", "),
              "."
            )
          )
        )

        reference <- reference |>
          dplyr::mutate(
            reference_lower = .data[[limits$lower]],
            reference_middle = .data[[limits$middle]],
            reference_upper = .data[[limits$upper]],
            reference_hover = paste0(
              "<b>Nederlandse referentie</b>",
              "<br><b>Leeftijd:</b> ",
              sprintf("%.1f", .data$age * 12),
              " maanden",
              "<br><b>Ondergrens:</b> ",
              sprintf("%.1f", .data$reference_lower),
              "<br><b>Mediaan:</b> ",
              sprintf("%.1f", .data$reference_middle),
              "<br><b>Bovengrens:</b> ",
              sprintf("%.1f", .data$reference_upper)
            ),
            median_hover = paste0(
              "<b>Referentiemediaan</b>",
              "<br><b>Leeftijd:</b> ",
              sprintf("%.1f", .data$age * 12),
              " maanden",
              "<br><b>D-score:</b> ",
              sprintf("%.1f", .data$reference_middle)
            )
          )

        # Reference band
        if (isTRUE(input$show_reference_band)) {

          g <- g +
            ggplot2::geom_ribbon(
              data = reference,
              mapping = ggplot2::aes(
                x = .data$age * 12,
                ymin = .data$reference_lower,
                ymax = .data$reference_upper,
                text = .data$reference_hover,
                group = 1
              ),
              inherit.aes = FALSE,
              fill = "#B8CFE0",
              alpha = 0.30
            )
        }

        # Lower and upper reference boundaries
        if (isTRUE(input$show_reference_boundaries)) {

          g <- g +
            ggplot2::geom_line(
              data = reference,
              mapping = ggplot2::aes(
                x = .data$age * 12,
                y = .data$reference_lower,
                group = 1
              ),
              inherit.aes = FALSE,
              colour = "#7A8C99",
              linewidth = 0.6,
              linetype = "dotted"
            ) +
            ggplot2::geom_line(
              data = reference,
              mapping = ggplot2::aes(
                x = .data$age * 12,
                y = .data$reference_upper,
                group = 1
              ),
              inherit.aes = FALSE,
              colour = "#7A8C99",
              linewidth = 0.6,
              linetype = "dotted"
            )
        }

        # Mediane referentielijn
        if (isTRUE(input$show_reference_median)) {

          g <- g +
            ggplot2::geom_line(
              data = reference,
              mapping = ggplot2::aes(
                x = .data$age * 12,
                y = .data$reference_middle,
               # text = .data$median_hover,
                group = 1
              ),
              inherit.aes = FALSE,
              colour = "#596A73",
              linewidth = 0.9,
              linetype = "dashed"
            )
        }
      }

      # ------------------------------------------------------------------------
      # Lijn tussen individuele meetpunten
      # ------------------------------------------------------------------------

      if (isTRUE(input$show_line)) {

        g <- g +
          ggplot2::geom_line(
            linewidth = 0.9,
            colour = "#1261A0",
            na.rm = TRUE
          )
      }

      # ------------------------------------------------------------------------
      # Onzekerheidsinterval rond de individuele D-score
      # ------------------------------------------------------------------------

      if (
        isTRUE(input$show_uncertainty) &&
        any(!is.na(dat$sem))
      ) {

        g <- g +
          ggplot2::geom_errorbar(
            mapping = ggplot2::aes(
              ymin = .data$d - 1.96 * .data$sem,
              ymax = .data$d + 1.96 * .data$sem
            ),
            width = 0.25,
            linewidth = 0.6,
            alpha = 0.55,
            colour = "#1261A0",
            na.rm = TRUE
          )
      }

      # ------------------------------------------------------------------------
      # Individuele meetpunten
      # ------------------------------------------------------------------------

      g <- g +
        ggplot2::geom_point(
          size = 3.5,
          colour = "#1261A0",
          fill = "white",
          shape = 21,
          stroke = 1.2,
          na.rm = TRUE
        ) +
        ggplot2::labs(
          x = "Leeftijd (maanden)",
          y = "D-score",
          title = "Ontwikkeling over de tijd"
        ) +
        ggplot2::theme_minimal(
          base_size = 13
        ) +
        ggplot2::theme(
          panel.grid.minor = ggplot2::element_blank(),
          plot.title = ggplot2::element_text(
            face = "bold"
          )
        )

      # ------------------------------------------------------------------------
      # Omzetten naar Plotly
      # ------------------------------------------------------------------------

      plotly::ggplotly(
        g,
        tooltip = "text"
      ) |>
        plotly::layout(
          hovermode = "closest",
          margin = list(
            l = 70,
            r = 30,
            b = 70,
            t = 60
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

    # ----------------------------------------------------------------------------
    # Plot renderen
    # ----------------------------------------------------------------------------

    output$plot <- plotly::renderPlotly({
      p()
    })

    # Geef de reactive plot terug, onder andere voor tests
    p
  })
}
