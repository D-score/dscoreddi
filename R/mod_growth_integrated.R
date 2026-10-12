#' Integrated D-score growth chart
#'
#' Shows Dutch reference curves, an individual D-score trajectory, clickable
#' item details grouped by domain, and optional domain-specific trajectories.
#'
#' Integrated D-score growth chart UI
#'
#' Integrated D-score growth chart
#'
#' @param id Module id.
#'
#' @return Shiny UI elements.
#' @export
mod_growth_integrated_ui <- function(id) {

  ns <- shiny::NS(id)

  shiny::tagList(

    shiny::fluidRow(

      shiny::column(
        width = 4,

        shiny::checkboxInput(
          inputId = ns("show_domains"),
          label = "Toon domeinlijnen",
          value = FALSE
        )
      ),

      shiny::column(
        width = 8,

        shiny::uiOutput(
          outputId = ns("domain_selector")
        )
      )
    ),

    shiny::fluidRow(

      # Plot links
      shiny::column(
        width = 9,
        shiny::div(
          class = "integrated-growth-plot",
        plotly::plotlyOutput(
          outputId = ns("plot"),
          height = "800px",
          width = "100%"
        )
        )
      ),

      # Itemdetails rechts
      shiny::column(
        width = 3,

        shiny::div(
          class = "integrated-item-panel",

          shiny::uiOutput(
            outputId = ns("click_instruction")
          ),

          shiny::uiOutput(
            outputId = ns("selected_occasion_title")
          ),

          shiny::uiOutput(
            outputId = ns("item_details")
          )
        )
      )
    )
  )
}
#' Server for the integrated D-score growth chart
#'
#' @param id Module id.
#' @param feedback_data A `dfeedback_data` object or reactive returning one.
#' @param child_id Selected child identifier or reactive returning one.
#' @param reference_key Optional key for `dscore::builtin_references`. If NULL,
#'   `feedback_data$settings$key` is used.
#' @param reference_population Optional reference population. If NULL,
#'   `feedback_data$settings$population` is used.
#' @param domain_set Optional set in `dscoreddi::domaintableddi`. If NULL,
#'   `feedback_data$settings$domain_set` is used when available.
#' @param domain_colors named vector with color codes for each domain.
#'
#' @return A reactive containing the selected contact moment.
#' @export
mod_growth_integrated_server <- function(
    id,
    feedback_data,
    child_id,
    reference_key = NULL,
    reference_population = NULL,
    domain_set = NULL,
    domain_colors = NULL) {

  shiny::moduleServer(id, function(input, output, session) {

    source_id <- session$ns("integrated_growth")
    # --------------------------------------------------------------------------
    # Fixed domain colors
    #
    # Supply a named vector through domain_colors if different domain names
    # or colors are required.
    # --------------------------------------------------------------------------

    default_domain_colors <- c(
      "communicatie" = "#FC8D62",
      "fijne motoriek" = "#8DA0CB",
      "grove motoriek" = "#F0E442",
      "Sociaal-emotioneel" = "#CC79A7",
      "Cognitie" = "#D55E00",
      "Taal" = "#56B4E9",
      "Persoonlijk-sociaal" = "#CC79A7",
      "Adaptief" = "#F0E442",
      "Overig" = "#7F7F7F"
    )

    if (is.null(domain_colors)) {
      domain_colors <- default_domain_colors
    }
    fd <- shiny::reactive({
      if (shiny::is.reactive(feedback_data)) feedback_data() else feedback_data
    })

    selected_child <- shiny::reactive({
      if (shiny::is.reactive(child_id)) child_id() else child_id
    })

    current_reference_key <- shiny::reactive({
      x <- fd()
      if (!is.null(reference_key)) return(reference_key)
      if (!is.null(x$settings$key) && length(x$settings$key) == 1L &&
          !is.na(x$settings$key) && nzchar(x$settings$key)) {
        return(x$settings$key)
      }
      "gsed2510"
    })

    current_reference_population <- shiny::reactive({
      x <- fd()
      if (!is.null(reference_population)) return(reference_population)
      if (!is.null(x$settings$population) &&
          length(x$settings$population) == 1L &&
          !is.na(x$settings$population) && nzchar(x$settings$population)) {
        return(x$settings$population)
      }
      "dutch"
    })

    current_domain_set <- shiny::reactive({
      x <- fd()
      if (!is.null(domain_set)) return(domain_set)
      if (!is.null(x$settings$domain_set) &&
          length(x$settings$domain_set) == 1L &&
          !is.na(x$settings$domain_set) && nzchar(x$settings$domain_set)) {
        return(x$settings$domain_set)
      }
      "VWO"
    })

    item_metadata <- shiny::reactive({
      metadata <- dscoreddi::itemtableVWO
      shiny::validate(shiny::need(
        all(c("item", "labelNL") %in% names(metadata)),
        "itemtableVWO moet de kolommen 'item' en 'labelNL' bevatten."
      ))
      metadata |>
        dplyr::select(.data$item, .data$labelNL) |>
        dplyr::distinct(.data$item, .keep_all = TRUE)
    })

    # Create one primary domain per item. If a weight column exists, the domain
    # with the largest weight is selected. Otherwise the first available domain
    # within the requested set is used.
    item_domains <- shiny::reactive({
      tab <- dscoreddi::domaintableddi
      shiny::validate(shiny::need(
        all(c("item", "domain") %in% names(tab)),
        "domaintableddi moet de kolommen 'item' en 'domain' bevatten."
      ))

      set_name <- current_domain_set()
      if ("set" %in% names(tab) && set_name %in% tab$set) {
        tab <- tab[tab$set == set_name, , drop = FALSE]
      }

      weight_candidates <- c("vote_weight", "weight", "vote", "w")
      weight_column <- intersect(weight_candidates, names(tab))

      if (length(weight_column) > 0L) {
        weight_column <- weight_column[1]
        tab <- tab |>
          dplyr::arrange(.data$item, dplyr::desc(.data[[weight_column]]))
      } else {
        tab <- tab |>
          dplyr::arrange(.data$item, .data$domain)
      }

      tab |>
        dplyr::select(.data$item, .data$domain) |>
        dplyr::distinct(.data$item, .keep_all = TRUE)
    })

    reference_data <- shiny::reactive({
      key <- current_reference_key()
      population <- current_reference_population()
    #  key = "gsed2510"
    #  population = "GSED-NLD"

      ref <- dscore::get_reference(population = population ,key = key) |>
        dplyr::arrange(.data$age)

      shiny::validate(shiny::need(
        nrow(ref) > 0L,
        paste0("Geen referentie voor key='", key,
               "' en population='", population, "'.")
      ))

      required <- c("age", "P3", "SDM2", "SDM1", "SD0",
                    "SDP1", "SDP2", "P97")
      shiny::validate(shiny::need(
        all(required %in% names(ref)),
        paste0("Referentiekolommen ontbreken: ",
               paste(setdiff(required, names(ref)), collapse = ", "))
      ))
      ref
    })

    child_scores <- shiny::reactive({
      x <- fd()
      child <- selected_child()
      shiny::req(x, child)

      id_column <- x$settings$id
      visit_column <- x$settings$visit

      shiny::validate(
        shiny::need(!is.null(x$visits), "D-scoregegevens ontbreken."),
        shiny::need(all(c(id_column, visit_column, "a", "d") %in%
                          names(x$visits)),
                    "Benodigde kolommen ontbreken in feedback_data$visits.")
      )

      dat <- x$visits[
        as.character(x$visits[[id_column]]) == as.character(child),
        , drop = FALSE
      ]
      dat <- dat[!is.na(dat$d), , drop = FALSE]
      shiny::validate(shiny::need(nrow(dat) > 0L,
                                  "Geen geldige D-scores voor dit kind."))

      if (!"daz" %in% names(dat)) dat$daz <- NA_real_
      if (!"n" %in% names(dat)) dat$n <- NA_integer_

      dat |>
        dplyr::mutate(
          age_months = .data$a * 12,
          visit_key = as.character(.data[[visit_column]]),
          hover = paste0(
            "<b>Leeftijd:</b> ", sprintf("%.0f", .data$age_months),
            " maanden",
            "<br><b>D-score:</b> ", sprintf("%.1f", .data$d),
            "<br><b>SDS:</b> ",
            dplyr::if_else(is.na(.data$daz), "niet beschikbaar",
                           sprintf("%.1f", .data$daz)),
            "<br><b>Aantal items:</b> ",
            dplyr::if_else(is.na(.data$n), "niet beschikbaar",
                           as.character(.data$n)),
            "<br><br><b>Klik voor itemdetails</b>"
          )
        ) |>
        dplyr::arrange(.data$a)
    })

    child_domains <- shiny::reactive({
      x <- fd()
      child <- selected_child()
      shiny::req(x, child)

      if (is.null(x$domains)) return(NULL)
      id_column <- x$settings$id
      if (!all(c(id_column, "domain", "a", "d") %in% names(x$domains))) {
        return(NULL)
      }

      dat <- x$domains[
        as.character(x$domains[[id_column]]) == as.character(child),
        , drop = FALSE
      ]
      dat <- dat[!is.na(dat$d), , drop = FALSE]
      if (nrow(dat) == 0L) return(NULL)

      if (!"n" %in% names(dat)) dat$n <- NA_integer_
      dat |>
        dplyr::mutate(
          age_months = .data$a * 12,
          hover = paste0(
            "<b>Domein:</b> ", .data$domain,
            "<br><b>Leeftijd:</b> ", sprintf("%.1f", .data$age_months),
            " maanden",
            "<br><b>Domeinscore:</b> ", sprintf("%.1f", .data$d),
            "<br><b>Aantal items:</b> ",
            dplyr::if_else(is.na(.data$n), "niet beschikbaar",
                           as.character(.data$n))
          )
        ) |>
        dplyr::arrange(.data$domain, .data$a)
    })

    output$domain_selector <- shiny::renderUI({
      if (!isTRUE(input$show_domains)) return(NULL)
      domains <- child_domains()
      # Complete fixed color mapping for domain names not yet included
      if (!is.null(domains)) {

        all_domain_names <- sort(
          unique(
            as.character(domains$domain)
          )
        )

        missing_domain_colors <- setdiff(
          all_domain_names,
          names(domain_colors)
        )

        if (length(missing_domain_colors) > 0L) {

          additional_colors <- grDevices::hcl.colors(
            n = length(missing_domain_colors),
            palette = "Dark 3"
          )

          names(additional_colors) <- missing_domain_colors

          domain_colors <- c(
            domain_colors,
            additional_colors
          )
        }
      }


      if (is.null(domains)) {
        return(shiny::tags$span(
          style = "color:#8a3b12;",
          "Domeinscores zijn niet beschikbaar."
        ))
      }
      choices <- sort(unique(as.character(domains$domain)))
      shiny::checkboxGroupInput(
        session$ns("selected_domains"),
        label = "Domeinen",
        choices = choices,
        selected = choices,
        inline = TRUE
      )
    })

    make_grid_shapes <- function(x_max, y_min, y_max) {
      shapes <- list()
      k <- 1L

      for (x in seq(0, x_max, by = 1)) {
        is_year <- x %% 12 == 0
        is_quarter <- x %% 3 == 0
        shapes[[k]] <- list(
          type = "line", x0 = x, x1 = x, y0 = y_min, y1 = y_max,
          xref = "x", yref = "y", layer = "below",
          line = list(
            color = if (is_year) "#00A878" else if (is_quarter) "#43CFA4" else "#B8EEDC",
            width = if (is_year) 1.25 else if (is_quarter) 0.8 else 0.35
          )
        )
        k <- k + 1L
      }

      for (y in seq(floor(y_min), ceiling(y_max), by = 1)) {
        is_major <- y %% 5 == 0
        shapes[[k]] <- list(
          type = "line", x0 = 0, x1 = x_max, y0 = y, y1 = y,
          xref = "x", yref = "y", layer = "below",
          line = list(
            color = if (is_major) "#43CFA4" else "#B8EEDC",
            width = if (is_major) 0.8 else 0.35
          )
        )
        k <- k + 1L
      }
      shapes
    }

    p <- shiny::reactive({
      scores <- child_scores()
      ref <- reference_data()
      domains <- child_domains()

      max_month <- max(c(48, scores$age_months, ref$age * 12), na.rm = TRUE)
      max_month <- ceiling(max_month / 12) * 12
      max_month <- 48 #hard coded

      domain_y <- if (!is.null(domains)) domains$d else numeric(0)
      y_min <- floor(min(c(0, ref$P3, scores$d, domain_y), na.rm = TRUE) / 5) * 5
      y_max <- ceiling(max(c(90, ref$P97, scores$d, domain_y), na.rm = TRUE) / 5) * 5

      # --------------------------------------------------------------------------
      # Fixed axis scaling
      #
      # scaleratio gives the y-axis the same total visual span as the x-axis.
      # For x = 0 to 48 and y = 0 to 90:
      # scaleratio = 48 / 90
      # --------------------------------------------------------------------------

      x_range <- c(
        0,
        max_month
      )

      y_range <- c(
        y_min,
        y_max
      )

      x_span <- diff(x_range)
      y_span <- diff(y_range)

      axis_scale_ratio <- x_span / y_span

      reference_specs <- list(
        list(column = "SDM2", label = "-2",  color = "#4BCB9D", width = 1),
        list(column = "SDM1", label = "-1",  color = "#18B982", width = 1.1),
        list(column = "SD0",  label = "0",   color = "#009E73", width = 2),
        list(column = "SDP1", label = "+1",  color = "#18B982", width = 1.1),
        list(column = "SDP2", label = "+2",  color = "#4BCB9D", width = 1)
      )

      plot <- plotly::plot_ly(source = source_id)

      for (spec in reference_specs) {

        plot <- plot |>
          plotly::add_lines(
            data = ref,
            x = ~age * 12,
            y = ref[[spec$column]],

            # Geen hoverinformatie voor referentielijnen
            hoverinfo = "skip",

            name = spec$label,

            line = list(
              color = spec$color,
              width = spec$width
            ),

            showlegend = FALSE
          )
      }

      plot <- plot |>
        plotly::add_lines(
          data = scores,
          x = ~age_months,
          y = ~d,
          name = "D-score kind",
          line = list(color = "#2C3E50", width = 2.5),
          hoverinfo = "skip"
        ) |>
        plotly::add_markers(
          data = scores,
          x = ~age_months,
          y = ~d,
          customdata = ~visit_key,
          text = ~hover,
          hoverinfo = "text",
          name = "D-score kind",
          marker = list(
            color = "#2C3E50", size = 11, symbol = "circle",
            line = list(color = "white", width = 1.5)
          ),
          showlegend = FALSE
        )

      if (
        isTRUE(input$show_domains) &&
        !is.null(domains)
      ) {

        selected <- input$selected_domains

        # NULL kan kort voorkomen voordat de dynamische UI is geïnitialiseerd.
        # Bij een lege selectie worden géén domeinlijnen weergegeven.
        if (is.null(selected)) {
          selected <- character(0)
        }

        domains_to_plot <- domains[
          as.character(domains$domain) %in% selected,
          ,
          drop = FALSE
        ]

        if (nrow(domains_to_plot) > 0L) {

          domain_names <- sort(
            unique(
              as.character(domains_to_plot$domain)
            )
          )

          for (domain_name in domain_names) {

            ddat <- domains_to_plot[
              as.character(domains_to_plot$domain) == domain_name,
              ,
              drop = FALSE
            ]

            domain_color <- unname(
              domain_colors[domain_name]
            )

            if (
              length(domain_color) == 0L ||
              is.na(domain_color)
            ) {
              domain_color <- "#7F7F7F"
            }

            plot <- plot |>

              plotly::add_lines(
                data = ddat,
                x = ~age_months,
                y = ~d,
                text = ~hover,
                hoverinfo = "text",
                name = domain_name,
                line = list(
                  color = domain_color,
                  width = 2,
                  dash = "solid"
                ),
                showlegend = TRUE
              ) |>

              plotly::add_markers(
                data = ddat,
                x = ~age_months,
                y = ~d,
                text = ~hover,
                hoverinfo = "text",
                name = domain_name,
                marker = list(
                  color = domain_color,
                  size = 7,
                  symbol = "circle",
                  line = list(
                    color = "white",
                    width = 0.8
                  )
                ),
                showlegend = FALSE
              )
          }
        }
      }
      # --------------------------------------------------------------------------
      # SDS-labels rechts aan het einde van de referentielijnen
      # --------------------------------------------------------------------------

      reference_end_age <- max(
        ref$age,
        na.rm = TRUE
      )

      reference_end_row <- which.min(
        abs(
          ref$age * 12 -
            max_month
        )
      )
      sds_annotations <- lapply(
        reference_specs,
        function(spec) {

          list(
            x = max_month - 0.7,
            y = ref[[spec$column]][reference_end_row],

            xref = "x",
            yref = "y",

            text = spec$label,

            showarrow = FALSE,

            xanchor = "right",
            yanchor = "middle",

            font = list(
              size = if (spec$label == "0") 14 else 13,
              color = spec$color,
              family = "Arial, sans-serif"
            ),

            bgcolor = "rgba(248,255,252,0.75)",
            borderpad = 1
          )
        }
      )

      # --------------------------------------------------------------------------
      # X-axis labels
      #
      # De maanden 3, 6 en 9 worden als reguliere x-aslabels weergegeven.
      # De volledige jaren worden afzonderlijk als grotere annotaties toegevoegd.
      # --------------------------------------------------------------------------

      all_quarter_ticks <- seq(
        from = 0,
        to = max_month,
        by = 3
      )

      # Alleen de tussenliggende maanden 3, 6 en 9.
      month_ticks <- all_quarter_ticks[
        all_quarter_ticks %% 12 != 0
      ]

      month_labels <- as.character(
        month_ticks %% 12
      )

      # Volledige jaren: 0, 1, 2, 3 en 4.
      year_ticks <- seq(
        from = 0,
        to = max_month,
        by = 12
      )

      year_labels <- as.character(
        year_ticks / 12
      )
      # --------------------------------------------------------------------------
      # Jaarlabels onderaan de x-as
      #
      # Plotly ondersteunt per x-as maar één tickfont. Daarom worden de
      # jaartallen als annotaties toegevoegd, zodat ze groter kunnen worden
      # weergegeven dan de maandlabels.
      # --------------------------------------------------------------------------

      year_annotations <- lapply(
        seq_along(year_ticks),
        function(i) {

          list(
            x = year_ticks[i],

            # Plaats de jaren onder de maandlabels.
            y = -0.02,

            xref = "x",
            yref = "paper",

            text = year_labels[i],

            showarrow = FALSE,

            xanchor = "center",
            yanchor = "top",

            font = list(
              size = 16,
              color = "#111111",
              family = "Arial, sans-serif"
            )
          )
        }
      )

      plot_annotations <- c(
        sds_annotations,
        year_annotations
      )

      plot |>
        plotly::layout(
          title = NULL,
          annotations = plot_annotations,
          paper_bgcolor = "white",
          plot_bgcolor = "#F8FFFC",

          hovermode = "closest",

          showlegend = TRUE,

          shapes = make_grid_shapes(
            max_month,
            y_min,
            y_max
          ),

          # ----------------------------------------------------------------------
          # X-axis: months
          # ----------------------------------------------------------------------

          xaxis = list(

            title = list(
              text = "Leeftijd in jaren/maanden",
              font = list(
                size = 16
              )
            ),

            range = x_range,

            tickmode = "array",
            tickvals = month_ticks,
            ticktext = month_labels,

            tickfont = list(
              size = 10,
              color = "#333333"
            ),

            ticks = "outside",
            ticklen = 4,

            showgrid = FALSE,
            zeroline = FALSE,
            mirror = TRUE,

            linecolor = "#00A878",
            linewidth = 1.2,

            constrain = "domain"
          ),


          # ----------------------------------------------------------------------
          # Y-axis
          # ----------------------------------------------------------------------

          yaxis = list(

            title = list(
              text = "D-score",
              font = list(
                size = 16
              )
            ),

            range = y_range,

            tick0 = y_min,
            dtick = 5,

            tickfont = list(
              size = 10,
              color = "#333333"
            ),

            ticks = "outside",
            ticklen = 4,

            showgrid = FALSE,
            zeroline = FALSE,
            mirror = TRUE,

            linecolor = "#00A878",
            linewidth = 1.2,

            # Koppel y-schaal aan x-schaal
            scaleanchor = "x",
            scaleratio = axis_scale_ratio,

            constrain = "domain",
            constraintoward = "middle"
          ),
           margin = list(
            l = 75,
            r = 75,
            b = 90,
            t = 100
          )
        ) |>

        plotly::config(
          displaylogo = FALSE,
          responsive = TRUE,
          modeBarButtonsToRemove = c(
            "lasso2d",
            "select2d"
          )
        )
    })

    output$plot <- plotly::renderPlotly(p())

    selected_visit <- shiny::reactiveVal(NULL)

    shiny::observeEvent(selected_child(), {
      selected_visit(NULL)
    }, ignoreInit = TRUE)

    shiny::observeEvent(
      plotly::event_data("plotly_click", source = source_id,
                         priority = "event"),
      {
        click <- plotly::event_data("plotly_click", source = source_id,
                                    priority = "event")
        if (!is.null(click) && "customdata" %in% names(click) &&
            length(click$customdata) > 0L &&
            !is.na(click$customdata[[1]])) {
          selected_visit(as.character(click$customdata[[1]]))
        }
      },
      ignoreNULL = TRUE
    )

    selected_items <- shiny::reactive({
      x <- fd()
      child <- selected_child()
      visit <- selected_visit()
      shiny::req(child, visit)

      id_column <- x$settings$id
      visit_column <- x$settings$visit
      age_column <- x$settings$age

      dat <- x$items[
        as.character(x$items[[id_column]]) == as.character(child) &
          as.character(x$items[[visit_column]]) == as.character(visit),
        , drop = FALSE
      ]
      dat <- dat[!is.na(dat$score), , drop = FALSE]
      shiny::validate(shiny::need(nrow(dat) > 0L,
                                  "Geen afgenomen items op dit meetmoment."))

      remove_columns <- intersect(c("label", "labelNL", "domain"), names(dat))
      if (length(remove_columns) > 0L) {
        dat <- dat |> dplyr::select(-dplyr::all_of(remove_columns))
      }

      dat |>
        dplyr::left_join(item_metadata(), by = "item") |>
        dplyr::left_join(item_domains(), by = "item") |>
        dplyr::mutate(
          labelNL = dplyr::if_else(
            is.na(.data$labelNL) | .data$labelNL == "",
            .data$item, .data$labelNL
          ),
          domain = dplyr::if_else(
            is.na(.data$domain) | .data$domain == "",
            "Overig", as.character(.data$domain)
          ),
          result_symbol = dplyr::case_when(
            .data$score == 1 ~ "+",
            .data$score == 0 ~ "-",
            TRUE ~ ""
          ),
          age_months = .data[[age_column]] * 12
        ) |>
        dplyr::arrange(.data$domain, dplyr::desc(.data$score), .data$labelNL)
    })

    output$click_instruction <- shiny::renderUI({
      if (!is.null(selected_visit())) return(NULL)
      shiny::div(
        class = "alert alert-info",
        "Klik op een D-scorepunt om de afgenomen items te bekijken."
      )
    })

    output$selected_occasion_title <- shiny::renderUI({
      if (is.null(selected_visit())) return(NULL)
      dat <- selected_items()
      shiny::tags$h4(
        paste0("Afgenomen items bij ", sprintf("%.0f", round(dat$age_months[1])),
               " maanden")
      )
    })

    output$item_details <- shiny::renderUI({
      if (is.null(selected_visit())) return(NULL)
      dat <- selected_items()
      split_items <- split(dat, dat$domain, drop = TRUE)

      domain_blocks <- lapply(names(split_items), function(domain_name) {
        domain_dat <- split_items[[domain_name]]
        item_rows <- lapply(seq_len(nrow(domain_dat)), function(i) {
          symbol <- domain_dat$result_symbol[i]
          color <- if (identical(symbol, "+")) "#218739" else "#D55E00"
          shiny::tags$div(
            style = paste0(
              "display:flex;gap:10px;align-items:baseline;",
              "padding:4px 0;border-bottom:1px solid #eeeeee;"
            ),
            shiny::tags$span(
              style = paste0("font-weight:bold;color:", color,
                             ";font-size:18px;width:14px;"),
              symbol
            ),
            shiny::tags$span(domain_dat$labelNL[i])
          )
        })
        shiny::tags$div(
          style = "margin-bottom:16px;",
          shiny::tags$h5(style = "font-weight:700;margin-bottom:6px;",
                         domain_name),
          do.call(shiny::tagList, item_rows)
        )
      })
      do.call(shiny::tagList, domain_blocks)
    })

    selected_visit
  })
}
