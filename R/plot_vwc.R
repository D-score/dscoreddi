#' Van Wiechen Continue plot
#'
#' Draw plotly object of domain specific plot with VWC items.
#'
#' @param data data.frame with columsn as in `dscoreddi::prefdat`, c("A10", "A50", "A90", "domein", "labelNL", "nr", "labelNLnr", "sideA", "sideB").
#' @param domein character vector for domein filter, any of c("Fijne motoriek", "Communicatie","Grove motoriek")
#' @param age age in decimal years
#' @param selected character vector with item names of selected items.
#' @param vw_history data.frame with history of vw responses column c("age", "item", "value")
#'
#' @importFrom plotly plot_ly add_segments add_markers layout
#' @importFrom dplyr mutate group_by filter slice left_join arrange rename desc pull
#' @importFrom forcats fct_reorder
#' @importFrom rlang .data
#'
#' @return plot_ly object
#' @export
#'
#' @examples
#' plot_vwc()
plot_vwc <- function(
    data = dscoreddi::prefdat,
    domein = "Grove motoriek",
    age = NULL,
    selected = NULL,
    vw_history = NULL){

  if(is.null(data)){data <- dscoreddi::prefdat}
  if(is.null(selected)) selected <- length(0)
  if(!"domein" %in% names(data)){stop("De kolom domein zit niet in de data.")}
  if(!domein %in% data$domein){stop("Het gespecificeerde domein zit niet in de data.")}
  if(!all(c("A10", "A50", "A90", "domein", "labelNL", "nr", "labelNLnr", "sideA", "sideB") %in% names(data))){warning("Niet alle vereiste kolomnamen komen voor in de data: A10, A50, A90, domein, labelNL, nr, labelNLnr, sideA, sideB")}

  agemos <- NULL
  if(!is.null(age)) agemos <- age * 12

  vwhist <- data.frame(item = NA, value = NA, admin = NA, admintxt = NA)
  if(!is.null(vw_history)){
    vwhist <-
      vw_history |>
      #admin = for each item the age of first pass, or last fail
      group_by(.data$item) |>
      mutate(pass = ifelse(any(.data$value == 1), 1, 0)) |>
      filter(.data$value == .data$pass) |>
      group_by(.data$item) |>
      mutate(admin = ifelse(.data$pass == 1, min(.data$age), max(.data$age)))|>
      group_by(.data$item)|>
      slice(1) |> #laatste meting
      mutate(admin = .data$admin * 12,
             admintxt = ifelse(.data$value == 0, paste("niet behaald in maand", round(.data$admin, 0)), paste("behaald in maand", round(.data$admin, 0))))|>
      dplyr::select(.data$item, .data$value, .data$admin, .data$admintxt)
  }


##plot parameters:
  major <- c(0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15, seq(15,51,3))
  minor <- c(0.5,1.5,2.5,3.5,4.5,5.5,6.5,7.5,8.5,9.5,10.5,11.5,12.5,13.5,14.5,16,17,19,20,22,23,25,26,28,29,31,32,34,35,37,38,40,41,43,44,46,47,49,50)
  xtick <- c(major, minor) |> sort()
  xtick_text <- ifelse(xtick %in% major, xtick, "")
  statuscols <- c("candidate" = "#000000", "passed" = "#999999", "failed" = "#b03060", "continuous" = "#3399ff", "selected"= "#e6550d")

##basic plot for gm:
plot_data <-
data |>
  rename(kenmerk = .data$labelNL) |>
  filter(.data$sideA==1 | .data$sideB ==1) |>
  filter(.data$domein == !!domein) |>
  mutate(
  A10 = ifelse(.data$nr %in% 52:54, 0, .data$A10),
  A50 = ifelse(.data$nr %in% 52:54, NA, .data$A50),
  A90 = ifelse(.data$nr %in% 52:54, 12, .data$A90)) |>
  left_join(vwhist, by = "item") |>
  mutate(
    labelNLn = forcats::fct_reorder(.data$labelNLn, desc(.data$nr))) |>
  arrange(desc(.data$nr))


status <- plot_data |>
  mutate(
    status = "candidate",
    status = ifelse((!is.na(.data$value) & .data$value == 1), "passed", status),
    #previously fail as maroon
    status = ifelse((!is.na(.data$value) & .data$value == 0), "failed", status),
    #continuous items blue.
    status = ifelse((!is.null(agemos) && agemos < 15) & .data$item %in% c("ddigmd052", "ddigmd053", "ddigmd054"), "continuous", status ),
    status = ifelse((!is.null(agemos) && agemos < 3) & .data$item %in% c("ddigmd055"), "continuous", status ),
    status = ifelse(.data$item %in% selected, "selected", status)
  ) |>
  pull(.data$status)


plot_data |>
  plot_ly(x = ~A50,
          y= ~labelNLn,
          hoverinfo = "text") |>
  add_segments(x = ~A10, y = ~labelNLn,
               xend = ~A90, yend = ~labelNLn,
               color = status,
               colors = statuscols,
               showlegend = FALSE
  )|>
  add_markers(xaxis = "x2", inherit = TRUE, data = NULL, color = I("transparent")) |>
  add_markers(
    x = ~A10,
    y = ~labelNLn,
    color = status,
    colors = statuscols,
    text = ~kenmerk,
    marker = list(symbol = "line-ns-open",
                  size = 3,
                  line = list(
                    color = status,
                    colors = statuscols,
                    width = 2
                  )
    ),
    showlegend = FALSE
  ) |>
  add_markers(
    x = ~A50,
    y = ~labelNLn,
    color = status,
    colors = statuscols,
    text = ~kenmerk,
    showlegend = FALSE
  ) |>
  add_markers(
    x = ~A90,
    y = ~labelNLn,
    color = status,
    colors = statuscols,
    text = ~kenmerk,
    marker = list(symbol = "line-ns-open",
                  size = 3,
                  line = list(
                    color = status,
                    colors = statuscols,
                    width = 2
                  )
    ),
    showlegend = FALSE
  ) |>
  add_markers(
    x = ~admin,
    y = ~labelNLn,
    color = status,
    colors = statuscols,
    text = ~c(admintxt),
    marker = list(symbol = "x-thin-open",
                  size = 3,
                  line = list(
                    color = status,
                    colors = statuscols,
                    width = 2
                  )
    ),
    showlegend = FALSE
  ) |>
  layout(
    xaxis = list(
      title = "",
      tickvals = xtick,
      zeroline = FALSE,
      showgrid = TRUE,
      range = c(0,50),
      ticktext = xtick_text,
      tickangle = 0),
    xaxis2 = list(
      overlaying = "x",
      tickvals = xtick,
      ticktext = xtick_text,
      range = c(0,50),
      tickangle = 0,
      side = "top",
      showgrid = FALSE,
      zeroline = FALSE,
      tickfont = list(size = 11)
    ),
    yaxis = list(
      title = ""),
    margin = list(
      l = 0
    ),

    shapes = list(
      list(type = "line",
           y0 = 0,
           y1 = 1,
           yref = "paper",
           x0 = agemos,
           x1 = agemos,
           line = list(color = "black"))
    )
  )
}


#vline <- function(x){
#  list(type = "line",
#       y0 = 0,
#       y1 = 1,
#       yref = "paper",
#       x0 = x,
#       x1 = x,
#       line = list(color = "black"))
#}
