#' Select Van Wiechen Continue
#'
#' Select the suggested items to be shown in the VWC plot.
#'
#' @param data data.frame to select items from with at least column item
#' @param itembank data.frame item and tau
#' @param age age in decimal years
#' @param refperc percentile to base item selection on (given age)
#' @param nsuggest number of items to select, default = 6
#' @param vw_history data.frame with historical vw data with colums c("age", "item", "value")
#' @param passed_items character vector with names of passed_items to exclude. Only used when vw_history not available.
#'
#' @importFrom dplyr pull filter
#' @importFrom dinstrument dform1
#'
#' @return character vector
#' @export
#'
#' @examples
#' select_vwc(age = 2)
select_vwc <-
  function(data = dscoreddi::prefdat,
           itembank = dscoreddi::itembank_vwc,
           age = NULL,
           refperc = 90,
           nsuggest = 6,
           vw_history = NULL,
           passed_items = NULL){

    if(!is.null(age)) agemos <- age * 12

      if(!is.null(vw_history)){
        passed_items <- vw_history |> filter(.data$value == 1) |> pull(.data$item)
      } else{
        if(!is.null(passed_items)) passed_items <- passed_items
       else{
        passed_items <- length(0)
      }}


    item_candidates <- data |>
        filter((.data$sideA==1 | .data$sideB ==1) & !.data$nr %in% 52:54) |>
        filter(!.data$item %in% passed_items) |> #excluded passed items
        pull(.data$item)

      itembank_candidates <- itembank |>
        filter(.data$item %in% item_candidates)

      selected_items <- dform1(itembank = itembank_candidates, ageband = agemos, reference = dscoreddi::expanded_reference, population = "dutch", leniency = refperc, n = nsuggest)$item

      selected_items
  }
