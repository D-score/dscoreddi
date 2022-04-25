#' Percentile plot data
#'
#' Create the input data to plot the percentile references for each Van Wiechen
#' milestone for P10, P50 and P90, per domain.
#'
#' @param itembank itembank used to obtain suggested items given age an percentile.
#' @param reference reference for d-score given age
#' @param age age of the child (dynamic with use)
#' @param refperc percentile used to select suggetsed items (dynamic with use)
#' @param suggest number of suggested items (dynamic with use)
#' @importFrom dplyr recode %>% mutate arrange filter select .data left_join
#' @importFrom dmetric calculate_age_equivalents
#' @importFrom dinstrument dform1
#' @return data.frame used for plot
#' @export
percentile_plot_data <- function(#general input
           itembank,
           reference,
           age,
           refperc,
           suggest){

  refdata <- dmetric::calculate_age_equivalents(itembank = itembank,
                                                scalefactor = 2.099986,
                                                p = c(10,50,90),
                                                reference = reference)
  prefdat <- refdata %>%
    mutate(
      domain = substr(item, 4,5),
      domein = dplyr::recode(domain,
                             "cm" = "Communicatie",
                             "fm" = "Fijne motoriek",
                             "gm" = "Grove motoriek")) %>%
    #rename(leeftijd = A) %>%
    left_join(dscoreddi::itemtableVWO %>% select(item, labelNL, ID.VWO2005, month), by = "item") %>%
    mutate(nr = as.numeric(substr(item, 7, 9)),
           nr = ifelse(nr == 136, 35, nr),
           nr = ifelse(nr == 148, 40, nr),
           nr = ifelse(nr == 068, 68.1, nr),
           nr = ifelse(nr == 168, 68.2, nr),
           nr = ifelse(nr == 268, 68.3, nr),
           domein = ifelse(nr == 6, "Fijne motoriek", domein),

           labelNLnr = paste(nr, labelNL, sep = ". "),
           #labelNLn = ifelse(nr %in% 52:55, paste(nr, labelNL, sep = ".* "), labelNLn),
           labelNLn = ifelse(month < 10, paste(labelNLnr, month, sep = " |  "),
                             paste(labelNLnr, month, sep = " |") ),
           labelNLn = ifelse(nr %in% 52:55, paste(labelNLnr, "**", sep = " | "), labelNLn),
           sideA = ifelse(nr %in% c(1:12, 29:38, 52:67), 1, 0),
           sideB = ifelse(nr %in% c(11:28, 37:51, 66:75, 68.1, 68.2), 1, 0),
           A10 = ifelse(is.na(A10), 0, A10),
           A50 = ifelse(is.na(A50), 0, A50),
           A90 = ifelse(is.na(A90), 0, A90)
    ) %>%
    arrange(nr)

  item_candidates <- prefdat %>%
    filter(sideA==1 | sideB ==1) %>%
    select(item)

  itembank_candidates <-
    itembank %>%
    filter(item %in% item_candidates[[1]])

  selected_items <- dinstrument::dform1(itembank = itembank_candidates, ageband = age, reference = reference, scalefactor = 2.099986, leniency = refperc, n = suggest)$item

  prefdat %>%
    mutate(highlight = ifelse(item %in% selected_items, "#e6550d", "black"),
           bold = ifelse(item %in% selected_items, "bold", "plain"))

  #1A80C4 - blue color
  #31a354 - green color
  #e6550d - orange color


}
