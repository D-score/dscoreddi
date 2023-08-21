#' Rename VWO items to gsed naming scheme.
#'
#' Rename VWO2005 item names to names in GSED format.
#'
#' @param x character vector with the ID.VWO2005 item name, starting with "v".
#' @param copy A logical indicating whether any unmatches names should
#' be copied (\code{copy = TRUE}) or set to an empty string.
#' @param version default = "2005"; version 1996 also possible.
#' @return A character vector of length \code{length(x)} with VWO
#' item names replaced by gsed item name.
#' @details
#' The gsed-naming convention is as follows. Position 1-3 codes the
#' instrument, position 4-5 codes the domain, position 6 codes
#' direct/caregiver/message, positions 7-9 is a item sequence number.
#' The object itemtableVWO is used as a renaming table.
#' @export
#' @examples
#' rename_vwo_gsed("v1")
#' rename_vwo_gsed(c("v1", "v40"))
#' rename_vwo_gsed(c("v55", "v55a", "v55c"))
rename_vwo_gsed <- function(x, copy = TRUE, version = "2005"){

  if(version == "2005"){
  # vnr <- gsub("v", "", x)
  # vnr <- stringr::str_pad(vnr, 3, pad = "0")
  # vnr <- ifelse(grepl("v68a", x), "068", vnr)
  # vnr <- ifelse(grepl("v68b", x), "168", vnr)
  # vnr <- ifelse(grepl("v68c", x), "268", vnr)
  # vnr <- ifelse(grepl("v55", x), "055", vnr)
  # vnr <- ifelse(grepl("v55b", x), "155", vnr)
  # vnr <- ifelse(grepl("v55c", x), "255", vnr)
  # vnr <- ifelse(grepl("v55d", x), "355", vnr)
  # it <- dscore::get_itemtable()
  #  git <- it[grepl("ddi",x = it$item),"item"]
  #  gnr <- substr(git, 7,9)
  #  y <- git[match(vnr, gnr)]
   y <- dscoreddi::itemtableVWO[match(x,  dscoreddi::itemtableVWO$ID.VWO2005),"item"]

  }

  if(version == "1996"){
    y <-  dscoreddi::itemtableVWO[match(x,  dscoreddi::itemtableVWO$ID.VWO1996),"item"]
  }

  if(version == "duchenne"){
    y <-  dscoreddi::itemtableVWO[match(x,  dscoreddi::itemtableVWO$labelNL_duch), "item"]
  }

  if(copy)
  y[is.na(y)] <- x[is.na(y)]

  y

}

##itembank 1996 moet het zijn.


#load("data-raw/data/itembankVWO.rda")

#x <- itembankVWO$ID.VWO2005
#vnr <- gsub("v", "", x)
#vnr <- stringr::str_pad(vnr, 3, pad = "0")[1:81]

#it <- dscore::get_itemtable()
#it <- it[grepl("ddi",x = it$item),]
#gnr <- substr(it$item, 7,9)

#itmatch <- cbind(it, itembankVWO[match(gnr, vnr),c("ID.VWO2005", "labelEN", "labelNL")])
#check matching results by inspecting labels
#View(itmatch)
#matches fit, so use the matches to translate the ID.VWO2005 names to gsed names
#itmatch$test <- rename_vwo_gsed(itmatch$ID.VWO2005)
#View(itmatch)
