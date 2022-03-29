#* @apiTitle TNO D-score API

#* Get the D-score item bank
#* @get /itembank
function(){
  dscore::builtin_itembank
}

#* Get D-score reference distribution
#* @param population A string describing the population. Currently supported are
#*   "dutch" and "gcdg" (default).
#* @get /reference
function(population = "gcdg"){
  dscore::get_reference(population = population)
}

#* Get itemtable VWO
#* @get /itemtableVWO
function(){
  dscoreddi::itemtableVWO
}

#* Get age equivalents
#* @inheritsParams dmetric::calculate_age_equivalents
#* @post /get_age_equivalents
function(model = "null",
         itembank = "null",
         scalefactor = "null",
         p = c(10, 50, 90),
         reference = dscore::get_reference(),
         metric = "dscore"){
  if (model[[1]] == "null") model <- NULL
  if (itembank[[1]] == "null") itembank <- NULL
  if (scalefactor[[1]] == "null") scalefactor <- NULL
  dmetric::calculate_age_equivalents(
    model = model,
    itembank = itembank,
    scalefactor = scalefactor,
    p = p,
    reference = reference,
    metric = metric)
}

