dcat_algorithm2 <- function (itembank, p_start = 50, p_next = 50, data = NULL, stop_sem = 1.901462,
          min_length = NULL, max_length = 15, scalefactor = 4.064264,
          dens = FALSE, age_rep = NULL, reference = dscore::get_reference(),
          min_m2sd = min_length, d_true = FALSE)
{
  if (is.null(data)) {
    data <- dinstrument::sim(n = 1, reference = reference,
                             itembank = itembank, pop = "P50")
  }
  if (d_true) {
    d_ts <- dscore::dscore(data = data, items = itembank$item)
  }
  if (is.null(age_rep)) {
    age = data$age
  }
  if (!is.null(age_rep)) {
    age = age_rep/12
  }
  if (!"key" %in% names(itembank))
    itembank$key <- "gsed"
  items <- scores <- dscores <- sems <- dazs <- vector()
  dscore_qp <- dscore_post <- list()
  obs_items <- names(data)[!is.na(data)][names(data)[!is.na(data)] %in%
                                           itembank[, "item"]]
  if (length(obs_items) == 0) {
    stop("no observed scores for items in itembank")
  }
  itembank_candidates <- itembank[which(itembank[, "item"] %in%
                                          obs_items), ]
  if (is.null(max_length)) {
    max_length <- nrow(itembank_candidates)
  }
  if (is.null(min_length)) {
    min_length <- 0
  }
  start_it <- dcat_start(itembank = itembank_candidates, p = p_start,
                         age = age, scalefactor = scalefactor, reference= reference)
  item <- start_it[, "item"]
  itembank_candidates <- itembank_candidates[-which(itembank_candidates[,
                                                                        "item"] == item), ]
  answer <- data[, item]
  items <- c(items, item)
  scores <- c(scores, answer)
  df1 <- data.frame(matrix(c(age, scores), nrow = 1))
  colnames(df1) <- c("age", items)
  dscore_n_full <- dscore::dscore(data = df1, items = items,
                                  itembank = itembank)
  if (dens) {
    qp <- -10:100
    mu <- count_mu_gcdg(age)
    dscore_start <- stats::dnorm(qp, mu, 5)
    dscore_qp[[item]] <- qp
    dscore_post[[item]] <- dscore::dscore_posterior(data = df1,
                                                    items = items, itembank = itembank)[1, ]
  }
  dscore_n <- dscore_n_full$d
  dscore_sem <- dscore_n_full$sem
  dscores <- c(dscores, dscore_n_full$d)
  sems <- c(sems, dscore_n_full$sem)
  dazs <- c(dazs, dscore_n_full$daz)
  sdm2_age <- approx(x = reference$age, y = reference$SDM2,
                     xout = age, rule = 2)$y
  if (!is.na(dscore_n) & dscore_n < sdm2_age) {
    min_length <- min_m2sd
  }
  while (length(items) < max_length & (length(items) < min_length |
                                       (!is.na(dscore_sem & dscore_sem > stop_sem)))) {
    next_it <- dcat_next(itembank = itembank_candidates,
                         p = p_next, dscore = dscore_n)
    item <- next_it[, "item"]
    itembank_candidates <- itembank_candidates[-which(itembank_candidates[,
                                                                          "item"] == item), ]
    items <- c(items, item)
    answer <- data[, item]
    scores <- c(scores, answer)
    df1 <- data.frame(matrix(c(age, scores), nrow = 1))
    colnames(df1) <- c("age", items)
    sdm2_age <- approx(x = reference$age, y = reference$SDM2,
                       xout = age, rule = 2)$y
    dscore_n_full <- dscore::dscore(data = df1, items = items,
                                    itembank = itembank)
    dscore_n <- dscore_n_full$d
    dscore_sem <- dscore_n_full$sem
    dscores <- c(dscores, dscore_n)
    sems <- c(sems, dscore_n_full$sem)
    dazs <- c(dazs, dscore_n_full$daz)
    if (dens) {
      dscore_qp[[item]] <- qp
      dscore_post[[item]] <- dscore::dscore_posterior(data = df1,
                                                      items = items, itembank = itembank)[1, ]
    }
    if (dscore_n < sdm2_age) {
      min_length <- min_m2sd
    }
  }
  df <- data.frame(items = items, labels = itembank[match(items,
                                                          itembank[, "item"]), "label"], tau = itembank[match(items,
                                                                                                              itembank[, "item"]), "tau"], scores = scores, d = dscores,
                   sem = sems, daz = dazs)
  if (d_true) {
    df$a <- d_ts$a
    df$d_ts <- d_ts$d
    df$daz_ts <- d_ts$daz
    df$sem_ts <- d_ts$sem
  }
  if (!dens) {
    return(df)
  }
  if (dens) {
    return(list(df = df, start = dscore_start, qp = dscore_qp,
                posterior = dscore_post))
  }
}
