refdata <- dmetric::calculate_age_equivalents(model = model_input$model, p = c(10,20,80,90), reference = dscore::get_reference(population = "dutch"))


pref <- refdata %>%
  #pivot_longer(cols = c(everything(), -item), names_pattern = "(.)(.*)",
  #             names_to = c(".value", "percentile")) %>%
  #rename(Leeftijd = A) %>%
  #mutate(percentile = as.numeric(percentile)) %>%
  left_join(model_input$model$itemtable) %>%
  mutate(domain = plyr::revalue(domain, c("cm" = "Communication",
                                          "fm" = "Fine motor",
                                          "gm" = "Gross motor"))) %>%
  left_join({ddomain::itemtableVWO %>% select(item, labelNL, ID.VWO2005)}, by = "item") %>%
  mutate(labelNL = ifelse(item == "ddicmd148", "Begrijpt spelopdrachtjes (M)", labelNL),
         labelNL = ifelse(item == "ddicmd136", "Reageert op mondeling verzoek (M)", labelNL)) %>%
  drop_na(labelNL)


library(openxlsx)

write.xlsx(pref, "data-raw/data/ddi_leeftijd_percentielen.xlsx")





## uitbreiding referentie met count model


load("~/Documents/GitHub/dmetric/data/smoccmodel.rda")
model <- smoccmodel$model

scalefactor <- smoccmodel$model$transform[2]
itembank <- dscore::builtin_itembank %>% filter(key == "dutch")

#expand reference with a count model (based on dmetric/expand_referenced.Rmd)
#44.35 - 1.8 * t + 28.47 * log(t + 0.25)

expanded_reference <- dscore::get_reference(population = "dutch") %>% select(pop, age, mu) %>%
  bind_rows(data.frame(pop = "dutch", age = seq(3.2, 5, 0.04), mu = NA)) %>%
  mutate(
    mu1 = mu,
    mu = ifelse(is.na(mu), 44.35 - 1.8 * age + 28.47 * log(age + 0.25), mu))

ggplot(expanded_reference, aes(x = age, y = mu))+
  geom_line()+
  geom_line(aes(x = age, y = mu1, color = "red"))


calculate_age_equivalents <- function (itembank, scalefactor, p = c(10, 50, 90),
                                       reference,
                                       metric = "dscore")
{
  if (!inherits(model, "dmodel"))
    stop("Argument `model` not of class `dmodel`")
  ib <- itembank #changed
  names(ib)[names(ib) == "lex_gsed"] <- "item"
  #if (metric == "logit")
  #  ib$tau <- model$fit$item$b[ib$item]
  scalefactor <- scalefactor #chaned
  pd <- matrix(ib$tau, nrow = nrow(ib), ncol = length(p)) +
    matrix(scalefactor * qlogis(p/100), nrow = nrow(ib),
           ncol = length(p), byrow = TRUE)
  pa <- approx(x = reference$mu, y = reference$age, xout = as.vector(pd))$y *
    12
  pa <- matrix(pa, ncol = length(p))
  pda <- data.frame(ib[, "item"], round(pd, 2), round(pa,
                                                      2))
  names(pda) <- c("item", paste0("D", p), paste0("A",
                                                 p))
  pda
}


refdata <- calculate_age_equivalents(itembank = itembank, scalefactor = scalefactor, p = c(10, 50, 90), reference = expanded_reference)


pref <- refdata %>%
  #pivot_longer(cols = c(everything(), -item), names_pattern = "(.)(.*)",
  #             names_to = c(".value", "percentiel")) %>%
  mutate(#percentiel = as.numeric(percentiel),
    domain = substr(item, 4,5),
    domein = plyr::revalue(domain, c("cm" = "Communicatie",
                                     "fm" = "Fijne motoriek",
                                     "gm" = "Grove motoriek"))) %>%
  #rename(leeftijd = A) %>%
  left_join(ddomain::itemtableVWO %>% select(item, labelNL, ID.VWO2005), by = "item") %>%
  left_join(model$itemtable %>% select(item, label), by = "item")



