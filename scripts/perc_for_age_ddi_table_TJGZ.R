#van wiechen table contactmoment ncj

vwtab <- read.xlsx("data-raw/data/vanwiechen_percentage_cm.xlsx")

colnames(vwtab) <- c("nr", "NLlabel", "leeftijdwkn", "wk1", "wk2", "n/N", "%", "SMOCK", "SW", "Perc_D")

vwtab <-
vwtab %>%
  mutate(mnd1 = wk1 * 7 / 30,
         mnd1 = ifelse(is.na(mnd1), as.numeric(leeftijdwkn), mnd1),
         mnd2 = wk2 * 7 / 30,
         mnd2 = ifelse(is.na(mnd2), as.numeric(leeftijdwkn), mnd2),
         mnd1 = round(mnd1,1),
         mnd2 = round(mnd2,1),
         DH = `%`

  ) %>%
  select(-leeftijdwkn)





# Kans op behalen van mijlpalen

load("~/Documents/GitHub/dmetric/data/smoccmodel.rda")
model <- smoccmodel$model

scalefactor <- smoccmodel$model$transform[2]
itembank <- dscore::builtin_itembank %>% filter(key == "dutch")

#expand reference with a count model (based on dmetric/expand_referenced.Rmd)
#44.35 - 1.8 * t + 28.47 * log(t + 0.25)

expanded_reference <- dscore::get_reference(population = "dutch") %>% select(pop, age, mu) %>%
  bind_rows(data.frame(pop = "dutch", age = seq(3.2, 5, 0.04), mu = NA),
            data.frame(pop = "dutch", age = 0, mu = NA)) %>%
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

load("data/itemtableVWO.rda")

pref <- refdata %>%
  #pivot_longer(cols = c(everything(), -item), names_pattern = "(.)(.*)",
  #             names_to = c(".value", "percentiel")) %>%
  mutate(#percentiel = as.numeric(percentiel),
    domain = substr(item, 4,5),
    domein = plyr::revalue(domain, c("cm" = "Communicatie",
                                     "fm" = "Fijne motoriek",
                                     "gm" = "Grove motoriek"))) %>%
  #rename(leeftijd = A) %>%
  left_join(itemtableVWO %>% select(item, labelNL, ID.VWO2005, month), by = "item") %>%
  mutate(nr = as.numeric(substr(item, 7, 9)),
         nr = ifelse(nr == 136, 35, nr),
         nr = ifelse(nr == 148, 40, nr),
         nr = ifelse(nr == 168, 68.1, nr),
         nr = ifelse(nr == 268, 68.2, nr),
         domein = ifelse(nr == 6, "Fijne motoriek", domein),

         labelNLn = paste(nr, labelNL, sep = ". "),
         labelNLn = ifelse(nr %in% 52:55, paste(nr, labelNL, sep = ".* "), labelNLn),
         labelNLn = ifelse(month < 10, paste(labelNLn, month, sep = " |  "), paste(labelNLn, month, sep = " |") ),
         sideA = ifelse(nr %in% c(1:12, 29:38, 52:67), 1, 0),
         sideB = ifelse(nr %in% c(11:28, 37:51, 66:75, 68.1, 68.2), 1, 0),
         A10 = ifelse(is.na(A10), 0, A10),
         A50 = ifelse(is.na(A50), 0, A50),
         A90 = ifelse(is.na(A90), 0, A90)

  ) %>%
  arrange(nr)


vwtab_per <-
vwtab %>%
  left_join(pref, by = "nr") %>%
  select(nr, labelNLn, NLlabel, A10, A50, A90, mnd1, mnd2, DH, SMOCK, SW) %>%
  mutate(conflict1 = ifelse(A90 < mnd1 & DH < 90, 1, 0),
         conflict2 = ifelse(A90 > mnd2 & DH > 90, 1, 0)
         )

##hieraan toevoegen
#ifelse leeftijd A90 > mnd1 & DH > 90
#ifelse  leeftijd A90 < mnd1 & DH < 90

write.xlsx(vwtab_per, file = "results/Tabel_vwo_percleeftijd_TJGZtab.xlsx")
