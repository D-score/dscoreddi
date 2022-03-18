

## global for van wiechen continue
library(dscoreddi)
library(ggplot2)
library(dplyr)
library(tidyr)
library(dscore)
library(dmetric)
library(gridExtra)
library(cowplot)
library(openxlsx)

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


theme_set(theme_light())
calculate_age_equivalents <- function (itembank, scalefactor, p = c(10, 50, 90),
                                       reference,
                                       metric = "dscore")
{

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



#child data for demo
references <- dscore::builtin_references %>%
  filter(pop == "dutch") %>%
  mutate(month = age * 12) %>%
  select(month, SDM2:SDP2) %>%
  filter(month <= 60) %>%
  pivot_longer(names_to = "centile", values_to = "d", cols = -month)

childdat <- data.frame(month = c(1,2, 3, 6),
                       d = c(12,16,24,35))
