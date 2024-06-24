#expanded_reference
library(dplyr)
library(ggplot2)
#expand reference with a count model (based on dmetric/expand_referenced.Rmd)
#44.35 - 1.8 * t + 28.47 * log(t + 0.25)

 expanded_reference_du <- dscore::get_reference(population = "dutch", key = "dutch") %>%
   select(population, key, age, mu) %>%
   bind_rows(data.frame(population = "dutch", key = "dutch", age = seq(3.2, 7, 0.04), mu = NA),
             data.frame(population = "dutch", key = "dutch", age = 0, mu = NA)) %>%
   mutate(
     mu1 = mu,
     mu = ifelse(is.na(mu), 44.35 - 1.8 * age + 28.47 * log(age + 0.25), mu))

 #estimate extension
ref3p <- dscore::get_reference(population = "dutch", key = "gsed2406") %>% filter(age > 3)
lm(mu ~age + log(age), data = ref3p) %>% summary()

expanded_reference_gsed <- dscore::get_reference(population = "dutch", key = "gsed2406") %>%
  select(population, key, age, mu) %>%
  bind_rows(data.frame(population = "dutch", key = "gsed2406", age = seq(3.6, 7, 0.04), mu = NA),
            data.frame(population = "dutch", key = "gsed2406", age = 0, mu = NA)) %>%
  mutate(
    mu1 = mu,
    #mu = ifelse(is.na(mu), 51.5 - 5.1 * age + 34.7 * log(age + 0.25), mu))
    mu = ifelse(is.na(mu), 50.4 - 5.4 * age + 38.1 * log(age), mu)) %>%
  bind_rows(data.frame(population = "dutch", key = "gsed2406", age = 8, mu = 87, mu1 = NA))

ggplot()+
  geom_line(data = expanded_reference_gsed, aes(x = age, y = mu))+
  geom_line(data = expanded_reference_du, aes(x = age, y = mu), color = "red")+
  ylim(0,100)

ggplot(expanded_reference_gsed, aes(x=age, y = mu))+
  geom_line()+
  geom_line(aes(x = age, y = mu1), color = "red")+
  ylim(0,100)

expanded_reference <-
  bind_rows(expanded_reference_du, expanded_reference_gsed)

usethis::use_data(expanded_reference, overwrite = TRUE)
