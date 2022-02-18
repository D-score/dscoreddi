# item curves voor van Wiechen items

load("~/Documents/GitHub/dmetric/data/smoccmodel.rda")

library(dmetric)
library(dplyr)
library(tidyr)
library(dscore)

gcdg_lean

plot_p_d_item(data = gcdg_lean , model = smoccmodel)


model <- smoccmodel$model
data <- gcdg_lean

modelb <- model$dscore
delta <- dscore::get_tau(model$items, key = "", itembank = model$itembank)
data2 <- data$itm %>%
  filter(item %in% model$items) %>%
  left_join(data$visit, by = c("subjid", "agedays")) %>%
  left_join(modelb, by = c("subjid", "agedays")) %>%
  dplyr::select(subjid, agedays, item, value, d)


#plot pass for d
pass <- data2 %>%
  tidyr::drop_na("value", "d") %>%
  mutate(dgp = cut(.data$d, breaks = seq(0, 70, 1)),
         agemos = round(.data$agedays / 365.25 * 12, 3)) %>%
  tidyr::drop_na(dgp) %>%
  group_by(dgp, item) %>%
  summarise(
    p = round(100 * mean(.data$value, na.rm = TRUE)),
    a = mean(.data$agemos, na.rm = TRUE),
    d = mean(.data$d, na.rm = TRUE),
    n = n(),
    .groups = "drop")

pass <- pass %>%
  left_join(model$itemtable, by = "item") %>%
  arrange(.data$item, dgp) %>%
  dplyr::select(item, dgp, a, d, p, n, domain, label) %>%
  filter(n >= 10 & p > 0 & p < 100) %>%
  mutate(domain = plyr::revalue(domain, c("cm" = "Communication",
                                          "fm" = "Fine motor",
                                          "gm" = "Gross motor")))

plot <- ggplot(pass, aes(d, p, group = item, colour = domain,
                         label = label)) +
  scale_x_continuous(paste0("D-score (", model$name,")"),
                     limits = c(0, 70),
                     breaks = seq(0, 70, 10)) +
  scale_y_continuous("% pass", breaks = seq(0, 100, 20),
                     limits = c(0, 100)) +
  geom_line(size = 0.3) +
  geom_point(size = 1.0) +
  theme(legend.position = "none")

plot


#plot pass for age
pass <- data2 %>%
  tidyr::drop_na("value", "d") %>%
  mutate(dgp = cut(.data$d, breaks = seq(0, 70, 1)),
         agemos = round(.data$agedays / 365.25 * 12, 3),
         agem = cut(.data$agemos, breaks = seq(0, 50, 1))) %>%
  tidyr::drop_na(agem) %>%
  group_by(agem, item) %>%
  summarise(
    p = round(100 * mean(.data$value, na.rm = TRUE)),
    a = mean(.data$agemos, na.rm = TRUE),
    d = mean(.data$d, na.rm = TRUE),
    n = n(),
    .groups = "drop")

pass <- pass %>%
  left_join(model$itemtable, by = "item") %>%
  arrange(.data$item, agem) %>%
  dplyr::select(item, agem, a, d, p, n, domain, label) %>%
  filter(n >= 10 & p > 0 & p < 100) %>%
  mutate(domain = plyr::revalue(domain, c("cm" = "Communication",
                                          "fm" = "Fine motor",
                                          "gm" = "Gross motor")))

plot <- ggplot(pass, aes(a, p, group = item, colour = domain,
                         label = label)) +
  scale_x_continuous("Age in months",
                     limits = c(0, 50),
                     breaks = seq(0, 50, 10)) +
  scale_y_continuous("% pass", breaks = seq(0, 100, 20),
                     limits = c(0, 100)) +
  geom_line(size = 0.3) +
  geom_point(size = 1.0) +
  theme(legend.position = "none")

plot




refdata <- dmetric::calculate_age_equivalents(model = model, p = seq(1,99, 1), reference = dscore::get_reference(population = "dutch"))


pref <- refdata %>%
  pivot_longer(cols = c(everything(), -item), names_pattern = "(.)(.*)",
               names_to = c(".value", "percentile")) %>%
  mutate(percentile = as.numeric(percentile)) %>%
  left_join(model$itemtable) %>%
  mutate(domain = plyr::revalue(domain, c("cm" = "Communication",
                                          "fm" = "Fine motor",
                                          "gm" = "Gross motor")))


p1 <- ggplot(pref, aes(x = A, y = percentile, group = item, color = domain))+
  geom_line() +
  xlab("Age (months)") +
  ylab("Probability to pass")

ggplotly(p1)






plot_list <- vector("list", length(unique(pref$item)))
names(plot_list) <- unique(pref$item)

for(i in names(plot_list)){

  plotdata <- pref %>% filter(item == i)
  the_label  <- plotdata$label[1]

 plot_list[[i]] <-
  ggplot(plotdata, aes(x = A, y = percentile))+
    geom_line() +
    xlab("Age (months)") +
    ylab("Probability to pass")+
    annotate("text", x = 1, y = 2, hjust = 0, label = the_label)+
    xlim(0,36) + ylim(0,100) +
   theme_light()

}

pdf(file = "p_for_age_ddi.pdf",width = 10, height = 5)

plot_list

dev.off()


#labels staan nog in het Engels - moeten naar het Nederlands.
