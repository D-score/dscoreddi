df1 <- data.frame(id = c("David", "Rob"),
                  age = c(15/12, 15/12),
                  ddifmd011 = c(1,1),
                  ddifmm012 = c(1,0),
                  ddicmm037 = c(1,1),
                  ddigmm066 = c(1,1),
                  ddigmm067 = c(NA,0))

library(dscore)
dscore::dscore(data = df1, items = c("ddifmd011", "ddifmm012", "ddicmm037", "ddigmm066", "ddigmm067"),
                key = "dutch")
dscore::dscore_posterior(data = df1, items = c("ddifmd011", "ddifmm012", "ddicmm037", "ddigmm066", "ddigmm067"),
                 key = "dutch",
                 metric = "dscore")

dscore:::calc_dscore
dscore:::count_mu_dutch(12/15)
dscore:::count_mu_dutch


library(ggplot)

## plot to prepare for prior





#scalefactor <- smoccmodel$model$transform[2]
itembank <- dscore::builtin_itembank %>% filter(key == "gsed2212")

#expand reference with a count model (based on dmetric/expand_referenced.Rmd)
#44.35 - 1.8 * t + 28.47 * log(t + 0.25)

ref <- dscore::get_reference(population = "phase1") %>%
  pivot_longer(cols = starts_with("SD"), names_to = "SDline", values_to = "dscore")

ggplot(ref, aes(x = age*12, y = dscore, group = SDline))+
  geom_line()+
ylim(0,100)+
 # geom_vline(xintercept= 1 )+
#  geom_vline(xintercept = 15)+
#  geom_vline(xintercept = 24)+
  ylab("D-score")+
  xlab("Age (months)")
