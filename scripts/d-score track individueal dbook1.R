#d-score trajectories dbook1
library(dplyr)
library(ggplot2)

model <- dscoreddi::smoccmodel$model

# set.seed(81104)
set.seed(3)
kids <- sample(unique(model$dscore$subjid), 5)
data <- model$dscore %>%
  filter(subjid %in% kids) %>%
  mutate(subjid = as.factor(subjid),
         agemos = agedays / 365.25 * 12)

reference1 <- dplyr::select(dscore::get_reference(population = "dutch"), age, SDM2:SDP2) %>%
  mutate(month = age * 12) %>%
  filter(month <= 30)

reference <-
  reference1%>%
  tidyr::gather(key = centile, value = d, -month, -age)
polygon <- data.frame(
  x = c(reference1$month,rev(reference1$month)),
  y = c(reference1$SDM2, rev(reference1$SDP2)))

g10 <- ggplot(data, aes_string(x = "agemos", y = "d", colour = "subjid")) +
  scale_x_continuous("Age (in months)",
                     limits = c(0, 30),
                     breaks = seq(0, 30, 6)) +
  scale_y_continuous("D-score",
                     limits = c(0, 75),
                     breaks = seq(0, 70, 10)) +
 # geom_polygon(data = polygon, aes(x = x, y = y),
#               col = "transparent", fill = "grey", alpha = 0.1) +
  geom_line(mapping = aes(x = month, y = d, group = centile),
            data = reflong, colour = "grey", alpha = 0.7) +
  geom_line(data = data %>% filter(subjid == 5300999), size = 1.5, color = "#619CFF")+
  geom_point(data = data %>% filter(subjid == 5300999), size = 2.5, color = "#619CFF")+
  geom_point(size = 2) +
  geom_line(size = 1) +

  theme(legend.position = "none", text = element_text(size = 16))
g10

plotg <- function(dat){
 ggplot(dat, aes_string(x = "agemos", y = "d", colour = "subjid")) +
  scale_x_continuous("Age (in months)",
                     limits = c(0, 30),
                     breaks = seq(0, 30, 6)) +
  scale_y_continuous("D-score",
                     limits = c(0, 75),
                     breaks = seq(0, 70, 10)) +
 # geom_polygon(aes(x = x, y = y),
#               data = polygon, col = "transparent", fill = "grey", alpha = 0.1) +
  geom_line(mapping = aes(x = month, y = d, group = centile),
            data = reference, colour = "grey", alpha = 0.7) +
  geom_point(size = 2.5) +
  geom_line(size = 1.5) +
  scale_color_manual(values = c("5300999" = "#619CFF"))+
  theme(legend.position = "none", text = element_text(size = 16))
}


g1 <- plotg(data %>% filter(subjid == 5300999)%>% slice(1))
g2 <- plotg(data %>% filter(subjid == 5300999)%>% slice(1:2))
g3 <- plotg(data %>% filter(subjid == 5300999)%>% slice(1:3))
g4 <- plotg(data %>% filter(subjid == 5300999)%>% slice(1:4))
g5 <- plotg(data %>% filter(subjid == 5300999)%>% slice(1:5))
g6 <- plotg(data %>% filter(subjid == 5300999)%>% slice(1:6))
g7 <- plotg(data %>% filter(subjid == 5300999)%>% slice(1:7))
g8 <- plotg(data %>% filter(subjid == 5300999)%>% slice(1:8))
g9 <- plotg(data %>% filter(subjid == 5300999)%>% slice(1:9))

g1
g2
g3
g4
g5
g6
g7
g8
g9
g10
