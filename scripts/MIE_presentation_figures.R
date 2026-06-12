## MIE presentation

devtools::load_all("~/OneDrive - TNO/Documents/GitHub/dscoreddi")
library(ggplot2)
library(dplyr)
library(dscore)
library(openxlsx)
library(tidyr)
theme_set(theme_light())
## `calculate_age_equivalents`


calculate_age_equivalents <-
  function (model = NULL, itembank = NULL, scalefactor = NULL,
            p = c(10, 50, 90), reference = dscore::get_reference(), metric = "dscore")
  {
    if (!is.null(model)) {
      if (!inherits(model, "dmodel"))
        stop("Argument `model` not of class `dmodel`")
      ib <- model$itembank
      scalefactor <- model$transform[2]
      if (metric == "logit")
        ib$tau <- model$fit$b[ib$item]
    }
    if (is.null(model))
      ib <- itembank
    names(ib)[names(ib) == "lex_gsed"] <- "item"
    pd <- matrix(ib$tau, nrow = nrow(ib), ncol = length(p)) +
      matrix(scalefactor * qlogis(p/100), nrow = nrow(ib),
             ncol = length(p), byrow = TRUE)
    pa <- approx(x = reference$mu, y = reference$age, xout = as.vector(pd))$y *
      12
    pa <- matrix(pa, ncol = length(p))
    pda <- data.frame(ib[, "item"], round(pd, 2), round(pa, 2))
    names(pda) <- c("item", paste0("D", p), paste0("A", p))
    pda
  }



reference1 <- dscore::builtin_references %>% filter(population == "dutch" & key == "dutch") %>%
  pivot_longer(P3:P97, names_to = "percentile", values_to = "d")

ggplot(reference1, aes(age * 12, d, group = percentile))+
  geom_line(color = "skyblue", lwd = 1)+
  ggtitle("Reference for D-score")+
  ylab("D-score")+
  scale_x_continuous("Age in months", breaks = seq(0,60, 3))


### 1. Bepaal de D-score voor de P10/P50/P90
#stack 3 blocks
#input: scalefactor, itembank tau, percentiles
scalefactor <- 2.099986
example <- itembank_vwc %>% filter(item == "ddifmd015" & key ==  "dutch") %>% dplyr::select(c("item", "tau", "label"))
p1 <- 1:100

#input reference
reference <- dscoreddi::expanded_reference %>% filter(population == "dutch" & key == "dutch") %>% mutate(agemos = age * 12)

#compute logistic function and transform to d-score scale
lg <- qlogis(p1/100)
lgd <- scalefactor * lg
dif <- example$tau + lgd
df <- data.frame(p = p1, d = dif)

#find D-score for percentile of interest
f1 <- ggplot(df, aes(d, p))+
  geom_line() +
  ggtitle("D-score by pass probability for `Makes tower of three cubes`")+
  scale_y_continuous(breaks = seq(0,100,10), limits = c(0,100), name = "Pass probability")+
  xlab("D-score")+
  geom_segment(y = 10, x = 0, xend = df[df$p==10, "d"], yend = 10, lty = 2) +
  geom_segment(y = -10, x = df[df$p==10, "d"], xend = df[df$p==10, "d"], yend = 10, lty = 2)+
  geom_segment(y = 50, x = 0, xend = df[df$p==50, "d"], yend = 50, lty = 2) +
  geom_segment(y = -10, x = df[df$p==50, "d"], xend = df[df$p==50, "d"], yend = 50, lty = 2)+
  geom_segment(y = 90, x = 0, xend = df[df$p==90, "d"], yend = 90, lty = 2) +
  geom_segment(y = -10, x = df[df$p==90, "d"], xend = df[df$p==90, "d"], yend = 90, lty = 2) +
  geom_point(x = df[df$p==10, "d"], y = -2.6, pch = 25, fill = "red", size = 3) +
  geom_point(x = df[df$p==50, "d"], y = -2.6, pch = 25, fill = "red", size = 3) +
  geom_point(x = df[df$p==90, "d"], y = -2.6, pch = 25, fill = "red", size = 3)

f1

jpeg(filename = "~/OneDrive - TNO/Documents/GitHub/dscoreddi/results/MIE_fig1.jpg",width = 1400, height = 1200, res = 300)
f1

dev.off()


### 2. Bepaal de leeftijd bij gemiddelde D-score

#use reference to find the age that corresponds with the D-score
pa <- approx(x = reference$mu, y = reference$agemos, xout = as.vector(df$d))$y *
  12
df <- data.frame(p = p1, d = dif, a = pa)

f2 <- ggplot(reference, aes(agemos, mu))+
  geom_line()+
  ggtitle("Age at average D-score")+
  ylab("D-score")+
  scale_x_continuous("Age in months", breaks = seq(0,60, 3), limits = c(0, 60))+
  geom_segment(y = df[df$p==10, "d"],
               x = -10,
               xend = approx(x = reference$mu, y = reference$agemos, xout = df[df$p==10, "d"])$y,
               yend = df[df$p==10, "d"], lty = 2)+
  geom_segment(y = -10,
               x = approx(x = reference$mu, y = reference$agemos, xout = df[df$p==10, "d"])$y,
               xend = approx(x = reference$mu, y = reference$agemos, xout = df[df$p==10, "d"])$y,
               yend = df[df$p==10, "d"], lty = 2)+

  geom_segment(y = df[df$p==50, "d"],
               x = -10,
               xend = approx(x = reference$mu, y = reference$agemos, xout = df[df$p==50, "d"])$y,
               yend = df[df$p==50, "d"], lty = 2)+
  geom_segment(y = -10,
               x = approx(x = reference$mu, y = reference$agemos, xout = df[df$p==50, "d"])$y,
               xend = approx(x = reference$mu, y = reference$agemos, xout = df[df$p==50, "d"])$y,
               yend = df[df$p==50, "d"], lty = 2)+

  geom_segment(y = df[df$p==90, "d"],
               x = -10,
               xend = approx(x = reference$mu, y = reference$agemos, xout = df[df$p==90, "d"])$y,
               yend = df[df$p==90, "d"], lty = 2)+
  geom_segment(y = -10,
               x = approx(x = reference$mu, y = reference$agemos, xout = df[df$p==90, "d"])$y,
               xend = approx(x = reference$mu, y = reference$agemos, xout = df[df$p==90, "d"])$y,
               yend = df[df$p==90, "d"], lty = 2)+
  geom_point(x = approx(x = reference$mu, y = reference$agemos, xout = df[df$p==10, "d"])$y, y = 3, pch = 25, fill = "red", size = 3) +
  geom_point(x = approx(x = reference$mu, y = reference$agemos, xout = df[df$p==50, "d"])$y, y = 3, pch = 25, fill = "red", size = 3) +
  geom_point(x = approx(x = reference$mu, y = reference$agemos, xout = df[df$p==90, "d"])$y, y = 3, pch = 25, fill = "red", size = 3)


f2

jpeg(filename = "~/OneDrive - TNO/Documents/GitHub/dscoreddi/results/MIE_fig2.jpg",width = 1400, height = 1200, res = 300)
f2

dev.off()



### 3. Van Wiechen continue plaatje
vwcdf <- dmetric::calculate_age_equivalents(itembank = itembank_vwc %>% filter(key == "dutch" & item == "ddifmd015"),scalefactor = 2.099986, p = c(2, 10, 50, 90, 98), reference = reference1)



ggplot(vwcdf)+ #selecteer milestons met A10 < 15 maanden
  geom_point(aes(x = item,  y= A50))+
  geom_errorbar(aes(x =item ,ymin = A10, ymax = A90), lwd = 1, width = 0.1 )+
  coord_flip()+ xlab("") + ylab("")+
  theme(legend.position = "none", axis.text.y = element_blank() ) +
  scale_y_continuous(name = "", breaks = c(0,1,2,3,4,5,6,7,8,9,10,12,15, seq(15,51,3)), minor_breaks = c(0.5,1.5,2.5,3.5,4.5,5.5,6.5,7.5,8.5,9.5,10.5,11,13,14,16,17,19,20,22,23,25,26,28,29,31,32,34,35,37,38,40,41,43,44,46,47,49,50), limits= c(12,36),position = "right", sec.axis= dup_axis(name = "Age (months)"), expand = c(0,0.1))+
  ggtitle("Age percentiles for `Makes tower of three cubes`")




ib <- itembank_vwc
p <- c(10, 50, 90)
scalefactor <- 2.099986

#matrix met difficulties van de items in elke kolom (kolom voor elk percentiel dat je wilt berekenen)
matrix(ib$tau, nrow = nrow(ib), ncol = length(p))

#transform de percentielen (kansen) naar logits met log(p/(1-p)) < qlogis(p) en zet de logits om naar d-scores met de scalefactor.
qlogis(p/100)
scalefactor * qlogis(p/100)

#tel deze logit factor voor het percentiel op bij de tau van het item. De tau van het item is de p50, de logit factor in d-score units geeft het verschil naar de andere percentielen.
matrix(ib$tau, nrow = nrow(ib), ncol = length(p)) +
  matrix(scalefactor * qlogis(p/100), nrow = nrow(ib),
         ncol = length(p), byrow = TRUE)


#de d-score in de p10 kolom geeft dus de d-score waarbij 10% van de kinderen dit item kan.




## Percentielen


dscoreddi::smoccmodel$model$transform[2]
refdataNL <- dmetric::calculate_age_equivalents(itembank = itembank_vwc |> filter(key == "dutch"),
                                                scalefactor = 2.099986,
                                                p = c(2, 10, 50, 90, 98),
                                                reference = expanded_reference |> filter(population == "dutch" & key == "dutch")) |>
  mutate(key = "dutch")
#load("data/itemtableVWO.rda")

refdat <- refdataNL |>
  mutate(
    domain = substr(item, 4,5),
    domein = dplyr::recode(domain, "cm" = "Communicatie",
                           "fm" = "Fijne motoriek",
                           "gm" = "Grove motoriek")) |>
  #rename(leeftijd = A) |>
  left_join(itemtableVWO |> select(item, labelEN, ID.VWO2005, month), by = "item") |>
  mutate(nr = as.numeric(substr(item, 7, 9)),
         # nr = ifelse(nr == 136, 35, nr), #dubbel met nr 035? -- 035 zit neit in model; 136 wel - wijzig op voorhand
         # nr = ifelse(nr == 148, 40, nr), #dubbel met nr 040? -- 040 zit niet in model; 148 wel - wijzig op voorhand
         nr = ifelse(nr == 068, 68.1, nr),
         nr = ifelse(nr == 168, 68.2, nr),
         nr = ifelse(nr == 268, 68.3, nr),
         domein = ifelse(nr == 6, "Fijne motoriek", domein),
         labelENnr = paste(nr, labelEN, sep = ". "),
         labelENnr = gsub("\\(M; can ask parents\\)", "", labelENnr),

#         labelENnr = ifelse(nr %in% c(28,51,75),paste(nr,"\u1D43",". " ,labelEN, sep = "") ,paste(nr, labelEN, sep = ". ")),
 #        labelENnr = ifelse(nr %in% c(32,38),paste(nr,"\u1D9C",". " ,labelEN, sep = "") ,labelENnr),
  #       labelENnr = ifelse(nr %in% c(17, 18, 19, 23, 24, 25, 27, 43, 44, 47, 48, 71, 72,74, 68.1, 68.2, 68.3),paste(nr,"\u1D47",". " ,labelEN, sep = "") ,labelENnr),
         #labelENn = ifelse(nr %in% 52:55, paste(nr, labelEN, sep = ".* "), labelENn),
        # labelENn = ifelse(month < 10, paste(labelENnr, month, sep = " |  "),
         #                  paste(labelENnr, month, sep = " |") ),
         #labelENn = ifelse(nr %in% 52:55, paste(labelENnr, " --", sep = " | "), labelENn),
         labelENnr = forcats::fct_reorder(labelENnr, -nr),
         sideA = ifelse(nr %in% c(1:12, 29:38, 52:67), 1, 0),
         sideB = ifelse(nr %in% c(11:28, 37:51, 66:75, 68.1, 68.2, 68.3), 1, 0),
         A2 = ifelse(is.na(A2), 0, A2),
         A10 = ifelse(is.na(A10), 0, A10),
         A50 = ifelse(is.na(A50), 0, A50),http://127.0.0.1:25741/graphics/01de1756-dea8-4727-9bea-37b22afb9c77.png
         A90 = ifelse(is.na(A90), 0, A90),
         A98 = ifelse(is.na(A98), 0, A98)

  ) |>
  arrange(A50) |>
  filter(sideA == 1 | sideB == 1)

refdatex <- refdat |> filter(nr %in% 13:20)

ggplot(data = refdatex , aes(y = reorder(labelENnr, -A98), x = A50))+
  geom_point(size = 1)+
  geom_errorbar(aes(xmin = A10, xmax = A90), size = 1) +
  geom_errorbar(aes(xmin = A2, xmax = A98), size = 0.5, lty = 2) +
  xlab("Age (months)")+
  ylab("")+
  #facet_grid(domein ~. ,space = "free", switch = "y", scales = "free")+
  theme(legend.position = "none", text = element_text(size = 12), axis.text.y = element_text(size = 14) )+
 # theme(legend.position = "none", axis.text.y = element_blank() ) +
  scale_x_continuous(name = "", breaks = c(0,1,2,3,4,5,6,7,8,9,10,12,15, seq(15,51,3)), minor_breaks = c(0.5,1.5,2.5,3.5,4.5,5.5,6.5,7.5,8.5,9.5,10.5,11,13,14,16,17,19,20,22,23,25,26,28,29,31,32,34,35,37,38,40,41,43,44,46,47,49,50), limits= c(0,48),position = "top", sec.axis= dup_axis(name = "Age (months)"), expand = c(0,0.1))


