library(gghighlight)
library(ggplot2)
library(ragg)
library(showtext)
library(sysfonts)
library(tidyverse)

## Poor data-wrangling
bls <- read.csv("BLS8_Data.csv", sep = ",")
bls[bls == -999] <- NA

bls2 <- bls %>%
  filter(wave == 2017) %>%
  summarise(lrptb) %>%
  drop_na() %>%
  mutate(mean = mean(lrptb))

camara = 
  tribble(
    ~Partidos,~wave, ~placement, 
    "MDB", 1990, 4.889401,
    "PCdoB", 1990, 1.222749,
    "PDT", 1990, 3.119048,
    "DEM", 1990, 7.743119,
    "PR", 1990, 7.238095,
    "PSB", 1990, 2.214286,
    "PSDB", 1990, 3.947115,
    "PT", 1990, 1.504717,
    "PTB", 1990, 6.923445,
    "MDB", 1993, 5.028902,
    "PCdoB", 1993, 1.493827,
    "PDT", 1993, 3.506024,
    "DEM", 1993, 7.506024,
    "PR", 1993, 7.361963,
    "PSB", 1993, 2.345455,
    "PSDB", 1993, 4.369697,
    "PT", 1993, 2.03012,
    "PTB", 1993, 6.751553, 
    "MDB", 1997, 5.47651,
    "PCdoB", 1997, 1.606667,
    "PDT", 1997, 3.256579,
    "DEM", 1997, 8.096774, 
    "PR", 1997, 7.482759,
    "PSB", 1997, 2.806667,
    "PSDB", 1997, 5.862745,
    "PT", 1997, 1.928105,
    "PTB", 1997, 7.28,
    "MDB", 2001, 5.94697, 
    "PCdoB", 2001, 1.692308, 
    "PDT", 2001, 3.434109,
    "DEM", 2001, 8.209302,
    "PR", 2001, 7.039683, 
    "PSB", 2001, 2.861538,
    "PSDB", 2001, 6.068702,
    "PT", 2001, 2.19084,
    "PTB", 2001, 7.102362,
    "MDB", 2005, 5.82906,
    "PCdoB", 2005, 2.700855,
    "PDT", 2005, 4.068376,
    "DEM", 2005, 7.769231,
    "PR", 2005, 6.878261,
    "PSB", 2005, 3.478261,
    "PSDB", 2005, 5.91453, 
    "PT", 2005, 3.62069,
    "PTB", 2005, 6.704348, 
    "MDB", 2009, 5.852713,
    "PCdoB", 2009, 2.678571,
    "PDT", 2009, 4.085366, 
    "DEM", 2009, 7.889764, 
    "PR", 2009, 6.890244, 
    "PSB", 2009, 3.745968, 
    "PSDB", 2009, 5.825397,
    "PT", 2009, 3.600806,
    "PTB", 2009, 6.566116,
    "MDB", 2013, 6.08209,
    "PCdoB", 2013, 2.772727,
    "PDT", 2013, 4.215385,
    "DEM", 2013, 8.074627,  
    "PR", 2013, 6.914729, 
    "PSB", 2013, 4.052239,
    "PSDB", 2013, 6.037879,
    "PT", 2013, 3.730769,
    "PTB", 2013, 6.453846,
    "MDB", 2017, 6.808824,
    "PCdoB", 2017, 2.41791,
    "PDT", 2017, 3.779412,
    "DEM", 2017, 8.311111,
    "PR", 2017, 7.052632,
    "PSB", 2017, 4.632353,
    "PSDB", 2017, 7.147059,
    "PT", 2017, 2.742647,
    "PTB", 2017, 6.083333)

#fonts
font_add(family = "regular", "Nunito-Regular.ttf")
font_add(family="bold", "Roboto-Bold.ttf")
showtext_auto()    

#Colour
parties_parties <- c("DEM" = "#7a9cae",
                     "MDB" ="#347535",
                     "PCdoB" =  "#e47348",
                     "PDT" = "#F153ac",
                     "PR" = "#782192",
                     "PSB" = "#D4cc20",
                     "PSDB" = "#2b14e5",
                     "PT" = "#bc0404",
                     "PTB" = "#060608")

camara$Partidos <- factor(camara$Partidos, levels = c("DEM", "MDB", "PCdoB", "PDT", "PR",
                                                "PSB", "PSDB", "PT", "PTB"))




teste <- camara %>%
  ggplot(aes(wave, placement, colour = Partidos)) +
  geom_line(size = 1.5, alpha = 1) +
  geom_point(size = 3) +
  scale_y_continuous(limits = c(0,10), breaks = c(0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10)) + coord_cartesian(expand=FALSE) +
  scale_x_continuous(limits = c(1988, 2019), 
                     breaks = c(1990, 1993, 1997, 2001, 
                                2005, 2009, 2013, 2017)) +
  scale_color_manual(values = parties_parties) +
  labs(title = "Posicionamento ideológico dos partidos brasileiros 1990-2017",
       subtitle = "Quanto mais próximo de 0, mais à esquerda é o partido",
       x = "",
       y = "Posicionamento ideológico",
       caption = "Dados: Zucco e Power (2020) | @oc_ipolunb") +
  theme_minimal() +
  guides(fill = guide_legend(#Title, Subtitle, Caption
                             title = "Partidos",
                             title.position = "left",
                             title.hjust = 0.5,
                             #Label
                             label.position = "bottom",
                             label.hjust = 0.5,
                             #Minor adjustments
                             nrow = 2,
                             reverse = F,
                             keyheight = 1,
                             keywidth = 5)) +
  theme(
    #Title, Subtitle, Caption
    plot.title = element_text(family="bold", hjust = 0.5),
    plot.subtitle = element_text(family="regular", hjust = 0.5),
    plot.caption = element_text(family="bold", size = 24, margin = margin(20,0,0,0)),
    #Axes
    axis.text = element_text(family="regular"),
    axis.title.y = element_text(family="bold"),
    #Legend
    legend.position = "top",
    legend.text = element_text(family="regular"),
    #Plus
    text = element_text(size = 35))

ggsave("posicionamento.png",
       plot=teste,
       device = agg_png(width = 8, height = 6, units = "in", res = 300))

##PSDB
a <- camara %>%
  ggplot(aes(wave, placement, colour = Partidos)) +
  geom_line(size = 1.5, alpha = 1) +
  geom_point(size = 3) +
  gghighlight(max(placement) > 5 & min(placement) < 4.7, label_key = Partidos, use_direct_label = F) +
  scale_y_continuous(limits = c(0,10), breaks = c(0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10)) + coord_cartesian(expand=FALSE) +
  scale_x_continuous(limits = c(1988, 2019), 
                     breaks = c(1990, 1993, 1997, 2001, 
                                2005, 2009, 2013, 2017)) +
  scale_color_manual(values = "#2b14e5") +
  labs(title = "Posicionamento ideológico dos partidos brasileiros 1990-2017",
       subtitle = "Quanto mais próximo de 0, mais à esquerda é o partido",
       x = "",
       y = "Posicionamento ideológico",
       caption = "Dados: Zucco e Power (2020) | @oc_ipolunb") +
  theme_minimal() +
  guides(fill = guide_legend(#Title, Subtitle, Caption
                              title = "Partidos",
                              title.position = "left",
                              title.hjust = 0.5,
                              #Label
                              label.position = "bottom",
                              label.hjust = 0.5,
                              #Minor adjustments
                              nrow = 2,
                              reverse = F,
                              keyheight = 1,
                              keywidth = 5)) +
  theme(
    #Title, Subtitle, Caption
    plot.title = element_text(family="bold", hjust = 0.5),
    plot.subtitle = element_text(family="regular", hjust = 0.5),
    plot.caption = element_text(family="bold", size = 24, margin = margin(20,0,0,0)),
    #Axes
    axis.text = element_text(family="regular"),
    axis.title.y = element_text(family="bold"),
    #Legend
    legend.position = "top",
    legend.text = element_text(family="regular"),
    #Plus
    text = element_text(size = 35))



ggsave("a.png",
       plot=a,
       device = agg_png(width = 8, height = 6, units = "in", res = 300))


## PSDB e PMDB
b <- camara %>%
  ggplot(aes(wave, placement, colour = Partidos)) +
  geom_line(size = 1.5, alpha = 1) +
  geom_point(size = 3) +
  gghighlight(max(placement) > 5 & min(placement) < 5, label_key = Partidos, use_direct_label = F) +
  scale_y_continuous(limits = c(0,10), breaks = c(0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10)) + coord_cartesian(expand=FALSE) +
  scale_x_continuous(limits = c(1988, 2019), 
                     breaks = c(1990, 1993, 1997, 2001, 
                                2005, 2009, 2013, 2017)) +
  scale_color_manual(values = c("#347535", "#2b14e5")) +
  labs(title = "Posicionamento ideol?gico dos partidos brasileiros 1990-2017",
       subtitle = "Quanto mais pr?ximo de 0, mais ? esquerda ? o partido",
       x = "",
       y = "Posicionamento ideol?gico",
       caption = "Dados: Zucco e Power (2020) | @oc_ipolunb") +
  theme_minimal() +
  guides(fill = guide_legend(#Title, Subtitle, Caption
                              title = "Partidos",
                              title.position = "left",
                              title.hjust = 0.5,
                              #Label
                              label.position = "bottom",
                              label.hjust = 0.5,
                              #Minor adjustments
                              nrow = 2,
                              reverse = F,
                              keyheight = 1,
                              keywidth = 5)) +
  theme(
    #Title, Subtitle, Caption
    plot.title = element_text(family="bold", hjust = 0.5),
    plot.subtitle = element_text(family="regular", hjust = 0.5),
    plot.caption = element_text(family="bold", size = 24, margin = margin(20,0,0,0)),
    #Axes
    axis.text = element_text(family="regular"),
    axis.title.y = element_text(family="bold"),
    #Legend
    legend.position = "top",
    legend.text = element_text(family="regular"),
    #Plus
    text = element_text(size = 35))
ggsave("b.png",
       plot=b,
       device = agg_png(width = 8, height = 6, units = "in", res = 300))
  