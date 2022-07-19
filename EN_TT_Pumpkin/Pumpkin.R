# Load packages
library(ggtext)
library(janitor)
library(tidytuesdayR)
library(tidyverse)
library(tidytext)
library(showtext)
library(ragg)


# Load data
tuesdata <- tidytuesdayR::tt_load(2021, week = 43)
pumpkins <- tuesdata$pumpkins


pumpkins1 <- pumpkins %>%
  separate(id, into = c("year", "type")) %>%
  mutate(type = fct_recode(type,
                           "Field Pumpkin" = "F",
                           "Giant Pumpkin" = "P",
                           "Giant Squash" = "S",
                           "Giant Watermelon" = "W",
                           "Long Gourd" = "L",
                           "Tomato" = "T"),
         weight_lbs = parse_number(weight_lbs)) %>%
  filter(country == "Finland") %>%
  group_by(year, type) %>%
  count() %>%
  arrange(type, year)

pumpkins1$year <- as.numeric(pumpkins1$year)

##to exclude
renegade = tribble(
  ~year, ~type, ~n,
  2015, "Giant Watermelon", 0,
  2016, "Giant Watermelon", 0,
  2017, "Giant Watermelon", 0,
  2018, "Giant Watermelon", 0
)

pumpkins1 <- rbind(pumpkins1, renegade)


pumpkins2 <- pumpkins1 %>% group_by(type) %>% summarise(cumulative = sum(n))

pumpkins1 <- pumpkins1 %>%
  mutate(type = factor(type, levels=c("Giant Pumpkin", "Field Pumpkin", "Tomato",
                                      "Long Gourd", "Giant Squash", "Giant Watermelon")))
                       
##Adding Fonts
font_add(family = "regular", "Nunito-Regular.ttf")
font_add(family= "bold", "ScheherazadeNew-Bold.ttf")
showtext_auto()    



## Theme 
trialtheme <- theme(
  #titles
  plot.title=element_markdown(family="bold", hjust=0, vjust = 1, size=45, color="black"),
  plot.title.position = "plot",
  plot.subtitle = element_markdown(family="bold", size=40, hjust=0, color="black"),
  plot.caption= element_text(family="bold", size=35, color="black", hjust=1),
  plot.caption.position = "plot",
  #background
  panel.border=element_blank(),
  panel.grid.major = element_line(colour = "grey"),
  panel.grid.minor = element_line(colour = "grey"),
  panel.background = element_rect(fill = "#ffffff"),
  plot.background = element_rect(fill = "#ffffff"),
  plot.margin=ggplot2::margin(0.5, 0.5, 0.5, 0.5, "in"),
  #axes
  axis.ticks.length=unit(0.15, "cm"),
  axis.ticks = element_blank(),
  axis.line = element_blank(),
  axis.title = element_text(size=40, family="bold", color="black"),
  axis.text.y = element_text(size=30, family="regular", color="black"),
  axis.text.x = element_text(size=30, family="regular", color="black"),
  #multi-plot 
  strip.text = element_text(family="regular", size=35, color="white"),
  strip.background = element_rect(fill= "#171f43"),
  panel.spacing = unit(2, "lines"),
  #no legend
  legend.position = "none")


my_plot <- pumpkins1 %>% ggplot(aes(year, n, group = type)) +
              geom_line(size = 1.5, color = "#043484") +
              coord_cartesian(expand=FALSE) +
              scale_x_continuous(breaks=scales::pretty_breaks()) +
              scale_y_continuous(breaks=scales::pretty_breaks()) +
              facet_wrap( ~ type) +
              xlab("") + ylab("Counts of Finnish entries in the GPC") +
              labs(title = "Did you know that some <span style='color:#d55d2a'>GPC participants</span> hail from <span style='color:#004fc6'>Finland</span>?",
                   subtitle = "<span style='color:#004fc6'>The Finns</span> have thrived especially in the planting of Giant and Field Pumpkins and Tomato when it comes to the <span style='color:#d55d2a'>GPC</span>.",
                   caption="Lucas Couto | #TidyTuesday | Twitter: @lucas_coutoz | Data: BigPumpkins.com")  +
              trialtheme

ggsave("finland.png",
       plot=my_plot,
       device = agg_png(width = 10, height = 8, units = "in", res = 300))