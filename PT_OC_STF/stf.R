library(tidyverse)
library(ggplot2)
library(ggtext)
library(showtext)
library(ragg)
library(ggalt)


## My Version 
STF = 
  tribble(
    ~name, ~indicacao, ~ano_indicacao, ~ano_aposentadoria, 
    "Gilmar Mendes", "FHC", 2002, 2030, 
    "Ricardo Lewandowski", "Lula", 2006, 2023,
    "Cármen Lúcia", "Lula", 2006, 2029,
    "Dias Toffoli", "Lula", 2009, 2042,
    "Luiz Fux", "Rousseff", 2011, 2028,
    "Rosa Weber", "Rousseff", 2011, 2023,
    "L. R. Barroso", "Rousseff", 2013, 2033,
    "Edson Fachin", "Rousseff", 2015, 2033,
    "A. de Moraes", "Temer", 2017, 2043,
    "Nunes Marques", "Bolsonaro", 2020, 2047)

##fonts 
font_add(family="regular", "NotoSansJP-Regular.otf")
font_add(family="bold", "Roboto-Bold.ttf")
showtext_auto()
    
trialtheme <- theme(
  #titles
  plot.title=element_text(family="bold", hjust=0, vjust = 1, size=45, color="black"),
  plot.title.position = "plot",
  plot.subtitle = element_markdown(family="bold", size=30, hjust=0, color="black"),
  plot.caption=element_text(family="bold", size=20, color="black", hjust=1),
  plot.caption.position = "plot",
  #background
  panel.border=element_blank(),
  panel.grid.major = element_blank(),
  panel.grid.minor = element_blank(),
  panel.background = element_rect(fill = "#Bfd6df"),
  plot.background = element_rect(fill = "#Bfd6df"),
  plot.margin=ggplot2::margin(0.5, 0.5, 0.5, 0.5, "in"),
  #axes
  axis.ticks.length=unit(0.15, "cm"),
  axis.ticks = element_blank(),
  axis.line = element_blank(),
  axis.title = element_blank(),
  axis.text.y = element_text(size=30, family="regular", color="black"),
  axis.text.x = element_text(size=20, family="regular", color="black"))


STF1 <- STF %>%
  mutate(name = fct_reorder(name, ano_aposentadoria)) %>%
  ggplot(aes(x = ano_indicacao, xend = ano_aposentadoria, y = name, colour = col_pallette)) +
  geom_dumbbell(colour_x = "#2596be", colour = "#040404", size = 5, colour_xend = "#0caedc", dot_guide = T) +
  scale_x_continuous(limits = c(2000, 2050), breaks = seq(2000, 2050, by = 5)) +
  geom_vline(xintercept = 2021, size = 3, color = "#28aa4e") +
  annotate('text', x = 2033, y = 1.5, label = "Rosa Weber e Lewandowski \nde saída em 2023", size = 8, family = 'regular', color = "#59504c") +
  annotate('text', x = 2040, y = 6.5, label = "Barroso e Fachin \nde saída em 2033", size = 8, family = 'regular', color = "#59504c") +
  labs(x = "Hours",
       y = NULL, 
       title = "Data limite de aposentadoria dos ministros do STF",
       subtitle = "<span style='color:#59504c'>Rosa Weber</span> e <span style='color:#59504c'>Lewandowski</span> se aposentam logo mais, mas...  \n<span style='color:#59504c'>Nunes Marques</span>, <span style='color:#59504c'>A. de Moraes</span> e <span style='color:#59504c'>Toffoli</span> têm um longo futuro pela frente",
       caption = "@oc_ipolunb") +
  trialtheme

