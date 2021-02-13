### WEEK 7 of TidyTuesday#####################################################
#
#https://github.com/rfordatascience/tidytuesday/blob/master/data/2021/2021-02-09/readme.md

# Libraries
library(tidyverse)

# Get the Data
tuesdata <- tidytuesdayR::tt_load(2021, week = 7)

# wrangle the income distribution dataset
plot_df<-tuesdata$income_distribution%>%
  # -- Filter for the races alone
  filter(race %in% c("Asian Alone","Black Alone", "White Alone",
                   "Hispanic (Any Race)"))%>%
  # -- Clean the income bracket to make it more concise and as factor
  mutate(income_bracket = str_replace(income_bracket,"[[:punct:]]000","k"),
         income_bracket = str_replace(income_bracket,"[[:punct:]]999",".k"),
         income_bracket = str_replace(income_bracket,"Under","<"),
         income_bracket = str_replace(income_bracket,"and over","+"),
         income_bracket = str_replace(income_bracket,"to \\$","-"),
         income_bracket = factor(income_bracket, 
                               levels = unique(income_bracket)),
         # -- Clean race
         race = case_when(
           race == "Asian Alone"~"Asian",
           race == "Black Alone"~"Black",
           race == "White Alone"~"White",
           race == "Hispanic (Any Race)"~"Hispanic"
         ))%>%
  # -- Filter for 1987
  filter(year > 1986)

# Build dataset containing the position for the annotations of text
ann_text <- data.frame(year = c(1987,2019,2019),
                       income_distribution = c(28.5,23.5,18),
                       income_bracket = factor(
                         levels(plot_df$income_bracket)[1],
                            levels = levels(plot_df$income_bracket))
                       )
# Build dataset for the means
mean_year<-plot_df%>%
  group_by(race, income_bracket)%>%
  summarise(year = mean(year), income_distribution = mean(income_distribution))

# The plot
ggplot(plot_df, aes(year, income_distribution))+
  # -- Trend smooth line with span 1
  geom_smooth(show.legend = F, aes(color = race), se = F, span = 1, size=0.7)+
  # -- mean points
  geom_point(data = mean_year, aes(color = race), size = 3, alpha = 0.6)+
  # -- faceted by income bracket
  facet_grid(~income_bracket)+
  xlab(NULL)+
  ylab("Percentage income year/race (%)")+
  scale_x_continuous(breaks = NULL)+
  labs(title = "Inconcome distribution trend\nby race and income bracket (1987-2019)",
          caption = "Source: Urban Institute and US Census
       @AbelTorresEspi2 for TidyTuesday 2021 w7",
       color = NULL)+
  # -- add the annotations
  geom_text(data = ann_text, label = c("1987", "Average", "2019"), size = 3.5,
            color = "steelblue3")+
  # -- by setting clip = "off", the annotations are not clipped by the 
  #facet panel
  coord_cartesian(clip = "off")+
  theme_minimal()+
  # -- Theming
  theme(panel.grid = element_line(linetype = "dotted", colour = "grey40"),
        panel.grid.minor = element_blank(),
        axis.title.x = element_text(hjust = 1, vjust = 0),
        legend.position = "bottom",
        legend.margin = margin(t = -10),
        legend.justification = "left",
        plot.background = element_rect(fill = "snow2", color = "black"),
        plot.margin = unit(rep(5,4), "mm"))

ggsave("ATE tidytuesday 2021 w7.png", height = 4, width = 8)
