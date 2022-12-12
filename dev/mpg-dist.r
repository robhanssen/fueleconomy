library(tidyverse)
library(lubridate)
library(ggridges)

load("Rdata/fuel.Rdata")

fuel %>%
    mutate(year = factor(year(date))) %>%
    filter(car_name != "2008 Nissan Altima") %>%
    ggplot + 
    aes(y = year, x = mpg) + geom_density_ridges() + 
    facet_wrap(~car_name, scales = "free_x")


fuel %>%
    mutate(year = factor(year(date))) %>%
    filter(car_name != "2008 Nissan Altima") %>%
    ggplot + 
    aes(x = year, y = mpg) + stat_ydensity(draw_quantiles = c(.25, .5, .75)) + 
    facet_wrap(~car_name, scales = "free_y")

    