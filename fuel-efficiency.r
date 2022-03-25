library(tidyverse)
library(lubridate)
theme_set(theme_light())

load("Rdata/fuel.Rdata")

cutoff_lo <- today() - years(1)

fuel %>%
    filter(car_name == "2013 Nissan Altima 2.5SV") %>%
    ggplot() +
    aes(date, mpg, color = car_name) +
    geom_point() +
    scale_x_date(limits = c(cutoff_lo, NA)) +
    geom_smooth(method = "lm", se = FALSE)