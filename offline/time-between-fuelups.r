library(tidyverse)
theme_set(theme_light())

load("Rdata/fuel.Rdata")


fuel %>%
    ggplot() +
    aes(timebetweenfuelups, fill = car_name) +
    geom_density() +
    geom_vline(xintercept = c(7 * 1:10), lty = 3, alpha = .7) +
    scale_x_continuous(breaks = seq(0, 70, 7), limits = c(0, 45)) +
    facet_wrap(~car_name) +
    labs(
        x = "Time between fuel-ups",
        y = NULL
    ) +
    theme(legend.position = "none")


fuel %>%
    filter(car_name != "2008 Nissan Altima") %>%
    ggplot() +
    aes(y = factor(year), x = timebetweenfuelups, fill = car_name) +
     ggridges::geom_density_ridges() +
    geom_vline(xintercept = c(7 * 1:10), lty = 3, alpha = .7) +
    scale_x_continuous(breaks = seq(0, 70, 7), limits = c(0, 56)) +
    facet_wrap(~car_name) +
    labs(
        x = "Time between fuel-ups",
        y = NULL
    ) +
    theme(legend.position = "none")