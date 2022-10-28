library(tidyverse)
library(lubridate)
library(patchwork)
theme_set(theme_light())

load("Rdata/fuel.Rdata")

car_colors <-
    c(
        "2013 Nissan Altima 2.5SV" = "#c04d35",
        "2011 Nissan Quest 3.5SL" = "#413D4A"
    )

fuel %>%
    filter(car_name != "2008 Nissan Altima", gallons < 20) %>%
    mutate(label = format(date, format = "%b %d, %Y")) %>%
    group_by(car_name) %>%
    slice_max(gallons, n = 10) %>%
    ungroup() %>%
    ggplot() +
    aes(y = fct_reorder(label, gallons), x = gallons, color = car_name) +
    geom_point(
        alpha = 1,
        show.legend = FALSE,
        size = 3
    ) +
    scale_x_continuous(
        breaks = 0:25,
        limits = c(15, 20)
    ) +
    scale_color_manual(values = car_colors) +
    facet_wrap(~car_name, scales = "free_y") + 
    labs(y = "Date", x = "Gallons of fuel")


fuel %>%
    filter(car_name != "2008 Nissan Altima", gallons < 20) %>%
    mutate(label = format(date, format = "%b %d, %Y")) %>%
    slice_max(cost, n = 30) %>%
    ggplot() +
    aes(y = fct_reorder(label, cost), x = cost, color = car_name) +
    geom_point(
        alpha = 1,
        show.legend = FALSE,
        size = 3
    ) +
    scale_x_continuous(labels = scales::dollar_format()) +
    scale_color_manual(values = car_colors) +
    labs(y = "Date", x = "Cost")


fuel %>%
    filter(car_name != "2008 Nissan Altima", gallons < 20) %>%
    mutate(label = format(date, format = "%b %d, %Y")) %>%
    slice_max(price, n = 30) %>%
    ggplot() +
    aes(y = fct_reorder(label, price), x = price, color = car_name) +
    geom_point(
        alpha = 1,
        show.legend = FALSE,
        size = 3
    ) +
    scale_x_continuous(labels = scales::dollar_format()) +
    scale_color_manual(values = car_colors) +
    labs(y = "Date", x = "Fuel price")


fuel %>%
    filter(car_name != "2008 Nissan Altima", gallons < 20) %>%
    mutate(label = format(date, format = "%b %d, %Y")) %>%
    group_by(car_name) %>%
    slice_max(miles, n = 20) %>%
    ungroup() %>%
    ggplot() +
    aes(y = fct_reorder(label, miles), x = miles, color = car_name) +
    geom_point(
        alpha = 1,
        show.legend = FALSE,
        size = 3
    ) +
    scale_x_continuous() +
    scale_color_manual(values = car_colors) +
    facet_wrap(~car_name, scales = "free") +
    labs(y = "Date", x = "Miles driven")


(fuel %>%
    filter(car_name != "2008 Nissan Altima", gallons < 20) %>%
    mutate(label = format(date, format = "%b %d, %Y")) %>%
    group_by(car_name) %>%
    slice_max(mpg, n = 20) %>%
    ungroup() %>%
    ggplot() +
    aes(y = fct_reorder(label, mpg), x = mpg, color = car_name) +
    geom_point(
        alpha = 1,
        show.legend = FALSE,
        size = 3
    ) +
    scale_x_continuous(limits = c(15, 40)) +
    scale_color_manual(values = car_colors) +
    facet_wrap(~car_name, scales = "free_y") +
    labs(y = "Date", x = "MPG")
) /
    (fuel %>%
        filter(car_name != "2008 Nissan Altima", gallons < 20) %>%
        mutate(label = format(date, format = "%b %d, %Y")) %>%
        group_by(car_name) %>%
        slice_min(mpg, n = 20) %>%
        ungroup() %>%
        ggplot() +
        aes(y = fct_reorder(label, mpg), x = mpg, color = car_name) +
        geom_point(
            alpha = 1,
            show.legend = FALSE,
            size = 3
        ) +
        scale_x_continuous(limits = c(15, 40)) +
        scale_color_manual(values = car_colors) +
        facet_wrap(~car_name, scales = "free_y") +
        labs(y = "Date", x = "MPG")
    )