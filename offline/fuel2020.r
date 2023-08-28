library(tidyverse)
theme_set(theme_light())
load("Rdata/fuel.Rdata")

year_range <- sort(unique(fuel$year))
year_length <- length(year_range)
year_today <- year(today())

cumulative <-
    fuel %>%
    arrange(date) %>%
    group_by(year) %>%
    mutate(
        cumulative_miles = cumsum(miles),
        cumulative_gallons = cumsum(gallons),
        cumulative_cost = cumsum(cost),
        display_date = date + years(year_today - year),
        yearf = factor(year)
    ) %>%
    ungroup()

color_range <- rep("gray70", year_length)
linetype_range <- rep("dotted", year_length)

pos_2020 <- which(year_range == 2020)
color_range[pos_2020] <- "purple"
color_range[year_length] <- "black"
linetype_range[pos_2020] <- "solid"
linetype_range[year_length] <- "solid"

cumulative %>%
    ggplot() +
    aes(display_date, cumulative_miles, color = yearf, linetype = yearf) +
    geom_line() +
    scale_x_date(date_labels = "%b %d") +
    scale_y_continuous(labels = scales::number_format()) +
    scale_color_manual(values = color_range) +
    scale_linetype_manual(values = linetype_range) +
    labs(
        x = "Date",
        y = "Cumulative distance (in miles)",
        title = "How did 2020 compare to other years?",
        subtitle = "in distance driven"
    ) +
    theme(legend.position = "none") +
    geom_vline(xintercept = as.Date("2022-03-15"), lty = 2, color = "gray50")

cumulative %>%
    ggplot() +
    aes(display_date, cumulative_gallons, color = yearf, linetype = yearf) +
    geom_line() +
    scale_x_date(date_labels = "%b %d") +
    scale_y_continuous(labels = scales::number_format()) +
    scale_color_manual(values = color_range) +
    scale_linetype_manual(values = linetype_range) +
    labs(
        x = "Date",
        y = "Cumulative distance (in gallons)",
        title = "How did 2020 compare to other years?",
        subtitle = "in gallons of gas"
    ) +
    theme(legend.position = "none") +
    geom_vline(xintercept = as.Date("2022-03-15"), lty = 2, color = "gray50")

cumulative %>%
    ggplot() +
    aes(display_date, cumulative_cost, color = yearf, linetype = yearf) +
    geom_line() +
    scale_x_date(date_labels = "%b %d") +
    scale_y_continuous(labels = scales::dollar_format()) +
    scale_color_manual(values = color_range) +
    scale_linetype_manual(values = linetype_range) +
    labs(
        x = "Date",
        y = "Cumulative distance (in gallons)",
        title = "How did 2020 compare to other years?",
        subtitle = "in dollars spent"
    ) +
    theme(legend.position = "none") +
    geom_vline(xintercept = as.Date("2022-03-15"), lty = 2, color = "gray50")