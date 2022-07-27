library(tidyverse)
library(lubridate)
library(patchwork)
theme_set(theme_light())

load("Rdata/fuel.Rdata")

#
# constants
#
liter_per_gallon <- 3.78
km_per_mile <- 1.609

# conversion factor between mpg and L/100km in formula
# [L/100km] = fuelconversion / [mpg]
fuelconversion <- liter_per_gallon * 100 / km_per_mile

construct_date <- function(year, month, day = 1) {
    lubridate::ymd(year * 1e4 + month * 1e2 + day)
}

display_date <- function(date, target_year = year(today())) {
    ddate <- date + lubridate::years(target_year - lubridate::year(date))
    lubridate::floor_date(ddate, unit = "month") + lubridate::days(1)
}

cumulative_fuel <-
    fuel %>%
    arrange(date) %>%
    group_by(year, month) %>%
    summarize(
        distance = sum(miles),
        volume = sum(gallons),
        money = sum(cost),
        .groups = "drop"
    ) %>%
    mutate(
        date = construct_date(year, month),
        display_date = display_date(date)
    ) %>%
    group_by(year) %>%
    mutate(across(distance:money, cumsum)) %>%
    ungroup()

year_length <- length(unique(cumulative_fuel$year))
colors <- c(rep("gray70", year_length - 2), "gray20", "black")
ltys <- c(rep(3, year_length - 2), 2, 1)


distance_plot <-
    cumulative_fuel %>%
    ggplot() +
    aes(
        x = display_date,
        y = distance,
        color = factor(year),
        linetype = factor(year)
    ) +
    geom_line() +
    scale_x_date(date_labels = "%b") +
    scale_y_continuous(
        labels = scales::number_format(),
        limits = c(0, NA),
        sec.axis = sec_axis(~ . / km_per_mile,
            name = "Cumulative distance (in km)"
        )
    ) +
    labs(
        x = "Date",
        y = "Cumulative distance (in miles)",
        caption = "Black line: current year\nDashed line: last year\nDotted lines: previous years" # nolint
    ) +
    scale_color_manual(values = colors) +
    scale_linetype_manual(values = ltys) +
    theme(legend.position = "none")

volume_plot <-
    cumulative_fuel %>%
    ggplot() +
    aes(
        x = display_date,
        y = volume,
        color = factor(year),
        linetype = factor(year)
    ) +
    geom_line() +
    scale_x_date(date_labels = "%b") +
    scale_y_continuous(
        labels = scales::number_format(),
        limits = c(0, NA),
        sec.axis = sec_axis(~ . * liter_per_gallon,
            name = "Cumulative fuel use (in L)"
        )
    ) +
    labs(
        x = "Date",
        y = "Cumulative fuel use (in gallons)",
        caption = "Black line: current year\nDashed line: last year\nDotted lines: previous years" # nolint
    ) +
    scale_color_manual(values = colors) +
    scale_linetype_manual(values = ltys) +
    theme(legend.position = "none")

money_plot <-
    cumulative_fuel %>%
    ggplot() +
    aes(
        x = display_date,
        y = money,
        color = factor(year),
        linetype = factor(year)
    ) +
    geom_line() +
    scale_x_date(date_labels = "%b") +
    scale_y_continuous(
        labels = scales::dollar_format(),
        limits = c(0, NA)
    ) +
    labs(
        x = "Date",
        y = "Cumulative spending",
        caption = "Black line: current year\nDashed line: last year\nDotted lines: previous years" # nolint
    ) +
    scale_color_manual(values = colors) +
    scale_linetype_manual(values = ltys) +
    theme(legend.position = "none")

p <- distance_plot + volume_plot + money_plot
ggsave("graphs/cumulative_data.png", width = 18, height = 6, plot = p)