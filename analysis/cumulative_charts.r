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

display_date <- function(date, target_year = year(today())) {
    ddate <- date + lubridate::years(target_year - lubridate::year(date))
    lubridate::floor_date(ddate, unit = "month") + lubridate::days(1)
}

cumulative_fuel_ytd <-
    fuel %>%
    mutate(display_date = display_date(date)) %>%
    filter(display_date <= today()) %>%
    group_by(year) %>%
    summarize(
        distance_ytd = sum(miles),
        volume_ytd = sum(gallons),
        money_ytd = sum(cost),
        .groups = "drop"
    )

cumulative_fuel_total <-
    fuel %>%
    group_by(year) %>%
    summarize(
        distance_total = sum(miles),
        volume_total = sum(gallons),
        money_total = sum(cost),
        .groups = "drop"
    )

distance_plot <-
    cumulative_fuel_total %>%
    ggplot() +
    geom_col(
        data = cumulative_fuel_total,
        aes(x = year, y = distance_total),
        fill = "gray70"
    ) +
    geom_col(
        data = cumulative_fuel_ytd,
        aes(x = year, y = distance_ytd),
        fill = "gray30"
    ) +
    scale_y_continuous(
        labels = scales::number_format(),
        limits = c(0, NA),
        sec.axis = sec_axis(~ . / km_per_mile,
            name = "Cumulative distance (in km)"
        )
    ) +
    labs(
        x = "",
        y = "Cumulative distance (in miles)",
    ) +
    theme(legend.position = "none")

volume_plot <-
    cumulative_fuel_total %>%
    ggplot() +
    geom_col(
        data = cumulative_fuel_total,
        aes(x = year, y = volume_total),
        fill = "gray70"
    ) +
    geom_col(
        data = cumulative_fuel_ytd,
        aes(x = year, y = volume_ytd),
        fill = "gray30"
    ) +
    scale_y_continuous(
        labels = scales::number_format(),
        limits = c(0, NA),
        sec.axis = sec_axis(~ . * liter_per_gallon,
            name = "Cumulative fuel use (in L)"
        )
    ) +
    labs(
        x = "",
        y = "Cumulative fuel use (in gallons)",
    ) +
    theme(legend.position = "none")

money_plot <-
    cumulative_fuel_total %>%
    ggplot() +
    geom_col(
        data = cumulative_fuel_total,
        aes(x = year, y = money_total),
        fill = "gray70"
    ) +
    geom_col(
        data = cumulative_fuel_ytd,
        aes(x = year, y = money_ytd),
        fill = "gray30"
    ) +
    scale_y_continuous(
        labels = scales::dollar_format(),
        limits = c(0, NA)
    ) +
    labs(
        x = "",
        y = "Cumulative spending",
    ) +
    theme(legend.position = "none")

p <- distance_plot + volume_plot + money_plot

ggsave("graphs/cumulative_year_data.png",
    width = 18, height = 6,
    plot = p
)
