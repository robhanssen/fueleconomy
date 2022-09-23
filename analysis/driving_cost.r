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

monthly_average_mileage_cost <-
    fuel %>%
    arrange(date) %>%
    group_by(car_name, year, month) %>%
    summarize(
        totalmiles = sum(miles),
        totalmoney = sum(cost),
        .groups = "drop"
    ) %>%
    mutate(
        date = construct_date(year, month),
        mileage_cost = totalmoney / totalmiles
    )

yearly_average_mileage_cost <-
    fuel %>%
    arrange(date) %>%
    group_by(car_name, year) %>%
    summarize(
        totalmiles = sum(miles),
        totalmoney = sum(cost),
        .groups = "drop"
    ) %>%
    mutate(
        date = construct_date(year, month = 7, day = 1),
        mileage_cost = totalmoney / totalmiles
    )

max_costs <-
    fuel %>%
    arrange(date) %>%
    mutate(mileage_cost = cost / miles) %>%
    group_by(car_name, year) %>%
    summarize(
        ymin = min(mileage_cost),
        ymax = max(mileage_cost),
        .groups = "drop"
    ) %>%
    mutate(date = construct_date(year, month = 7, day = 1))

ggplot(monthly_average_mileage_cost) +
    aes(date, mileage_cost, color = car_name) +
    geom_line() +
    geom_point(data = yearly_average_mileage_cost, size = 3) +
    geom_errorbar(data = max_costs,
            aes(y = NULL, ymin = ymin, ymax = ymax),
            alpha = .3) +
    scale_x_date(date_labels = "%Y") +
    scale_y_continuous(
        labels = scales::dollar_format(),
        limits = c(0, NA)
    ) +
    labs(
        x = "Date",
        y = "Average driving cost (in $/mile)"
    ) +
    theme(legend.position = c(0.8, 0.13))

ggsave("graphs/average_driving_cost.png", width = 8, height = 6)