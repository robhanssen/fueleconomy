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

monthly_average_mileage <-
    fuel %>%
    arrange(date) %>%
    group_by(car_name, year, month) %>%
    summarize(
        totalmiles = sum(miles),
        totalgallon = sum(gallons),
        .groups = "drop"
    ) %>%
    mutate(
        date = construct_date(year, month),
        mileage = totalmiles / totalgallon
    )

yearly_average_mileage <-
    fuel %>%
    arrange(date) %>%
    group_by(car_name, year) %>%
    summarize(
        totalmiles = sum(miles),
        totalgallon = sum(gallons),
        .groups = "drop"
    ) %>%
    mutate(
        date = construct_date(year, month = 7, day = 1),
        mileage = totalmiles / totalgallon
    )

max_mpg <-
    fuel %>%
    arrange(date) %>%
    group_by(car_name, year) %>%
    summarize(
        ymin = min(mpg),
        ymax = max(mpg),
        .groups = "drop"
    ) %>%
    mutate(date = construct_date(year, month = 7, day = 1))

lifetime_mpg <-
    fuel %>%
    arrange(date) %>%
    group_by(car_name) %>%
    summarize(
        totalmiles = sum(miles),
        totalgallon = sum(gallons),
        .groups = "drop"
    ) %>%
    mutate(
        mileage = totalmiles / totalgallon
    ) %>%
    filter(!str_detect(car_name, "2008"))



ggplot(monthly_average_mileage) +
    aes(date, mileage, color = car_name) +
    geom_line() +
    geom_point(data = yearly_average_mileage, size = 3) +
    geom_errorbar(
        data = max_mpg,
        aes(y = NULL, ymin = ymin, ymax = ymax),
        alpha = .3
    ) +
    scale_x_date(date_labels = "%Y") +
    scale_y_continuous(
        labels = scales::number_format(),
        limits = c(0, NA),
        sec.axis = sec_axis(~ fuelconversion / .,
            name = "Liter per 100 km",
            breaks = c(5:10, seq(10, 20, 2), seq(20, 30, 5))
        )
    ) +
    geom_hline(yintercept = lifetime_mpg$mileage, alpha = .5, lty = 3) +
    labs(
        x = "Date",
        y = "Average MPG",
        color = "Car"
    ) +
    theme(legend.position = c(0.8, 0.1))

ggsave("graphs/average_mpg.png", width = 8, height = 6)