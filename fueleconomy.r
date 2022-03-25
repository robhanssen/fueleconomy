library(tidyverse)
library(lubridate)
library(patchwork)
theme_set(theme_light())

source("predictive_models.r")
#
# constants
#
liter_per_gallon <- 3.78
km_per_mile <- 1.609

# conversion factor between mpg and L/100km in formula
# [L/100km] = fuelconversion / [mpg]
fuelconversion <- liter_per_gallon * 100 / km_per_mile

sixmonthsago <- today() %m-% months(6)

fuel <-
    list.files(path = "cars/", pattern = "*.csv", full.names = TRUE) %>%
    map_df(~ read_csv(.)) %>%
    mutate(
        date = as.Date(fuelup_date, format = "%m-%d-%Y"),
        car_name = factor(car_name),
        model = factor(model),
        dayofyear = yday(date),
        day = day(date),
        month = month(date),
        year = year(date),
        mpg = miles / gallons,
        l100km = fuelconversion / mpg,
        cost = gallons * price,
        recent = factor(ifelse(date > sixmonthsago,
            "Recent",
            "Over 6 months ago"
        ))
    ) %>%
    select(-fuelup_date) %>%
    arrange(car_name, date) %>%
    group_by(car_name) %>%
    mutate(
        timebetweenfuelups = date - lag(date),
        miles1 = odometer - lag(odometer)
    ) %>%
    ungroup() %>%
    filter(year > 2013)

save(fuel, file = "Rdata/fuel.Rdata")
#
#  gas price fluctations
#

fuelaverage <-
    fuel %>%
    group_by(year) %>%
    summarize(
        totalgal = sum(gallons),
        minprice = min(price),
        maxprice = max(price),
        totalcost = sum(cost),
        .groups = "drop"
    ) %>%
    mutate(
        fuelprice = totalcost / totalgal,
        date = as.Date(paste0(year, "-07-01"))
    )


fuel %>%
    group_by(year, month) %>%
    summarize(
        fueluse = sum(gallons),
        fuelcost = sum(cost),
        av_fuel_unit = fuelcost / fueluse
    ) %>%
    mutate(date = as.Date(paste0(year, "-", month, "-15"))) %>%
    ggplot() +
    aes(x = date, y = av_fuel_unit) +
    geom_point(
        data = fuelaverage,
        aes(y = fuelprice),
        alpha = .5,
        size = 3
    ) +
    geom_errorbar(
        data = fuelaverage,
        aes(ymin = minprice, ymax = maxprice, y = NULL),
        alpha = .5
    ) +
    geom_line() +
    labs(
        x = "Date",
        y = "Average monthly fuel cost ($/gal)",
        title = "Fuel cost fluctuations",
        caption = "Error bars shows min and max fuel costs"
    ) +
    scale_x_date(breaks = "1 year", date_labels = "%Y") +
    scale_y_continuous(
        labels = scales::dollar_format(accuracy = .01),
        limits = c(0, NA)
    ) +
    theme_light()

ggsave("graphs/gasprice-fluctuations.png", width = 8, height = 6)

#
# yearly use and expenses
#
#

yearlyoverview <-
    fuel %>%
    filter(year > 2012) %>%
    group_by(car_name, year) %>%
    summarize(
        totalmiles = sum(miles),
        totalgallons = sum(gallons),
        totalcost = sum(cost),
        .groups = "drop"
    ) %>%
    mutate(
        costpermile = totalcost / totalmiles,
        averagefuelefficiency = totalmiles / totalgallons
    )

totalmpg <-
    fuel %>%
    filter(car_name != "2008 Nissan Altima", year > 2012) %>%
    group_by(car_name) %>%
    summarize(
        totalgallons = sum(gallons),
        totalmiles = sum(miles),
        .groups = "drop"
    ) %>%
    mutate(totalmpg = totalmiles / totalgallons) %>%
    pull(totalmpg)

yearlycost <-
    yearlyoverview %>%
    ggplot() +
    aes(factor(year), totalcost, fill = car_name) +
    geom_bar(stat = "identity") +
    labs(
        x = "Year",
        y = "Total Cost (in US$)",
        fill = "Car",
        title = "Total annual spend"
    ) +
    theme(legend.position = "none")

yearlymiles <-
    yearlyoverview %>%
    ggplot() +
    aes(year, totalmiles, fill = car_name) +
    geom_bar(stat = "identity") +
    labs(
        title = "Total annual distance",
        x = "Year",
        y = "Total Distance (in miles)",
        fill = "Car"
    ) +
    scale_y_continuous(
        sec.axis =
            sec_axis(
                name = "Total Distance (in km)",
                ~ . * km_per_mile
            )
    ) +
    scale_x_continuous(breaks = 2010 + 2 * 0:10)

costpermile <-
    yearlyoverview %>%
    ggplot() +
    aes(year, costpermile, color = car_name) +
    geom_line() +
    labs(
        x = "Year",
        y = "Average Cost per mile (in US$/mi)",
        fill = "Car",
        title = "Average Cost per mile"
    ) +
    theme(legend.position = "none") +
    scale_x_continuous(breaks = 2010 + 2 * 0:10)

mpgplot <-
    yearlyoverview %>%
    ggplot() +
    aes(year, averagefuelefficiency, color = car_name) +
    geom_line() +
    labs(
        x = "Year",
        y = "Average fuel efficiency (in miles/gal)",
        fill = "Car",
        title = "Average Fuel Efficiency"
    ) +
    theme(legend.position = "none") +
    scale_x_continuous(breaks = 2010 + 2 * 0:10) +
    scale_y_continuous(
        breaks = 2 * 0:50,
        sec.axis = sec_axis(~ fuelconversion / .,
            name = "Average fuel efficiency (in L/100km)"
        )
    ) +
    geom_hline(yintercept = totalmpg, lty = 2)

(yearlycost + yearlymiles) / (mpgplot + costpermile)

ggsave("graphs/yearlyoverview.png", width = 11, height = 8)

#
# Miles, gallons and $ spent predictions
#

gallonmodel <- function(tbl) {
    lm(totalgallons ~ yday, data = tbl)
}

costmodel <- function(tbl) {
    lm(totalcost ~ yday, data = tbl)
}

milesmodel <- function(tbl) {
    lm(totalmiles ~ yday, data = tbl)
}

yday_augment <- function(mod) {
    broom::augment(mod, newdata = tibble::tibble(yday = c(1, 365)))
}

fuelmodelprep <-
    fuel %>%
    mutate(yday = yday(date)) %>%
    arrange(year, yday) %>%
    group_by(year) %>%
    mutate(
        totalmiles = cumsum(miles),
        totalcost = cumsum(cost),
        totalgallons = cumsum(gallons)
    ) %>%
    select(year, yday, totalmiles, totalgallons, totalcost)

fuelmodels <-
    fuelmodelprep %>%
    nest() %>%
    mutate(
        galmodel = map(data, gallonmodel),
        cosmodel = map(data, costmodel),
        milmodel = map(data, milesmodel)
    )

fuelfitteddata <-
    fuelmodels %>%
    mutate(
        cosmodelinfo = map(cosmodel, yday_augment),
        galmodelinfo = map(galmodel, yday_augment),
        milmodelinfo = map(milmodel, yday_augment)
    )

galmod <-
    fuelfitteddata %>%
    unnest(galmodelinfo) %>%
    group_by(year) %>%
    slice_max(.fitted) %>%
    ggplot() +
    aes(year, .fitted, fill = factor(year)) +
    geom_col() +
    geom_point(
        data = fuelmodelprep %>%
            group_by(year) %>%
            slice_max(totalgallons),
        aes(y = totalgallons)
    ) +
    scale_x_continuous(breaks = 2012 + 1:100, minor_breaks = NULL) +
    scale_y_continuous(breaks = 200 * 0:100) +
    labs(
        x = "Year",
        y = "Fuel Use (in gallons)",
        title = "Fuel use by year",
        caption = "Bars: predicted use\nPoints: actual use"
    ) +
    theme(legend.position = "none")

milmod <-
    fuelfitteddata %>%
    unnest(milmodelinfo) %>%
    group_by(year) %>%
    slice_max(.fitted) %>%
    ggplot() +
    aes(year, .fitted, fill = factor(year)) +
    geom_col() +
    geom_point(
        data = fuelmodelprep %>%
            group_by(year) %>%
            slice_max(totalmiles),
        aes(y = totalmiles)
    ) +
    scale_x_continuous(breaks = 2012 + 1:100, minor_breaks = NULL) +
    scale_y_continuous(
        breaks = 5000 * 0:100,
        labels = scales::comma_format(suffix = "\nmiles")
    ) +
    labs(
        x = "Year",
        y = "Distance driven (in miles)",
        title = "Distance driven by year",
        caption = "Bars: predicted distance\nPoints: actual distance"
    ) +
    theme(legend.position = "none")

cosmod <-
    fuelfitteddata %>%
    unnest(cosmodelinfo) %>%
    group_by(year) %>%
    slice_max(.fitted) %>%
    ggplot() +
    aes(year, .fitted, fill = factor(year)) +
    geom_col() +
    geom_point(
        data = fuelmodelprep %>%
            group_by(year) %>%
            slice_max(totalcost),
        aes(y = totalcost)
    ) +
    scale_x_continuous(breaks = 2012 + 1:100, minor_breaks = NULL) +
    scale_y_continuous(breaks = 500 * 0:100, labels = scales::dollar_format()) +
    labs(
        x = "Year",
        y = "Spending on gasoline (in USD)",
        title = "Spend on gasoline by year",
        caption = "Bars: predicted spending\nPoints: actual spending"
    ) +
    theme(legend.position = "none")

p2 <- milmod + galmod + cosmod
ggsave("graphs/spendingmodel.png", width = 18, height = 6, plot = p2)


#
# try to predict gas use for the full year based on periods of various lengths
#

period_list <- c(45, 60, 90, 120, 180, 365)

average_gallons <-
    fuel %>%
    ungroup() %>%
    mutate(year = year(date)) %>%
    filter(year != year(today())) %>%
    summarize(
        totalgal = sum(gallons),
        years = length(unique(year)),
        .groups = "drop"
    ) %>%
    mutate(average_gallons = totalgal / years) %>%
    pull(average_gallons)

gallon_prediction <-
    map_df(period_list, ~ predict_gallons_used(fuel, .x)) %>%
    ggplot() +
    aes(factor(period), gallons_used, fill = car_name) +
    geom_col() +
    scale_y_continuous(
        labels = scales::comma_format(),
        sec.axis = sec_axis(~ . * liter_per_gallon,
            name = "Predicted gas (in L)",
            labels = scales::comma_format()
        )
    ) +
    labs(
        x = "# of days used in prediction",
        y = "Predicted amounts of gas by end-of-year (in gallons)",
        fill = "Car",
        title = paste("Gasoline use prediction for end of year", year(today()))
    ) +
    geom_hline(yintercept = average_gallons, lty = 2, color = "gray50") +
    theme(legend.position = "none")

average_cost <-
    fuel %>%
    ungroup() %>%
    mutate(year = year(date)) %>%
    filter(year != year(today())) %>%
    summarize(
        totalcost = sum(cost),
        years = length(unique(year)),
        .groups = "drop"
    ) %>%
    mutate(average_cost = totalcost / years) %>%
    pull(average_cost)

cost_prediction <-
    map_df(period_list, ~ predict_fuel_cost(fuel, .x)) %>%
    ggplot() +
    aes(factor(period), total_fuel_cost, fill = car_name) +
    geom_col() +
    scale_y_continuous(labels = scales::dollar_format()) +
    labs(
        x = "# of days used in prediction",
        y = "Predicted fuel cost (in $)",
        fill = "Car",
        title = paste("Gasoline cost prediction for end of year", year(today()))
    ) +
    geom_hline(yintercept = average_cost, lty = 2, color = "gray50") +
    theme(legend.position = "none")

average_miles <-
    fuel %>%
    ungroup() %>%
    mutate(year = year(date)) %>%
    filter(year != year(today())) %>%
    summarize(
        totalmiles = sum(miles),
        years = length(unique(year)),
        .groups = "drop"
    ) %>%
    mutate(average_miles = totalmiles / years) %>%
    pull(average_miles)

distance_prediction <-
    map_df(period_list, ~ predict_miles(fuel, .x)) %>%
    ggplot() +
    aes(factor(period), total_miles, fill = car_name) +
    geom_col() +
    scale_y_continuous(
        labels = scales::comma_format(),
        sec.axis = sec_axis(~ . * km_per_mile,
            name = "Predicted distance (in km)",
            labels = scales::comma_format()
        )
    ) +
    labs(
        x = "# of days used in prediction",
        y = "Predicted distance driven (in mi)",
        fill = "Car",
        title = paste("Miles prediction for end of year", year(today()))
    ) +
    geom_hline(yintercept = average_miles, lty = 2, color = "gray50") +
    theme(legend.position = "none")

p <- distance_prediction + gallon_prediction + cost_prediction

ggsave("graphs/predicted-car-use.png", width = 18, height = 6)