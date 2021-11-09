library(tidyverse)
library(lubridate)
library(patchwork)
theme_set(theme_light())

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
    map_df(~read_csv(.)) %>%
    mutate(date = as.Date(fuelup_date, format = "%m-%d-%Y"),
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
                                    "Over 6 months ago")
                                    )
                            ) %>%
    select(-fuelup_date) %>%
    arrange(car_name, date) %>%
    group_by(car_name) %>%
    mutate(timebetweenfuelups = date - lag(date),
           miles1 = odometer - lag(odometer)
            )

write_csv(fuel %>% arrange(date), "data/fuelups_processed.csv")

#
#  gas price fluctations
#

fuel %>%
            group_by(year, month) %>%
            summarize(fueluse = sum(gallons),
                      fuelcost = sum(cost),
                      av_fuel_unit = fuelcost / fueluse
                      ) %>%
            mutate(date = as.Date(paste0(year, "-", month, "-15"))) %>%
            ggplot +
                aes(x = date, y = av_fuel_unit) +
                geom_point() +
                geom_line() +
                labs(x = "Date", y = "Average fuel cost ($/gal)") +
                scale_x_date(breaks = "1 year", date_labels = "%Y") +
                theme_light()

#
# yearly use and expenses
#
#

yearlyoverview <-
    fuel %>%
    filter(year > 2012) %>%
    group_by(car_name, year) %>%
    summarize(totalmiles = sum(miles),
              totalgallons = sum(gallons),
              totalcost = sum(cost),
              .groups = "drop") %>%
    mutate(costpermile = totalcost / totalmiles,
           averagefuelefficiency = totalmiles / totalgallons)

totalmpg <-
    fuel %>%
    filter(car_name != "2008 Nissan Altima", year > 2012) %>%
    group_by(car_name) %>%
    summarize(totalgallons = sum(gallons),
             totalmiles = sum(miles),
             .groups = "drop") %>%
    mutate(totalmpg = totalmiles / totalgallons) %>%
    pull(totalmpg)

yearlycost <-
    yearlyoverview %>%
    ggplot() +
        aes(factor(year), totalcost, fill = car_name) +
        geom_bar(stat = "identity") +
        labs(x = "Year",
             y = "Total Cost (in US$)",
             fill = "Car",
             title = "Total annual spend") +
        theme(legend.position = "none")

yearlymiles <-
    yearlyoverview %>%
    ggplot() +
        aes(year, totalmiles, fill = car_name) +
        geom_bar(stat = "identity") +
        labs(title = "Total annual distance",
             x = "Year",
             y = "Total Distance (in miles)",
             fill = "Car") +
        scale_y_continuous(sec.axis = sec_axis(name = "Total Distance (in km)", ~ . * km_per_mile)) +
        scale_x_continuous(breaks = 2010 + 2 * 0:10)

costpermile <-
    yearlyoverview %>%
    ggplot() +
        aes(year, costpermile, color = car_name) +
        geom_line() +
        labs(x = "Year",
             y = "Average Cost per mile (in US$/mi)",
             fill = "Car",
             title = "Average Cost per mile") +
        theme(legend.position = "none")  +
        scale_x_continuous(breaks = 2010 + 2 * 0:10)

mpgplot <-
    yearlyoverview %>%
    ggplot() +
        aes(year, averagefuelefficiency, color = car_name) +
        geom_line() +
        labs(x = "Year",
             y = "Average fuel efficiency (in miles/mi)",
             fill = "Car",
             title = "Average Cost per mile") +
        theme(legend.position = "none")  +
        scale_x_continuous(breaks = 2010 + 2 * 0:10) +
        scale_y_continuous(breaks = 2 * 0:50) +
        geom_hline(yintercept = totalmpg, lty = 2)

(yearlycost + yearlymiles) / (mpgplot + costpermile)

ggsave("graphs/yearlyoverview.pdf", width = 11, height = 8)

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
    filter(year != 2012) %>%
    mutate(yday = yday(date)) %>%
    arrange(year, yday) %>%
    group_by(year) %>%
    mutate(totalmiles = cumsum(miles),
           totalcost = cumsum(cost),
           totalgallons = cumsum(gallons)) %>%
    select(year, yday, totalmiles, totalgallons, totalcost)
    
fuelmodels <-
    fuelmodelprep %>%
    nest() %>%
    mutate(galmodel = map(data, gallonmodel),
           cosmodel = map(data, costmodel),
           milmodel = map(data, milesmodel))

fuelfitteddata <-
    fuelmodels %>%
    mutate(cosmodelinfo = map(cosmodel, yday_augment),
           galmodelinfo = map(galmodel, yday_augment),
           milmodelinfo = map(milmodel, yday_augment)
    )

galmod <-
    fuelfitteddata %>%
    unnest(galmodelinfo) %>%
    group_by(year) %>%
    slice_max(.fitted) %>%
    ggplot + aes(year, .fitted, fill = factor(year)) +
    geom_col() +
    geom_point(data = fuelmodelprep %>%
                      group_by(year) %>%
                      slice_max(totalgallons),
                aes(y = totalgallons)
               )  +
    scale_x_continuous(breaks = 2012 + 1:100, minor_breaks = NULL) +
    scale_y_continuous(breaks = 200 * 0:100)  +
    labs(x = "Year",
         y = "Fuel Use (in gallons)",
         title = "Fuel use by year",
         caption = "Bars indicate predicted amount, points indicate actual use") +
    theme(legend.position = "none")

milmod <-
    fuelfitteddata %>%
    unnest(milmodelinfo) %>%
    group_by(year) %>%
    slice_max(.fitted) %>%
    ggplot + aes(year, .fitted, fill = factor(year)) +
    geom_col() +
    geom_point(data = fuelmodelprep %>%
                      group_by(year) %>%
                      slice_max(totalmiles),
                aes(y = totalmiles)
               )  +
    scale_x_continuous(breaks = 2012 + 1:100, minor_breaks = NULL) +
    scale_y_continuous(breaks = 5000 * 0:100, labels = scales::comma_format(suffix = "\nmiles"))  +
    labs(x = "Year",
         y = "Distance driven (in miles)",
         title = "Distance driven by year",
         caption = "Bars indicate predicted distance, points indicate actual distance") +
    theme(legend.position = "none")

cosmod <-
    fuelfitteddata %>%
    unnest(cosmodelinfo) %>%
    group_by(year) %>%
    slice_max(.fitted) %>%
    ggplot + aes(year, .fitted, fill = factor(year)) +
    geom_col() +
    geom_point(data = fuelmodelprep %>%
                      group_by(year) %>%
                      slice_max(totalcost),
                aes(y = totalcost)
               )  +
    scale_x_continuous(breaks = 2012 + 1:100, minor_breaks = NULL) +
    scale_y_continuous(breaks = 500 * 0:100, labels = scales::dollar_format())  +
    labs(x = "Year",
         y = "Spending on gasoline (in USD)",
         title = "Spend on gasoline by year",
         caption = "Bars indicate predicted spending, points indicate actual spending") +
    theme(legend.position = "none")

p2 <- milmod + galmod + cosmod
ggsave("graphs/spendingmodel.png", width = 18, height = 6, plot = p2)