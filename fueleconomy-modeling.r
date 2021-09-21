#
#
#
#
#
library(tidyverse)
library(lubridate)

#
# constants
#
literPerGallon <- 3.78
kmPerMile <- 1.609

# conversion factor between mpg and L/100km in formula
# [L/100km] = fuelconversion / [mpg]
fuelconversion <- literPerGallon * 100 / kmPerMile

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

#
# blame what president?
#

first_date <- as.Date("2020-04-30")

fuelcost <- fuel %>%
                mutate(president = ifelse(date < as.Date("2020-11-01"),
                                                    "Trump",
                                                    "Biden"
                                                ),
                            president = factor(president,
                                                levels = c("Trump", "Biden"))
                                                ) %>%
                filter(date > first_date)

#
prezmodel <- function(tbl) {
    lm(price ~ date, data = tbl)
}

prezaugment <- function(md) {
    broom::augment(md,
                   newdata = tibble(date = seq(from = first_date,
                                               to = today(),
                                               by = "month")),
                   interval = "confidence")
}


fuelmodel <-
    fuelcost %>%
    group_by(president) %>%
    nest() %>%
    mutate(prezmodel = map(data, prezmodel))

fuelmodelinfo <-
    fuelmodel %>%
    mutate(modelinfo = map(prezmodel, broom::glance)) %>%
    unnest(modelinfo)


fuelslope <-
    fuelmodel %>%
    mutate(parameters = map(prezmodel, broom::tidy)) %>%
    mutate(df_n  = map(data, nrow)) %>%
    unnest(parameters) %>%
    unnest(df_n) %>%
    filter(term == "date") %>%
    mutate(err.bar = std.error * qt(0.05 / 2, df_n, lower.tail = FALSE)) %>%
    mutate(estimate_UCL = estimate + err.bar,
           estimate_LCL = estimate - err.bar) %>%
    select(president, starts_with("estimate")) %>%

fueldata <-
    fuelmodel %>%
    mutate(fueldata = map(prezmodel, prezaugment)) %>%
    unnest(fueldata)


fuelcost %>%
    ggplot +
    aes(x = date, y = price, color = president) +
    geom_point() +
    geom_line(data = fueldata, aes(y = .fitted)) +
    geom_ribbon(alpha = .2,
                data = fueldata,
                aes(ymin = .lower,
                    ymax = .upper,
                    y = .fitted,
                    fill = president)) +
    scale_x_date(date_label = "%b\n%Y", date_breaks = "2 months") +
    scale_y_continuous(labels = scales::dollar_format()) +
    labs(x = "Date",
         y = "Fuel price ($/gal)",
         color = "President-(Elect)",
         fill = "President-(Elect)",
         title = "Price of fuel after the end of most lockdowns") +
    theme_light()

ggsave("graphs/fuelcost_increase_model.pdf", width = 11, height = 8)