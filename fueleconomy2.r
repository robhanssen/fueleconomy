library(tidyverse)
library(lubridate)
library(patchwork)
theme_set(theme_light())

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
        scale_y_continuous(sec.axis = sec_axis(name = "Total Distance (in km)", ~ . * kmPerMile)) + 
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
             y = "Average Cost per mile (in US$/mi)",
             fill = "Car",
             title = "Average Cost per mile") +
        theme(legend.position = "none")  +
        scale_x_continuous(breaks = 2010 + 2 * 0:10) +
        scale_y_continuous(breaks = 2 * 0:50) +
        geom_hline(yintercept = totalmpg, lty = 2)

(yearlycost + yearlymiles) / (mpgplot + costpermile)

ggsave("graphs/yearlyoverview.pdf", width = 11, height = 8)



#
# YTD yearly overview
#

yday_today <- yday(today())

fuel %>%
    filter(dayofyear <= yday_today) %>%
    group_by(car_name, year) %>%
    summarize(totalmiles = sum(miles),
              totalgallons = sum(gallons),
              totalcost = sum(cost)
              ) -> yearlyoverview_ytd

yearlyoverview_ytd %>%
        ggplot() +
            aes(year, totalcost, fill = car_name) +
            geom_bar(stat = "identity") +
            labs(x = "Year", y = "Total Cost (in US$)", fill = "Car")

yearlyoverview_ytd %>%
        ggplot() +
            aes(year, totalmiles, fill = car_name) +
            geom_bar(stat = "identity") +
            labs(title = "Total distance YTD",
                 x = "Year",
                 y = "Total Distance (in miles)",
                 fill = "Car") +
            scale_y_continuous(sec.axis = sec_axis(name = "Total Distance (in km)", ~ . * kmPerMile))

yearlyoverview_ytd %>%
        ggplot() +
            aes(year, totalgallons, fill = car_name) +
            geom_bar(stat = "identity") +
            labs(title = "Total fuel usage YTD",
                 x = "Year",
                 y = "Total Fuel Use (in gallons)",
                 fill = "Car") +
            scale_y_continuous(sec.axis = sec_axis(name = "Total Fuel Use (in L)", ~ . * literPerGallon))

#
# quarterly overview
#

fuel %>%
        mutate(quarter = quarters(date),
               quarter = paste0(year, quarter)) %>%
               group_by(car_name, quarter) %>%
               summarize(year = mean(year),
                         totalmiles = sum(miles),
                         totalgallons = sum(gallons),
                         totalcost = sum(cost)
                         ) -> quarteroverview

quarteroverview %>%
        filter(year >= 2017) %>%
        ggplot() +
            aes(factor(quarter), totalmiles, fill = car_name) +
            geom_bar(stat = "identity") +
            labs(title = "Total distance (full quarter)",
                 x = "Quarter",
                 y = "Total Distance (in miles)",
                 fill = "Car") +
            scale_y_continuous(sec.axis = sec_axis(name = "Total Distance (in km)", ~ . * kmPerMile))  +
            theme(axis.text.x = element_text(angle = 45, vjust = 1.0, hjust=1))


ggsave("graphs/quarterly_distance.png", width = 8, height = 6)
#
#  MPG calculations
#

fuel %>%
    ggplot() +
        aes(x = date, y = mpg, color = car_name) +
        geom_line() +
        geom_point() +
        labs(title = "Fuel Economy",
             x = "Date",
             y = "Fuel economy (in miles/gallons)",
             fill = "Car") +
        scale_y_continuous(limits = c(15, 40),
                           breaks = seq(15, 40, 5),
                           sec.axis = sec_axis(name = "Total Fuel Use (in L/100km)",
                                             ~ fuelconversion / .,
                                             breaks = seq(6, 20, 1)
                                             )
                            )

fuel %>%
    group_by(year, car_name) %>%
    ggplot() +
        aes(x = factor(year), y = mpg) +
        geom_boxplot() +
        geom_violin(alpha = 0.5) +
        facet_wrap(~car_name, scales = "free_x") +
        labs(title = "Fuel Economy",
             x = "Date",
             y = "Fuel economy (in miles/gallons)",
             fill = "Car") +
        scale_y_continuous(breaks = seq(15, 40, 5),
                           sec.axis = sec_axis(name = "Total Fuel Use (in L/100km)",
                                             ~ fuelconversion / .,
                                             breaks = seq(6, 20, 1)
                                             )
                            ) +
        coord_flip()

#
# time between fuelups
#
#

fuel %>%
    group_by(car_name) %>%
    mutate(timebetweenfuelups = date - lag(date)
            ) -> tt

tt %>%
    ggplot() +
        aes(timebetweenfuelups, fill = car_name) +
        geom_density(alpha = 0.5)

tt %>%
    ggplot() +
        aes(x = factor(year), y = timebetweenfuelups, color = car_name) +
        geom_violin()

#
#  long-term gas mileage comparison vs recent
#

averagedata <- fuel %>%
    group_by(car_name) %>%
    summarize(fuelecltmpg = sum(miles) / sum(gallons),
              fueleclt100km = fuelconversion / fuelecltmpg
              ) 

fuelcomparison <- fuel %>%
    mutate(fuelec = fuelconversion / mpg) %>%
    group_by(car_name, recent) %>%
    summarize(fuelecav = mean(fuelec),
              count = n(),
              err.bars = qnorm(0.975) * sd(fuelec, na.rm = TRUE) / sqrt(count)
              )

fuelcomparison %>%
    ggplot +
        aes(x = recent, y = fuelecav) +
        geom_point() +
        geom_errorbar(aes(ymin = fuelecav - err.bars, ymax = fuelecav + err.bars)) +
        scale_y_reverse(limits = c(16, 7),
                        breaks = seq(0, 20, 2),
                        sec.axis = sec_axis(name = "Average Fuel Use (in MPG)",
                                            ~ fuelconversion / .,
                                            breaks = seq(0, 40, 5)
                                            )
                        ) +
        labs(x = "Recent fuel-ups (within last 6 months)",
             y = "Average Fuel Use (in L/100 km)",
             caption = "Error bars represent the 95% confidence interval") +
        facet_wrap(~car_name) +
        geom_hline(yintercept = averagedata$fueleclt100km, lty = 3) +
        theme_light()

ggsave("graphs/fuel-use-longterm-comparison.pdf", width = 11, height = 8)

longtermshift <- fuel %>%
        inner_join(averagedata, by="car_name") %>%
        mutate(l100kmdiff = l100km - fueleclt100km,
               mpgdiff = mpg - fuelecltmpg) %>%
        group_by(car_name) 

longtermshift %>%
        group_by(car_name) %>%
        summarize(mn = mean(l100kmdiff), sd = sd(l100kmdiff), n=n()) %>%
        mutate(err.bars = qnorm(0.975) * sd / sqrt(n), LCL=mn - err.bars, UCL = mn + err.bars)
