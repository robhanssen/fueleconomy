library(tidyverse)
library(lubridate)
load("Rdata/fuel.Rdata")

cutoff_date <- as.Date("2020-03-29")
invasion <- as.Date("2022-02-24")
postinvasion <- as.Date("2022-07-01")

predict_fuel_price <-
    fuel %>%
    filter(date >= cutoff_date, date < invasion) %>%
    lm(price ~ date, data = .) %>%
    broom::augment(
        interval = "prediction",
        newdata = tibble(date = seq(cutoff_date - 39, today() + 40, 1))
    ) %>%
    rename(price = ".fitted")


predict_fuel_price_postinvasion <- # nolint
    fuel %>%
    filter(date > postinvasion) %>%
    lm(price ~ date, data = .) %>%
    broom::augment(
        interval = "prediction",
        newdata = tibble(date = seq(invasion, today() + 40, 1))
    ) %>%
    rename(price = ".fitted")


p1 <-
    fuel %>%
    filter(date >= cutoff_date) %>%
    ggplot() +
    aes(x = date, y = price) +
    geom_point(alpha = .3) +
    labs(
        x = "Date",
        y = "Fuel price (in $/gal)",
        caption = "Fuel prices in South Carolina.\nBlue band indicates confidence interval of prediction" # nolint
    ) +
    scale_x_date(
        date_labels = "%b\n %Y",
        date_breaks = "3 months",
        limits = c(cutoff_date - 40, today() + 40)
    ) +
    scale_y_continuous(
        labels = scales::dollar_format(accuracy = .01),
        limit = c(0, NA),
        sec.axis = sec_axis(~., labels = scales::dollar_format(accuracy = .01))
    ) +
    geom_line(
        data = predict_fuel_price,
        lty = 2,
        color = "gray50",
        alpha = .7
    ) +
    geom_ribbon(
        data = predict_fuel_price,
        aes(ymin = .lower, ymax = .upper),
        alpha = .1,
        fill = "blue"
    ) +
    geom_line(
        data = predict_fuel_price_postinvasion,
        lty = 2,
        color = "gray50",
        alpha = .7
    ) +
    geom_ribbon(
        data = predict_fuel_price_postinvasion,
        aes(ymin = .lower, ymax = .upper),
        alpha = .1,
        fill = "orange"
    ) +
    geom_vline(xintercept = invasion, lty = 3) +
    annotate("label",
        x = as.Date("2022-02-24"),
        y = .20,
        label = "Invasion of\nUkraine"
    ) +
    theme_light()

# ggsave("graphs/fuel-price-postCOVID.png", width = 6, height = 6, plot = p1)


models <-
    fuel %>%
    mutate(event = cut(date,
        breaks = c(cutoff_date - years(100), cutoff_date, invasion, postinvasion, today()),
        label = c("ignore", "post-COVID", "post-invasion", "russian decline")
    )) %>%
    filter(event != "ignore") %>%
    group_by(event) %>%
    nest() %>%
    mutate(pricemodel = map(data, ~ lm(price ~ date, data = .x)))

predict_multi <-
    models %>%
    mutate(pricepred = map(
        pricemodel,
        ~ broom::augment(.x,
            interval = "prediction",
            newdata = tibble(date = seq(
                cutoff_date,
                today() + 61,
                1
            ))
        )
    )) %>%
    unnest(pricepred) %>%
    rename(price = ".fitted") %>%
    filter(event != "post-invasion")

p2 <-
    fuel %>%
    filter(date >= cutoff_date) %>%
    ggplot() +
    aes(x = date, y = price) +
    geom_point(alpha = .3) +
    labs(
        x = "Date",
        y = "Fuel price (in $/gal)",
        caption = "Fuel prices in South Carolina.\nBlue band indicates confidence interval of prediction", # nolint
        color = "Timeframe",
        fill = "Timeframe"
    ) +
    scale_x_date(
        date_labels = "%b\n %Y",
        date_breaks = "3 months",
        limits = c(cutoff_date - 40, today() + 61)
    ) +
    scale_y_continuous(
        labels = scales::dollar_format(accuracy = .01),
        limit = c(0, NA),
        sec.axis = sec_axis(~., labels = scales::dollar_format(accuracy = .01))
    ) +
    coord_cartesian(ylim = c(0,5)) +
    geom_line(
        data = predict_multi,
        lty = 2,
        aes(color = event),
        alpha = .7
    ) +
    geom_ribbon(
        data = predict_multi,
        aes(ymin = .lower, ymax = .upper, fill = event),
        alpha = .1
    ) +
    geom_vline(xintercept = invasion, lty = 3) +
    annotate("label",
        x = as.Date("2022-02-24"),
        y = .20,
        label = "Invasion of\nUkraine"
    ) +
    theme_light()

# ggsave("graphs/fuel-price-by-event.png", width = 8, height = 6, plot = p2)


library(patchwork)
ggsave("graphs/both-price-plots.png", width = 16, height = 6, plot = p1 + p2)


fuel %>%
    ggplot() +
    aes(y = factor(year), x = price, fill = factor(year)) +
    ggridges::geom_density_ridges() +
    scale_x_continuous(breaks = seq(0, 10, .5),
                        labels = scales::dollar_format()
                        ) +
    labs(
        x = "Fuel price (per gallon)",
        y = NULL,
        caption = "Fuel prices in Upstate South Carolina"
    ) +
    theme(legend.position = "none")

ggsave("graphs/fuel-price-distribution.png", width = 6, height = 6)