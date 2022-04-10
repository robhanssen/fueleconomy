library(tidyverse)
load("Rdata/fuel.Rdata")

cutoff_date <- as.Date("2020-03-29")
invasion <- as.Date("2022-02-24")

predict_fuel_price <-
    fuel %>%
    filter(date >= cutoff_date, date < invasion) %>%
    lm(price ~ date, data = .) %>%
    broom::augment(
        interval = "prediction",
        newdata = tibble(date = seq(cutoff_date - 39, invasion + 40, 1))
    ) %>%
    rename(price = ".fitted")


fuel %>%
    filter(date >= cutoff_date) %>%
    ggplot() +
    aes(x = date, y = price) +
    geom_point(alpha = .3) +
    labs(
        x = "Date",
        y = "Fuel price (in $/gal)",
        caption = "Fuel prices in South Carolina.\nBlue band indicater confidence interval of prediction"
    ) +
    scale_x_date(
        date_labels = "%b\n %Y",
        date_breaks = "3 months",
        limits = c(cutoff_date - 40, as.Date("2022-05-01"))
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
    geom_vline(xintercept = as.Date("2021-01-21"), lty = 3) +
    geom_vline(xintercept = as.Date("2022-02-24"), lty = 3) +
    annotate("label",
        x = as.Date("2021-01-21"),
        y = .20,
        label = "Inauguration\nBiden"
    ) +
    annotate("label",
        x = as.Date("2022-02-24"),
        y = .20,
        label = "Invasion of\nUkraine"
    ) +
    theme_light()

ggsave("graphs/fuel-price-postCOVID.png", width = 6, height = 6)