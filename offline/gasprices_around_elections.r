# gas prices

library(tidyverse)
library(lubridate)

load("Rdata/fuel.Rdata")

fuel_restricted <-
    fuel %>%
    filter(date > lubridate::ymd(20200401))

datebreaks <-
    c(
        first(fuel_restricted$date),
        ymd(20220220),
        ymd(20220601),
        ymd(20230101),
        today()
    )

election_gas_prices <-
    fuel_restricted %>%
    mutate(period = cut(date, breaks = datebreaks)) %>%
    ggplot() +
    aes(date, price, color = period) +
    geom_smooth(method = "lm", se = TRUE, alpha = .1) +
    scale_color_manual(values = rep(
        "gray50",
        length(datebreaks) - 1
    )) +
    geom_point() +
    geom_vline(
        xintercept = lubridate::ymd(20221103),
        lty = 2, color = "gray50"
    ) +
    theme_light() +
    labs(x = "Date", y = "Fuel price 87 Octane ($/gal)") +
    scale_y_continuous(labels = scales::dollar_format()) +
    scale_x_date(date_label = "%b\n%Y", date_breaks = "3 months") +
    geom_vline(
        xintercept = lubridate::ymd(20210120),
        lty = 2, color = "gray50"
    ) +
    annotate("text",
        x = lubridate::ymd(20210120),
        y = 1.5, label = "Inauguration\nBiden"
    ) +
    annotate("text",
        x = lubridate::ymd(20221103),
        y = 1.5, label = "mid-terms\nNov 2022"
    ) +
    theme(legend.position = "none")

ggsave("graphs/gasprice_around_elections.png",
    width = 8, height = 5,
    plot = election_gas_prices
)
