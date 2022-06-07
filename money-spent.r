library(tidyverse)
library(lubridate)
theme_set(theme_light())

load("Rdata/fuel.Rdata")


spending <-
    fuel %>%
    group_by(month, year) %>%
    summarize(
        monthly_spend = sum(cost),
        monthly_gal = sum(gallons),
        .groups = "drop"
    ) %>%
    mutate(display_date = ymd(year * 1e4 + month * 1e2 + 1)) %>%
    arrange(display_date) %>%
    mutate(rolling_spend = zoo::rollsumr(monthly_spend, 12, na.pad = TRUE))

correction_factor <- 5

spending %>%
    filter(display_date >= ymd(20190101)) %>%
    ggplot() +
    aes(x = display_date, y = rolling_spend) +
    geom_point(color = "red", lty = 3) +
    geom_point(aes(y = rolling_spend / 12 * correction_factor),
        alpha = .5,
        lty = 3
    ) +
    geom_col(aes(y = correction_factor * monthly_spend, fill = factor(year)),
        alpha = .5
    ) +
    scale_y_continuous(
        label = scales::dollar_format(),
        name = "Rolling yearly spend",
        sec.axis = sec_axis(~ . / correction_factor,
            name = "Monthly spend",
            label = scales::dollar_format()
        )
    ) +
    labs(
        x = "Date",
        title = "Long-term monthly and yearly spend on gasoline",
        caption = "Red points: 12-month rolling sum spend\nGray point: monthly average based on 12-month rolling sum" # nolint
    ) +
    theme(legend.position = "none")

ggsave("graphs/long-term-spend.png", width = 8, height = 6)