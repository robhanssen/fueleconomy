library(tidyverse)
theme_set(theme_light() +
    theme(
        plot.title.position = "plot",
        axis.ticks = element_blank(),
        plot.caption = element_text(hjust = 0),
        plot.caption.position = "plot"
    ))

load("Rdata/fuel.Rdata")


altima_fuel <-
    fuel %>%
    filter(str_detect(car_name, "2013"))

alltime_avg <-
    altima_fuel %>%
    summarize(
        all_gallons = sum(gallons),
        all_miles = sum(miles)
    ) %>%
    mutate(all_mpg = all_miles / all_gallons)


totalrows <- nrow(altima_fuel)

mpg_cdf <- tibble(
    mpg_range = seq(23, 38, .5),
    mpg_cdf = map_dbl(
        mpg_range,
        ~ (altima_fuel %>% filter(mpg < .x) %>% nrow()) / totalrows
    )
)

last_points <- 10

latest_points <-
    altima_fuel %>%
    slice_max(date, n = last_points) %>%
    select(mpg, miles, gallons) %>%
    mutate(mpg_cdf_approx = map_dbl(
        mpg,
        ~ approx(mpg_cdf$mpg_range, mpg_cdf$mpg_cdf, xout = .x)$y
    ))

last_points_av <- tibble(
    mpg = summarize(latest_points,
        mpg_av = sum(miles) / sum(gallons)
    ) %>% pull(mpg_av),
    mpg_cdf_approx = approx(mpg_cdf$mpg_range, mpg_cdf$mpg_cdf, xout = mpg)$y
)


cap <- glue::glue("Note: red dots indicate last {last_points} fuel-up mpg data")


mpg_cdf_g <- mpg_cdf %>%
    ggplot(aes(x = mpg_range, y = mpg_cdf)) +
    geom_line(alpha = .3) +
    scale_x_continuous(breaks = seq(20, 40, 2)) +
    scale_y_continuous(labels = scales::percent_format()) +
    geom_point(
        data = latest_points,
        aes(x = mpg, y = mpg_cdf_approx),
        color = "red", size = 2
    ) +
    geom_point(
        data = last_points_av,
        aes(x = mpg, y = mpg_cdf_approx),
        color = "red", size = 5, shape = 3
    ) +
    geom_vline(xintercept = alltime_avg$all_mpg, alpha = .1, linewidth = 3) +
    annotate("text",
        x = alltime_avg$all_mpg + .2,
        y = .05, label = "All-time\naverage",
        hjust = 0
    ) +
    labs(
        x = "Fuel economy", y = "Cumulative density function",
        caption = cap
    )

ggsave("graphs/altima_mpg_cdf.png",
        height = 4, width = 6,
        plot = mpg_cdf_g)