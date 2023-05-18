library(tidyverse)
theme_set(theme_light() +
    theme(
        plot.title.position = "plot",
        axis.ticks = element_blank(),
        plot.caption = element_text(hjust = 0),
        plot.caption.position = "plot"
    ))

load("Rdata/fuel.Rdata")

alltime_avg <-
    fuel %>%
    group_by(car_name) %>%
    summarize(
        all_mpg = sum(miles) / sum(gallons)
    )

totalrows <-
    fuel %>%
    group_by(car_name) %>%
    summarize(n = n())


mpg_range <- seq(
    floor(min(fuel$mpg)) - 1,
    ceiling(max(fuel$mpg)) + 1,
    .5
)

fuel_cdf <-
    fuel %>%
    mutate(car_name = factor(car_name)) %>%
    split(.$car_name) %>%
    map(\(df)
    map_dbl(
        mpg_range,
        \(m) nrow(filter(df, mpg < m)) / nrow(df)
    )) %>%
    enframe() %>%
    unnest(value) %>%
    mutate(mpg = rep(mpg_range, length(unique(.$name)))) %>%
    select(car_name = name, mpg, mpg_cdf = value)

last_points <- 10
cap <- glue::glue("Note: dots indicate last {last_points} fuel-up mpg data")

last_fuelups <-
    fuel %>%
    group_by(car_name) %>%
    slice_max(date, n = last_points) %>%
    ungroup()

last10 <- last_fuelups %>%
    select(car_name, mpg)

last10_cdf <- map(
    unique(last10$car_name),
    ~ approx(
        fuel_cdf$mpg[fuel_cdf$car_name == .x],
        fuel_cdf$mpg_cdf[fuel_cdf$car_name == .x],
        xout = last10$mpg[last10$car_name == .x]
    )$y
) %>%
    enframe() %>%
    mutate(name = unique(last10$car_name)) %>%
    unnest(value) %>%
    bind_cols(last10) %>%
    select(car_name, mpg, mpg_cdf = value)


mpg_cdf_p <-
    fuel_cdf %>%
    ggplot(aes(x = mpg, y = mpg_cdf, color = car_name)) +
    geom_line(show.legend = FALSE, alpha = .3) +
        geom_point(data = last10_cdf, 
                aes(x = mpg, y = mpg_cdf)) +
    scale_y_continuous(
        labels = scales::percent_format(),
        breaks = seq(0, 1, length.out = 5)
    ) +
    labs(
        x = "Fuel economy", y = "Cumulative density function",
        caption = cap
    ) +
    geom_segment(
        data = alltime_avg,
        alpha = .3, linewidth = 3,
        aes(
            x = all_mpg, xend = all_mpg,
            y = 0, yend = 1
        )
    ) +
    facet_wrap(~car_name, ncol = 1, scale = "free_x") +
    theme(legend.position = "none") +
    scale_color_manual(values = c("gray10", "purple", "red"))

ggsave("graphs/fuelmpg_cdf.png",
    height = 8, width = 6,
    plot = mpg_cdf_p
)

