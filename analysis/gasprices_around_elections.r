#gas prices

library(tidyverse)
library(lubridate)

load("Rdata/fuel.Rdata")

fuel %>% filter(date > lubridate::ymd(20200401)) %>%
    mutate(period = cut(date, breaks = c(first(.$date), ymd(20220220), ymd(20220601), today()))) %>% 
    ggplot + 
    aes(date, price, color = period) + 
    geom_smooth(method = "lm", se = TRUE, alpha = .1) +
    scale_color_manual(values = rep("gray50", 3)) +
    geom_point() + 
    geom_vline(xintercept = lubridate::ymd(20221103), lty = 2, color = "gray50") + 
    theme_light() + 
    labs(x = "Date", y = "Fuel price 87 Octane ($/gal") + 
    scale_y_continuous(labels = scales::dollar_format()) + 
    scale_x_date(date_label = "%b\n%Y", date_breaks = "3 months") +
    geom_vline(xintercept = lubridate::ymd(20210120), lty = 2, color = "gray50") + 
    annotate("text", x = lubridate::ymd(20210120), y = 1.5, label = "Inauguration Biden") +
    annotate("text", x = lubridate::ymd(20221103), y = 1.5, label = "mid-terms Nov 2021") +
    theme(legend.position = "none")
