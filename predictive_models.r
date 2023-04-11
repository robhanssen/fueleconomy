#
# Predictive models
#

predict_gallons_used <- function(tbl, period) {
    pred_data <- tibble::tibble(date = c(
        lubridate::floor_date(today(), unit = "year"),
        lubridate::ceiling_date(today(), unit = "year")
    ))

    history <-
        tbl %>%
        filter(date > today() - days(period)) %>%
        group_by(car_name) %>%
        mutate(
            totalmiles = cumsum(miles),
            totalcost = cumsum(cost),
            totalgallons = cumsum(gallons),
            .groups = "drop"
        ) %>%
        select(date, car_name, totalmiles, totalgallons, totalcost)

    period_model <-
        history %>%
        group_by(car_name) %>%
        nest() %>%
        mutate(period_gal_model = map(
            data,
            ~ lm(totalgallons ~ date, data = .x)
        ))

    period_model_quality <-
        period_model %>%
        mutate(model_quality = map(period_gal_model, ~ broom::glance(.x))) %>%
        unnest(model_quality) %>%
        select(car_name, r.squared)

    period_predict <-
        period_model %>%
        mutate(prediction = map(
            period_gal_model,
            function(tbl) {
                tbl %>%
                    broom::augment(newdata = pred_data)
            }
        )) %>%
        unnest(prediction) %>%
        select(-data, -period_gal_model) %>%
        pivot_wider(
            names_from = date,
            values_from = .fitted
        ) %>%
        # mutate(gallons_used = cur_data()[[2]] - cur_data()[[1]]) %>%
        mutate(gallons_used = pick(everything())[[2]] - pick(everything())[[1]]) %>%
        select(car_name, gallons_used)

    period_predict %>%
        mutate(period = period) %>%
        bind_cols(r.squared = period_model_quality$r.squared)
}


predict_fuel_cost <- function(tbl, period) {
    pred_data <- tibble::tibble(date = c(
        lubridate::floor_date(today(), unit = "year"),
        lubridate::ceiling_date(today(), unit = "year")
    ))

    history <-
        tbl %>%
        filter(date > today() - days(period)) %>%
        group_by(car_name) %>%
        mutate(
            totalmiles = cumsum(miles),
            totalcost = cumsum(cost),
            totalgallons = cumsum(gallons),
            .groups = "drop"
        ) %>%
        select(date, car_name, totalmiles, totalgallons, totalcost)

    period_model <-
        history %>%
        group_by(car_name) %>%
        nest() %>%
        mutate(period_gal_model = map(data, ~ lm(totalcost ~ date, data = .x)))

    period_model_quality <-
        period_model %>%
        mutate(model_quality = map(period_gal_model, ~ broom::glance(.x))) %>%
        unnest(model_quality) %>%
        select(car_name, r.squared)

    period_predict <-
        period_model %>%
        mutate(prediction = map(
            period_gal_model,
            function(tbl) {
                tbl %>%
                    broom::augment(newdata = pred_data)
            }
        )) %>%
        unnest(prediction) %>%
        select(-data, -period_gal_model) %>%
        pivot_wider(
            names_from = date,
            values_from = .fitted
        ) %>%
        # mutate(total_fuel_cost = cur_data()[[2]] - cur_data()[[1]]) %>%
        mutate(total_fuel_cost = pick(everything())[[2]] - pick(everything())[[1]]) %>%
        select(car_name, total_fuel_cost)

    period_predict %>%
        mutate(period = period) %>%
        bind_cols(r.squared = period_model_quality$r.squared)
}


predict_miles <- function(tbl, period) {
    pred_data <- tibble::tibble(date = c(
        lubridate::floor_date(today(), unit = "year"),
        lubridate::ceiling_date(today(), unit = "year")
    ))

    history <-
        tbl %>%
        filter(date > today() - days(period)) %>%
        group_by(car_name) %>%
        mutate(
            totalmiles = cumsum(miles),
            totalcost = cumsum(cost),
            totalgallons = cumsum(gallons),
            .groups = "drop"
        ) %>%
        select(date, car_name, totalmiles, totalgallons, totalcost)

    period_model <-
        history %>%
        group_by(car_name) %>%
        nest() %>%
        mutate(period_gal_model = map(data, ~ lm(totalmiles ~ date, data = .x)))

    period_model_quality <-
        period_model %>%
        mutate(model_quality = map(period_gal_model, ~ broom::glance(.x))) %>%
        unnest(model_quality) %>%
        select(car_name, r.squared)

    period_predict <-
        period_model %>%
        mutate(prediction = map(
            period_gal_model,
            function(tbl) {
                tbl %>%
                    broom::augment(newdata = pred_data)
            }
        )) %>%
        unnest(prediction) %>%
        select(-data, -period_gal_model) %>%
        pivot_wider(
            names_from = date,
            values_from = .fitted
        ) %>%
        # mutate(total_miles = cur_data()[[2]] - cur_data()[[1]]) %>%
        mutate(total_miles = pick(everything())[[2]] - pick(everything())[[1]]) %>%
        select(car_name, total_miles)

    period_predict %>%
        mutate(period = period) %>%
        bind_cols(r.squared = period_model_quality$r.squared)
}