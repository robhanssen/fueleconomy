#
# the "official" fueleconomy package run
#

source("fueleconomy.r")

walk(
    list.files(path = "analysis/",
            pattern = "*.r$",
            full.names = TRUE),
    ~source(.x)
    )