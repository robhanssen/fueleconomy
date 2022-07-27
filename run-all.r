#
# the "official" fueleconomy package run
#

source("fueleconomy.r")

map(
    list.files(path = "analysis/",
            pattern = "*.r", 
            full.names = TRUE),
    ~source(.x)
    )