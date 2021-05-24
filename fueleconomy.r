# 
# 
# 
# 
# 
library(tidyverse)
library(lubridate)

#
# constants
#
literPerGallon = 3.78
kmPerMile = 1.609
fuelconversion = literPerGallon * 100 / kmPerMile  # conversion factor between mpg and L/100km in formula  [L/100km] = fuelconversion / [mpg]

sixmonthsago = today() - months(6)

# fuel_source <- read_csv("fuelups.csv")
fuel_source <-	
    list.files(path="cars/", pattern = "*.csv", full.names=TRUE) %>% 	
    map_df(~read_csv(.)) %>%
    mutate(date=as.Date(fuelup_date, format="%m-%d-%Y"),
           car_name = factor(car_name),
           model = factor(model),
           dayofyear = yday(date),
           day = day(date),
           month = month(date),
           year = year(date),
           mpg = miles/gallons,
           cost = gallons*price,
           recent = factor(ifelse(date > sixmonthsago, "Recent", "Over 6 months ago"))
           ) %>% 
    select(-fuelup_date) %>%
    arrange(car_name,date) %>%
    group_by(car_name) %>% 
    mutate( timebetweenfuelups = date - lag(date), 
            miles1 = odometer - lag(odometer)
            ) -> fuel

write_csv(fuel %>% arrange(date), "data/fuelups_processed.csv")

#
# yearly use and expenses
#
#

fuel %>% group_by(car_name, year) %>% summarize(totalmiles=sum(miles), totalgallons=sum(gallons),totalcost=sum(cost)) -> yearlyoverview

yearlyoverview %>% ggplot() + aes(year, totalcost, fill=car_name) + geom_bar(stat="identity") +
                    labs(x="Year", y="Total Cost (in US$)", fill="Car")

yearlyoverview %>%  filter(year>2012) %>%
                    ggplot() + aes(factor(year), totalmiles, fill=car_name) + geom_bar(stat="identity") +
                    labs(title="Total distance (full year)", x="Year", y="Total Distance (in miles)", fill="Car") + 
                    scale_y_continuous(sec.axis=sec_axis(name="Total Distance (in km)", ~ .*kmPerMile))


#
# YTD yearly overview
#

yday_today <- yday(today())

fuel %>% filter(dayofyear <= yday_today) %>% group_by(car_name, year) %>% summarize(totalmiles=sum(miles), totalgallons=sum(gallons),totalcost=sum(cost)) -> yearlyoverview_ytd

yearlyoverview_ytd %>% ggplot() + aes(year, totalcost, fill=car_name) + geom_bar(stat="identity") +
                    labs(x="Year", y="Total Cost (in US$)", fill="Car")

yearlyoverview_ytd %>% ggplot() + aes(year, totalmiles, fill=car_name) + geom_bar(stat="identity") +
                    labs(title="Total distance YTD", x="Year", y="Total Distance (in miles)", fill="Car") + 
                    scale_y_continuous(sec.axis=sec_axis(name="Total Distance (in km)", ~ .*kmPerMile))

yearlyoverview_ytd %>% ggplot() + aes(year, totalgallons, fill=car_name) + geom_bar(stat="identity") +
                    labs(title="Total fuel usage YTD", x="Year", y="Total Fuel Use (in gallons)", fill="Car") + 
                    scale_y_continuous(sec.axis=sec_axis(name="Total Fuel Use (in L)", ~ .*literPerGallon))

#
# quarterly overview
#

fuel %>% mutate(quarter = quarters(date), quarter=paste0(year,quarter)) %>% group_by(car_name, quarter) %>% summarize(year=mean(year), totalmiles=sum(miles), totalgallons=sum(gallons),totalcost=sum(cost)) -> quarteroverview

quarteroverview %>%  filter(year>=2017) %>%
                    ggplot() + aes(factor(quarter), totalmiles, fill=car_name) + geom_bar(stat="identity") +
                    labs(title="Total distance (full quarter)", x="Quarter", y="Total Distance (in miles)", fill="Car") + 
                    scale_y_continuous(sec.axis=sec_axis(name="Total Distance (in km)", ~ .*kmPerMile))


# 
#  MPG calculations
# 

# fuelconversion = 3.78/(1.609/100)  # conversion factor between mpg and L/100km in formula  [L/100km] = fuelconversion / [mpg]

fuel %>% ggplot() + aes(x=date, y=mpg, color=car_name) + geom_line() + geom_point() +
                    labs(title="Fuel Economy", x="Date", y="Fuel economy (in miles/gallons)", fill="Car") + 
                    scale_y_continuous(limits=c(15,40), breaks=seq(15,40,5),sec.axis=sec_axis(name="Total Fuel Use (in L/100km)", ~ fuelconversion/., breaks=seq(6,20,1)))

fuel %>% group_by(year,car_name) %>%
                ggplot() + aes(x=factor(year), y=mpg) + geom_boxplot() + geom_violin(alpha=0.5) + facet_wrap(~car_name, scales="free_x") + 
                    labs(title="Fuel Economy", x="Date", y="Fuel economy (in miles/gallons)", fill="Car") + 
                    scale_y_continuous(breaks=seq(15,40,5),sec.axis=sec_axis(name="Total Fuel Use (in L/100km)", ~ fuelconversion/., breaks=seq(6,20,1))) +
                    coord_flip()

#
# time between fuelups
#
#

fuel %>% group_by(car_name) %>% mutate(timebetweenfuelups = date - lag(date)) -> tt

tt %>% ggplot() + aes(timebetweenfuelups, fill=car_name) + geom_density(alpha=0.5)
tt %>% ggplot() + aes(x=factor(year),y=timebetweenfuelups, color=car_name) + geom_violin()

# 
#  long-term gas mileage comparison vs recent
# 

fuel %>% group_by(car_name) %>% summarize(fuelecltmpg = sum(miles)/sum(gallons), fueleclt100km = fuelconversion/fuelecltmpg) -> averagedata

fuel %>%    mutate(fuelec = fuelconversion/mpg) %>% 
            group_by(car_name, recent) %>% 
            summarize(  fuelecav = mean(fuelec),
                        count = n(),
                        err.bars = qnorm(0.975) * sd(fuelec, na.rm=TRUE)/sqrt(count)  ) %>% 
            ggplot + aes(x=recent, y= fuelecav) + 
            geom_point() + 
            geom_errorbar(aes(ymin=fuelecav - err.bars, ymax=fuelecav+err.bars)) + 
            scale_y_continuous(limits=c(7,16), breaks=seq(0,20,2), sec.axis=sec_axis(name="Average Fuel Use (in MPG)", ~ fuelconversion/., breaks=seq(0,40,5))) + 
            labs(x="Recent fuel-ups (within last 6 months)", y="Average Fuel Use (in L/100 km)", caption="Error bars represent the 95% confidence interval") + 
            facet_wrap(~car_name) + geom_hline(yintercept=averagedata$fueleclt100km, lty=3) + 
            theme_light() 
ggsave("graphs/fuel-use-longterm-comparison.pdf", width=11, height=8)
