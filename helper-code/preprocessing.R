library(tidyverse)
library(ggiraph)
library(glue)
library(geomtextpath)

#read in file 
df<- df|>mutate(month_name = month.abb[month])
df$month_name<-factor(df$month_name, month.abb[1:12])

carriers = c("JetBlue Airways","Delta Air Lines Inc.", "American Airlines Inc.",
             "Southwest Airlines Co.","Virgin America","United Air Lines Inc.",
             "Spirit Air Lines")


monthly_delays_carrier<- df|>filter(year==2022 & carrier_name %in% carriers)|>
  group_by(carrier_name, month, month_name)|>
  summarise(flights = sum(arr_flights, na.rm=TRUE),
            delays = sum(arr_del15, na.rm=TRUE))|>
  mutate(id = row_number(),
         delay_perc = delays/flights, 
         carrier_name = str_remove_all(carrier_name," Airways| Air Lines| Inc.| Airlines| Co."))



monthly_delays_total = df|>filter(year==2022)|>
  group_by(month, month_name)|>
  summarise(flights = sum(arr_flights, na.rm=TRUE),
            delays = sum(arr_del15, na.rm=TRUE))|>
  mutate(delay_perc = delays/flights)


airline_labels<-monthly_delays_carrier|>filter(month==12)|>
  mutate(label_y = case_when(carrier_name=="American" ~ delay_perc + 0.025,
                               TRUE ~ delay_perc + 0.015))
  

annual_by_carrier <- df|>filter(year==2022)|>
  group_by(carrier_name)|>
  summarise(flights = sum(arr_flights, na.rm=TRUE),
            delays = sum(arr_del15, na.rm=TRUE),
            cancellations = sum(arr_cancelled, na.rm=TRUE))|>
  mutate(id = row_number(),
         delay_perc = delays/flights, 
         carrier_name = str_remove_all(carrier_name," Airways| Air Lines| Inc.| Airlines| Co."))

