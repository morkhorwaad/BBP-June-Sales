library(tidyverse)

setup = function() {
  RAW_DATA = read.csv("item_sales.csv")
  
  list(raw = RAW_DATA)
}

data = setup()
raw = data$raw

## THIS NEEDS TO BE ADJUSTED FOR QTY SOLD SOMEHOW

beer_data = raw %>%
  filter(Menu == "Retail Beer") %>%
  select(Menu.Item, Order.Date) %>%
  mutate(Order.Date = as.POSIXct(Order.Date)) %>%
  group_by(Menu.Item) %>%
  arrange(Menu.Item, Order.Date) %>%
  mutate(time_diff = as.numeric(Order.Date - lag(Order.Date), unit='mins')) %>%
  filter(!is.na(time_diff)) %>%
  summarise(avg_timebetween = mean(time_diff)) %>%
  arrange(avg_timebetween)

View(beer_data)
