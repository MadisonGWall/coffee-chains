library(tidyverse)
library(janitor)

# Dunkin data processing-------------------------------------------------------
dunkin <- read_csv(here::here("data", "dunkin_stores.csv"))

dunkin <- dunkin |>
  clean_names() |>
  select(row_id, address_line_1, city, state, zip,
         mon_hrs, tue_hrs, wed_hrs, thu_hrs, fri_hrs, 
         sat_hrs, sun_hrs, loc_lat, loc_long)

dunkin |>
  rename(
    lat = loc_lat,
    long = loc_long
  ) |>
  mutate(
    lat = if_else(lat == "N/A", NA, lat),
    lat = as.numeric(lat),
    long = if_else(long == "N/A", NA, long),
    long = as.numeric(long)
  )

write_csv(dunkin, file = "data/dunkin_clean.csv")

# Starbucks data processing-----------------------------------------------------
starbucks <- read_csv(here::here("data", "starbucks.csv"))

starbucks <- starbucks |>
  clean_names() |>
  filter(country_code == "US") |>
  select(x1, slug, schedule, ownership_type_code,
         latitude, longitude, city, postal_code,
         country_subdivision_code, country_code) |>
  rename(id = x1,
         state = country_subdivision_code)

write_csv(starbucks, file = "data/starbucks_clean.csv")
