library(readxl)
library(tidycensus)

# Data from Census in 2023
census_leave_work <- read_xlsx(here::here("data", "leave_for_work.xlsx"))

census_leave_work <- census_leave_work |>
  mutate(
    across(everything(), ~ str_replace(.x, "%±.*", "")),
    across(-`Time`, ~ as.numeric(.x))
  ) |>
  select(-`...2`)

census_long <- pivot_longer(
  census_leave_work,
  cols = -`Time`,
  names_to = "State",
  values_to = "Percent"
)

census_wide <- pivot_wider(
  census_long,
  names_from = `Time`,
  values_from = `Percent`) |>
  filter(`State` != "District of Columbia")

leave_work_clean <- census_wide |>
  mutate(
    `6:00 AM - 9:59 AM` = `6:00 a.m. to 6:29 a.m.` + `6:30 a.m. to 6:59 a.m.` + `7:00 a.m. to 7:29 a.m.` + `7:30 a.m. to 7:59 a.m.` + `8:00 a.m. to 8:29 a.m.` + `8:30 a.m. to 8:59 a.m.` + `9:00 a.m. to 9:59 a.m.`,
    `10:00 AM - 3:59 PM` = `10:00 a.m. to 10:59 a.m.` + `11:00 a.m. to 11:59 a.m.` + `12:00 p.m. to 3:59 p.m.`,
    `4:00 PM - 11:59 PM` = `4:00 p.m. to 11:59 p.m.`
  ) |>
  select(`State`, `6:00 AM - 9:59 AM`, `10:00 AM - 3:59 PM`, `4:00 PM - 11:59 PM`)

saveRDS(leave_work_clean, "leave_work_2023.rds")