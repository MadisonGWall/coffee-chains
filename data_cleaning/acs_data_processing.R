library(tidyverse)
library(tidycensus)

# Data from American Community Survey in 2022

# Get census counties---------------------------------------------------------
counties <- get_acs(
  geography = "county",
  variables = "B01003_001E",   # total pop (placeholder)
  geometry = TRUE,
  state = state.abb_filtered,
  year = 2022
) |>
  select(GEOID, geometry)

saveRDS(counties, "counties_2022.rds")

# Adults in working age (18 to 66 - retirement age)--------------------------
age <- get_acs(
  geography = "county",
  state = state.abb_filtered,
  table = "B01001",
  year = 2022,
  output = "wide"
) |>
  rename(
    male_18_19 = B01001_007E, male_20 = B01001_008E, male_21 = B01001_009E,
    male_22_24 = B01001_010E, female_18_19 = B01001_031E, female_20 = B01001_032E,
    
    female_21 = B01001_033E, female_22_24 = B01001_034E, male_25_29 = B01001_011E,
    male_30_34 = B01001_012E, male_35_39 = B01001_013E, male_40_44 = B01001_014E,
    
    female_25_29 = B01001_035E, female_30_34 = B01001_036E, female_35_39 = B01001_037E,
    female_40_44 = 	B01001_038E, male_45_49 = B01001_015E, male_50_54 = B01001_016E,
    
    male_55_59 = B01001_017E, male_60_61 = B01001_018E, male_62_64 = B01001_019E,
    female_45_49 = B01001_039E, female_50_54 = B01001_040E, female_55_59 = B01001_041E,
    
    female_60_61 = B01001_042E, female_62_64 = B01001_043E, male_65_66 = B01001_020E,
    female_65_66 = B01001_044E,
  ) |>
  mutate(
    age_18_29 = male_18_19 + male_20 + male_21 + male_22_24 + male_25_29 +
      female_18_19 + female_20 + female_21 + female_22_24 + 
      female_25_29,
    
    age_30_44 = male_30_34 + male_35_39 + male_40_44 +
      female_30_34 + female_35_39 + female_40_44,
    
    age_45_66 = male_45_49 + male_50_54 + male_55_59 + male_60_61 +
      male_62_64 + female_45_49 + female_50_54 + female_55_59 +
      female_60_61 + female_62_64 + male_65_66 + female_65_66
  ) |>
  select(GEOID, NAME, age_18_29, age_30_44, age_45_66)
saveRDS(age, "age_2022.rds")

# Median household income 2022 census ---------------------------------------
median_income <- get_acs(
  geography = "county",
  state = state.abb_filtered,
  table = "B19013",
  year = 2022
) |>
  rename(
    median_hh = estimate
  ) |>
  select(-variable, -moe)
saveRDS(median_income, "median_hh_2022.rds")

# Total population 2022 census ---------------------------------------------
total_pop <- get_acs(
  geography = "county",
  state = state.abb_filtered,
  table = "B01003",
  year = 2022
) |>
  rename(
    total_pop = estimate
  ) |>
  select(-variable, -moe)
saveRDS(total_pop, "total_pop_2022.rds")

# Higher education ----------------------------------------------------------
higher_ed <- get_acs(
  geography = "county",
  state = state.abb_filtered,
  table = "B15003",
  year = 2022,
  output = "wide"
) |>
  rename(
    total_pop_over_25 = B15003_001E,
    total_bach = B15003_022E,
    total_master = B15003_023E,
    total_professional = B15003_024E,
    total_dr = B15003_025E,
  ) |>
  select(GEOID, NAME, total_pop_over_25, total_bach, total_master, total_professional, total_dr) |>
  mutate(
    total_higher_ed = total_bach + total_master + total_professional + total_dr,
    no_higher_ed = total_pop_over_25 - total_higher_ed - total_bach,
    per_higher_ed = round((total_higher_ed/total_pop_over_25),2),
    per_no_higher_ed = round((no_higher_ed/total_pop_over_25), 2)
  ) |>
  select(GEOID, NAME, total_higher_ed, no_higher_ed, total_pop_over_25, per_higher_ed, 
         per_no_higher_ed)
higher_ed
saveRDS(higher_ed, "higher_ed_2022.rds")

# Family households ----------------------------------------------------------
family_households <- get_acs(
  geography = "county",
  state = state.abb_filtered,
  table = "B11003",
  year = 2022,
  output = "wide"
) |>
  rename(
    total_households = B11003_001E,
    total_married_couple = B11003_002E,
    total_married_couple_kids = B11003_003E,
    total_married_couple_no_kids = B11003_007E,
    total_single = B11003_008E
  ) |>
  mutate(
    total_single_kids = B11003_010E + B11003_016E,
    total_single_no_kids = B11003_014E + B11003_020E,
    per_parents = round((total_single_kids + total_married_couple_kids)/total_households,2),
    per_not_parents = round((total_single_no_kids + total_married_couple_no_kids)/total_households,2)
  ) |>
  select(GEOID, NAME, total_households, total_married_couple_kids, total_married_couple_no_kids, 
         total_single_kids, total_single_no_kids, per_parents, per_not_parents)
saveRDS(family_households, "family_households_2022.rds")

# Transport -------------------------------------------------------------------
transport <- get_acs(
  geography = "county",
  state = state.abb_filtered,
  table = "B08301",
  year = 2022,
  output = "wide"
) |>
  rename(
    total_transport = B08301_001E,
    total_drive = B08301_002E,
    total_public_transport = B08301_010E
  ) |>
  mutate(
    per_drive = round(total_drive/total_transport,2),
    per_public_transit = round(total_public_transport/total_transport, 2)
  ) |>
  select(GEOID, NAME, per_drive, per_public_transit) # out of all commuters to work
saveRDS(transport, "transport_2022.rds")

# Land area & population density ----------------------------------------------
# By default ALAND is in square meters so divide by 2589988 to get square miles
land_area <- get_acs(
  geography = "county",
  state = state.abb_filtered,
  year = 2022,
  table = "B01003",
  geometry = TRUE,
  keep_geo_vars = TRUE,
  output = "wide"
) |>
  rename(
    total_pop = B01003_001E
  ) |>
  mutate(
    land_area_sq_mi = ALAND / 2589988.10,
    pop_density = total_pop/land_area_sq_mi,
    pop_density = round(pop_density, 2)
  ) |>
  st_drop_geometry() |>
  select(GEOID, total_pop, land_area_sq_mi, pop_density) |>
  as_tibble()
saveRDS(land_area, "land_area_2022.rds")
