# Notes ----------------------------------------------------------------------------------
#   Goal:   Download and clean population data for Oregon counties
#   Time:   < 1 minute

# Output ---------------------------------------------------------------------------------
#   data/clean/population/clean-or-county-pop-2018-2024.csv

# Setup ----------------------------------------------------------------------------------
  # Load packages
  pacman::p_load(
    here, readxl, janitor, tidyr, magrittr, data.table, tidyverse, tidycensus,
    lubridate, zoo
  )

# Load OR county population totals -------------------------------------------------------
  # Load OR population data
  pop_data_2020_2023 =
    here('data', 'raw', 'population', 'or-county-populations-2018-2024.xlsx') |>
    read_xlsx(skip = 3)
  # Clean names
  names(pop_data_2020_2023) = c('NAME', 'redundant', '2020', '2021', '2022', '2023')
  # Select relevant columns
  pop_data_2020_2023 %<>% select(NAME, '2020', '2021', '2022', '2023')
  # Remove non-US entries
  pop_data_2020_2023 %<>% filter(NAME != 'United States')
  # Remove leading period from county names
  pop_data_2020_2023 %<>% mutate(NAME = gsub('\\.', '', NAME))
  # Only keep counties
  pop_data_2020_2023 %<>% filter(str_detect(NAME, pattern = 'County'))
  # Separate county and state names
  pop_data_2020_2023 =
    separate_wider_delim(
      data = pop_data_2020_2023,
      cols = NAME,
      delim = ' County, ',
      names = c('county', 'state')
    )

# Load ACS data --------------------------------------------------------------------------
  # Loading ACS data for 2018-2021
  acs5_variables = tidycensus::load_variables(2021, 'acs5')
  # Function: For given year, get population data
  census_function = function(date) {
    data = get_acs(
      geography = 'county',
      variables = 'B01003_001',
      year = date,
      dataset = 'acs1'
    )
    data %>% mutate(year = date)
  }
  # Get population data for 2018-2019
  pop_data_2018_2019 = do.call(rbind, lapply(2018:2019, census_function))
  # Drop unnecessary columns
  pop_data_2018_2019 %<>% select(-variable, -moe)
  # Filter to counties
  pop_data_2018_2019 %<>% filter(str_detect(NAME, pattern = 'County'))
  # Separate county and state names
  pop_data_2018_2019 =
    separate_wider_delim(
      data = pop_data_2018_2019,
      cols = NAME,
      delim = ' County, ',
      names = c('county', 'state')
    )
  # Pivot data to wide format
  pop_data_2018_2019 %<>% pivot_wider(names_from = year, values_from = estimate)
  # Filter to Oregon
  pop_data_2018_2019 %<>% filter(state == 'Oregon')

# Combine datasets -----------------------------------------------------------------------
  # Join datasets
  pop_data_2018_2023 =
    inner_join(
      x = pop_data_2018_2019,
      y = pop_data_2020_2023,
      by = c('state', 'county')
    )
  # Convert to data.table
  setDT(pop_data_2018_2023)

# Interpolate monthly data ---------------------------------------------------------------
  # Pivot to long
  population_data =
    pop_data_2018_2023 |>
    pivot_longer(names_to = 'year', values_to = 'population', cols = c(4:9))
  setDT(population_data)
  # Add months
  population_data =
    population_data[, .(
      GEOID = GEOID,
      population = population,
      month = c('01', '02', '03', '04', '05', '06', '07', '08', '09', '10', '11', '12')
    ), by = .(county, year, state)]
  # Create dates
  population_data[, date := paste(year, month, sep = '-')]
  # Drop population on non-January months
  population_data[, population := case_when(
    date == '2018-01' ~ population,
    date == '2019-01' ~ population,
    date == '2020-01' ~ population,
    date == '2021-01' ~ population,
    date == '2022-01' ~ population,
    date == '2023-12' ~ population,
  )]
  # Order
  population_data = population_data[order(county, state, year, month), ]
  # Interpolate
  population_data[, `:=`(
    interpolated_population = zoo::na.approx(population)
  ), .(county, state)]
  # To wide
  temp =
    pivot_wider(
      population_data,
      id_cols = c(state, county, GEOID),
      names_from = date,
      values_from = interpolated_population,
      values_fill = 0
    ) %>% setDT()
  # Growth rate
  temp[, growth_rate := ((`2023-12` / `2018-01`)^(1 / 72) - 1), .(county, state)]
  temp[, `:=`(
    `2024-01` = `2022-12` * (1 + growth_rate),
    `2024-02` = `2022-12` * (1 + growth_rate)^2,
    `2024-03` = `2022-12` * (1 + growth_rate)^3,
    `2024-04` = `2022-12` * (1 + growth_rate)^4,
    `2024-05` = `2022-12` * (1 + growth_rate)^5,
    `2024-06` = `2022-12` * (1 + growth_rate)^6,
    `2024-07` = `2022-12` * (1 + growth_rate)^7,
    `2024-08` = `2022-12` * (1 + growth_rate)^8,
    `2024-09` = `2022-12` * (1 + growth_rate)^9,
    `2024-10` = `2022-12` * (1 + growth_rate)^10,
    `2024-11` = `2022-12` * (1 + growth_rate)^11,
    `2024-12` = `2022-12` * (1 + growth_rate)^12
  ), .(county, state)]
  temp %<>% select(-growth_rate)
  # Final data
  final_population_data =
    temp %>%
    pivot_longer(names_to = 'date', values_to = 'population', cols = c(4:length(temp)))
  # Separate date into year and month
final_population_data %<>%
  separate_wider_delim(
    cols = date,
    delim = '-',
    names = c('year', 'month'),
    cols_remove = FALSE
  )
  setDT(final_population_data)
  # Round population
  final_population_data[, population := round(population, 0)]
  # Convert to date
  final_population_data[, date := ym(date)]
  # Rename and calculate state population
  final_population_data %<>% rename(county_population = population)
  final_population_data[, state_population := sum(county_population), .(state, date)]
  # Save
  write_csv(
    x = final_population_data,
    file = here('data', 'clean', 'population', 'clean-or-county-pop-2018-2024.csv')
  )