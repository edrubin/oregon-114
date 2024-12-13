# Notes ----------------------------------------------------------------------------------
#   Goal:   Clean state population data
#   Time:   < 1 minute

# Output ---------------------------------------------------------------------------------
#   data/clean/population/clean-state-populations-2000-2024.csv

# Setup ----------------------------------------------------------------------------------
  # Load packages
  pacman::p_load(
    here, data.table, tidyverse, readxl, janitor, tidyr, magrittr, tidycensus, zoo
  )

# Load state-population data, 2000-2010 --------------------------------------------------
  # Load data for 2000-2010
  pop_data_2000_2010 =
    here('data', 'raw', 'population', 'state-population-2000-2010.xls') |>
    read_xls(skip = 3)
  # Clean names
  names(pop_data_2000_2010) = c(
    'state', '2000.1', '2000', '2001', '2002', '2003', '2004', '2005', '2006',
    '2007', '2008', '2009', '2010', '2010.1'
  )
  # Drop unwanted columns
  pop_data_2000_2010 %<>% select(-'2000.1', -'2010.1')
  # Select state populations
  pop_data_2000_2010 %<>% .[6:56, ]
  # Remove leading period from state names
  pop_data_2000_2010 %<>% mutate(state = gsub('\\.', '', state))

# Load state-population data, 2020-2023 --------------------------------------------------
  # Load 2020-2023 data
  pop_data_2023 =
    here('data', 'raw', 'population', 'state-population-2020-2023.csv') |>
    read_csv()
  # Grab relevant columns
  pop_data_2023 %<>% select(NAME, POPESTIMATE2023)
  # There are multiple counts for 2020 census pops; just keep one
  names(pop_data_2023) = c('state', '2023')
  # Get states
  pop_data_2023 = pop_data_2023[15:65, ]
  # Remove leading period from state names
  pop_data_2023 %<>% mutate(state = gsub('\\.', '', state))

# Load ACS data --------------------------------------------------------------------------
  # Find ACS variables
  acs5_variables = tidycensus::load_variables(2021, 'acs5')
  # Function: For given year, get population data
  census_function = function(date){
    data = get_acs(
      geography = 'state',
      variables = 'B01003_001',
      year = date,
      dataset = 'acs1'
    )
    data %<>% mutate(year = date)
  }
  # Run the function for 2011-2022
  pop_data_2011_2022 = do.call(rbind, lapply(2011:2022, census_function))
  # Drop unnecessary columns
  pop_data_2011_2022 %<>% select(-variable, -moe)
  # Pivot data wide
  pop_data_2011_2022 %<>% pivot_wider(names_from = year, values_from = estimate)

# Combine datasets -----------------------------------------------------------------------
  # Join
  temp =
    inner_join(pop_data_2000_2010, pop_data_2011_2022, by = c('state' = 'NAME'))
  state_population_data_2000_2023 =
    inner_join(temp, pop_data_2023, by = c('state' = 'state'))
  state_population_data_2000_2023 %<>% select(GEOID, everything()) 
  state_population_data_2000_2023 %<>% setDT()

# Interpolate ----------------------------------------------------------------------------
  # Interpolate months
  population_data =
    state_population_data_2000_2023 |>
    pivot_longer(names_to = 'year', values_to = 'population', cols = c(3:26))
  setDT(population_data)
  # Create months
  population_data = population_data[, .(
    GEOID = GEOID,
    population = population,
    month = c('01', '02', '03', '04', '05', '06', '07', '08', '09', '10', '11', '12')
  ), by = .(year, state)]
  population_data[, date := paste(year, month, sep = '-')]
  population_data[, population := case_when(
    date == '2000-01' ~ population,
    date == '2001-01' ~ population,
    date == '2002-01' ~ population,
    date == '2003-01' ~ population,
    date == '2004-01' ~ population,
    date == '2005-01' ~ population,
    date == '2006-01' ~ population,
    date == '2007-01' ~ population,
    date == '2008-01' ~ population,
    date == '2009-01' ~ population,
    date == '2010-01' ~ population,
    date == '2011-01' ~ population,
    date == '2012-01' ~ population,
    date == '2013-01' ~ population,
    date == '2014-01' ~ population,
    date == '2015-01' ~ population,
    date == '2016-01' ~ population,
    date == '2017-01' ~ population,
    date == '2018-01' ~ population,
    date == '2019-01' ~ population,
    date == '2020-01' ~ population,
    date == '2021-01' ~ population,
    date == '2022-01' ~ population,
    date == '2023-12' ~ population,
    )]
  population_data = population_data[order(state, year, month), ]
  # Interpolate
  population_data[, interpolated_population := zoo::na.approx(population), state]
  # To wide
  temp = pivot_wider(
    population_data,
    id_cols = c(state, GEOID),
    names_from = date,
    values_from = interpolated_population,
    values_fill = 0
  ) %>% setDT()
  # Growth rates
  temp[, growth_rate := ((`2023-12` / `2018-01`)^(1 / 72) - 1), state]
  temp[, `:=`(
    `2024-01` = `2023-12` * (1 + growth_rate),
    `2024-02` = `2023-12` * (1 + growth_rate)^2,
    `2024-03` = `2023-12` * (1 + growth_rate)^3,
    `2024-04` = `2023-12` * (1 + growth_rate)^4,
    `2024-05` = `2023-12` * (1 + growth_rate)^5,
    `2024-06` = `2023-12` * (1 + growth_rate)^6,
    `2024-07` = `2023-12` * (1 + growth_rate)^7,
    `2024-08` = `2023-12` * (1 + growth_rate)^8,
    `2024-09` = `2023-12` * (1 + growth_rate)^9,
    `2024-10` = `2023-12` * (1 + growth_rate)^10,
    `2024-11` = `2023-12` * (1 + growth_rate)^11,
    `2024-12` = `2023-12` * (1 + growth_rate)^12
  ), state]
  temp %<>% select(-growth_rate)
  # Final dataset
  final_population_data =
    temp |>
    pivot_longer(
      names_to = 'date',
      values_to = 'population',
      cols = c(3:length(temp))
    ) |>
    setDT()
  # Split date into year and month
  final_population_data %<>%
    separate_wider_delim(
      cols = date,
      delim = '-',
      names = c('year', 'month'),
      cols_remove = FALSE
    )
  # Plot to confirm plausibility
  # ggplot(data = final_population_data) +
  #   geom_line(aes(x = ym(date), y = population, color = state)) +
  #   geom_vline(xintercept = ym('2023-12'), color = 'black') +
  #   theme_minimal()
  # Save data
  write_csv(
    x = final_population_data,
    file = here(
      'data', 'clean', 'population',
      'clean-state-populations-2000-2024.csv'
    )
  )
