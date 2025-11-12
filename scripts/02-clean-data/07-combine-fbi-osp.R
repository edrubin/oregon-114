# Notes ----------------------------------------------------------------------------------
#   Goal:   Combine datasets; create analysis-ready background-check dataset
#   Time:   ~ 5 seconds

# Output ---------------------------------------------------------------------------------
#   data/clean/background-checks/bgc-osp-fbi.csv

# Setup ----------------------------------------------------------------------------------
  # Load packages
  pacman::p_load(
    here, tidyverse, readxl, janitor, tidyr, magrittr, data.table, tidycensus, lubridate
  )

# Load data ------------------------------------------------------------------------------
  # Load FBI (state-level) data
  state_bgc_data =
    here('data', 'clean', 'background-checks', 'bgc-fbi-200001-202403.csv') |>
    read_csv() |>
    setDT()
  # Restrict to dates covered by OSP data (2018-02 to 2024-04)
  state_bgc_data %<>% .[date > '2018-02-01' & date < '2024-04-01']
  # Load OSP (county-level) data
  oregon_bgc_data =
    here('data', 'clean', 'background-checks', 'bgc-osp-201802-202404.csv') |>
    read_csv() |>
    setDT()
  # Match dates
  oregon_bgc_data %<>% .[month_date > '2018-02-01' & month_date < '2024-04-01']

# Clean and combine background-check datasets --------------------------------------------
  # Keeping one entry per month-date for Oregon; replace FBI Oregon with OSP Oregon
  # Slim down Oregon data (we just need county-month)
  oregon_bgc_data %<>% distinct(month_date, .keep_all = TRUE)
  oregon_bgc_data %<>%
    transmute(
      state = 'Oregon',
      date = month_date,
      rate = per_month_total,
      pop = state_population
    ) %>%
    setDT()
  # Slim down FBI data
  state_bgc_data = state_bgc_data[state != 'Oregon', ]
  state_bgc_data %<>% transmute(
    state,
    date,
    rate = rate_standard_sales,
    pop = population
  )
  # Combine
  bgc_data =
    rbindlist(
      list(state_bgc_data, oregon_bgc_data),
      use.names = TRUE,
      fill = TRUE
    )
  # Define treatment
  bgc_data[, `:=`(
    treated_october = if_else(state == 'Oregon' & date >= '2022-10-01', 1, 0),
    treated_september = if_else(state == 'Oregon' & date >= '2022-09-01', 1, 0),
    treated_november = if_else(state == 'Oregon' & date >= '2022-11-01', 1, 0),
    treated_placebo_year = if_else(state == 'Oregon' & date >= '2021-10-01', 1, 0)
  )]

# Save -----------------------------------------------------------------------------------
  # Save data as CSV
  write_csv(
    x = bgc_data,
    file = here('data', 'clean', 'background-checks', 'bgc-osp-fbi.csv')
  )