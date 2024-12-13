# Notes ----------------------------------------------------------------------------------
#   Goal:   Background checks by quartile of support for Measure 114
#   Time:   ~ 5 seconds

# Output ---------------------------------------------------------------------------------
#   data/clean/background-checks/bgc-quartile.csv

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

# Clean data -----------------------------------------------------------------------------
  # Slim down Oregon data; we will be looking at the county/monthly level
  oregon_bgc_data %<>% distinct(county, month_date, .keep_all = TRUE)
  oregon_bgc_data %<>% mutate(state = 'Oregon')
  # Slim down state data
  state_bgc_data = state_bgc_data[ state != 'Oregon', ]
  state_bgc_data %<>% mutate(place = paste(state, 'state', sep = ' '))
  state_bgc_data  %<>% select(state, place, date, rate_standard_sales)
  names(state_bgc_data) = c('state', 'place', 'date', 'rate')
  # Grab desired (quartile of support) columns
  oregon_bgc_data %<>%
    select(state, support_quantile, month_date, quartile_per_month_total) %>%
    setDT()
  # Keep four rows per month_date
  quart_data =
    oregon_bgc_data %>%
    distinct(month_date, support_quantile, .keep_all = TRUE) 
  # Set names
  names(quart_data) = c('state', 'place', 'date', 'rate')
  # Join OSP and FBI
  quart_data = rbind(state_bgc_data, quart_data) |> as.data.table()
  # Check balance
  quart_data[, .N, place]
  quart_data[, .N, date]
  # Add treatment
  quart_data[, oct_treated := if_else(date >= '2022-10-01' & state == 'Oregon', 1, 0)]
  quart_data[, july_treated := if_else(date >= '2022-07-01' & state == 'Oregon', 1, 0)]
  # Code quartile as factors with nicer labels
  quart_data[, `:=`(
    quartile = as.factor(case_when(
      place == '[11,20.8]' ~ 1,
      place == '(20.8,31]' ~ 2,
      place == '(31,43.5]' ~ 3,
      place == '(43.5,74]' ~ 4,
      TRUE ~ 0
    ))
  )]

# Save -----------------------------------------------------------------------------------
  # Save data as CSV
  write_csv(
    x = quart_data,
    file = here('data', 'clean', 'background-checks', 'bgc-quartile.csv')
  )
