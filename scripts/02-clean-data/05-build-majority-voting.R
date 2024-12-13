# Notes ----------------------------------------------------------------------------------
#   Goal:   Background checks by county majority/minority support for Measure 114
#   Time:   ~ 5 seconds

# Output ---------------------------------------------------------------------------------
#   data/clean/background-checks/bgc-majority.csv

# Setup ----------------------------------------------------------------------------------
  # Load packages
  pacman::p_load(
    here, readxl, janitor, tidyr, magrittr, data.table, tidycensus,
    tidyverse, gsynth, ggpubr, cowplot, eventstudyr, fixest
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
  state_bgc_data = state_bgc_data[state != 'Oregon']
  state_bgc_data %<>% mutate(place = paste(state, 'state', sep = ' '))
  state_bgc_data  %<>% select(state, place, date, rate_standard_sales)
  names(state_bgc_data) = c('state', 'place', 'date', 'rate')
  # Grab desired (majority) columns
  oregon_bgc_data %<>%
    select(state, majority_yes, month_date, majority_per_month_total) %>%
    setDT()
  # Keep 2 rows per month_date
  maj_data = oregon_bgc_data %>% distinct(month_date, majority_yes, .keep_all = TRUE) 
  maj_data %<>%
  mutate(majority_yes = if_else(majority_yes == 1, 'MAJORITY YES', "MAJORITY NO"))
  # Set names
  names(maj_data) = c('state', 'place', 'date', 'rate')
  # Join OSP and FBI
  maj_data = rbind(state_bgc_data, maj_data) |> as.data.table()
  # Check balance
  maj_data[, .N, place]
  maj_data[, .N, date]
  # Add treatment
  maj_data[, oct_treated := if_else(date >= '2022-10-01' & state == 'Oregon', 1, 0)]
  maj_data[, july_treated := if_else(date >= '2022-07-01' & state == 'Oregon', 1, 0)]

# Save -----------------------------------------------------------------------------------
  # Save data as CSV
  write_csv(
    x = maj_data,
    file = here('data', 'clean', 'background-checks', 'bgc-majority.csv')
  )
