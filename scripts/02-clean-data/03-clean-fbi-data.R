# Notes ----------------------------------------------------------------------------------
#   Goal:   Clean FBI background-check data
#   Time:   ~ 5 seconds

# Output ---------------------------------------------------------------------------------
#   data/clean/background-checks/bgc-fbi-200001-202403.csv

# Setup ----------------------------------------------------------------------------------
  # Load packages
  pacman::p_load(
    here, readxl, janitor, tidyr, magrittr, data.table, tidycensus, tidyverse
  )

# Load data: Background checks and population --------------------------------------------
  # Load background-check data, 1998-2023
  bgc_data =
    here('data', 'raw', 'background-checks', 'nics-bgc-202403.csv') |>
    read_csv()
  setDT(bgc_data)
  # Load state-level population data 2000-2023
  population_data =
    here('data', 'clean', 'population', 'clean-state-populations-2000-2024.csv') |>
    read_csv() |>
    setDT()

# Clean data -----------------------------------------------------------------------------
  # Subset to lower 48
  lower48 = setdiff(state.name, c('Alaska', 'Hawaii'))
  bgc_data = bgc_data[state %in% lower48, ]
  population_data = population_data[state %in% lower48, ]
  # Drop extra months from population data
  population_data = population_data[!(date %in% c(
    '2024-04', '2024-05', '2024-06', '2024-07', '2024-08', '2024-09', '2024-10',
    '2024-11', '2024-12'
  )), ]
  # Make month into a time variable
  bgc_data[, date := ym(month)]
  # Make a year and month variable out of the 'month' column
  bgc_data =
    separate_wider_delim(
      data = bgc_data,
      cols = month,
      delim = '-',
      names = c('year', 'month')
    )
  setDT(bgc_data)
  # Convert year and month to numeric
  bgc_data[, `:=`(year = as.numeric(year), month = as.numeric(month))]
  # Drop the 1998 and 1999 data to line up with Census data
  bgc_data = bgc_data[year != 1998 & year != 1999, ]

# Calculate totals -----------------------------------------------------------------------
  # Desired 'totals'
  #   - private sales: private_sale_handgun + private_sale_long_gun + private_sale_other
  #   - non-private sales
  #   - total without permitting
  #   - create a permit variable
  bgc_data[, `:=`(
    totals_private_sales =
      sum(
        private_sale_handgun, private_sale_long_gun, private_sale_other,
        na.rm = TRUE
      ),
    totals_standard_sales = sum(handgun, long_gun, other, multiple, na.rm = TRUE),
    totals_permit = sum(permit, permit_recheck, na.rm = TRUE),
    totals_no_permit =
      case_when(
        !is.na(permit) & !is.na(permit_recheck) ~ totals - permit - permit_recheck,
        !is.na(permit) & is.na(permit_recheck) ~ totals - permit,
        is.na(permit) & !is.na(permit_recheck) ~ totals - permit_recheck,
        is.na(permit) & is.na(permit_recheck) ~ totals
      )
    ),
  by = .(state, date)]
  # Indicate treated
  bgc_data[, `:=`(
    treated_june = if_else(state == 'Oregon' & date > '2022-05-01', 1, 0),
    treated_july = if_else(state == 'Oregon' & date > '2022-06-01', 1, 0),
    treated_aug = if_else(state == 'Oregon' & date > '2022-07-01', 1, 0),
    treated_sept = if_else(state == 'Oregon' & date > '2022-08-01', 1, 0),
    treated_oct = if_else(state == 'Oregon' & date > '2022-09-01', 1, 0),
    treated_nov = if_else(state == 'Oregon' & date > '2022-10-01', 1, 0)
  )]

# Combine datasets -----------------------------------------------------------------------
  # Combine Census data with the background-check data
  population_data[, month := as.numeric(month)]
  # Join
  bgc_data = left_join(
    x = bgc_data,
    y = population_data,
    by = c('year', 'month', 'state')
  )
  bgc_data = rename(bgc_data, 'date' = 'date.x')
  bgc_data[, date.y := NULL]

# Calculate per capita rates -------------------------------------------------------------
  # Create monthly per capita rates for totals, handgun, long gun, and sales
  bgc_data[, `:=`(
    rate_totals = (totals / population) * 1e5,
    rate_handgun = (handgun / population) * 1e5,
    rate_long_gun = (long_gun / population) * 1e5,
    rate_permit = (totals_permit / population) * 1e5,
    rate_no_permit = (totals_no_permit / population) * 1e5,
    rate_private_sales = (totals_private_sales / population) * 1e5,
    rate_standard_sales = (totals_standard_sales / population) * 1e5
   ), by = .(state, date)]

# Save -----------------------------------------------------------------------------------
  # Save new dataset
  write_csv(
    x = bgc_data,
    file = here('data', 'clean', 'background-checks', 'bgc-fbi-200001-202403.csv')
  )