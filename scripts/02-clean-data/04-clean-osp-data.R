# Notes ----------------------------------------------------------------------------------
#   Goal:   Clean FBI background-check data
#   Time:   ~ 10 seconds

# Output ---------------------------------------------------------------------------------
#   data/clean/bgc-osp-201802-202404.csv

# Data notes -----------------------------------------------------------------------------
#   There is a 'submitted' category that only applies to the final 7 days of data.
#   We will remove these days, as they aren't broadly relevant. The same is true for the
#   "in process" category. Both occur in May/June of 2024. We drop these months.

# Setup ----------------------------------------------------------------------------------
  # Load packages
  pacman::p_load(
    here, readxl, janitor, tidyr, magrittr, data.table, tidycensus, tidyverse
  )

# Load data ------------------------------------------------------------------------------
  # Oregon OSP background checks
  osp_bgc_data_2_27_2018 =
    here('data', 'raw', 'background-checks', 'oregon-background-checks.csv') |>
    read_csv() |>
    setDT() |>
    clean_names()
  osp_bgc_data_6_4_2019 =
    here('data', 'raw', 'background-checks', 'oregon-background-checks-202406.csv') |>
    read_csv() |>
    setDT() |>
    clean_names()
  # County population data
  county_pop =
    here('data', 'clean', 'population', 'clean-or-county-pop-2018-2024.csv') |>
    read_csv() |>
    setDT()
  # Measure 114 support
  measure_114_support =
    here('data', 'raw', 'election-results-114', 'election-results-nyt.xlsx') |>
    read_xlsx() |>
    clean_names() |>
    setDT()
  # Oregon regions
  oregon_regions =
    here('data', 'raw', 'oregon-dot', 'oregon-regions.xlsx') |>
    read_xlsx(sheet = 2) |>
    clean_names() |>
    setDT()

# Clean data -----------------------------------------------------------------------------
  # Create quantiles for voter support of measure 114
  measure_114_support[, `:=`(
    support_quantile = cut_number(yes, n = 4),
    majority_yes = if_else(yes >= 50, 1, 0)
  )]
  measure_114_support[, percent_of_votes_in_percent_in := NULL]
  # Reformat county names in region crosswalk for eventual join
  oregon_regions %<>% mutate(county = gsub(' County', '', countyname))
  # Change names of newer OSP data
  names(osp_bgc_data_6_4_2019) = names(osp_bgc_data_2_27_2018) 
  # Reformat date
  osp_bgc_data_6_4_2019 %<>% mutate(date = mdy(date))
  # Trim both data to the right date windows
  osp_bgc_data_2_27_2018 %<>% filter(between(date, ymd('2018-02-27'), ymd('2019-06-03')))
  # Combine datasets (stack)
  bgc_data = rbindlist(list(osp_bgc_data_2_27_2018, osp_bgc_data_6_4_2019))
  setorder(bgc_data, date, county)
  # Harmonize background-check status capitalization
  bgc_data[, status := toupper(status)]
# NOTE There are 3936 canceled BGCs on 12/13/22 and 12/14/22
  bgc_data %<>% .[county != 'Grand Total:' & county != 'Unknown' & county != 'None']
  # To wide
  bgc_data %<>%
    pivot_wider(names_from = status, values_from = number) %>%
    clean_names()
  bgc_data %<>% setDT()
  # Limit dates before May 2024 (May was in progress)
  bgc_data %<>% filter(date <= '2024-04-30')
  bgc_data %<>% select(-submitted, -in_process)
  # balance panel
  bgc_data %<>% complete(nesting(county), date = full_seq(date, period = 1))
  setDT(bgc_data)
  # Replace NAs with 0
  bgc_data[, `:=`(
    approved = if_else(is.na(approved) == TRUE, 0, approved),
    canceled = if_else(is.na(canceled) == TRUE, 0, canceled),
    denied   = if_else(is.na(denied) == TRUE, 0, denied),
    pending  = if_else(is.na(pending) == TRUE, 0, pending)
  )]
  # Add a year, month
  bgc_data =
    separate_wider_delim(
      data = bgc_data,
      cols = date, delim = '-',
      names = c('year', 'month', 'day'),
      cols_remove = FALSE
    ) |> setDT()
  # Add month (first of month)
  bgc_data[, month_date := ym(paste(year, month, sep = '-'))]
  # Add a day of the week label
  bgc_data[, week_day := lubridate::wday(date, label = TRUE)]
  # Add a week index first week of data = 1, last week of date = total days/7
  bgc_data[, week_index := floor_date(date, unit = '1 week'), county]
  bgc_data[, `:=`(
    week_index_tuesday = floor_date(
      date,
      unit = '1 week',
      week_start = getOption('lubridate.week.start', 'Tuesday')
    )
  ), county]
  # Add population
  county_pop$year %<>% as.character()
  bgc_data = merge.data.table(
    x = bgc_data,
    y = county_pop,
    by.x = c('county', 'year', 'month', 'month_date'),
    by.y = c('county', 'year', 'month', 'date')
  )
  # Add measure 114 voting/support data
  bgc_data = merge.data.table(
    x = bgc_data,
    y = measure_114_support,
    by = 'county'
  )
  # Add Oregon regions
  bgc_data = merge.data.table(
    x = bgc_data,
    y = oregon_regions,
    by = 'county',
  )
  # Add population by majority, quantile, and region
  bgc_data[, majority_population := sum(county_population), .(date, majority_yes)]
  bgc_data[, quartile_population := sum(county_population), .(date, support_quantile)]
  bgc_data[, region_population := sum(county_population), .(date, region_dot)]
  # Rearrange columns
  bgc_data %<>% select(
    county, GEOID, year, month, month_date, date, week_day, week_index,
    state_population, county_population,
    majority_population, quartile_population, region_population,
    yes, no, total_votes, support_quantile, majority_yes, region_dot,
    approved, canceled, denied, pending
  )
  # To factor
  bgc_data$majority_yes %<>% as.factor()
  bgc_data$region_dot %<>% as.factor()
  setDT(bgc_data)

# Time by county calculations ------------------------------------------------------------
# NOTE Many of these are redundant, but helps with book keeping
  # By county-day: Total, approved, canceled, denied, pending
  bgc_data[, `:=`(
    county_day_total = sum(approved, canceled, denied, pending),
    county_day_approved = sum(approved),
    county_day_canceled = sum(canceled),
    county_day_denied = sum(denied),
    county_day_pending = sum(pending),
    per_county_day_total =
      ((sum(approved, canceled, denied, pending) / county_population) * 1e5) %>% round(2),
    per_county_day_approved =
      ((sum(approved) / county_population) * 1e5) %>% round(2),
    per_county_day_canceled =
      ((sum(canceled) / county_population) * 1e5) %>% round(2),
    per_county_day_denied =
      ((sum(denied) / county_population) * 1e5) %>% round(2),
    per_county_day_pending =
      ((sum(pending) / county_population) * 1e5) %>% round(2)
  ), by = .(county, date)]
  # By county-month: total, approved, canceled, denied, pending
  bgc_data[, `:=`(
    county_month_total = sum(approved, canceled, denied, pending),
    county_month_approved = sum(approved),
    county_month_canceled = sum(canceled),
    county_month_denied = sum(denied),
    county_month_pending = sum(pending),
    per_county_month_total =
      ((sum(approved, canceled, denied, pending) / county_population) * 1e5) %>% round(2),
    per_county_month_approved =
      ((sum(approved) / county_population) * 1e5) %>% round(2),
    per_county_month_canceled =
      ((sum(canceled) / county_population) * 1e5) %>% round(2),
    per_county_month_denied =
      ((sum(denied) / county_population) * 1e5) %>% round(2),
    per_county_month_pending =
      ((sum(pending) / county_population) * 1e5) %>% round(2)
  ), by = .(county, month_date)]
  # By county-week: total, approved, canceled, denied, pending
  bgc_data[, `:=`(
    county_week_total = sum(approved, canceled, denied, pending),
    county_week_approved = sum(approved),
    county_week_canceled = sum(canceled),
    county_week_denied = sum(denied),
    county_week_pending = sum(pending),
    per_county_week_total =
      ((sum(approved, canceled, denied, pending) / county_population) * 1e5) %>% round(2),
    per_county_week_approved =
      ((sum(approved) / county_population) * 1e5) %>% round(2),
    per_county_week_canceled =
      ((sum(canceled) / county_population) * 1e5) %>% round(2),
    per_county_week_denied =
      ((sum(denied) / county_population) * 1e5) %>% round(2),
    per_county_week_pending =
      ((sum(pending) / county_population) * 1e5) %>% round(2)
  ), by = .(county, week_index)]
  # By day: total, approved, canceled, denied, pending
  bgc_data[, `:=`(
    day_total = sum(approved, canceled, denied, pending),
    day_approved = sum(approved),
    day_canceled = sum(canceled),
    day_denied = sum(denied),
    day_pending = sum(pending),
    per_day_total =
      ((sum(approved, canceled, denied, pending) / state_population) * 1e5) %>% round(2),
    per_day_approved =
      ((sum(approved) / state_population) * 1e5) %>% round(2),
    per_day_canceled =
      ((sum(canceled) / state_population) * 1e5) %>% round(2),
    per_day_denied =
      ((sum(denied) / state_population) * 1e5) %>% round(2),
    per_day_pending =
      ((sum(pending) / state_population) * 1e5) %>% round(2)
  ), by = .(date)]
  # By month: total, approved, canceled, denied, pending
  bgc_data[, `:=`(
    month_total = sum(approved, canceled, denied, pending),
    month_approved = sum(approved),
    month_canceled = sum(canceled),
    month_denied = sum(denied),
    month_pending = sum(pending),
    per_month_total =
      ((sum(approved, canceled, denied, pending) / state_population) * 1e5) %>% round(2),
    per_month_approved =
      ((sum(approved) / state_population) * 1e5) %>% round(2),
    per_month_canceled =
      ((sum(canceled) / state_population) * 1e5) %>% round(2),
    per_month_denied =
      ((sum(denied) / state_population) * 1e5) %>% round(2),
    per_month_pending =
      ((sum(pending) / state_population) * 1e5) %>% round(2)
  ), by = .(month_date)]
  # By week: total, approved, canceled, denied, pending
  bgc_data[, `:=`(
    week_total = sum(approved, canceled, denied, pending),
    week_approved = sum(approved),
    week_canceled = sum(canceled),
    week_denied = sum(denied),
    week_pending = sum(pending),
    per_week_total =
      ((sum(approved, canceled, denied, pending) / state_population) * 1e5) %>% round(2),
    per_week_approved =
      ((sum(approved) / state_population) * 1e5) %>% round(2),
    per_week_canceled =
      ((sum(canceled) / state_population) * 1e5) %>% round(2),
    per_week_denied =
      ((sum(denied) / state_population) * 1e5) %>% round(2),
    per_week_pending =
      ((sum(pending) / state_population) * 1e5) %>% round(2)
  ), by = .(week_index)]

# Time by vote-share ---------------------------------------------------------------------
  # By day-majority: total, approved, canceled, denied, pending
  bgc_data[, `:=`(
    majority_day_total = sum(approved, canceled, denied, pending),
    majority_day_approved = sum(approved),
    majority_day_canceled = sum(canceled),
    majority_day_denied = sum(denied),
    majority_day_pending = sum(pending),
    majority_per_day_total =
      ((sum(approved, canceled, denied, pending) / majority_population) * 1e5) %>% round(2),
    majority_per_day_approved =
      ((sum(approved) / majority_population) * 1e5) %>% round(2),
    majority_per_day_canceled =
      ((sum(canceled) / majority_population) * 1e5) %>% round(2),
    majority_per_day_denied =
      ((sum(denied) / majority_population) * 1e5) %>% round(2),
    majority_per_day_pending =
      ((sum(pending) / majority_population) * 1e5) %>% round(2)
  ), by = .(date, majority_yes)]
  # By month and majority: total, approved, canceled, denied, pending
  bgc_data[, `:=`(
    majority_month_total = sum(approved, canceled, denied, pending),
    majority_month_approved = sum(approved),
    majority_month_canceled = sum(canceled),
    majority_month_denied = sum(denied),
    majority_month_pending = sum(pending),
    majority_per_month_total =
      ((sum(approved, canceled, denied, pending) / majority_population) * 1e5) %>% round(2),
    majority_per_month_approved =
      ((sum(approved) / majority_population) * 1e5) %>% round(2),
    majority_per_month_canceled =
      ((sum(canceled) / majority_population) * 1e5) %>% round(2),
    majority_per_month_denied =
      ((sum(denied) / majority_population) * 1e5) %>% round(2),
    majority_per_month_pending =
      ((sum(pending) / majority_population) * 1e5) %>% round(2)
  ), by = .(month_date, majority_yes)]
  # By week and majority: total, approved, canceled, denied, pending
  bgc_data[, `:=`(
    majority_week_total = sum(approved, canceled, denied, pending),
    majority_week_approved = sum(approved),
    majority_week_canceled = sum(canceled),
    majority_week_denied = sum(denied),
    majority_week_pending = sum(pending),
    majority_per_week_total =
      ((sum(approved, canceled, denied, pending) / majority_population) * 1e5) %>% round(2),
    majority_per_week_approved =
      ((sum(approved) / majority_population) * 1e5) %>% round(2),
    majority_per_week_canceled =
      ((sum(canceled) / majority_population) * 1e5) %>% round(2),
    majority_per_week_denied =
      ((sum(denied) / majority_population) * 1e5) %>% round(2),
    majority_per_week_pending =
      ((sum(pending) / majority_population) * 1e5) %>% round(2)
  ), by = .(week_index, majority_yes)]
  # By day and vote quartile: total, approved, canceled, denied, pending
  bgc_data[, `:=`(
    quartile_day_total = sum(approved, canceled, denied, pending),
    quartile_day_approved = sum(approved),
    quartile_day_canceled = sum(canceled),
    quartile_day_denied = sum(denied),
    quartile_day_pending = sum(pending),
    quartile_per_day_total =
      ((sum(approved, canceled, denied, pending) / quartile_population) * 1e5) %>% round(2),
    quartile_per_day_approved =
      ((sum(approved) / quartile_population) * 1e5) %>% round(2),
    quartile_per_day_canceled =
      ((sum(canceled) / quartile_population) * 1e5) %>% round(2),
    quartile_per_day_denied =
      ((sum(denied) / quartile_population) * 1e5) %>% round(2),
    quartile_per_day_pending =
      ((sum(pending) / quartile_population) * 1e5) %>% round(2)
  ), by = .(date, support_quantile)]
  # By month and vote quartile: total, approved, canceled, denied, pending
  bgc_data[, `:=`(
    quartile_month_total = sum(approved, canceled, denied, pending),
    quartile_month_approved = sum(approved),
    quartile_month_canceled = sum(canceled),
    quartile_month_denied = sum(denied),
    quartile_month_pending = sum(pending),
    quartile_per_month_total =
      ((sum(approved, canceled, denied, pending) / quartile_population) * 1e5) %>% round(2),
    quartile_per_month_approved =
      ((sum(approved) / quartile_population) * 1e5) %>% round(2),
    quartile_per_month_canceled =
      ((sum(canceled) / quartile_population) * 1e5) %>% round(2),
    quartile_per_month_denied =
      ((sum(denied) / quartile_population) * 1e5) %>% round(2),
    quartile_per_month_pending =
      ((sum(pending) / quartile_population) * 1e5) %>% round(2)
  ), by = .(month_date, support_quantile)]
  # By week and vote quartile: total, approved, canceled, denied, pending
  bgc_data[, `:=`(
    quartile_week_total = sum(approved, canceled, denied, pending),
    quartile_week_approved = sum(approved),
    quartile_week_canceled = sum(canceled),
    quartile_week_denied = sum(denied),
    quartile_week_pending = sum(pending),
    quartile_per_week_total =
      ((sum(approved, canceled, denied, pending) / quartile_population) * 1e5) %>% round(2),
    quartile_per_week_approved =
      ((sum(approved) / quartile_population) * 1e5) %>% round(2),
    quartile_per_week_canceled =
      ((sum(canceled) / quartile_population) * 1e5) %>% round(2),
    quartile_per_week_denied =
      ((sum(denied) / quartile_population) * 1e5) %>% round(2),
    quartile_per_week_pending =
      ((sum(pending) / quartile_population) * 1e5) %>% round(2)
    ), by = .(week_index, support_quantile)]
  # By day and vote region: total, approved, canceled, denied, pending
  bgc_data[, `:=`(
    region_day_total = sum(approved, canceled, denied, pending),
    region_day_approved = sum(approved),
    region_day_canceled = sum(canceled),
    region_day_denied = sum(denied),
    region_day_pending = sum(pending),
    region_per_day_total =
      ((sum(approved, canceled, denied, pending) / region_population) * 1e5) %>% round(2),
    region_per_day_approved =
      ((sum(approved) / region_population) * 1e5) %>% round(2),
    region_per_day_canceled =
      ((sum(canceled) / region_population) * 1e5) %>% round(2),
    region_per_day_denied =
      ((sum(denied) / region_population) * 1e5) %>% round(2),
    region_per_day_pending =
      ((sum(pending) / region_population) * 1e5) %>% round(2)
  ), by = .(date, region_dot)]
  # By month and vote region: total, approved, canceled, denied, pending
  bgc_data[, `:=`(
    region_month_total = sum(approved, canceled, denied, pending),
    region_month_approved = sum(approved),
    region_month_canceled = sum(canceled),
    region_month_denied = sum(denied),
    region_month_pending = sum(pending),
    region_per_month_total =
      ((sum(approved, canceled, denied, pending) / region_population) * 1e5) %>% round(2),
    region_per_month_approved =
      ((sum(approved) / region_population) * 1e5) %>% round(2),
    region_per_month_canceled =
      ((sum(canceled) / region_population) * 1e5) %>% round(2),
    region_per_month_denied =
      ((sum(denied) / region_population) * 1e5) %>% round(2),
    region_per_month_pending =
      ((sum(pending) / region_population) * 1e5) %>% round(2)
  ), by = .(month_date, region_dot)]
  # By week and vote region: total, approved, canceled, denied, pending
  bgc_data[, `:=`(
    region_week_total = sum(approved, canceled, denied, pending),
    region_week_approved = sum(approved),
    region_week_canceled = sum(canceled),
    region_week_denied = sum(denied),
    region_week_pending = sum(pending),
    region_per_week_total =
      ((sum(approved, canceled, denied, pending) / region_population) * 1e5) %>% round(2),
    region_per_week_approved =
      ((sum(approved) / region_population) * 1e5) %>% round(2),
    region_per_week_canceled =
      ((sum(canceled) / region_population) * 1e5) %>% round(2),
    region_per_week_denied =
      ((sum(denied) / region_population) * 1e5) %>% round(2),
    region_per_week_pending =
      ((sum(pending) / region_population) * 1e5) %>% round(2)
  ), by = .(week_index, region_dot)]

# Save -----------------------------------------------------------------------------------
  # Save as CSV
  write_csv(
    x = bgc_data,
    file = here('data', 'clean', 'background-checks', 'bgc-osp-201802-202404.csv')
  )