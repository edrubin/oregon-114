# Notes ----------------------------------------------------------------------------------
#   Goal:   Interactive state-month figures to check outliers in background checks
#   Time:   Very short

# Output ---------------------------------------------------------------------------------
#   none

# Setup ----------------------------------------------------------------------------------
  # Load packages
  pacman::p_load(
    here, magrittr, tidyverse, readxl, data.table, collapse, lubridate,
    ggplot2, plotly, cowplot, scales, synthdid, viridis, extrafont
  )
  # Define colors
  col_trt = viridis::magma(1e2)[40]
  col_ctl = 'grey72'
  col_qtl = viridis::magma(1e2)[c(1, 50, 70, 85)]
  # Weekday colors
  col_wk = viridis::magma(7, end = .93)
  # Civids but with purple and orange
  # col_new = c('#4A148C', '#FFCC80')
  col_new = c('#311B92', '#FFCC80')

# Define sets of states ------------------------------------------------------------------
  # Load definitions
  here('scripts', '03-run-analyses', '00-define-groups.R') |> source()


# Appendix: Long-run time series, Oregon vs. others --------------------------------------
  # Define dates of interest with labels
  date_dt =
    list(
      data.table(d = '20081104', l = 'Obama elected'),
      data.table(d = '20121214', l = 'Sandy Hook'),
      data.table(d = '20141202', l = 'San Bernardino'),
      data.table(d = '20180214', l = 'Parkland'),
      data.table(d = '20200120', l = 'COVID-19 begins'),
      # data.table(d = '20220524', l = 'Uvalde'),
      data.table(d = '20221108', l = 'Meas. 114'),
      NULL
    ) |> rbindlist()
  date_dt[, d := ymd(d)]
  # Load the data
  bgc_data =
    here('data', 'clean', 'background-checks', 'bgc-fbi-200001-202403.csv') |>
    fread()
  # Drop Alabama and North Carolina
  bgc_data %<>% .[!(state %in% drop_states)]
  # Sum BGCs and population by month and OR vs. non-OR
  bgc_dt =
    bgc_data[, .(
      state,
      year,
      month,
      bgc = totals_standard_sales,
      permit, permit_recheck, handgun, long_gun, other, multiple,
      pop = population
    )]
  # Scale everything by population
  bgc_dt =
    bgc_dt[, `:=`(
      permit = permit / pop * 1e5,
      permit_recheck = permit_recheck / pop * 1e5,
      handgun = handgun / pop * 1e5,
      long_gun = long_gun / pop * 1e5,
      other = other / pop * 1e5,
      multiple = multiple / pop * 1e5
    )]
  # Add date
  bgc_dt[, date := ymd(paste0(year, '-', month, '-01'))]
  # Drop dates to desired range
  d1 = ymd(20180301)
  dn = ymd(20240301)
  bgc_dt %<>% .[between(date, d1, dn)]
  # Impose date restrictions
  date_dt %<>% .[between(d, d1, dn)]
  # Add indicators for outliers
  bgc_dt[, `:=`(
    permit_outlier = permit > fnth(permit, .995),
    permit_recheck_outlier = permit_recheck > fnth(permit_recheck, .995),
    handgun_outlier = handgun > fnth(handgun, .999),
    long_gun_outlier = long_gun > fnth(long_gun, .999),
    other_outlier = other > fnth(other, .995),
    multiple_outlier = multiple > fnth(multiple, .999)
  )]
  bgc_dt[, `:=`(
    permit_outlier = fmax(permit_outlier),
    permit_recheck_outlier = fmax(permit_recheck_outlier),
    handgun_outlier = fmax(handgun_outlier),
    long_gun_outlier = fmax(long_gun_outlier),
    other_outlier = fmax(other_outlier),
    multiple_outlier = fmax(multiple_outlier)
  ), by = state]

  
  # Total permits
  plot_ly(
    data = bgc_dt[permit_outlier == TRUE | state == 'Oregon'],
    x = ~date,
    y = ~permit,
    color = ~state,
    colors = viridis::magma(n = 10, end = .9),
    type = 'scatter',
    mode = 'lines',
    line = list(width = 1)
  ) |>
  layout(
    xaxis = list(title = 'Month of sample'),
    yaxis = list(title = 'Background checks per 100k (FBI)'),
    legend = list(title = list(text = 'State'))
  )

  # Permit rechecks
  plot_ly(
    data = bgc_dt[permit_recheck_outlier == TRUE | state == 'Oregon'],
    x = ~date,
    y = ~permit_recheck,
    color = ~state,
    colors = viridis::magma(n = 10, end = .9),
    type = 'scatter',
    mode = 'lines',
    line = list(width = 1)
  ) |>
  layout(
    xaxis = list(title = 'Month of sample'),
    yaxis = list(title = 'Background rechecks per 100k (FBI)'),
    legend = list(title = list(text = 'State'))
  )

  # Handgun background checks
  plot_ly(
    data = bgc_dt[handgun_outlier == TRUE | state == 'Oregon'],
    x = ~date,
    y = ~handgun,
    color = ~state,
    colors = viridis::magma(n = 10, end = .9),
    type = 'scatter',
    mode = 'lines',
    line = list(width = 1)
  ) |>
  layout(
    xaxis = list(title = 'Month of sample'),
    yaxis = list(title = 'Handgun background checks per 100k (FBI)'),
    legend = list(title = list(text = 'State'))
  )

  # Long-gun background checks
  plot_ly(
    data = bgc_dt[long_gun_outlier == TRUE | state == 'Oregon'],
    x = ~date,
    y = ~long_gun,
    color = ~state,
    colors = viridis::magma(n = 10, end = .9),
    type = 'scatter',
    mode = 'lines',
    line = list(width = 1)
  ) |>
  layout(
    xaxis = list(title = 'Month of sample'),
    yaxis = list(title = 'Long-gun background checks per 100k (FBI)'),
    legend = list(title = list(text = 'State'))
  )

  # 'Other' background checks
  plot_ly(
    data = bgc_dt[other_outlier == TRUE | state == 'Oregon'],
    x = ~date,
    y = ~other,
    color = ~state,
    colors = viridis::magma(n = 10, end = .9),
    type = 'scatter',
    mode = 'lines',
    line = list(width = 1)
  ) |>
  layout(
    xaxis = list(title = 'Month of sample'),
    yaxis = list(title = 'Other background checks per 100k (FBI)'),
    legend = list(title = list(text = 'State'))
  )

  # Multiple background checks
  plot_ly(
    data = bgc_dt[multiple_outlier == TRUE | state == 'Oregon'],
    x = ~date,
    y = ~other,
    color = ~state,
    colors = viridis::magma(n = 10, end = .9),
    type = 'scatter',
    mode = 'lines',
    line = list(width = 1)
  ) |>
  layout(
    xaxis = list(title = 'Month of sample'),
    yaxis = list(title = 'Multiple background checks per 100k (FBI)'),
    legend = list(title = list(text = 'State'))
  )
