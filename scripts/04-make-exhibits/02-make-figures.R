# Notes ----------------------------------------------------------------------------------
#   Goal:   Make main figures
#   Time:   ???

# Output ---------------------------------------------------------------------------------
#   exhibits/figures/*

# Setup ----------------------------------------------------------------------------------
  # Load packages
  pacman::p_load(
    here, magrittr, data.table, lubridate,
    ggplot2, cowplot, scales, synthdid, viridis
  )
  # Define colors
  col_trt = viridis::magma(1e2)[40]
  col_ctl = 'grey72'
  col_qtl = viridis::magma(1e2)[c(1, 50, 70, 85)]

# Define sets of states ------------------------------------------------------------------
  # Load definitions
  here('scripts', '03-run-analyses', '00-define-groups.R') |> source()

# Figure: Google Trends ------------------------------------------------------------------
  # Define trends location
  dir_trends = here('data', 'raw', 'google-trends')
  # Load Google Trends datasets
  # d1 = here(dir_trends, 'trends-firearm-or.csv') |> fread()
  d1 = here(dir_trends, 'trends-backgroundcheck-or.csv') |> fread()
  d2 = here(dir_trends, 'trends-magazine(firearms)-or.csv') |> fread()
  d3 = here(dir_trends, 'trends-measure114-or.csv') |> fread()
  d4 = here(dir_trends, 'trends-gunstore-or.csv') |> fread()
  d5 = here(dir_trends, 'trends-2ndamendment-or.csv') |> fread()
  d6 = here(dir_trends, 'trends-gunsafety-or.csv') |> fread()
  d7 = here(dir_trends, 'trends-uvalde-or.csv') |> fread()
  d8 = here(dir_trends, 'trends-gun-or.csv') |> fread()
  # Fix names
  setnames(d1, new = c('week', 'interest'))
  setnames(d2, new = c('week', 'interest'))
  setnames(d3, new = c('week', 'interest'))
  setnames(d4, new = c('week', 'interest'))
  setnames(d5, new = c('week', 'interest'))
  setnames(d6, new = c('week', 'interest'))
  setnames(d7, new = c('week', 'interest'))
  setnames(d8, new = c('week', 'interest'))
  # Add column with topic
  # d1[, topic := 'Firearm']
  d1[, topic := 'Background Check']
  d2[, topic := 'Magazine']
  d3[, topic := 'Measure 114']
  d4[, topic := 'Gun Store']
  d5[, topic := '2nd Amendment']
  d6[, topic := 'Gun Safety']
  d7[, topic := 'Uvalde']
  d8[, topic := 'Gun']
  # Fix interest < 1
  d3[interest == '<1', interest := 0]
  d3[, interest := as.integer(interest)]
  # Bind
  trends_dt = rbindlist(list(d1, d2, d3, d4, d5, d6, d7, d8))
  # Set factor level order
  lvls = c(
   'Uvalde', '2nd Amendment', 'Gun Safety', 'Gun',
   'Measure 114', 'Gun Store', 'Background Check', 'Magazine',
   NULL
  )
  trends_dt[, `:=`(
    topic = factor(
      topic,
      levels = lvls,
      labels = lvls,
      ordered = TRUE
    )
  )]
  # Plot data
  gg_tmp =
    ggplot(
      data = trends_dt[between(week, ymd(20190811), ymd(20240421))],
      aes(x = week, y = interest, color = topic)
    ) +
    geom_hline(yintercept = 0, linewidth = .25) +
    geom_vline(
      xintercept = ymd(20220524),
      linewidth = .25,
      linetype = 'dashed'
    ) +
    geom_vline(
      xintercept = ymd(20221108),
      linewidth = .25,
      linetype = 'dashed'
    ) +
    geom_line(linewidth = .5) +
    geom_point(size = .3) +
    scale_y_continuous(
      'Google Trends search interest within Oregon',
      labels = comma
    ) +
    scale_x_date(
      'Week of sample',
    ) +
    scale_color_viridis_d(
      'Google Trends topic',
      labels = lvls,
      end = .9,
      direction = -1,
      option = 'magma',
    ) +
    theme_minimal(base_size = 16, base_family = 'Fira Sans Condensed') +
    theme(
      legend.position = 'bottom',
      plot.margin = margin(0, 0, 0, 0),
      legend.margin = margin(0, 0, 0, 0),
    ) +
    facet_wrap(vars(topic), nrow = 4, dir = 'v') +
    theme(legend.position = 'none')
  # Save plot
  ggsave(
    plot = gg_tmp,
    path = here('exhibits', 'figures'),
    filename = 'google-trends.png',
    device = ragg::agg_png,
    width = 10,
    height = 7
  )
  rm(gg_tmp)

# Figure: Main synth comparison ----------------------------------------------------------
  # Load the data
  bgc_data =
    here('data', 'clean', 'background-checks', 'bgc-osp-fbi.csv') |>
    fread()
  # Drop Alabama and North Carolina
  bgc_data %<>% .[!(state %in% drop_states)]
  # Recode dates
  bgc_data[, date := date |> ymd()]
  # Set up data for synthdid
  bgc_mat =
    bgc_data[!(state %in% border_states), .(state, date, rate, trt = treated_october)] |>
    panel.matrices()
  # Run synthdid on state-level data
  sdid_est = synthdid_estimate(bgc_mat$Y, bgc_mat$N0, bgc_mat$T0)
  # Extract data from synthdid_plot for our own plot
  main_dt =
    sdid_est |>
    synthdid_plot(overlay = 1) |>
    layer_data()
  setDT(main_dt)
  setnames(main_dt, old = 'PANEL', new = 'p')
  # Make our own plot
  gg_tmp =
    ggplot(
      data = main_dt,
      aes(
        x = as.Date(x), y = y,
        color = as.character(group),
        linetype = as.character(group),
        linewidth = as.character(group),
      )
    ) +
    # geom_hline(yintercept = 0) +
    geom_vline(
      xintercept = ymd(20221101),
      linetype = 'solid',
      linewidth = 2,
      alpha = .5,
      color = col_qtl[4],
    ) +
    geom_vline(
      xintercept = ymd(20221201),
      linetype = 'solid',
      linewidth = 2,
      alpha = .5,
      color = col_qtl[3],
    ) +
    geom_line(key_glyph = 'timeseries') +
    annotate(
      'text',
      x = ymd(20221101),
      y = main_dt$y |> max(),
      label = 'Meas. 114',
      hjust = 1.1,
      vjust = 0,
      size = 5,
      family = 'Fira Sans Condensed',
      color = col_qtl[4],
    ) +
    annotate(
      'text',
      x = ymd(20221201),
      y = main_dt$y |> max(),
      label = 'Judicial stay',
      hjust = -.05,
      vjust = 0,
      size = 5,
      family = 'Fira Sans Condensed',
      color = col_qtl[3],
    ) +
    scale_linewidth_manual(
      '',
      values = c(.7, .8),
      labels = c('Synth.', 'Oregon'),
    ) +
    scale_color_manual(
      '',
      values = c(col_ctl, col_trt),
      labels = c('Synth.', 'Oregon'),
    ) +
    scale_linetype_manual(
      '',
      values = c('8282', 'solid'),
      labels = c('Synth.', 'Oregon'),
    ) +
    scale_x_date('Month', date_breaks = '1 year', date_labels = '%Y') +
    scale_y_continuous('Background checks per 100k residents', labels = comma) +
    theme_minimal(base_size = 16, base_family = 'Fira Sans Condensed') +
    theme(
      legend.position = 'bottom',
      legend.key.width = unit(1.5, 'cm'),
      legend.key.height = unit(.9, 'cm'),
    )
  # Save plot
  ggsave(
    plot = gg_tmp,
    path = here('exhibits', 'figures'),
    filename = 'full-estimate-synthdid.png',
    width = 10,
    height = 5
  )
  rm(gg_tmp)

# Figure: Cumulative effects -------------------------------------------------------------
# NOTE Continues with estimates from 1a above
  # Collapse data to true - synth for each date
  acc_dt = main_dt[, .(y = diff(y)), by = x]
  # Set pre-10/22 to 0
  acc_dt[x < as.Date('2022-10-01'), y := 0]
  # Calculate cumulative effects
  acc_dt[, y_c := cumsum(y)]
  # Make the plot
  gg_tmp =
    ggplot(
      data = acc_dt,
      aes(
        x = as.Date(x), y = y_c,
      )
    ) +
    geom_area(alpha = .8, color = col_trt, fill = col_trt) +
    # geom_line(color = '#FF7F0D') +
    geom_vline(
      xintercept = ymd(20221101),
      linetype = 'solid',
      linewidth = 2,
      alpha = .5,
      color = col_qtl[4],
    ) +
    geom_vline(
      xintercept = ymd(20221201),
      linetype = 'solid',
      linewidth = 2,
      alpha = .5,
      color = col_qtl[3],
    ) +
    geom_hline(yintercept = 0) +
    scale_x_date('Month') +
    scale_y_continuous('Accumulated effect of Meas. 114 (per 100k)', labels = comma) +
    theme_minimal(base_size = 16, base_family = 'Fira Sans Condensed') +
    theme(legend.position = 'none')
  # Save plot
  ggsave(
    plot = gg_tmp,
    path = here('exhibits', 'figures'),
    filename = 'full-cumulative-synthdid-post.png',
    width = 10,
    height = 4.5
  )

# Figure: Effect by majority/minority support --------------------------------------------
  # Load the data
  bgc_data =
    here('data', 'clean', 'background-checks', 'bgc-majority.csv') |>
    fread()
  # Drop Alabama and North Carolina
  bgc_data %<>% .[!(state %in% drop_states)]
  # Recode dates
  bgc_data[, date := date |> ymd()]
  # Add group for majority: -1: non-OR, 0: <50%, 1: >=50%
  bgc_data[, `:=`(
    majority = fcase(
      state != 'Oregon', -1,
      place == 'MAJORITY NO', 0,
      place == 'MAJORITY YES', 1
    )
  )]
  # Iterate over majority/minority group
  m_dt =
    lapply(X = 0:1, FUN = function(g) {
      # Set up data for synthdid
      bgc_mat =
        bgc_data[!(state %in% border_states) & majority %in% c(-1, g), .(
          state, date, rate,
          trt = oct_treated
        )] |>
        panel.matrices()
      # Run synthdid on state-level data
      sdid_est = synthdid_estimate(bgc_mat$Y, bgc_mat$N0, bgc_mat$T0)
      # Extract data from synthdid_plot for our own plot
      main_dt =
        sdid_est |>
        synthdid_plot(overlay = 1) |>
        layer_data()
      setDT(main_dt)
      setnames(main_dt, old = 'PANEL', new = 'p')
      # Add majority/minority variable
      main_dt[, support := ifelse(g == 0, '<50%', '>=50%')]
      # Return
      return(main_dt)
    }) |>
    rbindlist()
  # Calculate treatment effect by date-support
  setorder(m_dt, support, x, group)
  m_dt %<>% .[, .(y = diff(y)), by = .(x, support)]
  # Plot
  gg_tmp =
    ggplot(
      data = m_dt,
      aes(x = as.Date(x), y = y, fill = support, color = support)
    ) +
    geom_hline(yintercept = 0, linewidth = .25) +
    geom_vline(
      xintercept = ymd(20221101),
      linetype = 'solid',
      linewidth = 2,
      alpha = .5,
      color = col_qtl[4],
    ) +
    geom_vline(
      xintercept = ymd(20221201),
      linetype = 'solid',
      linewidth = 2,
      alpha = .5,
      color = col_qtl[3],
    ) +
    annotate(
      'text',
      x = ymd(20221101),
      y = m_dt$y |> max(),
      label = 'Meas. 114',
      hjust = 1.1,
      vjust = 0,
      size = 5,
      family = 'Fira Sans Condensed',
      color = col_qtl[4],
    ) +
    annotate(
      'text',
      x = ymd(20221201),
      y = m_dt$y |> max(),
      label = 'Judicial stay',
      hjust = -.05,
      vjust = 0,
      size = 5,
      family = 'Fira Sans Condensed',
      color = col_qtl[3],
    ) +
    geom_line(linewidth = .9, key_glyph = 'rect') +
    scale_y_continuous(
      'Induced BGCs per 100k residents',
      labels = comma
    ) +
    scale_x_date(
      'Month of sample',
      breaks = c(
        seq.Date(from = ymd(20180101), to = ymd(20240101), by = '1 years')
      ),
      labels = year
    ) +
    scale_color_manual(
      'Meas. 114 support',
      values = col_qtl[c(2, 3)]
    ) +
    scale_fill_manual(
      'Meas. 114 support',
      values = col_qtl[c(2, 3)]
    ) +
    theme_minimal(base_size = 16, base_family = 'Fira Sans Condensed') +
    theme(
      legend.position = 'bottom',
      legend.key.width = unit(1.5, 'cm'),
      legend.key.height = unit(.9, 'cm'),
    )
  # Save plot
  ggsave(
    plot = gg_tmp,
    path = here('exhibits', 'figures'),
    filename = 'majority-estimate-synthdid.png',
    width = 10,
    height = 5
  )
  rm(gg_tmp)

# Figure: Cumulative effect by majority/minority support ---------------------------------
  # Set pre-10/22 to 0
  m_dt[x < as.Date('2022-10-01'), y := 0]
  # Calculate cumulative effects
  m_dt[, y_c := cumsum(y), by = support]
  # Plot
  gg_tmp =
    ggplot(
      data = m_dt,
      aes(
        x = as.Date(x), y = y_c,
        color = support, fill = support
      )
    ) +
    geom_area(alpha = .4, position = 'identity') +
    # geom_line(color = '#FF7F0D') +
    geom_hline(yintercept = 0) +
    geom_vline(
      xintercept = ymd(20221101),
      linetype = 'solid',
      linewidth = 2,
      alpha = .5,
      color = col_qtl[4],
    ) +
    geom_vline(
      xintercept = ymd(20221201),
      linetype = 'solid',
      linewidth = 2,
      alpha = .5,
      color = col_qtl[3],
    ) +
    scale_x_date('Month') +
    scale_y_continuous('Accumulated effect of Meas. 114 (per 100k)', labels = comma) +
    scale_color_manual(
      'Meas. 114 support',
      values = col_qtl[c(2, 3)]
    ) +
    scale_fill_manual(
      'Meas. 114 support',
      values = col_qtl[c(2, 3)]
    ) +
    theme_minimal(base_size = 16, base_family = 'Fira Sans Condensed') +
    theme(legend.position = 'none')
  # Save plot
  ggsave(
    plot = gg_tmp,
    path = here('exhibits', 'figures'),
    filename = 'majority-cumulative-synthdid-post.png',
    width = 10,
    height = 4.5
  )

# Appendix figure: State weights ---------------------------------------------------------
  # Estimate SDID, SCM, and DID
  sdid_est = synthdid_estimate(bgc_mat$Y, bgc_mat$N0, bgc_mat$T0)
  scm_est = sc_estimate(bgc_mat$Y, bgc_mat$N0, bgc_mat$T0)
  did_est = did_estimate(bgc_mat$Y, bgc_mat$N0, bgc_mat$T0)
  # Extract weights following synthdid::synthdid_units_plot
  plot_dt =
    lapply(list(sdid_est, scm_est, did_est), function(est) {
      estimate = est
      setup = attr(est, "setup")
      weights = attr(est, "weights")
      Y = setup$Y - synthdid:::contract3(setup$X, weights$beta)
      N0 = setup$N0
      N1 = nrow(Y) - N0
      T0 = setup$T0
      T1 = ncol(Y) - T0
      lambda.pre = c(weights$lambda, rep(0, T1))
      lambda.post = c(rep(0, T0), rep(1/T1, T1))
      omega.control = c(weights$omega, rep(0, N1))
      omega.treat = c(rep(0, N0), rep(1/N1, N1))
      difs = as.vector(t(omega.treat) %*% Y %*% (lambda.post - 
          lambda.pre)) - as.vector(Y[1:N0, ] %*% (lambda.post - 
          lambda.pre))
      se = NA
      include.units = 1:N0
      data.table(
        estimator = attr(est, 'estimator'),
        y = difs[include.units],
        unit = rownames(Y)[include.units], 
        weight = omega.control[include.units],
        estimate = c(est),
        se = se
      )
    }) |>
    rbindlist()
  # Add state abbreviations
  plot_dt %<>% merge(
    y = data.table(unit = state.name, abbr = state.abb),
    by = 'unit',
    all.x = TRUE, all.y = FALSE
  )
  # Add estimator names
  plot_dt[, `:=`(
    estimator = factor(
      estimator,
      levels = c('synthdid_estimate', 'sc_estimate', 'did_estimate'),
      labels = c('SDID', 'SCM', 'DID'),
      ordered = TRUE
    )
  )]
  # Plot individual estimates
  gg_est =
    ggplot(
      data = plot_dt,
      aes(x = abbr, y = y, color = estimator, fill = estimator)
    ) +
    geom_hline(yintercept = 0, linewidth = .25) +
    geom_hline(
      aes(yintercept = estimate, color = estimator),
      linetype = 'solid',
      linewidth = 2,
      alpha = .5
    ) +
    geom_point(size = 3.5) +
    scale_x_discrete('State') +
    scale_y_continuous('Estimate') +
    scale_color_viridis_d('Estimator', option = 'magma', begin = .4, end = .8) +
    scale_fill_viridis_d('Estimator', option = 'magma', begin = .4, end = .8) +
    theme_minimal(base_size = 16, base_family = 'Fira Sans Condensed') +
    facet_grid(estimator ~ ., scales = 'free_y') +
    theme(
      legend.position = 'none',
      axis.title.x = element_blank(),
      axis.text.x = element_blank(),
    )
  # Plot weights
  gg_wts =
    ggplot(
      data = plot_dt,
      aes(x = abbr, y = weight, color = estimator, fill = estimator)
    ) +
    geom_col() +
    geom_hline(yintercept = 0, linewidth = .25) +
    geom_point(data = plot_dt[weight == 0], size = 2, shape = 4, stroke = 1.5) +
    scale_x_discrete('State') +
    scale_y_continuous('Weight') +
    scale_color_viridis_d('Estimator', option = 'magma', begin = .4, end = .8) +
    scale_fill_viridis_d('Estimator', option = 'magma', begin = .4, end = .8) +
    theme_minimal(base_size = 16, base_family = 'Fira Sans Condensed') +
    facet_grid(estimator ~ ., scales = 'free_y') +
    theme(legend.position = 'none')
  # Save stacked plot
  ggsave(
    plot =
      plot_grid(
        gg_est, gg_wts,
        align = 'v',
        ncol = 1,
        rel_heights = c(1, 1)
      ),
    path = here('exhibits', 'figures'),
    filename = 'methods-estimates-weights.png',
    width = 12,
    height = 10,
  )

# Appendix figure: Effect by quartile of support -----------------------------------------
  # Load the data
  bgc_data =
    here('data', 'clean', 'background-checks', 'bgc-quartile.csv') |>
    fread()
  # Drop Alabama and North Carolina
  bgc_data %<>% .[!(state %in% drop_states)]
  # Recode dates
  bgc_data[, date := date |> ymd()]
  # Iterate over quartiles
  q_dt =
    lapply(X = 1:4, FUN = function(q) {
      # Set up data for synthdid
      bgc_mat =
        bgc_data[!(state %in% border_states) & quartile %in% c(0, q), .(
          state, date, rate,
          trt = oct_treated
        )] |>
        panel.matrices()
      # Run synthdid on state-level data
      sdid_est = synthdid_estimate(bgc_mat$Y, bgc_mat$N0, bgc_mat$T0)
      # Extract data from synthdid_plot for our own plot
      main_dt =
        sdid_est |>
        synthdid_plot(overlay = 1) |>
        layer_data()
      setDT(main_dt)
      setnames(main_dt, old = 'PANEL', new = 'p')
      # Add quartile
      main_dt[, q := q]
      # Return
      return(main_dt)
    }) |>
    rbindlist()
  # Calculate treatment effect by date-quantile 
  setorder(q_dt, q, x, group)
  q_dt %<>% .[, .(y = diff(y)), by = .(x, q)]
  # Plot
  gg_tmp =
    ggplot(
      data = q_dt,
      aes(x = as.Date(x), y = y, fill = as.factor(q), color = as.factor(q))
    ) +
    geom_line(linewidth = .9, key_glyph = 'rect') +
    geom_hline(yintercept = 0, linewidth = .25) +
    geom_vline(
      xintercept = ymd(20221108),
      linewidth = .25,
      linetype = 'dashed'
    ) +
    scale_y_continuous(
      'Background checks per 100k residents',
      labels = comma
    ) +
    scale_x_date(
      'Month of sample',
      breaks = c(
        seq.Date(from = ymd(20180101), to = ymd(20240101), by = '1 years')
      ),
      labels = year
    ) +
    scale_color_manual(
      'Meas. 114 support',
      labels = c('<21%', '21–31%', '31–44%', '>44%'),
      values = col_qtl
    ) +
    scale_fill_manual(
      'Meas. 114 support',
      labels = c('<21%', '21–31%', '31–44%', '>44%'),
      values = col_qtl
    ) +
    theme_minimal(base_size = 16, base_family = 'Fira Sans Condensed') +
    theme(
      legend.position = 'bottom',
      legend.key.width = unit(1.5, 'cm'),
      legend.key.height = unit(.9, 'cm'),
    )
  # Save plot
  ggsave(
    plot = gg_tmp,
    path = here('exhibits', 'figures'),
    filename = 'quartile-estimate-synthdid.png',
    width = 12,
    height = 7
  )
  rm(gg_tmp)

# Appendix figure: Cumulative effect by quartile of support ------------------------------
  # Set pre-10/22 to 0
  q_dt[x < as.Date('2022-10-01'), y := 0]
  # Calculate cumulative effects
  q_dt[, y_c := cumsum(y), by = q]
  # Plot
  gg_tmp =
    ggplot(
      data = q_dt,
      aes(
        x = as.Date(x), y = y_c,
        color = as.factor(q), fill = as.factor(q)
      )
    ) +
    geom_area(alpha = .4, position = 'identity') +
    # geom_line(color = '#FF7F0D') +
    geom_hline(yintercept = 0) +
    scale_x_date('Month') +
    scale_y_continuous('Accumulated effect of Meas. 114 (per 100k)', labels = comma) +
    scale_color_manual(
      'Meas. 114 support',
      labels = c('<21%', '21–31%', '31–44%', '>44%'),
      values = col_qtl
    ) +
    scale_fill_manual(
      'Meas. 114 support',
      labels = c('<21%', '21–31%', '31–44%', '>44%'),
      values = col_qtl
    ) +
    theme_minimal(base_size = 16, base_family = 'Fira Sans Condensed') +
    theme(legend.position = 'none')
  # Save plot
  ggsave(
    plot = gg_tmp,
    path = here('exhibits', 'figures'),
    filename = 'quartile-cumulative-synthdid-post.png',
    width = 12,
    height = 6.5
  )

# Figure: Daily OPS time series ----------------------------------------------------------
  # Weekday colors
  col_wk = viridis::magma(7, end = .93)
  # Define dates of interest with labels
  date_dt =
    list(
      data.table(d = '20221001', s = 'Oct 10', l = 'Trt. def. begins'),
      data.table(d = '20221108', s = 'Nov 8', l = 'Meas. 114'),
      data.table(d = '20221206', s = 'Dec 6', l = 'Judicial stay'),
      data.table(d = '20221125', s = 'Nov 25', l = 'Black Friday'),
      data.table(d = '20231124', s = 'Nov 24', l = 'Black Friday'),
      data.table(d = '20221225', s = 'Dec 25', l = 'Christmas'),
      data.table(d = '20231225', s = 'Dec 25', l = 'Christmas'),
      NULL
    ) |> rbindlist()
  date_dt[, d := ymd(d)]
  # Load the data
  bgc_data =
    here('data', 'clean', 'background-checks', 'bgc-osp-201802-202404.csv') |>
    fread()
  # Grab the desired data and aggregate to state level
  bgc_dt = bgc_data[, .(
    bgc = first(day_total),
    pop = first(state_population)
  ), by = date]
  # Drop dates to desired range
  # d1 = bgc_dt[, x |> min() |> as.Date()]
  # dn = bgc_dt[, x |> max() |> as.Date()]
  # d1 = trends_dt[, week |> min()]
  # dn = trends_dt[, week |> max()]
  # d1 = ymd(20211108)
  # dn = ymd(20231108)
  # d1 = ymd(20220101)
  # dn = ymd(20231231)
  d1 = ymd(20220901)
  dn = ymd(20230131)
  bgc_dt %<>% .[between(date, d1, dn)]
  # Impose date restrictions
  date_dt %<>% .[between(d, d1, dn)]
  # Add day of week
  bgc_dt[, wd := wday(date, label = TRUE, week_start = 1)]
  # Time-series figure
  gg_tmp =
    ggplot(
      data = bgc_dt,
      aes(x = date, y = bgc, color = wd)
    ) +
    geom_hline(yintercept = 0, linewidth = .25) +
    # Treatment begins
    geom_vline(
      xintercept = date_dt[l == 'Trt. def. begins']$d,
      linewidth = 2, alpha = .2,
      color = col_wk[6],
    ) +
    # Meas. 114
    geom_vline(
      xintercept = date_dt[l == 'Meas. 114', d],
      color = col_wk[2],
      linewidth = 2, alpha = .2,
    ) +
    # Judicial stay
    geom_vline(
      xintercept = date_dt[l == 'Judicial stay', d],
      color = col_wk[2],
      linewidth = 2, alpha = .2,
    ) +
    # Black Friday(s)
    geom_vline(
      xintercept = date_dt[l == 'Black Friday', d],
      color = col_wk[5],
      linewidth = 2, alpha = .2,
    ) +
    # Christmas(es)
    geom_vline(
      xintercept = date_dt[l == 'Christmas', d],
      color = col_wk[7],
      linewidth = 2, alpha = .2,
    ) +
    # The time series
    geom_line(linewidth = .3, color = 'grey70') +
    geom_point(size = 1.5) +
    scale_x_date(
      'Day of sample',
      date_breaks = '1 month',
    ) +
    scale_y_continuous('Background checks (OR State Police)', labels = comma) +
    scale_color_viridis_d('Day of week', option = 'magma', end = .93) +
    theme_minimal(base_size = 17, base_family = 'Fira Sans Condensed') +
    theme(
      axis.title.x = element_blank(),
      axis.text.x = element_blank(),
      legend.position = 'top',
      legend.key.width = unit(1.5, 'cm'),
      legend.key.height = unit(.9, 'cm'),
      panel.grid.minor.x = element_blank(),
    ) +
    guides(color = guide_legend(nrow = 1, override.aes = list(linewidth = 1)))
  # Plot event timeline
  gg_timeline =
    ggplot(
      data = bgc_dt |> dplyr::mutate(y = 0),
      aes(x = date, y = y)
    ) +
    scale_x_date(
      'Day of sample',
      date_breaks = '1 month',
    ) +
    # Blank segment to make room for timeline
    annotate(
      'segment',
      x = bgc_dt$date |> min(), xend = bgc_dt$date |> max(),
      y = -.1, yend = 1.8,
      color = NA,
    ) +
    # Timeline
    annotate(
      'segment',
      x = bgc_dt$date |> min(), xend = bgc_dt$date |> max(),
      y = 0, yend = 0,
      color = 'black'
    ) +
    # Treatment period
    geom_point(
      data = date_dt[l == 'Trt. def. begins'], aes(x = d, y = 0),
      color = col_wk[6],
      size = 3
    ) +
    annotate(
      'rect',
      xmin = date_dt[l == 'Trt. def. begins']$d,
      xmax = bgc_dt$date |> max(),
      ymin = 0,
      ymax = 1,
      fill = col_wk[6],
      color = NA,
      alpha = .1
    ) +
    geom_vline(
      xintercept = date_dt[l == 'Trt. def. begins']$d,
      linewidth = 2, alpha = .2,
      color = col_wk[6],
    ) +
    annotate(
      'text',
      x = date_dt[l == 'Trt. def. begins']$d,
      y = .99,
      hjust = -.03,
      vjust = -.5,
      label = 'Treatment period',
      color = col_wk[6],
      size = 4.5,
      family = 'Fira Sans Condensed',
    ) +
    # Meas. 114
    geom_vline(
      xintercept = date_dt[l == 'Meas. 114', d],
      color = col_wk[2],
      linewidth = 2, alpha = .2,
    ) +
    geom_point(
      data = date_dt[l == 'Meas. 114'], aes(x = d, y = 0),
      color = col_wk[2],
      size = 3
    ) +
    geom_text(
      data = date_dt[l == 'Meas. 114'],
      aes(x = d, y = .99, label = l),
      hjust = -.05,
      vjust = -.5,
      angle = 0,
      color = col_wk[2],
      size = 4.5,
      family = 'Fira Sans Condensed',
    ) +
    # Judicial stay
    geom_vline(
      xintercept = date_dt[l == 'Judicial stay', d],
      color = col_wk[2],
      linewidth = 2, alpha = .2,
    ) +
    geom_point(
      data = date_dt[l == 'Judicial stay'], aes(x = d, y = 0),
      color = col_wk[2],
      size = 3
    ) +
    geom_text(
      data = date_dt[l == 'Judicial stay'],
      aes(x = d, y = .99, label = l),
      hjust = -.05,
      vjust = -.5,
      angle = 0,
      color = col_wk[2],
      size = 4.5,
      family = 'Fira Sans Condensed',
    ) +
    # Black Friday(s)
    geom_vline(
      xintercept = date_dt[l == 'Black Friday', d],
      color = col_wk[5],
      linewidth = 2, alpha = .2,
    ) +
    geom_point(
      data = date_dt[l == 'Black Friday'], aes(x = d, y = 0),
      color = col_wk[5],
      size = 3
    ) +
    geom_text(
      data = date_dt[l == 'Black Friday'],
      aes(x = d, y = .4, label = l),
      hjust = 1.03,
      vjust = 0,
      angle = 0,
      color = col_wk[5],
      size = 4.5,
      family = 'Fira Sans Condensed',
    ) +
    # Christmas(es)
    geom_vline(
      xintercept = date_dt[l == 'Christmas', d],
      color = col_wk[7],
      linewidth = 2, alpha = .2,
    ) +
    geom_point(
      data = date_dt[l == 'Christmas'], aes(x = d, y = 0),
      color = col_wk[7],
      size = 3
    ) +
    geom_text(
      data = date_dt[l == 'Christmas'],
      aes(x = d, y = .4, label = l),
      hjust = 1.03,
      vjust = 0,
      angle = 0,
      color = col_wk[7],
      size = 4.5,
      family = 'Fira Sans Condensed',
    ) +
    # Theme elements
    theme_minimal(base_size = 17, base_family = 'Fira Sans Condensed') +
    theme(
      axis.text.y = element_blank(),
      # axis.text.x = element_blank(),
      axis.title.y = element_blank(),
      axis.ticks.y = element_blank(),
      plot.margin = margin(-12.8, 0, 5, 0),
      legend.margin = margin(0, 0, 0, 0),
      panel.grid.minor.y = element_blank(),
      panel.grid.major.y = element_blank(),
      panel.grid.minor.x = element_blank(),
    )
  # Save plot with timeline
  ggsave(
    plot =
      plot_grid(
        gg_tmp, gg_timeline,
        align = 'v', ncol = 1, rel_heights = c(9, 2)
      ),
    path = here('exhibits', 'figures'),
    filename = 'time-series-daily-osp-timeline.png',
    width = 11,
    height = 7
  )

# Appendix figure: Placebo 1 year prior (still Oregon) -----------------------------------
  # Load the data
  bgc_data =
    here('data', 'clean', 'background-checks', 'bgc-osp-fbi.csv') |>
    fread()
  # Drop Alabama and North Carolina
  bgc_data %<>% .[!(state %in% drop_states)]
  # Recode dates
  bgc_data[, date := date |> ymd()]
  # Update treatment status for placebo
  bgc_data[state == 'Oregon', treated_october := 1 * (date >= ymd(20211001))]
  # Drop time period after 2022-10-01
  bgc_data %<>% .[date < ymd(20221001)]
  # Set up data for synthdid
  bgc_mat =
    bgc_data[!(state %in% border_states), .(state, date, rate, trt = treated_october)] |>
    panel.matrices()
  # Run synthdid on state-level data
  sdid_est = synthdid_estimate(bgc_mat$Y, bgc_mat$N0, bgc_mat$T0)
  # Extract data from synthdid_plot for our own plot
  main_dt =
    sdid_est |>
    synthdid_plot(overlay = 1) |>
    layer_data()
  setDT(main_dt)
  setnames(main_dt, old = 'PANEL', new = 'p')
  # Make our own plot
  gg_tmp =
    ggplot(
      data = main_dt,
      aes(
        x = as.Date(x), y = y,
        color = as.character(group),
        linetype = as.character(group),
        linewidth = as.character(group),
      )
    ) +
    # geom_hline(yintercept = 0) +
    geom_vline(
      xintercept = ymd(20211001),
      linewidth = .25,
      linetype = 'longdash',
      color = 'orange',
    ) +
    annotate(
      'rect',
      xmin = ymd(20211001), xmax = ymd(20221001),
      ymin = -Inf, ymax = Inf,
      fill = 'orange',
      color = NA,
      alpha = .1,
    ) +
    annotate(
      'text',
      x = ymd(20211001),
      y = main_dt$y |> max(),
      label = 'Placebo trt. period',
      color = 'orange',
      hjust = -.1,
      size = 5,
      family = 'Fira Sans Condensed',
    ) +
    geom_line(key_glyph = 'timeseries') +
    scale_linewidth_manual(
      '',
      values = c(.7, .8),
      labels = c('Synth.', 'Oregon'),
    ) +
    scale_color_manual(
      '',
      values = c(col_ctl, col_trt),
      labels = c('Synth.', 'Oregon'),
    ) +
    scale_linetype_manual(
      '',
      values = c('8282', 'solid'),
      labels = c('Synth.', 'Oregon'),
    ) +
    scale_x_date('Month') +
    scale_y_continuous('Background checks per 100k residents', labels = comma) +
    theme_minimal(base_size = 16, base_family = 'Fira Sans Condensed') +
    theme(
      legend.position = 'bottom',
      legend.key.width = unit(1.5, 'cm'),
      legend.key.height = unit(.9, 'cm'),
    )
  # Save plot
  ggsave(
    plot = gg_tmp,
    path = here('exhibits', 'figures'),
    filename = 'full-estimate-synthdid-placebo-or2021.png',
    width = 12,
    height = 7
  )
  rm(gg_tmp)
  # Collapse data to true - synth for each date
  acc_dt = main_dt[, .(y = diff(y)), by = x]
  # Set pre-10/22 to 0
  acc_dt[x < as.Date('2021-10-01'), y := 0]
  # Calculate cumulative effects
  acc_dt[, y_c := cumsum(y)]
  # Make the plot
  gg_tmp =
    ggplot(
      data = acc_dt,
      aes(
        x = as.Date(x), y = y_c,
      )
    ) +
    geom_area(alpha = .8, color = col_trt, fill = col_trt) +
    # geom_line(color = '#FF7F0D') +
    geom_hline(yintercept = 0) +
    scale_x_date('Month') +
    scale_y_continuous('Accumulated effect of Meas. 114 (per 100k)', labels = comma) +
    theme_minimal(base_size = 16, base_family = 'Fira Sans Condensed') +
    theme(legend.position = 'none')
  # Save plot
  ggsave(
    plot = gg_tmp,
    path = here('exhibits', 'figures'),
    filename = 'full-cumulative-synthdid-post-placebo-or2021.png',
    width = 12,
    height = 6.5
  )

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
  bgc_dt = bgc_data[, .(
    bgc = sum(totals_standard_sales),
    pop = sum(population)
  ), by = .(year, month, grp = ifelse(state == 'Oregon', 'OR', 'Non-OR'))]
  bgc_dt[, `:=`(
    grp =
      factor(
        grp,
        levels = c('OR', 'Non-OR'),
        labels = c('Oregon', 'Others'),
        ordered = TRUE
      )
  )]
  # Calculate BGC rate per 100k
  bgc_dt[, rate := bgc / pop * 1e5]
  # Fix date
  bgc_dt[, date := ymd(paste0(year, '-', month, '-01'))]
  # Drop dates to desired range
  d1 = ymd(20050101)
  dn = ymd(20231231)
  bgc_dt %<>% .[between(date, d1, dn)]
  # Impose date restrictions
  date_dt %<>% .[between(d, d1, dn)]
  # Time-series figure
  gg_tmp =
    ggplot(
      data = bgc_dt,
      aes(x = date, y = rate)
    ) +
    # Axis
    geom_hline(yintercept = 0, linewidth = .25) +
    # Dates
    geom_vline(
      data = date_dt,
      aes(xintercept = d),
      linewidth = 2,
      alpha = .3,
      color = col_wk[6],
    ) +
    geom_text(
      data = date_dt,
      aes(x = d, y = 0, label = l),
      hjust = -.01,
      vjust = 1.8,
      size = 4.5,
      family = 'Fira Sans Condensed',
      color = col_wk[6],
    ) +
    # Time series
    geom_line(aes(color = grp, linewidth = grp)) +
    # geom_point(size = 1.5) +
    scale_x_date('Month of sample') +
    scale_y_continuous('Background checks per 100k (FBI)', labels = comma) +
    scale_color_manual('', values = c(col_trt, col_ctl)) +
    scale_linewidth_manual('', values = c(.8, .6)) +
    theme_minimal(base_size = 17, base_family = 'Fira Sans Condensed') +
    theme(legend.position = 'bottom') +
    guides(color = guide_legend(nrow = 1, override.aes = list(linewidth = 1)))
  # Save plot with timeline
  ggsave(
    plot = gg_tmp,
    path = here('exhibits', 'figures'),
    filename = 'time-series-monthly-fbi.png',
    device = ragg::agg_png,
    width = 11,
    height = 7.5
  )

# Appendix: Compare OR time-series, FBI and OSP ------------------------------------------
  # Load data
  fbi_dt =
    here('data', 'clean', 'background-checks', 'bgc-fbi-200001-202403.csv') |>
    fread()
  osp_dt =
    here('data', 'clean', 'background-checks', 'bgc-osp-201802-202404.csv') |>
    fread()
  # Grab desired variables from Oregon FBI data
  fbi_dt %<>% .[state == 'Oregon', .(
    mos = ymd(paste0(year, '-', month, '-01')),
    bgc = totals_standard_sales,
    type = 'FBI'
  )]
  osp_dt %<>% .[, .(
    bgc = sum(county_day_total),
    type = 'OSP'
  ), by = .(mos = month_date |> ymd())]
  # Combine data
  bgc_dt = rbindlist(list(fbi_dt, osp_dt), use.names = TRUE, fill = TRUE)
  # Define start and stop dates
  d1 = ymd(20180301)
  d2 = ymd(20240301)
  # Time-series figure
  gg_tmp =
    ggplot(
      data = bgc_dt[between(mos, d1, d2)],
      aes(x = mos, y = bgc, color = type)
    ) +
    # Axis
    geom_hline(yintercept = 0, linewidth = .25) +
    # Dates
    geom_vline(
      xintercept = ymd(20221101),
      linetype = 'solid',
      linewidth = 2,
      alpha = .5,
      color = col_qtl[4],
    ) +
    # Time series
    geom_line(aes(color = type, linewidth = type)) +
    # geom_point(size = 1.5) +
    scale_x_date('Month of sample') +
    scale_y_continuous('Background checks', labels = comma) +
    scale_color_manual('Data source', values = c(col_qtl[1], col_qtl[3])) +
    scale_linewidth_manual('Data source', values = c(.8, .6)) +
    theme_minimal(base_size = 17, base_family = 'Fira Sans Condensed') +
    theme(legend.position = 'bottom') +
    guides(color = guide_legend(nrow = 1, override.aes = list(linewidth = 1)))
  # Save plot with timeline
  ggsave(
    plot = gg_tmp,
    path = here('exhibits', 'figures'),
    filename = 'time-series-monthly-fbi-osp.png',
    device = ragg::agg_png,
    width = 10,
    height = 7.5
  )
