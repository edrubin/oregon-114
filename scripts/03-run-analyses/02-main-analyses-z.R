# Notes ----------------------------------------------------------------------------------
#   Goal:   Run main analyses on pre-trt, within-state standardized rates
#   Time:   ~ 15 minutes with 10 cores (and 500 placebo replications)

# Output ---------------------------------------------------------------------------------
#   data/clean/analyses/standardized/results-all.qs
#   data/clean/analyses/standardized/results-nonpoc.qs
#   data/clean/analyses/standardized/results-nonp2p.qs
#   data/clean/analyses/standardized/results-nonborder.qs
#   data/clean/analyses/standardized/results-brady.qs

# Data notes -----------------------------------------------------------------------------
#   - We always drop Alabama, North Carolina, Alaska, and Hawaii

# Setup ----------------------------------------------------------------------------------
  # Load packages
  pacman::p_load(
    here, janitor, tidyr, magrittr, data.table, qs, parallel, future.apply,
    tidyverse, scales, gsynth, fixest, kableExtra, synthdid
  )

# Function: Parallelize synthdid placebo function ----------------------------------------
  # Parallelize the placebo function (synthdid:::placebo_se)
# NOTE For our setting parallelization on 10 cores speeds up the process by 7x
  par_placebo_se = function(estimate, replications, cores = detectCores(), seed = 123) {
    setup = attr(estimate, 'setup')
    opts = attr(estimate, 'opts')
    weights = attr(estimate, 'weights')
    N1 = nrow(setup$Y) - setup$N0
    if (setup$N0 <= N1) {
        stop('must have more controls than treated units to use the placebo se')
    }
    theta = function(ind) {
      N0 = length(ind) - N1
      weights.boot = weights
      weights.boot$omega = synthdid:::sum_normalize(weights$omega[ind[1:N0]])
      do.call(
        synthdid_estimate,
        c(
          list(
            Y = setup$Y[ind, ],
            N0 = N0,
            T0 = setup$T0,
            X = setup$X[ind, , ],
            weights = weights.boot
          ),
          opts
        )
      )
    }
# NOTE Update for parallelization is here
    set.seed(seed)
    plan(multicore, workers = cores)
    rep_out = future_replicate(
      n = replications,
      expr = theta(sample(1:setup$N0)),
      future.seed = TRUE
    ) |> as.numeric()
    sqrt((replications - 1) / replications) * sd(rep_out)
  }

# Define sets of states ------------------------------------------------------------------
  # Load definitions
  here('scripts', '03-run-analyses', '00-define-groups.R') |> source()

# Load data ------------------------------------------------------------------------------
# NOTE Data run 2018-03-01 to 2023-03-01 and use OSP data in place of FBI data for Oregon
# Outcomes:
#   - total OSP checks (approved, canceled and denied)
#   - FBI standard sales (HG + LG + other + multiple)
  # Load the data
  bgc_data =
    here('data', 'clean', 'background-checks', 'bgc-osp-fbi.csv') |>
    fread()
  # Drop population
  bgc_data[, pop := NULL]
  # Recode dates
  bgc_data[, date := date |> ymd()]
  # Calculate pre-treatment, state-level mean and standard deviation of rate
  state_dt =
    bgc_data[date < ymd(20221001), .(
      rate_mean = mean(rate),
      rate_sd = sd(rate)
    ), by = state]
  # Merge
  bgc_data %<>%
    merge(
      x = state_dt,
      by = 'state',
      all.x = TRUE,
      all.y = FALSE
    )
  # Standardize weight (using pre-trt state data): rate = (rate - rate_mean) / rate_sd
  bgc_data[, rate := (rate - rate_mean) / rate_sd]
  # Drop pre-treatment standard deviation
  bgc_data[, `:=`(rate_mean = NULL, rate_sd = NULL)]

# Function: Synthdid estimates for specified subsets -------------------------------------
# NOTE .dates_start and .dates_stop must be the same length (and are inclusive)
  est_fun = function(
    .data,
    .subset = NULL,
    .dates_start = -Inf,
    .dates_stop = Inf,
    .dates_label = NULL,
    .trt_month = 'October',
    .inf = TRUE
  ) {
    # Define dataset using stated treatment month
    bgc_dt = .data[, c(
      'state', 'date', 'rate',
      paste0('treated_', str_to_lower(.trt_month))
    ), with = FALSE]
    # Drop 'always drop' states
    bgc_dt %<>% .[!(state %in% drop_states), ]
    # Subset data spatially (if requested)
    if (!is.null(.subset)) {
      if (.subset == 'No P2P') bgc_dt %<>% .[!(state %in% p2p_states), ]
      if (.subset == 'No POC') bgc_dt %<>% .[!(state %in% poc_states), ]
      if (.subset == 'No border') bgc_dt %<>% .[!(state %in% border_states), ]
      if (.subset == 'No Brady exempt') bgc_dt %<>% .[!(state %in% brady_states), ]
    }
    # Subset data temporally
    # If multiple subsets: take implied date subsets and then bind them together
    bgc_dt =
      lapply(
        X = seq_along(.dates_start),
        FUN = function(i) {
          bgc_dt[between(
            date,
            ifelse(.dates_start[i] == -Inf, -Inf, .dates_start[i]) |> as.Date(),
            ifelse(.dates_stop[i] == Inf, Inf, .dates_stop[i]) |> as.Date()
          ), ]
        }
      ) |>
      rbindlist(use.names = TRUE, fill = TRUE)
    # Create a character expression of dates
    .dates_desc =
      lapply(
        X = seq_along(.dates_start),
        FUN = function(i) {
          paste0(
            ifelse(
              .dates_start[i] == -Inf,
              bgc_dt$date |> min(na.rm = TRUE) |> str_remove_all('-'),
              .dates_start[i] |> str_remove_all('-')
            ),
            '-',
            ifelse(
              .dates_stop[i] == Inf,
              bgc_dt$date |> max(na.rm = TRUE) |> str_remove_all('-'),
              .dates_stop[i] |> str_remove_all('-')
            )
          )
        }
      ) |>
      unlist() |>
      paste(collapse = '; ')
    # Create synthdid matrix
    bgc_mat = bgc_dt |> panel.matrices()
    # Estimate synth DID, synth control, and standard DID
    est_sdid = synthdid_estimate(bgc_mat$Y, bgc_mat$N0, bgc_mat$T0)
    est_scm = sc_estimate(bgc_mat$Y, bgc_mat$N0, bgc_mat$T0)
    est_did = did_estimate(bgc_mat$Y, bgc_mat$N0, bgc_mat$T0)
    # Sum treated periods
    sum_sdid =
      est_sdid |>
      synthdid::synthdid_effect_curve() |>
      sum()
    sum_scm =
      est_scm |>
      synthdid::synthdid_effect_curve() |>
      sum()
    sum_did =
      est_did |>
      synthdid::synthdid_effect_curve() |>
      sum()
    # Standard error (if requested)
    if (.inf == TRUE) {
      se_sdid = est_sdid |> par_placebo_se(replications = 500, seed = 123)
      se_scm = est_scm |> par_placebo_se(replications = 500, seed = 123)
      se_did = est_did |> par_placebo_se(replications = 500, seed = 123)
    } else {
      se_sdid = se_scm = se_did = NA
    }
    # Create a nice data frame with results
    r_dt = data.table(
      subset = ifelse(is.null(.subset), 'All', .subset),
      trt_month = .trt_month,
      dates = ifelse(is.null(.dates_label), 'All', .dates_label),
      dates_ranges = .dates_desc,
      est_sdid = est_sdid[1],
      est_scm = est_scm[1],
      est_did = est_did[1],
      se_sdid =  as.numeric(se_sdid),
      se_scm =  as.numeric(se_scm),
      se_did =  as.numeric(se_did),
      accum_sdid = sum_sdid,
      accum_scm = sum_scm,
      accum_did = sum_did
    )
    # Add weights
    wts_sdid = est_sdid |> synthdid_controls() |> as.data.table()
    wts_scm = est_scm |> synthdid_controls() |> as.data.table()
    wts_did = est_sdid |> synthdid_controls() |> as.data.table()
    r_dt[, wts_sdid := wts_sdid]
    r_dt[, wts_scm := wts_scm]
    r_dt[, wts_did := wts_did]
    # Return
    return(r_dt)
  }

# Estimates: All states ------------------------------------------------------------------
  # Estimates for all states
  results_all =
    list(
      # All states, all dates, total effect (all post periods)
      est_fun(
        .data = bgc_data,
        .inf = TRUE
      ),
      # All states, one treated month: 2022-10-01 (October)
      est_fun(
        .data = bgc_data,
        .dates_start = -Inf,
        .dates_stop = '2022-10-01',
        .dates_label = 'October',
        .inf = TRUE
      ),
      # All states, treatment is Nov. and Dec. 2022 (Post-election; pre-stay)
      est_fun(
        .data = bgc_data,
        .dates_start = c(-Inf, '2022-11-01'),
        .dates_stop = c('2022-09-01', '2022-12-01'),
        .dates_label = 'Post-election; pre-stay',
        .inf = TRUE
      ),
      # All states, first three months of 2023 (early post-stay)
      est_fun(
        .data = bgc_data,
        .dates_start = c(-Inf, '2023-01-01'),
        .dates_stop = c('2022-09-01', '2023-03-01'),
        .dates_label = 'Early post-stay',
        .inf = TRUE
      ),
      # All states, after April 2023 (long run)
      est_fun(
        .data = bgc_data,
        .dates_start = c(-Inf, '2023-04-01'),
        .dates_stop = c('2022-09-01', Inf),
        .dates_label = 'Long run',
        .inf = TRUE
      ),
      NULL
    ) |>
    rbindlist(use.names = TRUE, fill = TRUE)

# Estimates: Non-POC states --------------------------------------------------------------
  # Estimates for non-POC states
  results_nonpoc =
    list(
      # Non-POC states, all dates, total effect (all post periods)
      est_fun(
        .data = bgc_data,
        .subset = 'No POC',
        .inf = TRUE
      ),
      # Non-POC states, one treated month: 2022-10-01 (October)
      est_fun(
        .data = bgc_data,
        .subset = 'No POC',
        .dates_start = -Inf,
        .dates_stop = '2022-10-01',
        .dates_label = 'October',
        .inf = TRUE
      ),
      # Non-POC states, treatment is Nov. and Dec. 2022 (Post-election; pre-stay)
      est_fun(
        .data = bgc_data,
        .subset = 'No POC',
        .dates_start = c(-Inf, '2022-11-01'),
        .dates_stop = c('2022-09-01', '2022-12-01'),
        .dates_label = 'Post-election; pre-stay',
        .inf = TRUE
      ),
      # Non-POC states, first three months of 2023 (early post-stay)
      est_fun(
        .data = bgc_data,
        .subset = 'No POC',
        .dates_start = c(-Inf, '2023-01-01'),
        .dates_stop = c('2022-09-01', '2023-03-01'),
        .dates_label = 'Early post-stay',
        .inf = TRUE
      ),
      # Non-POC states, after April 2023 (long run)
      est_fun(
        .data = bgc_data,
        .subset = 'No POC',
        .dates_start = c(-Inf, '2023-04-01'),
        .dates_stop = c('2022-09-01', Inf),
        .dates_label = 'Long run',
        .inf = TRUE
      ),
      NULL
    ) |>
    rbindlist(use.names = TRUE, fill = TRUE)

# Estimates: Non-P2P states --------------------------------------------------------------
  # Estimates for non-P2P states
  results_nonp2p =
    list(
      # Non-P2P states, all dates, total effect (all post periods)
      est_fun(
        .data = bgc_data,
        .subset = 'No P2P',
        .inf = TRUE
      ),
      # Non-P2P states, one treated month: 2022-10-01 (October)
      est_fun(
        .data = bgc_data,
        .subset = 'No P2P',
        .dates_start = -Inf,
        .dates_stop = '2022-10-01',
        .dates_label = 'October',
        .inf = TRUE
      ),
      # Non-P2P states, treatment is Nov. and Dec. 2022 (Post-election; pre-stay)
      est_fun(
        .data = bgc_data,
        .subset = 'No P2P',
        .dates_start = c(-Inf, '2022-11-01'),
        .dates_stop = c('2022-09-01', '2022-12-01'),
        .dates_label = 'Post-election; pre-stay',
        .inf = TRUE
      ),
      # Non-P2P states, first three months of 2023 (early post-stay)
      est_fun(
        .data = bgc_data,
        .subset = 'No P2P',
        .dates_start = c(-Inf, '2023-01-01'),
        .dates_stop = c('2022-09-01', '2023-03-01'),
        .dates_label = 'Early post-stay',
        .inf = TRUE
      ),
      # Non-P2P states, after April 2023 (long run)
      est_fun(
        .data = bgc_data,
        .subset = 'No P2P',
        .dates_start = c(-Inf, '2023-04-01'),
        .dates_stop = c('2022-09-01', Inf),
        .dates_label = 'Long run',
        .inf = TRUE
      ),
      NULL
    ) |>
    rbindlist(use.names = TRUE, fill = TRUE)

# Estimates: Non-border states -----------------------------------------------------------
  # Estimates for non-border states
  results_nonborder =
    list(
      # Non-border states, all dates, total effect (all post periods)
      est_fun(
        .data = bgc_data,
        .subset = 'No border',
        .inf = TRUE
      ),
      # Non-border states, one treated month: 2022-10-01 (October)
      est_fun(
        .data = bgc_data,
        .subset = 'No border',
        .dates_start = -Inf,
        .dates_stop = '2022-10-01',
        .dates_label = 'October',
        .inf = TRUE
      ),
      # Non-border states, treatment is Nov. and Dec. 2022 (Post-election; pre-stay)
      est_fun(
        .data = bgc_data,
        .subset = 'No border',
        .dates_start = c(-Inf, '2022-11-01'),
        .dates_stop = c('2022-09-01', '2022-12-01'),
        .dates_label = 'Post-election; pre-stay',
        .inf = TRUE
      ),
      # Non-border states, first three months of 2023 (early post-stay)
      est_fun(
        .data = bgc_data,
        .subset = 'No border',
        .dates_start = c(-Inf, '2023-01-01'),
        .dates_stop = c('2022-09-01', '2023-03-01'),
        .dates_label = 'Early post-stay',
        .inf = TRUE
      ),
      # Non-border states, after April 2023 (long run)
      est_fun(
        .data = bgc_data,
        .subset = 'No border',
        .dates_start = c(-Inf, '2023-04-01'),
        .dates_stop = c('2022-09-01', Inf),
        .dates_label = 'Long run',
        .inf = TRUE
      ),
      NULL
    ) |>
    rbindlist(use.names = TRUE, fill = TRUE)

# Estimates: Non-Brady-exempt states -----------------------------------------------------
  # Estimates for non-Brady-exempt states
  results_brady =
    list(
      # Non-Brady-exempt states, all dates, total effect (all post periods)
      est_fun(
        .data = bgc_data,
        .subset = 'No Brady exempt',
        .inf = TRUE
      ),
      # Non-Brady-exempt states, one treated month: 2022-10-01 (October)
      est_fun(
        .data = bgc_data,
        .subset = 'No Brady exempt',
        .dates_start = -Inf,
        .dates_stop = '2022-10-01',
        .dates_label = 'October',
        .inf = TRUE
      ),
      # Non-Brady-exempt states, treatment is Nov. and Dec. 2022 (Post-election; pre-stay)
      est_fun(
        .data = bgc_data,
        .subset = 'No Brady exempt',
        .dates_start = c(-Inf, '2022-11-01'),
        .dates_stop = c('2022-09-01', '2022-12-01'),
        .dates_label = 'Post-election; pre-stay',
        .inf = TRUE
      ),
      # Non-Brady-exempt states, first three months of 2023 (early post-stay)
      est_fun(
        .data = bgc_data,
        .subset = 'No Brady exempt',
        .dates_start = c(-Inf, '2023-01-01'),
        .dates_stop = c('2022-09-01', '2023-03-01'),
        .dates_label = 'Early post-stay',
        .inf = TRUE
      ),
      # Non-Brady-exempt states, after April 2023 (long run)
      est_fun(
        .data = bgc_data,
        .subset = 'No Brady exempt',
        .dates_start = c(-Inf, '2023-04-01'),
        .dates_stop = c('2022-09-01', Inf),
        .dates_label = 'Long run',
        .inf = TRUE
      ),
      NULL
    ) |>
    rbindlist(use.names = TRUE, fill = TRUE)

# Save estimate objects ------------------------------------------------------------------
  # Save
  qsave(
    x = results_all,
    file = here('data', 'clean', 'analyses', 'standardized', 'results-all.qs'),
    preset = 'high'
  )
  qsave(
    x = results_nonpoc,
    file = here('data', 'clean', 'analyses', 'standardized', 'results-nonpoc.qs'),
    preset = 'high'
  )
  qsave(
    x = results_nonp2p,
    file = here('data', 'clean', 'analyses', 'standardized', 'results-nonp2p.qs'),
    preset = 'high'
  )
  qsave(
    x = results_nonborder,
    file = here('data', 'clean', 'analyses', 'standardized', 'results-nonborder.qs'),
    preset = 'high'
  )
  qsave(
    x = results_brady,
    file = here('data', 'clean', 'analyses', 'standardized', 'results-brady.qs'),
    preset = 'high'
  )
