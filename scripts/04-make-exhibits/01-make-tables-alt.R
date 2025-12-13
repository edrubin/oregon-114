# Notes ----------------------------------------------------------------------------------
#   Goal:   Make alternative-gun-count tables
#   Time:   < 1 minute

# Output ---------------------------------------------------------------------------------
#   exhibits/tables/tab-00-sdid-alt.tex
#   exhibits/tables/tab-01-sdid-alt.tex
#     exhibits/tables/tab-01a-sdid-alt.tex
#     exhibits/tables/tab-01b-sdid-alt.tex

# Setup ----------------------------------------------------------------------------------
  # Load packages
  pacman::p_load(
    here, janitor, tidyr, magrittr, data.table, qs, parallel, future.apply,
    tidyverse, scales, gsynth, kableExtra
  )

# Define sets of states ------------------------------------------------------------------
  # Load definitions
  here('scripts', '03-run-analyses', '00-define-groups.R') |> source()

# Load results ---------------------------------------------------------------------------
  # Load results
  results_all =
    here('data', 'clean', 'analyses', 'results-alt-guns-all.qs') |>
    qread()
  results_nonpoc =
    here('data', 'clean', 'analyses', 'results-alt-guns-nonpoc.qs') |>
    qread()
  results_nonp2p =
    here('data', 'clean', 'analyses', 'results-alt-guns-nonp2p.qs') |>
    qread()
  results_nonborder =
    here('data', 'clean', 'analyses', 'results-alt-guns-nonborder.qs') |>
    qread()
  results_brady =
    here('data', 'clean', 'analyses', 'results-alt-guns-brady.qs') |>
    qread()
  results_recheck =
    here('data', 'clean', 'analyses', 'results-alt-guns-recheck.qs') |>
    qread()
  # Make a list of results
  res_list =
  list(results_nonborder, results_nonpoc, results_nonp2p, results_brady, results_recheck)

# Function to wrap vector entries in parentheses -----------------------------------------
  # The function
  wrap = function(x) {
    x |> as.character() %>% paste0('(', ., ')')
  }

# Table 1: Synth DID results -------------------------------------------------------------
  # Combine estimates (column per spatial subset; row per temporal subset)
  main_table =
    map_dfc(
      .x = res_list,
      .f = function(obj) {
        obj[, .(est_sdid, se_sdid)] |>
          pivot_longer(cols = c('est_sdid', 'se_sdid')) |>
          select(value) |>
          mutate(value = comma(value, accuracy = .1))
      }
    )
  setDT(main_table)
  setnames(main_table, paste0('c', seq_len(ncol(main_table))))
  main_table = main_table[, lapply(.SD, as.character)]
  # Cumulative effects
  acc_table =
    map_dfc(
      .x = res_list,
      .f = function(obj) {
        obj[, .(accum_sdid |> comma(accuracy = .1))]
      }
    )
  setDT(acc_table)
  setnames(acc_table, paste0('c', seq_len(ncol(acc_table))))
  # Wrap SEs (even rows) in parentheses
  for (j in seq_len(ncol(main_table))) {
    set(
      x = main_table,
      i = seq(2, main_table[, .N], 2),
      j = j,
      value =
        main_table[seq(2, .N, 2), ..j] %>%
        unlist() %>%
        paste0('(', ., ')')
    )
  }
  # Add column for time periods
  main_table =
    cbind(
      data.table(
        a = c(rbind(results_all$dates, rep('', main_table[, .N / 2])))
      ),
      main_table
    )
  acc_table = cbind(data.table(a = results_all$dates), acc_table)
  # Number of donor states to accumulation effect table
  n_st =
    here('data', 'clean', 'background-checks', 'bgc-osp-fbi.csv') |>
    fread() |>
    select(state) |>
    unlist() |>
    n_distinct() |>
    subtract(1)
  acc_table =
    rbindlist(list(
      acc_table,
      data.table(
        'N. Potential Donor States',
        n_st - n_distinct(border_states),
        n_st - n_distinct(poc_states),
        n_st - n_distinct(p2p_states),
        n_st - n_distinct(brady_states),
        n_st - n_distinct(recheck_states)
      )
    ))
  # Sample
  acc_table =
    rbindlist(list(
      acc_table,
      data.table(
        'Potential control states',
        'Non-border',
        'Non-POC',
        'Non-P2P',
        'Non-Brady-ex.',
        'Non-Recheck'
      )
    ))
  # Define column names
  c_names = c(
    'Time frame',
    paste0('(', 1:5, ')')
  )
  # Start latex table
# NOTE Some formatting done by hand on Overleaf
  main_table |>
    kbl(format = 'latex', align = 'lccccc', col.names = c_names, booktabs = TRUE) |>
    column_spec(1, italic = TRUE) |>
    save_kable(here('exhibits', 'tables', 'tab-01a-sdid-alt.tex'))
  acc_table |>
    kbl(format = 'latex', align = 'lccccc', col.names = c_names, booktabs = TRUE) |>
    column_spec(1, italic = TRUE) |>
    save_kable(here('exhibits', 'tables', 'tab-01b-sdid-alt.tex'))
  # Load saved tables
  t1 =
    here('exhibits', 'tables', 'tab-01a-sdid-alt.tex') |>
    readLines()
  t2 =
    here('exhibits', 'tables', 'tab-01b-sdid-alt.tex') |>
    readLines()
  # Add indentation
  t1[grepl('^\\\\em', t1)] = paste0('\\quad', t1[grepl('^\\\\em', t1)])
  t2[grepl('^\\\\em', t2)] = paste0('\\quad', t2[grepl('^\\\\em', t2)])
  # Combine tables
  final_table = c(
    t1 |> head(5),
    paste0(
      '\\multicolumn{5}{l}{\\hspace*{-.5em}',
      '\\textit{\\textbf{Panel A:} Monthly effect (per 100k)}} \\\\'
    ),
    t1 |> tail(-5) |> head(-2),
    '\\midrule',
    paste0(
      '\\multicolumn{5}{l}{\\hspace*{-.5em}',
      '\\textit{\\textbf{Panel B:} Accumulated effect (per 100k)}} \\\\'
    ),
    t2 |> tail(-5)
  )
  # Update alignment definition
  final_table[grepl('lcc', final_table)] =
    '\\begin{tabular}{@{}lccccc@{}}'
  # Italicize and indent time frame title
  final_table %<>% str_replace('Time frame', '\\\\quad\\\\textit{Time frame}')
  # Drop orphan linespace
  final_table = final_table[!grepl('addlinespace', final_table)]
  # Add some line spacing after rows with standard errors (with parentheses)
  final_table = final_table |>
    map_chr(~{
      if (grepl('\\([0-9].*\\)', .x)) {
        paste0(.x, '\\addlinespace')
      } else {
        .x
      }
    })
  # Remove linespace after column titles
  final_table[1:4] = final_table[1:4] |> str_remove('\\\\addlinespace')
  # Add line before 'N. Potential Donor States'
  i = which(grepl('N. Potential', final_table))
  final_table = c(
    final_table[1:(i - 1)],
    '\\midrule',
    final_table[i:length(final_table)]
  )
  # Drop toprule
  final_table = final_table[!grepl('toprule', final_table)]
  # Save final table
  writeLines(
    text = final_table,
    con = here('exhibits', 'tables', 'tab-01-sdid-alt.tex')
  )
