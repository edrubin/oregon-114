# Notes ----------------------------------------------------------------------------------
#   Goal:   Make main tables
#   Time:   < 1 minute

# Output ---------------------------------------------------------------------------------
#   exhibits/tables/tab-00-sdid-main.tex
#   exhibits/tables/tab-01-sdid-main.tex
#     exhibits/tables/tab-01a-sdid-main.tex
#     exhibits/tables/tab-01b-sdid-main.tex
#   exhibits/tables/tab-02-methods.tex
#     exhibits/tables/tab-02a-methods.tex
#     exhibits/tables/tab-02b-methods.tex

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
    here('data', 'clean', 'analyses', 'results-all.qs') |>
    qread()
  results_nonpoc =
    here('data', 'clean', 'analyses', 'results-nonpoc.qs') |>
    qread()
  results_nonp2p =
    here('data', 'clean', 'analyses', 'results-nonp2p.qs') |>
    qread()
  results_nonborder =
    here('data', 'clean', 'analyses', 'results-nonborder.qs') |>
    qread()
  results_brady =
    here('data', 'clean', 'analyses', 'results-brady.qs') |>
    qread()
  # Make a list of results
  res_list = list(results_nonborder, results_nonpoc, results_nonp2p, results_brady)

# Function to wrap vector entries in parentheses -----------------------------------------
  # The function
  wrap = function(x) {
    x |> as.character() %>% paste0('(', ., ')')
  }

# Table 0: Synth DID results from one sample ---------------------------------------------
  # Define desired spatial sample
  res = copy(results_nonborder)
  # Format estimate and standard error
  tab =
    rbind(
      res$est_sdid |> comma(.1),
      res$se_sdid |> comma(.1) |> wrap()
    )
  # Add cumulative effect
  tab = rbind(tab, res$accum_sdid |> comma(.1))
  # Add SE for cumulative effect
  tab = rbind(
    tab,
    c(
      res$se_sdid[1] * 18,
      res$se_sdid[2],
      res$se_sdid[3] * 2,
      res$se_sdid[4] * 3,
      res$se_sdid[5] * 12
    ) |>
    comma(.1) |>
    wrap()
  )
  # Date information
  # tab = rbind(tab, res$dates)
  tab = rbind(
    tab,
    c(
      '{\\small Oct. `22--Mar. `24}',
      '{\\small Oct. `22}',
      '{\\small Nov. `22--Dec. `23}',
      '{\\small Jan. `23--Mar. `23}',
      '{\\small Apr. `23--Mar. `24}'
    )
  )
  # Add row names
  tab = cbind(
    c('Monthly effect', '', 'Accum. effect', '', 'Trt. period'),
    tab
  )
  # Add column names
  c_names = c('', paste0('(', 1:5, ')'))
  tab =
    tab |>
    kbl(
      escape = FALSE,
      format = 'latex',
      align = 'lccccc',
      row.names = FALSE,
      col.names = c_names,
      booktabs = TRUE
    ) |>
    column_spec(1, italic = TRUE) |>
    save_kable(here('exhibits', 'tables', 'tab-00-sdid-main.tex'))
  tab =
    here('exhibits', 'tables', 'tab-00-sdid-main.tex') |>
    readLines()
  # Update alignment definition
  tab[grepl('lcc', tab)] = '\\begin{tabular}{@{}lccccc@{}}'
  # Drop toprule
  tab = tab[!grepl('toprule', tab)]
  # Drop empty line
  tab %<>% tail(-1)
  # Add top-of-table text
  tab =
    c(
      tab[1],
      paste0(
        '& \\textit{Pooled} & \\multicolumn{4}{c}{\\textit{Decomposed}} \\\\',
        '\\cmidrule(lr){2-2} \\cmidrule(lr){3-6}'
      ),
      paste0(
        '&& \\textit{\\small Pre} & \\textit{\\small Immediate} &',
        '\\textit{\\small Post-stay} & \\textit{\\small Long-run} \\\\'
      ),
      tab[-1]
    )
  # Add line spacing after rows with standard errors and accumulation effect
  tab[c(7, 9)] = paste0(tab[c(7, 9)], '\\addlinespace')
  # Save
  writeLines(
    text = tab,
    con = here('exhibits', 'tables', 'tab-00-sdid-main.tex')
  )

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
        n_st - n_distinct(brady_states)
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
        'Non-Brady-exempt'
      )
    ))
  # Define column names
  c_names = c(
    'Time frame',
    paste0('(', 1:4, ')')
  )
  # Start latex table
# NOTE Some formatting done by hand on Overleaf
  main_table |>
    kbl(format = 'latex', align = 'lcccc', col.names = c_names, booktabs = TRUE) |>
    column_spec(1, italic = TRUE) |>
    save_kable(here('exhibits', 'tables', 'tab-01a-sdid-main.tex'))
  acc_table |>
    kbl(format = 'latex', align = 'lcccc', col.names = c_names, booktabs = TRUE) |>
    column_spec(1, italic = TRUE) |>
    save_kable(here('exhibits', 'tables', 'tab-01b-sdid-main.tex'))
  # Load saved tables
  t1 =
    here('exhibits', 'tables', 'tab-01a-sdid-main.tex') |>
    readLines()
  t2 =
    here('exhibits', 'tables', 'tab-01b-sdid-main.tex') |>
    readLines()
  # Add indentation
  t1[grepl('^\\\\em', t1)] = paste0('\\quad', t1[grepl('^\\\\em', t1)])
  t2[grepl('^\\\\em', t2)] = paste0('\\quad', t2[grepl('^\\\\em', t2)])
  # Combine tables
  final_table = c(
    t1 |> head(5),
    paste0(
      '\\multicolumn{5}{l}{\\hspace*{-.5em}',
      '\\textit{\\textbf{Panel A:} Monthly effect (per 100k)}} \\\\',
    ),
    t1 |> tail(-5) |> head(-2),
    '\\midrule',
    paste0(
      '\\multicolumn{5}{l}{\\hspace*{-.5em}',
      '\\textit{\\textbf{Panel B:} Accumulated effect (per 100k)}} \\\\',
    ),
    t2 |> tail(-5)
  )
  # Update alignment definition
  final_table[grepl('lcc', final_table)] =
    '\\begin{tabular}{@{}lcccc@{}}'
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
    con = here('exhibits', 'tables', 'tab-01-sdid-main.tex')
  )


# Table 2: Comparing estimators ----------------------------------------------------------
  # Choose set of results
  res = results_nonborder
  # Combine estimates (column per spatial subset; row per temporal subset)
  main_table =
    map_dfr(
      .x = seq_len(nrow(res)),
      .f = function(i) {
        rbind(
          res[i, .(est_sdid, est_scm, est_did)] |> unlist() |> comma(.1),
          res[i, .(se_sdid, se_scm, se_did)] |> unlist() |> comma(.1) |> wrap()
        ) |>
        as.data.table()
      }
    )
  setDT(main_table)
  setnames(main_table, paste0('c', seq_len(ncol(main_table))))
  main_table = main_table[, lapply(.SD, as.character)]
  # Cumulative effects
  acc_table = res[, .(accum_sdid, accum_scm, accum_did)]
  acc_table %<>% .[, lapply(.SD, comma, accuracy = .1)]
  setDT(acc_table)
  setnames(acc_table, paste0('c', seq_len(ncol(acc_table))))
  # Add column for time periods
  main_table =
    cbind(
      data.table(
        a = c(rbind(results_all$dates, rep('', main_table[, .N / 2])))
      ),
      main_table
    )
  acc_table = cbind(data.table(a = results_all$dates), acc_table)
  # Define column names
  c_names = c(
    'Time frame',
    wrap(1:3)
  )
  # Start latex table
# NOTE Some formatting done by hand on Overleaf
  main_table |>
    kbl(format = 'latex', align = 'lcccc', col.names = c_names, booktabs = TRUE) |>
    column_spec(1, italic = TRUE) |>
    save_kable(here('exhibits', 'tables', 'tab-02a-methods.tex'))
  acc_table |>
    kbl(format = 'latex', align = 'lcccc', col.names = c_names, booktabs = TRUE) |>
    column_spec(1, italic = TRUE) |>
    save_kable(here('exhibits', 'tables', 'tab-02b-methods.tex'))
  # Load saved tables
  t1 =
    here('exhibits', 'tables', 'tab-02a-methods.tex') |>
    readLines()
  t2 =
    here('exhibits', 'tables', 'tab-02b-methods.tex') |>
    readLines()
  # Add indentation
  t1[grepl('^\\\\em', t1)] = paste0('\\quad', t1[grepl('^\\\\em', t1)])
  t2[grepl('^\\\\em', t2)] = paste0('\\quad', t2[grepl('^\\\\em', t2)])
  # Combine tables
  final_table = c(
    t1 |> head(5),
    paste0(
      '\\multicolumn{4}{l}{\\hspace*{-.5em}',
      '\\textit{\\textbf{Panel A:} Monthly effect (per 100k)}} \\\\'
    ),
    t1 |> tail(-5) |> head(-2),
    '\\midrule',
    paste0(
      '\\multicolumn{4}{l}{\\hspace*{-.5em}',
      '\\textit{\\textbf{Panel B:} Accumulated effect (per 100k)}} \\\\'
    ),
    t2 |> tail(-5)
  )
  # Update alignment definition
  final_table[grepl('lcc', final_table)] =
    '\\begin{tabular}{@{}lccc@{}}'
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
  # Drop toprule
  final_table = final_table[!grepl('toprule', final_table)]
  # Add text above table
  final_table = c(
    final_table |> head(2),
    '& \\multicolumn{3}{c}{\\textbf{Estimator}} \\\\ \\cmidrule(lr){2-4}',
    '& \\textit{SDID} & \\textit{SCM} & \\textit{DID} \\\\',
    final_table |> tail(-2)
  )
  # Save final table
  writeLines(
    text = final_table,
    con = here('exhibits', 'tables', 'tab-02-methods.tex')
  )
