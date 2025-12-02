# Notes ----------------------------------------------------------------------------------
#   Goal:   Compare purchase location to home location for MA firearm purchases
#   Time:   < 7 seconds

# Output ---------------------------------------------------------------------------------

# Data notes -----------------------------------------------------------------------------
#   Data source of firearm purchases:
#     https://www.mass.gov/info-details/data-about-firearms-licensing-and-transactions
#   Data source of MA zip codes:
#     https://www.mass.gov/info-details/massgis-data-zip-codes-5-digit-from-here-navteq
#   Data source for MA FIPS codes and names:
#     https://transition.fcc.gov/oet/info/maps/census/fips/fips.txt

# Setup ----------------------------------------------------------------------------------
  # Load packages
  library(pacman)
  p_load(
    here, janitor, stringr, magrittr, tidyverse,
    data.table, collapse, sf, ggplot2, viridis, scales
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

# Load zipcode data ----------------------------------------------------------------------
  # Load MA county FIPS-NAME crosswalk
  co_xwalk =
    here('data', 'raw', 'massachusetts', 'county-fips.txt') |>
    fread(header = FALSE)
  # Fix table
  co_xwalk[, V3 := NULL]
  setnames(co_xwalk, c('fips', 'name'))
  # Names to uppercase
  co_xwalk[, name := name |> str_to_upper()]

# Load zipcode data ----------------------------------------------------------------------
  # Load MA zip shapefile
  zip_sf =
    here('data', 'raw', 'massachusetts', 'zipcodes_nt', 'ZIPCODES_NT_POLY.shp') |>
    st_read()
  # Grab desired columns and convert to data table
  zip_dt = zip_sf |> fselect(POSTCODE, COUNTY) |> as.data.table()
  zip_dt[, geometry := NULL]
  # Clean names
  setnames(
    zip_dt,
    zip_dt[0, ] |> clean_names() |> names()
  )
  # Merge on FIPS code
  zip_dt %<>%
    merge(
      y = co_xwalk,
      by.x = 'county',
      by.y = 'name'
    )
  # Create a mapping of zip code to a vector of matching counties
# NOTE One zip can map to multiple counties (4% of cases)
  zip_co =
    zip_dt[, .(
      counties = fips |> funique() |> paste(collapse = '|')
    ), by = postcode]

# Load purchase data ---------------------------------------------------------------------
  # Find files
  file_v =
    here('data', 'raw', 'massachusetts') |>
    dir(pattern = 'Dealer', full.names = TRUE) |>
    dir(full.names = TRUE)
  # Load MA firearm license data
  ma_dt =
    lapply(
      X = file_v,
      FUN = fread, showProgress = FALSE
    ) |>
    rbindlist(use.names = TRUE, fill = TRUE)
  # Clean names
  setnames(
    ma_dt,
    ma_dt[0, ] |> clean_names() |> names()
  )
  # Drop strange entry
  ma_dt %<>% .[transaction_type != 'MASS ROWDY']
  # Grab desired variables
  ma_dt =
    ma_dt[, .(
      date_chr = date_of_transaction,
      ffl = ffl_number,
      dealer_zip = dealer_shop_zip,
      buyer_zip
    )]
  # Format dates
  ma_dt[nchar(date_chr) == 8, date := date_chr |> mdy()]
  ma_dt[nchar(date_chr) == 19, date := date_chr |> ymd_hms() |> as.Date()]
  ma_dt[, date_chr := NULL]
  # Pad zip codes
  ma_dt[, `:=`(
    dealer_zip = dealer_zip |> str_pad(5, 'left', 0),
    buyer_zip = buyer_zip |> str_pad(5, 'left', 0)
  )]
  # Back out dealer county FIPS
  ma_dt[, `:=`(
    dealer_co_ffl = paste0('25', str_sub(ffl, 4, 6))
  )]
  # Collapse to dealer-buyer zip counts (don't need microdata)
  ag_co =
    ma_dt[, .(
      n = .N
    ), by = .(dealer_co_ffl, dealer_zip, buyer_zip)]
  # Merge on counties implied by zips
  ag_co %<>%
    merge(
      y = zip_co[, .(dealer_zip = postcode, dealer_co_z = counties)],
      by = 'dealer_zip',
      all.x = TRUE,
      all.y = FALSE
    )
  ag_co %<>%
    merge(
      y = zip_co[, .(buyer_zip = postcode, buyer_co_z = counties)],
      by = 'buyer_zip',
      all.x = TRUE,
      all.y = FALSE
    )

# Summary of shopping patterns: County matches -------------------------------------------
  # Exact county-level matches (conservative)
  # 56.5%
  ag_co[buyer_co_z == dealer_co_z, fsum(n)] / ag_co[, fsum(n)]
  # Removing uncertainty in matches (i.e., dropping multi-cnty zips)
  # 59.6%
  sub = ag_co[nchar(buyer_co_z) == 5 & nchar(dealer_co_z) == 5]
  sub %<>% .[dealer_co_z == dealer_co_ffl]
  sub[buyer_co_z == dealer_co_z, fsum(n)] / sub[, fsum(n)]
  # Does dealer county (FFL) match any of buyer's counties?
  # 57.9%
  ag_co[str_detect(string = dealer_co_ffl, pattern = buyer_co_z), fsum(n)] /
    ag_co[, fsum(n)]

# Calculate distances between buyer and dealer zips --------------------------------------
  # Calculate distances between zip codes for observed zip codes (using centroids)
# NOTE All dealer zips are included in buyer zips
  buyer_sf =
    zip_sf |>
    fsubset(POSTCODE %in% funique(ma_dt$buyer_zip)) |>
    fselect(buyer_zip = POSTCODE, area = AREA_SQMI)
  # Take centroids of grouped zip codes (some have multiple polygons)
  buyer_sf =
    buyer_sf |>
    group_by(buyer_zip) |>
    summarize(st_union(geometry)) |>
    st_centroid()
  # Calculate distances
  zip_dist = st_distance(x = buyer_sf)
  # As data table
  rownames(zip_dist) = buyer_sf$buyer_zip
  colnames(zip_dist) = buyer_sf$buyer_zip
  dist_dt = zip_dist |> as.data.frame.table() |> as.data.table()
  # Clean up names and convert to km
  setnames(
    dist_dt,
    c('buyer_zip', 'dealer_zip', 'dist')
  )
  # Reformat variables
  dist_dt[, `:=`(
    buyer_zip = buyer_zip |> as.character(),
    dealer_zip = dealer_zip |> as.character(),
    dist = as.numeric(dist / 1e3)
  )]
  # Join distances with aggregated buyer-dealer counts
  ag_dist =
    merge(
      x = ma_dt[, .(n = .N), .(buyer_zip, dealer_zip)],
      y = dist_dt,
      by = c('buyer_zip', 'dealer_zip'),
      all.x = TRUE,
      all.y = FALSE
    )

# Summary of shopping patterns: Distances ------------------------------------------------
  # Summarize distance distribution (weighting by n)
# NOTE OR's area is 254,810 km2 with 36 counties
#      average (square) county is 84 km wide
#      We test from 42 km (essentially center to next county)
  ag_dist[, .(
    mean = dist |> fmean(w = n),
    median = dist |> fmedian(w = n),
    p05 = dist |> fnth(n = .05, w = n),
    p10 = dist |> fnth(n = .10, w = n),
    p25 = dist |> fnth(n = .25, w = n),
    p75 = dist |> fnth(n = .75, w = n),
    p90 = dist |> fnth(n = .90, w = n),
    p95 = dist |> fnth(n = .95, w = n),
    pct_within_42km = fmean(dist < 42, w = n),
    pct_within_50km = fmean(dist < 50, w = n)
  )]


# Histogram of distances -----------------------------------------------------------------
  # Define max distance for plotting
  max_dist = 150
  # Plot histogram of distances
  dist_plot = ggplot(
    data =
      ag_dist[!is.na(dist), .(dist = rep(dist, n))] |>
      fmutate(dist = fifelse(dist > max_dist, max_dist, dist)),
    aes(x = dist),
  ) +
  geom_histogram(
    bins = ceiling(max_dist * .7),
    fill = col_trt,
    color = col_trt,
  ) +
  geom_hline(yintercept = 0, linewidth = .25) +
  scale_y_continuous(
    'Count (firearms purchased)',
    labels = comma,
  ) +
  scale_x_continuous(
    'Firearm buyer\'s distance to dealer (km)',
    breaks = seq(0, max_dist, by = 50),
    labels = c(
      seq(0, max_dist - 50, by = 50) |> comma(),
      paste0(max_dist, '+')
    )
  ) +
  theme_minimal(
    base_size = 16,
    base_family = 'Fira Sans Condensed',
  ) +
  coord_cartesian(
    xlim = c(0, max_dist),
  )
# Save plot
  ggsave(
    plot = dist_plot,
    path = here('exhibits', 'figures'),
    filename = 'mass-hist-purchase-distances.pdf',
    device = cairo_pdf,
    # filename = 'mass-hist-purchase-distances.png',
    # device = ragg::agg_png,
    width = 12,
    height = 5
  )
