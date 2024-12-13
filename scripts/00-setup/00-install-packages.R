# Notes ----------------------------------------------------------------------------------
#   Goal:   Install all necessary packages for the project.
#   Time:   Depends on the number of packages already installed (and other stuff?).

# Install packages -----------------------------------------------------------------------
  # Install `pacman`
  install.packages('pacman', repos = 'https://cloud.r-project.org/')
  # Use `pacman` to install CRAN packages
  pacman::p_load(
    renv, here, devtools, tidyverse, readxl, janitor, tidyr, magrittr, data.table,
    tidycensus, lubridate, gsynth, zoo, ggpubr, cowplot, eventstudyr, fixest,
    rmarkdown, kableExtra, httpgd
  )
  # Install `synthdid` from GitHub
  devtools::install_github('synth-inference/synthdid')
  # Initialize `renv`
  renv::init()
