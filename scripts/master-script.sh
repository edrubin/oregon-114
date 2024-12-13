# !/bin/bash

# Goal: Run all other scripts for the project.
# Note: Requires raw `data` folder in the project directory.

# Make sure we have the folders for figures and tables
# If they don't exist, create them
cd ..
mkdir -p exhibits
mkdir -p exhibits/figures
mkdir -p exhibits/tables
mkdir -p data/processed
cd scripts

# Ensure we have the required R packages
Rscript --vanilla 00-setup/00-install-packages.R

# Set the project directory as the `here` home
cd ..
Rscript --vanilla -e "here::set_here()"
cd scripts

# Script to scrape the NICS data
cd 01-scrape-nics
python3 -m venv venv
source venv/bin/activate
make update
cd ..

# Scripts to clean the data
# NOTE Different Rscript options for first two scripts to load Census API key
Rscript --no-restore --no-save 02-clean-data/01-prep-oregon-county-pop.R
Rscript --no-restore --no-save 02-clean-data/02-prep-state-pop.R
Rscript --vanilla 02-clean-data/03-clean-fbi-data.R
Rscript --vanilla 02-clean-data/04-clean-osp-data.R
Rscript --vanilla 02-clean-data/05-build-majority-voting.R
Rscript --vanilla 02-clean-data/06-build-quartile-voting.R
Rscript --vanilla 02-clean-data/07-combine-fbi-osp.R

# Scripts to analyze the data
Rscript --vanilla 03-analyze/01-main-analyses.R
# Script to produce exhibits
Rscript --vanilla 04-make-exhibits/01-make-tables.R
Rscript --vanilla 04-make-exhibits/01-make-figures.R
