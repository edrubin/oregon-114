# Oregon Measure 114

## Intro

This repository contains the code and data for our analysis of Oregon Measure 114's effects on the demand for firearms in Oregon. [Measure 114](https://en.wikipedia.org/wiki/Oregon_Ballot_Measure_114) was approved by Oregon's voters in November 2022 but encountered legal issues and never went into law. We use this policy variation to study how the threat of firearm restrictions affects the demand for firearms.

Paper draft coming soon...

## Reproducing the results

To run the master script `master-script.sh`:

1. ensure you've downloaded the requisite files (and match the structure) of `data/raw`;
2. open a terminal in the root directory of the project (i.e., `oregon-114`);
3. navigate to the `scripts` directory, i.e., `cd ./scripts`;
4. make the script executable with `chmod +x master-script.sh`;
5. run the script with `./master-script.sh`.

*Optional but **recommended*** (before running the script): 

- obtain a [Census API key](http://api.census.gov/data/key_signup.html);
- add the key to your `.Renviron` by running (in R):

```R
tidycensus::census_api_key('your_key_here', install = TRUE)
```

*Credit:* The NICS scraping uses the [nics-firearm-background-checks](https://github.com/BuzzFeedNews/nics-firearm-background-checks) Github repo published by Jeremy Singer-Vine.
