# =====================================================================
# 00_settings.R  -  ALL assumptions live here
# Every other script starts with:  source("analysis/00_settings.R")
# Change a number here, re-run run_all.R, and every table/chart updates.
# Anything marked ASSUMPTION is NOT in the data: say so out loud in the pitch.
# =====================================================================

library(tidyverse)   # data wrangling + charts
library(lubridate)   # dates
library(scales)      # $ and % labels

dir.create("outputs", showWarnings = FALSE)

# ---- File locations (put the downloaded files in data/) -------------
FILE_LISTINGS <- "data/listings.csv.gz"
FILE_CALENDAR <- "data/calendar.csv.gz"
FILE_TAX      <- "data/property-tax-report.csv"   # City of Vancouver Open Data
FILE_CODES    <- "data/neighbourhood_code_lookup.csv"  # you fill this in (script 03 explains)

# ---- Cleaning choices (each one is a decision-log entry) ------------
MAX_MIN_NIGHTS <- 28     # keep listings whose minimum stay is BELOW this many nights
ACTIVE_ONLY    <- TRUE   # TRUE = keep only listings with >= 1 review in last 12 months
PRICE_CAP_PCT  <- 0.99   # drop nightly prices above this percentile (use 1 for no cap)
MIN_GROUP_N    <- 15     # smallest number of listings we trust for a group

# ---- Money assumptions (ASSUMPTIONS - change and test them) ---------
BUDGET            <- 10e6   # DDD mandate, CAD
CLOSING_COST_PCT  <- 0.04   # ASSUMPTION: transfer tax, legal, inspection, as share of price
VARIABLE_COST_PCT <- 0.30   # ASSUMPTION: platform fee + cleaning + management + supplies, share of revenue
FIXED_COST_PCT    <- 0.010  # ASSUMPTION: insurance + utilities + maintenance, share of price per year
STRATA_FEE_PCT    <- 0.005  # ASSUMPTION: strata fees for condos only, share of price per year
PROPERTY_TAX_FALLBACK_PCT <- 0.003  # ASSUMPTION: used only when the tax file has no tax_levy for a group
LICENCE_FEE       <- 1000   # ASSUMPTION: annual licence/registration cost. CHECK the City fee page.
RAMP_UP_HAIRCUT   <- 0.10   # ASSUMPTION: new listings start with no reviews, so earn 10% less in 2027

# ---- Property tax data choices --------------------------------------
PRICE_FLOOR     <- 300000   # ignore assessed values below this (parking stalls, storage lockers)
MIN_TAX_N       <- 30       # smallest number of tax records we trust for a group
RESIDENTIAL_REGEX <- "Dwelling|Multiple|Comprehensive|Residential"  # check with count(tax, zone_category)

# ---- Scenarios for the 2027 range -----------------------------------
# rev_mult = change in revenue vs. trailing year; var_pct = variable cost share;
# months_lost = months of revenue lost (licence delay or an opt-out ban)
SCENARIOS <- tribble(
  ~scenario,                 ~rev_mult, ~var_pct, ~months_lost,
  "Low",                      0.80,      0.40,     3,
  "Base",                     1.00,      0.30,     0,
  "High",                     1.10,      0.25,     0,
  "Stress: opt-out Jun 2027", 1.00,      0.30,     7
)

# ---- Shared helpers --------------------------------------------------
ia_caption <- "Data: Inside Airbnb, Vancouver, 15 June 2026, CC BY 4.0"
tax_caption <- paste0(ia_caption, " | City of Vancouver Open Data")

# Make column names lowercase_with_underscores (replaces the janitor package)
tidy_names <- function(df) {
  names(df) <- names(df) |> tolower() |> str_replace_all("[^a-z0-9]+", "_") |> str_remove("_$")
  df
}

# Read a CSV whose separator might be ; (Vancouver portal) or , (everything else)
read_any_csv <- function(path) {
  first_line <- readLines(path, n = 1, warn = FALSE)
  sep <- if (str_count(first_line, ";") > str_count(first_line, ",")) ";" else ","
  read_delim(path, delim = sep, show_col_types = FALSE, guess_max = 100000)
}