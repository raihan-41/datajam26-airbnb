# DataJam 2026 - Airbnb Vancouver (DDD $10M mandate)

Recommends where, how many and what type of short-term rental properties DDD should buy,
with an expected 2027 net yield and range. Language: R (RStudio).

## Data (not stored in this repo - download into a folder called `data/`)
| File | Source |
|---|---|
| `listings.csv.gz`, `calendar.csv.gz` | Inside Airbnb, Vancouver, 15 June 2026, CC BY 4.0 (Get the Data page, use the detailed listings file) |
| `property-tax-report.csv` | City of Vancouver Open Data, "Property tax report" (export as CSV) |
| `neighbourhood_code_lookup.csv` | Made by script 03; fill in the last column (see script) |

## How to run
1. Open the project in RStudio (File > Open Project, or set the working directory to this folder).
2. Install packages once: `install.packages(c("tidyverse", "lubridate", "scales"))`
3. Put the data files in `data/`.
4. Run `run_all.R` (or the scripts in `analysis/` in order, 01 to 04).
   On the first run, script 03 writes `data/neighbourhood_code_lookup.csv`. Fill it in, then re-run 03 and 04.

## Scripts
| Script | What it does |
|---|---|
| `00_settings.R` | Every assumption and cleaning setting in one place |
| `01_clean.R` | Cleans listings, logs every step, tests alternative cleaning choices, validates revenue, builds revenue tables |
| `02_calendar.R` | Cross-checks revenue with the calendar and shows monthly patterns |
| `03_prices.R` | Builds a price proxy (assessed value) from the City property tax report |
| `04_portfolio.R` | Net yield by segment, few-vs-many simulation, 2027 scenarios, charts |

Outputs (tables and charts) are written to `outputs/`.

## Limits
- Assessed value is a proxy for purchase price, not a sale price.
- Cost percentages in `00_settings.R` are assumptions, not data.
- Inside Airbnb revenue and occupancy are estimates, and the data is one scrape on one day.
- Compliance (licence) rates are estimates from an imperfect field.

## AI assistance
AI tools helped draft code and explanations. Every number in the pitch was traced to a script
and checked by a team member who can explain it.
