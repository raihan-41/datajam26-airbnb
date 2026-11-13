# Run the whole analysis in order. Open the project in RStudio, then run this file.
# (The project folder must be the working directory: use File > Open Project.)
source("analysis/01_clean.R")
source("analysis/02_calendar.R")
source("analysis/03_prices.R")
source("analysis/04_portfolio.R")
