# =====================================================================
# 03_prices.R  -  What does a property cost? (City of Vancouver property tax report)
# Data: City of Vancouver Open Data, "Property tax report" (BC Assessment values)
#
# IMPORTANT LIMITS (say these in the pitch):
#   * Assessed value is NOT a purchase price. It is BC Assessment's estimate of market
#     value, set at an earlier date. We treat it as a proxy and test +/-15% in script 04.
#   * The file has no bedroom counts, so we match on property TYPE, not bedrooms:
#       STRATA title -> Condo/Apartment      LAND title -> House
#   * We match to Airbnb neighbourhoods using the file's neighbourhood_code and a lookup
#     table YOU must fill in (see section 4). A wrong mapping is worse than none.
# =====================================================================

source("analysis/00_settings.R")

# ---------------------------------------------------------------------
# 1. LOAD (download the CSV by hand from the Open Data page into data/,
#    or let this try to fetch the 2026 report)
# ---------------------------------------------------------------------
if (!file.exists(FILE_TAX)) {
  cat("Tax file not found - trying to download the 2026 report...\n")
  options(timeout = 900)
  url <- paste0("https://opendata.vancouver.ca/api/explore/v2.1/catalog/datasets/",
                "property-tax-report/exports/csv?where=report_year%3D%272026%27&delimiter=%2C")
  ok <- tryCatch({ download.file(url, FILE_TAX, mode = "wb"); TRUE },
                 error = function(e) FALSE)
  if (!ok) stop("Download failed. Download the CSV by hand from the Vancouver Open Data ",
                "'Property tax report' page and save it as ", FILE_TAX)
}

tax <- read_any_csv(FILE_TAX) |> tidy_names()

cat("\nTax rows:", nrow(tax), "  columns:", ncol(tax), "\n")
cat("Columns:", paste(names(tax), collapse = ", "), "\n")

needed <- c("current_land_value", "current_improvement_value", "legal_type",
            "neighbourhood_code", "tax_assessment_year")
missing_cols <- setdiff(needed, names(tax))
if (length(missing_cols) > 0) stop("Missing columns: ", paste(missing_cols, collapse = ", "),
                                   "\nRun names(tax) and adjust this script.")

# ---------------------------------------------------------------------
# 2. LOOK BEFORE FILTERING (paste this output to your team)
# ---------------------------------------------------------------------
cat("\nTax assessment years in the file:\n");  print(count(tax, tax_assessment_year))
cat("\nLegal types:\n");                       print(count(tax, legal_type, sort = TRUE))
if ("zone_category" %in% names(tax)) {
  cat("\nZone categories:\n");                 print(count(tax, zone_category, sort = TRUE), n = 30)
}

# ---------------------------------------------------------------------
# 3. FILTER WITH A LOG (same idea as the Airbnb cleaning log)
# ---------------------------------------------------------------------
tax_log <- tibble()
add_log <- function(step, d) {
  tax_log <<- bind_rows(tax_log, tibble(
    step = step, rows = nrow(d),
    median_value = median(d$assessed_value, na.rm = TRUE)))
}

tax <- tax |>
  mutate(across(c(current_land_value, current_improvement_value), as.numeric),
         tax_assessment_year = as.integer(tax_assessment_year),
         assessed_value = current_land_value + current_improvement_value)
add_log("0. Raw", tax)

# The file can hold several years. Using more than one would count each property twice.
latest_year <- max(tax$tax_assessment_year, na.rm = TRUE)
tax <- tax |> filter(tax_assessment_year == latest_year)
add_log(paste0("A. Latest assessment year only (", latest_year, ")"), tax)

tax <- tax |> filter(!is.na(assessed_value), assessed_value >= PRICE_FLOOR)
add_log(paste0("B. Assessed value >= $", format(PRICE_FLOOR, big.mark = ",", scientific = FALSE)), tax)

tax <- tax |> mutate(property_group = case_when(
  legal_type == "STRATA" ~ "Condo/Apartment",
  legal_type == "LAND"   ~ "House",
  TRUE                   ~ NA_character_))
tax <- tax |> filter(!is.na(property_group))
add_log("C. Strata (condo) or Land (house) titles only", tax)

if ("zone_category" %in% names(tax)) {
  tax <- tax |> filter(str_detect(zone_category, regex(RESIDENTIAL_REGEX, ignore_case = TRUE)))
  add_log("D. Residential zones only", tax)
  cat("\nIf the row count after step D looks far too small, fix RESIDENTIAL_REGEX in 00_settings.R\n")
}

cat("\n===== TAX CLEANING LOG =====\n")
print(tax_log |> mutate(across(where(is.numeric), ~ round(.x))), n = Inf)
write_csv(tax_log, "outputs/tax_cleaning_log.csv")

# ---------------------------------------------------------------------
# 4. MATCH NEIGHBOURHOOD CODES TO AIRBNB NEIGHBOURHOOD NAMES
#    We do NOT guess. We write a template, you fill in the names.
# ---------------------------------------------------------------------
code_summary <- tax |>
  group_by(neighbourhood_code) |>
  summarise(n_records = n(), median_value = median(assessed_value), .groups = "drop") |>
  arrange(neighbourhood_code)

airbnb_names <- read_csv("outputs/listings_clean.csv", show_col_types = FALSE) |>
  distinct(neighbourhood_cleansed) |> arrange(neighbourhood_cleansed)

if (!file.exists(FILE_CODES)) {
  template <- code_summary |> mutate(neighbourhood_cleansed = "")
  write_csv(template, FILE_CODES)
  cat("\n*** No lookup table yet. A template was written to ", FILE_CODES, " ***\n", sep = "")
  cat("Open it, type the matching Airbnb neighbourhood name in the last column for each code\n")
  cat("(use the City's documentation for what each code means). Leave blank if unsure.\n")
  cat("Airbnb neighbourhood names to choose from:\n"); print(airbnb_names$neighbourhood_cleansed)
}

lookup <- read_csv(FILE_CODES, show_col_types = FALSE, col_types = cols(.default = "c")) |>
  select(neighbourhood_code, neighbourhood_cleansed) |>
  filter(!is.na(neighbourhood_cleansed), str_trim(neighbourhood_cleansed) != "")

# ---------------------------------------------------------------------
# 5. PRICE TABLE
#    If the lookup is filled in -> prices by neighbourhood x type.
#    If not -> a city-wide price by type (clearly labelled "ALL").
# ---------------------------------------------------------------------
summarise_values <- function(d, ...) {
  d |>
    group_by(...) |>
    summarise(n_tax        = n(),
              p25_value    = quantile(assessed_value, 0.25),
              median_value = median(assessed_value),
              p75_value    = quantile(assessed_value, 0.75),
              median_levy  = if ("tax_levy" %in% names(d)) median(as.numeric(tax_levy), na.rm = TRUE) else NA_real_,
              .groups = "drop") |>
    filter(n_tax >= MIN_TAX_N)
}

if (nrow(lookup) > 0) {
  tax_named <- tax |> mutate(neighbourhood_code = as.character(neighbourhood_code)) |>
    inner_join(lookup, by = "neighbourhood_code")
  cat("\nTax records matched to an Airbnb neighbourhood:", nrow(tax_named), "of", nrow(tax),
      "(", round(100 * nrow(tax_named) / nrow(tax)), "% )\n")
  prices <- summarise_values(tax_named, neighbourhood_cleansed, property_group)

  # Join check: which Airbnb neighbourhoods have NO price? (state this in Q&A)
  unmatched <- anti_join(airbnb_names, prices, by = "neighbourhood_cleansed")
  cat("Airbnb neighbourhoods with no price data:", nrow(unmatched), "\n")
  if (nrow(unmatched) > 0) print(unmatched)
} else {
  cat("\n*** FALLBACK: using ONE city-wide price per property type. ***\n")
  cat("Fill in ", FILE_CODES, " to get neighbourhood-level prices.\n", sep = "")
  prices <- summarise_values(tax, property_group) |> mutate(neighbourhood_cleansed = "ALL", .before = 1)
}

cat("\n===== PRICE TABLE =====\n")
print(prices |> mutate(across(where(is.numeric), ~ round(.x))), n = 40)
write_csv(prices, "outputs/prices_by_segment.csv")

cat("\nDone with 03_prices.R.\n")
