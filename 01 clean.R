# =====================================================================
# 01_clean.R  -  Clean the Inside Airbnb listings and measure revenue
# Data: Inside Airbnb, Vancouver, 15 June 2026, CC BY 4.0
#
# What this script produces (all in outputs/):
#   cleaning_log.csv          rows left + median revenue after EVERY step  -> decision log
#   sensitivity.csv           same data cleaned 4 different ways           -> decision log
#   revenue_by_segment.csv    revenue by neighbourhood x property type     -> used by 04
#   listings_clean.csv        one row per cleaned listing (has ids: never show on slides)
#   charts (.png)             for the slides
# =====================================================================

source("analysis/00_settings.R")

# ---------------------------------------------------------------------
# 1. LOAD
# ---------------------------------------------------------------------
raw <- read_csv(FILE_LISTINGS, show_col_types = FALSE) |> tidy_names()
cat("\nRaw rows:", nrow(raw), "  Raw columns:", ncol(raw), "\n")

needed <- c("id", "price", "room_type", "property_type", "minimum_nights",
            "neighbourhood_cleansed", "bedrooms", "availability_365",
            "estimated_occupancy_l365d", "estimated_revenue_l365d",
            "number_of_reviews_ltm", "license")
missing_cols <- setdiff(needed, names(raw))
if (length(missing_cols) > 0) stop("Missing columns: ", paste(missing_cols, collapse = ", "))

# ---------------------------------------------------------------------
# 2. KEEP ONLY WHAT WE NEED (this also drops host names, URLs, descriptions)
#    and turn text into numbers. Prices are stored as text like "$1,234.00".
# ---------------------------------------------------------------------
df0 <- raw |>
  transmute(
    id,
    neighbourhood_cleansed,
    room_type,
    property_type,
    bedrooms,
    accommodates,
    minimum_nights,
    availability_365,
    number_of_reviews_ltm,
    estimated_occupancy_l365d,
    price                   = parse_number(as.character(price)),
    estimated_revenue_l365d = parse_number(as.character(estimated_revenue_l365d)),
    has_licence             = !is.na(license) & str_trim(license) != ""   # flag only, never the number
  )

# Group property types into things DDD can actually buy.
#  House  = a detached home (matches a "land" title in the tax data)
#  Condo  = apartment / condo / townhouse (matches a "strata" title in the tax data)
#  Other  = guest suites, guesthouses etc. (not sold as separate properties)
df0 <- df0 |>
  mutate(
    pt = str_to_lower(property_type),
    property_group = case_when(
      str_detect(pt, "townhouse|condo|rental unit|apartment|loft|serviced") ~ "Condo/Apartment",
      str_detect(pt, "home|house|villa|bungalow|cottage|cabin|chalet")       ~ "House",
      TRUE ~ "Other"),
    bedroom_group = case_when(
      is.na(bedrooms) ~ "Unknown",
      bedrooms <= 1   ~ "Studio/1BR",
      bedrooms == 2   ~ "2BR",
      TRUE            ~ "3+BR")
  ) |>
  select(-pt)

cat("\nProperty types and how we grouped them (CHECK THIS LOOKS SENSIBLE):\n")
print(df0 |> filter(room_type == "Entire home/apt") |>
        count(property_type, property_group, sort = TRUE), n = 30)

# ---------------------------------------------------------------------
# 3. A CLEANING FUNCTION WITH A LOG
#    Wrapping the steps in a function lets us re-run them with different
#    settings (section 5) and compare - that comparison IS your decision log.
# ---------------------------------------------------------------------
clean_listings <- function(data, active_only = ACTIVE_ONLY, price_cap_pct = PRICE_CAP_PCT) {
  
  log <- tibble()
  add_log <- function(step, d) {
    log <<- bind_rows(log, tibble(
      step           = step,
      rows           = nrow(d),
      median_revenue = median(d$estimated_revenue_l365d, na.rm = TRUE),
      mean_revenue   = mean(d$estimated_revenue_l365d, na.rm = TRUE),
      median_price   = median(d$price, na.rm = TRUE)))
  }
  
  add_log("0. Raw", data)
  
  d <- data |> filter(room_type == "Entire home/apt")        # DDD buys whole properties
  add_log("A. Entire home/apt only", d)
  
  d <- d |> filter(minimum_nights < MAX_MIN_NIGHTS)          # short-term stays only
  add_log(paste0("B. Minimum stay < ", MAX_MIN_NIGHTS, " nights"), d)
  
  d <- d |> filter(!is.na(price), price > 0, !is.na(estimated_revenue_l365d))
  add_log("C. Valid price and revenue estimate", d)
  
  if (active_only) {                                         # survivors only
    d <- d |> filter(number_of_reviews_ltm > 0)
    add_log("D. Active (>=1 review in last 12 months)", d)
  }
  
  cap <- quantile(d$price, price_cap_pct, na.rm = TRUE)      # cap extreme nightly prices
  d <- d |> filter(price <= cap)
  add_log(paste0("E. Nightly price <= $", round(cap), " (", price_cap_pct * 100, "th percentile)"), d)
  
  d <- d |> filter(property_group != "Other")                # must be a buyable property
  add_log("F. House or Condo/Apartment only", d)
  
  list(data = d, log = log)
}

# ---------------------------------------------------------------------
# 4. RUN IT WITH OUR CHOSEN SETTINGS
# ---------------------------------------------------------------------
result <- clean_listings(df0)
df     <- result$data

cat("\n===== CLEANING LOG (put this on a slide / in the appendix) =====\n")
print(result$log |> mutate(across(where(is.numeric), ~ round(.x))), n = Inf)
write_csv(result$log, "outputs/cleaning_log.csv")

# ---------------------------------------------------------------------
# 5. SENSITIVITY: what if we had cleaned differently?
#    Each row is one alternative. If the medians barely move, the choice
#    doesn't matter. If they move a lot, that is a decision worth defending.
# ---------------------------------------------------------------------
variants <- expand_grid(active_only = c(TRUE, FALSE), price_cap_pct = c(0.99, 1))

sensitivity <- pmap_dfr(variants, function(active_only, price_cap_pct) {
  d <- clean_listings(df0, active_only, price_cap_pct)$data
  tibble(active_only, price_cap_pct,
         rows           = nrow(d),
         median_revenue = median(d$estimated_revenue_l365d),
         p75_revenue    = quantile(d$estimated_revenue_l365d, 0.75),
         median_house   = median(d$estimated_revenue_l365d[d$property_group == "House"]),
         median_condo   = median(d$estimated_revenue_l365d[d$property_group == "Condo/Apartment"]))
})

cat("\n===== SENSITIVITY (decision-log evidence) =====\n")
print(sensitivity |> mutate(across(where(is.numeric), ~ round(.x, 2))))
write_csv(sensitivity, "outputs/sensitivity.csv")

# ---------------------------------------------------------------------
# 6. VALIDATION: do two different revenue methods roughly agree?
#    Method 1: Inside Airbnb's estimate (built from reviews)
#    Method 2: nightly price x nights marked unavailable (booked OR blocked, so an UPPER bound)
# ---------------------------------------------------------------------
df <- df |> mutate(rev_calendar = price * (365 - availability_365))

ratio <- median(df$rev_calendar / pmax(df$estimated_revenue_l365d, 1), na.rm = TRUE)
corr  <- cor(df$rev_calendar, df$estimated_revenue_l365d, use = "complete.obs")

cat("\n===== VALIDATION =====\n")
cat("Median revenue, Inside Airbnb estimate:", round(median(df$estimated_revenue_l365d)), "\n")
cat("Median revenue, calendar method:       ", round(median(df$rev_calendar, na.rm = TRUE)), "\n")
cat("Median ratio (calendar / estimate):    ", round(ratio, 2), "\n")
cat("Correlation between the two:           ", round(corr, 2), "\n")
cat("Occupancy column range (should be nights, 0-365):",
    range(df$estimated_occupancy_l365d, na.rm = TRUE), "\n")

p_val <- ggplot(df, aes(estimated_revenue_l365d, rev_calendar)) +
  geom_point(alpha = 0.25) +
  geom_abline(slope = 1, intercept = 0, colour = "red") +
  scale_x_continuous(labels = dollar, limits = c(0, 150000)) +
  scale_y_continuous(labels = dollar, limits = c(0, 150000)) +
  labs(title = "Two revenue methods compared",
       subtitle = "Red line = perfect agreement. Calendar method counts blocked nights as booked.",
       x = "Inside Airbnb estimated annual revenue", y = "Price x unavailable nights",
       caption = ia_caption) +
  theme_minimal()
ggsave("outputs/validation_scatter.png", p_val, width = 7, height = 5, dpi = 200)

# ---------------------------------------------------------------------
# 7. LICENCE CHECK (an ESTIMATE: the licence field is imperfect)
# ---------------------------------------------------------------------
cat("\n===== LICENCE CHECK (estimate) =====\n")
cat("Share of clean listings showing licence text:", round(100 * mean(df$has_licence)), "%\n")
lic_tbl <- df |> group_by(has_licence) |>
  summarise(n = n(), median_revenue = median(estimated_revenue_l365d), .groups = "drop")
print(lic_tbl)
write_csv(lic_tbl, "outputs/licence_comparison.csv")

# ---------------------------------------------------------------------
# 8. REVENUE TABLES  (p25 / median / p75 give a range with no fancy statistics)
# ---------------------------------------------------------------------
summarise_rev <- function(data, ...) {
  data |>
    group_by(...) |>
    summarise(n              = n(),
              median_price   = median(price),
              median_occ_pct = median(estimated_occupancy_l365d / 365 * 100, na.rm = TRUE),
              p25_revenue    = quantile(estimated_revenue_l365d, 0.25),
              median_revenue = median(estimated_revenue_l365d),
              p75_revenue    = quantile(estimated_revenue_l365d, 0.75),
              .groups = "drop") |>
    filter(n >= MIN_GROUP_N) |>
    arrange(desc(median_revenue))
}

by_segment <- summarise_rev(df, neighbourhood_cleansed, property_group)
by_neigh   <- summarise_rev(df, neighbourhood_cleansed)
by_bed     <- summarise_rev(df, bedroom_group)
by_type    <- summarise_rev(df, property_group)

cat("\n===== BY PROPERTY TYPE (whole city) =====\n");  print(by_type)
cat("\n===== BY BEDROOM GROUP (whole city) =====\n");  print(by_bed)
cat("\n===== TOP 10 SEGMENTS (neighbourhood x type) BY MEDIAN REVENUE =====\n")
print(head(by_segment, 10))
cat("\nSegments dropped for having fewer than", MIN_GROUP_N, "listings:",
    nrow(distinct(df, neighbourhood_cleansed, property_group)) - nrow(by_segment), "\n")

write_csv(by_segment, "outputs/revenue_by_segment.csv")
write_csv(by_neigh,   "outputs/revenue_by_neighbourhood.csv")
write_csv(by_bed,     "outputs/revenue_by_bedroom.csv")

p_neigh <- by_neigh |>
  slice_head(n = 12) |>
  mutate(neighbourhood_cleansed = fct_reorder(neighbourhood_cleansed, median_revenue)) |>
  ggplot(aes(median_revenue, neighbourhood_cleansed)) +
  geom_pointrange(aes(xmin = p25_revenue, xmax = p75_revenue)) +
  geom_text(aes(label = paste0("n=", n)), nudge_y = 0.35, size = 3) +
  scale_x_continuous(labels = dollar) +
  labs(title = "Median annual revenue by neighbourhood",
       subtitle = "Dot = median, line = 25th to 75th percentile. Entire homes only.",
       x = "Estimated annual revenue per listing", y = NULL, caption = ia_caption) +
  theme_minimal()
ggsave("outputs/revenue_by_neighbourhood.png", p_neigh, width = 8, height = 5.5, dpi = 200)

p_type <- ggplot(df, aes(property_group, estimated_revenue_l365d)) +
  geom_boxplot(outlier.alpha = 0.2) +
  scale_y_continuous(labels = dollar, limits = c(0, 150000)) +
  labs(title = "Annual revenue by property type", x = NULL,
       y = "Estimated annual revenue per listing", caption = ia_caption) +
  theme_minimal()
ggsave("outputs/revenue_by_type.png", p_type, width = 6, height = 4.5, dpi = 200)

# ---------------------------------------------------------------------
# 9. SAVE CLEAN DATA for the next scripts (has listing ids: do NOT show on slides)
# ---------------------------------------------------------------------
write_csv(df |> select(id, neighbourhood_cleansed, property_group, bedroom_group,
                       price, availability_365, estimated_occupancy_l365d,
                       estimated_revenue_l365d, rev_calendar, has_licence),
          "outputs/listings_clean.csv")

cat("\nDone with 01_clean.R. Check the outputs/ folder.\n")