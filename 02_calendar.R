# =====================================================================
# 02_calendar.R  -  Use the calendar file to cross-check revenue and see seasonality
# Data: Inside Airbnb, Vancouver, 15 June 2026, CC BY 4.0
#
# The calendar has one row per listing per night for the next 365 days.
# CAUTION: available = "f" means booked OR blocked by the host. We cannot tell which.
# CAUTION: nights far in the future are less booked simply because guests
#          have not booked yet. So treat the monthly pattern as a rough guide.
# =====================================================================

source("analysis/00_settings.R")

clean <- read_csv("outputs/listings_clean.csv", show_col_types = FALSE)

# ---------------------------------------------------------------------
# 1. LOAD ONLY OUR CLEANED LISTINGS (filtering keeps memory use small)
# ---------------------------------------------------------------------
calendar <- read_csv(FILE_CALENDAR, show_col_types = FALSE) |>
  tidy_names() |>
  filter(listing_id %in% clean$id)

if (!"price" %in% names(calendar)) {
  calendar$price <- NA_real_
}

calendar <- calendar |>
  mutate(date        = ymd(date),
         price       = parse_number(as.character(price)),
         unavailable = as.character(available) %in% c("f", "FALSE"))

cat("\nCalendar rows kept:", nrow(calendar), "for", n_distinct(calendar$listing_id), "listings\n")
cat("Date range:", format(min(calendar$date)), "to", format(max(calendar$date)), "\n")

# ---------------------------------------------------------------------
# 2. CONSISTENCY CHECK: does the calendar agree with availability_365 in the listings file?
# ---------------------------------------------------------------------
per_listing <- calendar |>
  group_by(listing_id) |>
  summarise(nights_in_calendar = n(),
            nights_unavailable = sum(unavailable),
            .groups = "drop") |>
  inner_join(clean |> select(id, availability_365, estimated_occupancy_l365d),
             by = c("listing_id" = "id")) |>
  mutate(nights_available_cal = nights_in_calendar - nights_unavailable)

cat("\n===== CALENDAR vs LISTINGS FILE =====\n")
cat("Correlation, available nights (calendar vs availability_365):",
    round(cor(per_listing$nights_available_cal, per_listing$availability_365, use = "complete.obs"), 2), "\n")
cat("Correlation, unavailable nights vs Inside Airbnb occupancy estimate:",
    round(cor(per_listing$nights_unavailable, per_listing$estimated_occupancy_l365d, use = "complete.obs"), 2), "\n")
cat("Median unavailable nights per listing (calendar):", median(per_listing$nights_unavailable), "\n")
cat("Median occupancy estimate (nights per year):     ", median(per_listing$estimated_occupancy_l365d, na.rm = TRUE), "\n")

# ---------------------------------------------------------------------
# 3. MONTH-BY-MONTH PATTERN
# ---------------------------------------------------------------------
monthly <- calendar |>
  mutate(month = month(date, label = TRUE)) |>
  group_by(month) |>
  summarise(pct_unavailable = mean(unavailable) * 100,
            median_price    = median(price, na.rm = TRUE),
            .groups = "drop")

cat("\n===== BY MONTH (rough guide only) =====\n")
print(monthly)
write_csv(monthly, "outputs/calendar_by_month.csv")

p_month <- ggplot(monthly, aes(month, pct_unavailable, group = 1)) +
  geom_line() + geom_point() +
  labs(title = "Share of nights unavailable, by month",
       subtitle = "Unavailable = booked or blocked. Future months look emptier because guests book late.",
       x = NULL, y = "% of nights unavailable", caption = ia_caption) +
  theme_minimal()
ggsave("outputs/calendar_by_month.png", p_month, width = 7, height = 4.5, dpi = 200)

# Median nightly price by month (only if the calendar actually has prices)
if (any(!is.na(monthly$median_price))) {
  p_price <- ggplot(monthly, aes(month, median_price, group = 1)) +
    geom_line() + geom_point() +
    scale_y_continuous(labels = dollar) +
    labs(title = "Median advertised nightly price, by month",
         x = NULL, y = "Median nightly price", caption = ia_caption) +
    theme_minimal()
  ggsave("outputs/calendar_price_by_month.png", p_price, width = 7, height = 4.5, dpi = 200)
} else {
  cat("\nNote: the calendar has no prices in this snapshot, so no price chart was made.\n")
}

cat("\nDone with 02_calendar.R.\n")