# =====================================================================
# 04_portfolio.R  -  Return per dollar, few vs. many, and the 2027 range
#
# Inputs (made by scripts 01 and 03):
#   outputs/revenue_by_segment.csv   revenue per listing by neighbourhood x property type
#   outputs/prices_by_segment.csv    assessed value (our price proxy) by the same segments
#   outputs/listings_clean.csv       individual cleaned listings (used for the simulation)
#
# METRIC: net yield = net operating income (NOI) / all-in purchase cost
#   NOI = revenue - variable costs - fixed costs (property tax, insurance, strata, licence)
#   All-in cost = assessed value x (1 + closing costs)
#   It ignores mortgage financing and price appreciation (say so in the pitch).
# =====================================================================

source("analysis/00_settings.R")
set.seed(2027)   # makes the simulation give the same answer every run

rev_seg  <- read_csv("outputs/revenue_by_segment.csv", show_col_types = FALSE)
prices   <- read_csv("outputs/prices_by_segment.csv",  show_col_types = FALSE)
listings <- read_csv("outputs/listings_clean.csv",     show_col_types = FALSE)

# ---------------------------------------------------------------------
# 1. JOIN REVENUE TO PRICE
# ---------------------------------------------------------------------
if (all(prices$neighbourhood_cleansed == "ALL")) {
  cat("\n*** WARNING: prices are CITY-WIDE (no neighbourhood lookup). Differences between\n",
      "    neighbourhoods below come from REVENUE only, not from cost. Say this in the pitch. ***\n")
  segments <- rev_seg |> inner_join(select(prices, -neighbourhood_cleansed), by = "property_group")
} else {
  segments <- rev_seg |> inner_join(prices, by = c("neighbourhood_cleansed", "property_group"))
}
cat("\nSegments with both revenue and price data:", nrow(segments),
    "of", nrow(rev_seg), "revenue segments\n")

# ---------------------------------------------------------------------
# 2. UNIT ECONOMICS: one function turns revenue into NOI for one property for one year
# ---------------------------------------------------------------------
unit_noi <- function(revenue, value, levy, group,
                     rev_mult = 1, var_pct = VARIABLE_COST_PCT, months_lost = 0, value_mult = 1) {
  value <- value * value_mult
  gross <- revenue * rev_mult * (1 - RAMP_UP_HAIRCUT) * (12 - months_lost) / 12
  tax   <- ifelse(is.na(levy), value * PROPERTY_TAX_FALLBACK_PCT, levy)
  fixed <- tax + value * FIXED_COST_PCT +
           ifelse(group == "Condo/Apartment", value * STRATA_FEE_PCT, 0) + LICENCE_FEE
  gross * (1 - var_pct) - fixed
}

segments <- segments |>
  mutate(label         = paste0(neighbourhood_cleansed, " - ", property_group),
         price_all_in  = median_value * (1 + CLOSING_COST_PCT),
         unit_noi_base = unit_noi(median_revenue, median_value, median_levy, property_group),
         net_yield     = unit_noi_base / price_all_in,
         max_units     = floor(BUDGET / price_all_in)) |>
  filter(max_units >= 1) |>
  arrange(desc(net_yield))

cat("\n===== TOP 10 SEGMENTS BY BASE-CASE NET YIELD =====\n")
print(segments |>
        transmute(label, listings = n, median_revenue = round(median_revenue),
                  price_all_in = round(price_all_in), net_yield_pct = round(net_yield * 100, 1),
                  max_units) |> head(10))
write_csv(segments, "outputs/segment_economics.csv")

# ---------------------------------------------------------------------
# 3. SENSITIVITY: assessed values are a proxy, so what if prices are 15% off?
# ---------------------------------------------------------------------
price_sens <- expand_grid(segments |> slice_head(n = 5), value_mult = c(0.85, 1, 1.15)) |>
  mutate(noi = unit_noi(median_revenue, median_value, median_levy, property_group, value_mult = value_mult),
         yield_pct = round(noi / (median_value * value_mult * (1 + CLOSING_COST_PCT)) * 100, 1)) |>
  select(label, value_mult, yield_pct)

cat("\n===== PRICE SENSITIVITY (top 5 segments) =====\n")
print(price_sens |> pivot_wider(names_from = value_mult, values_from = yield_pct,
                                names_prefix = "prices_x"))
write_csv(price_sens, "outputs/price_sensitivity.csv")

# ---------------------------------------------------------------------
# 4. SIMULATION: the spread of outcomes for a portfolio
#    We "buy" random listings from each segment (sampling real listings with
#    replacement) and add up their NOI. Repeating 2,000 times shows the range.
#    Limits: assumes new properties perform like existing active ones, and
#    each property's result is independent (a city-wide shock is NOT included -
#    that is what the scenarios in section 6 are for).
# ---------------------------------------------------------------------
simulate_plan <- function(plan, reps = 2000) {
  total_noi <- rep(0, reps)
  for (i in seq_len(nrow(plan))) {
    p    <- plan[i, ]
    revs <- listings$estimated_revenue_l365d[listings$neighbourhood_cleansed == p$neighbourhood_cleansed &
                                             listings$property_group == p$property_group]
    noi   <- unit_noi(revs, p$median_value, p$median_levy, p$property_group)
    draws <- matrix(sample(noi, p$units * reps, replace = TRUE), nrow = p$units)  # one column per run
    total_noi <- total_noi + colSums(draws)
  }
  capital <- sum(plan$units * plan$price_all_in)
  yields  <- total_noi / capital
  tibble(p10_yield = quantile(yields, 0.10), p90_yield = quantile(yields, 0.90))
}

summarise_plan <- function(name, plan) {
  sim <- simulate_plan(plan)
  capital <- sum(plan$units * plan$price_all_in)
  base_noi <- sum(plan$units * plan$unit_noi_base)
  bind_cols(tibble(strategy = name, properties = sum(plan$units),
                   capital_deployed = capital, base_noi = base_noi,
                   base_yield = base_noi / capital), sim)
}

# ---------------------------------------------------------------------
# 5. FEW vs MANY: put the whole $10M into ONE segment and compare outcomes
# ---------------------------------------------------------------------
few_many <- map_dfr(seq_len(nrow(segments)), function(i) {
  plan <- segments[i, ] |> mutate(units = max_units)
  summarise_plan(plan$label, plan) |>
    mutate(property_group = plan$property_group)
})

cat("\n===== FEW vs MANY: yield and its spread by number of properties =====\n")
print(few_many |>
        transmute(strategy, properties,
                  base_yield_pct = round(base_yield * 100, 1),
                  p10_pct = round(p10_yield * 100, 1), p90_pct = round(p90_yield * 100, 1),
                  spread_pts = round((p90_yield - p10_yield) * 100, 1)) |> head(15))
write_csv(few_many, "outputs/few_vs_many.csv")

# The spread (p90 - p10) should shrink as the number of properties grows. Check:
if (sd(few_many$properties) > 0) {
  cat("\nCorrelation between number of properties and spread of outcomes:",
      round(cor(few_many$properties, few_many$p90_yield - few_many$p10_yield), 2),
      "(negative = more properties, narrower range)\n")
} else {
  cat("\nEvery segment buys the same number of properties, so there is nothing to compare.\n",
      "(If prices are city-wide, fill in the neighbourhood lookup in script 03.)\n")
}

# Three candidate strategies
best_house <- segments |> filter(property_group == "House") |> slice(1) |> mutate(units = max_units)
best_condo <- segments |> filter(property_group == "Condo/Apartment") |> slice(1) |> mutate(units = max_units)
mixed      <- segments |> slice_head(n = 3) |>
  mutate(units = floor((BUDGET / 3) / price_all_in)) |> filter(units >= 1)

strategies <- list()
if (nrow(best_house) > 0) strategies[["Few: best house segment"]]   <- best_house
if (nrow(best_condo) > 0) strategies[["Many: best condo segment"]]  <- best_condo
if (nrow(mixed) > 0)      strategies[["Mixed: top 3 segments, 1/3 each"]] <- mixed

strategy_table <- imap_dfr(strategies, ~ summarise_plan(.y, .x))

cat("\n===== STRATEGY COMPARISON =====\n")
print(strategy_table |>
        transmute(strategy, properties, capital = round(capital_deployed),
                  base_noi = round(base_noi),
                  base_yield_pct = round(base_yield * 100, 1),
                  p10_pct = round(p10_yield * 100, 1), p90_pct = round(p90_yield * 100, 1)))
write_csv(strategy_table, "outputs/strategy_comparison.csv")

# ---------------------------------------------------------------------
# 6. THE 2027 RANGE for the recommended plan (default: the mixed portfolio)
#    Scenarios (set in 00_settings.R) shift revenue, costs, and months lost.
# ---------------------------------------------------------------------
recommended <- mixed
cat("\n===== RECOMMENDED PLAN (default = mixed top 3; change `recommended` to test others) =====\n")
print(recommended |> transmute(label, units, price_each = round(price_all_in)))

scenario_results <- pmap_dfr(SCENARIOS, function(scenario, rev_mult, var_pct, months_lost) {
  noi <- sum(recommended$units *
               unit_noi(recommended$median_revenue, recommended$median_value,
                        recommended$median_levy, recommended$property_group,
                        rev_mult = rev_mult, var_pct = var_pct, months_lost = months_lost))
  capital <- sum(recommended$units * recommended$price_all_in)
  tibble(scenario, noi = noi, capital = capital, yield = noi / capital)
})

cat("\n===== 2027 SCENARIOS =====\n")
print(scenario_results |> mutate(noi = round(noi), capital = round(capital), yield = round(yield * 100, 1)))
write_csv(scenario_results, "outputs/scenarios.csv")

# ---------------------------------------------------------------------
# 7. CHARTS FOR THE SLIDES
# ---------------------------------------------------------------------
p_yield <- segments |>
  slice_head(n = 12) |>
  mutate(label = fct_reorder(label, net_yield)) |>
  ggplot(aes(net_yield, label, fill = property_group)) +
  geom_col() +
  scale_x_continuous(labels = percent) +
  labs(title = "Base-case net yield by segment (top 12)",
       subtitle = "Net operating income / all-in purchase cost. Assessed value used as price proxy.",
       x = "Net yield", y = NULL, fill = NULL, caption = tax_caption) +
  theme_minimal() + theme(legend.position = "bottom")
ggsave("outputs/yield_by_segment.png", p_yield, width = 8, height = 5.5, dpi = 200)

p_fm <- ggplot(few_many, aes(properties, base_yield, colour = property_group)) +
  geom_pointrange(aes(ymin = p10_yield, ymax = p90_yield), alpha = 0.7) +
  scale_y_continuous(labels = percent) +
  labs(title = "Few vs. many: more properties narrow the range of outcomes",
       subtitle = "Dot = base-case yield, line = 10th to 90th percentile of 2,000 simulated portfolios",
       x = "Number of properties bought with the $10M", y = "Net yield",
       colour = NULL, caption = tax_caption) +
  theme_minimal() + theme(legend.position = "bottom")
ggsave("outputs/few_vs_many.png", p_fm, width = 8, height = 5, dpi = 200)

p_sc <- scenario_results |>
  mutate(scenario = fct_inorder(scenario)) |>
  ggplot(aes(scenario, yield)) +
  geom_col() +
  geom_text(aes(label = paste0(round(yield * 100, 1), "%")), vjust = -0.4) +
  scale_y_continuous(labels = percent, expand = expansion(mult = c(0, 0.12))) +
  labs(title = "2027 net yield under four scenarios (recommended plan)",
       x = NULL, y = "Net yield", caption = tax_caption) +
  theme_minimal()
ggsave("outputs/scenarios.png", p_sc, width = 8, height = 4.5, dpi = 200)

cat("\nDone with 04_portfolio.R. Charts and tables are in outputs/.\n")
