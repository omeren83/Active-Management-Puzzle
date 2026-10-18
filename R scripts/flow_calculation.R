# =============================================================================
# FUND FLOW CALCULATION AND VALIDATION                                     v1.3
#
# v1.3 changes (Batch 4, Oct 2026):
#   - TNA, lagged TNA and raw flows are computed once on panel_master and
#     joined into panel_incubation / panel_trimmed, so the first
#     post-incubation month keeps its flow. Winsorisation stays per panel.
#   - tna_source is NA (not "total") when neither TNA field is available.
#
# v1.2 changes (pipeline audit, Oct 2026):
#   - ret_net_raw is now the Bloomberg index return itself, which is NAV-based and
#     net of fees (see data_import_and_cleaning.R v1.4). v1.1 subtracted ER/12
#     from an already-net series, overstating flows by TNA_{t-1} * ER/12.
#   - No flow is computed in a month where the TNA source switches between
#     class_assets and total_assets (count printed at run time); the
#     switch is a change of measurement base, not an investor flow.
#   - Winsorisation cut-offs for the proportional flows now exclude December,
#     matching the text (December is set to NA in any case).
#
# v1.1 changes:
#   - Net-return derivation comment rewritten (superseded by v1.2).
#
# Computes Sirri-Tufano (1998) flows from TNA and unwinsorised net returns,
# appends results to all three panels (panel_master, panel_incubation,
# panel_trimmed), and validates against Bloomberg-supplied fund_flow values.
#
# Formula: Flow_{i,t} = TNA_{i,t} - TNA_{i,t-1} * (1 + R_{i,t})
# where TNA = class_assets if available, else total_assets
#       R   = ret_net_raw: the NAV-based (net-of-fee) index return, screened
#             for data errors but not winsorised. Net is the correct return
#             for the identity, since TNA grows by the net return.
#
# NOTE: The flow formula is an accounting identity, so it uses unclipped
# returns. Winsorisation is applied to the OUTPUT (flow_calc_pct_win) only.
#
# New columns appended to all three panels:
#   tna               - TNA in USD millions (class_assets or total_assets)
#   tna_source        - "class", "total", or NA when no TNA (audit trail)
#   tna_lag           - lagged TNA
#   flow_calc         - Sirri-Tufano flow in USD millions (NA when tna_source
#                       differs from the previous month)
#   flow_calc_pct     - proportional flow (flow_calc / tna_lag)
#   is_december       - TRUE for December obs (year-end distribution artifact)
#   flow_calc_pct_win - flow_calc_pct winsorised at 1/99 pct (cut-offs from
#                       non-December months), NA in December
#   flow_lseg_pct     - Bloomberg fund_flow / tna_lag (for validation only)
#   flow_lseg_pct_win - winsorised Bloomberg flow (for validation only)
#
# Reference: Sirri, E.R. & Tufano, P. (1998). Costly search and mutual fund
#   flows. Journal of Finance, 53(5), 1589-1622.
# Reference: Carhart, M.M. (1997). On persistence in mutual fund performance.
#   Journal of Finance, 52(1), 57-82.
# Reference: Wermers, R. (2000). Mutual fund performance. Journal of Finance,
#   55(4), 1655-1695.
# Reference: Barras, L., Scaillet, O., & Wermers, R. (2010). False discoveries
#   in mutual fund performance. Journal of Finance, 65(1), 179-216.
#
# Dependencies: dplyr
# Requires: data_import_and_cleaning.R run first (ret_net_raw must be present)
# =============================================================================

library(dplyr)

# =============================================================================
# HELPER: winsorise a vector at low/high quantiles
#   Also defined in data_import_and_cleaning.R as a shared utility.
#   Redefined here so this script can run standalone if needed.
# =============================================================================
if (!exists("winsorise")) {
  winsorise <- function(x, low = 0.01, high = 0.99) {
    q <- quantile(x, probs = c(low, high), na.rm = TRUE)
    pmax(pmin(x, q[2]), q[1])
  }
}

# =============================================================================
# CORE FLOWS: computed once on panel_master (full histories), v1.3
#   Computing within the truncated panels lost the first post-incubation
#   month (its lagged TNA lies before the Evans cutoff).
# =============================================================================
compute_flow_core <- function(panel) {
  panel %>%
    # --- 1. TNA: class_assets preferred, total_assets as fallback -----------
    mutate(
      use_class  = !is.na(class_assets) & class_assets > 0,
      tna        = if_else(use_class, class_assets, total_assets),
      tna_source = case_when(use_class    ~ "class",
                             !is.na(tna)  ~ "total",
                             TRUE         ~ NA_character_)  # NA when no TNA
    ) %>%
    select(-use_class) %>%
    # --- 2. Sirri-Tufano flow -----------------------------------------------
    group_by(Ticker) %>%
    arrange(date) %>%
    mutate(
      # v-audit: no lag across an internal gap (calendar step > 1 month)
      .step         = (as.integer(format(date, "%Y")) * 12L + as.integer(format(date, "%m"))) -
                      lag(as.integer(format(date, "%Y")) * 12L + as.integer(format(date, "%m"))),
      tna_lag       = if_else(.step == 1L, lag(tna), NA_real_),
      # Net (NAV-based) return; no flow across a class/total TNA switch
      flow_calc     = if_else(tna_source == lag(tna_source),
                              tna - tna_lag * (1 + ret_net_raw), NA_real_),
      flow_calc_pct = if_else(!is.na(tna_lag) & tna_lag > 0,
                              flow_calc / tna_lag, NA_real_)
    ) %>%
    select(-.step) %>%
    ungroup()
}

# =============================================================================
# PER-PANEL STEP: December flag and winsorisation (cut-offs panel-specific)
# =============================================================================
winsorise_flows <- function(panel) {
  panel %>%
    mutate(
      is_december       = (format(date, "%m") == "12"),
      # cut-offs from non-December months only; December itself set to NA
      flow_calc_pct_win = if_else(is_december, NA_real_,
                                  winsorise(if_else(is_december, NA_real_, flow_calc_pct))),
      # Bloomberg proportional flow on same TNA base (validation only)
      flow_lseg_pct     = if_else(!is.na(tna_lag) & tna_lag > 0 & !is.na(fund_flow),
                                  fund_flow / tna_lag, NA_real_),
      flow_lseg_pct_win = if_else(is_december, NA_real_,
                                  winsorise(if_else(is_december, NA_real_, flow_lseg_pct)))
    )
}

# =============================================================================
# APPLY: core on master, joined into the truncated panels by (Ticker, date)
# =============================================================================
FLOW_CORE_COLS <- c("tna", "tna_source", "tna_lag", "flow_calc", "flow_calc_pct")

panel_master <- compute_flow_core(panel_master)
flow_lookup  <- panel_master %>% select(Ticker, date, all_of(FLOW_CORE_COLS))

join_flows <- function(panel) {
  out <- panel %>%
    select(-any_of(FLOW_CORE_COLS)) %>%
    left_join(flow_lookup, by = c("Ticker", "date"))
  stopifnot(nrow(out) == nrow(panel))
  out
}
panel_incubation <- join_flows(panel_incubation)
panel_trimmed    <- join_flows(panel_trimmed)
rm(flow_lookup)

panel_master     <- winsorise_flows(panel_master)
panel_incubation <- winsorise_flows(panel_incubation)
panel_trimmed    <- winsorise_flows(panel_trimmed)

cat("Flow variables appended to all three panels.\n")
cat("\npanel_trimmed - calculated flow summary (USD millions):\n")
print(summary(panel_trimmed$flow_calc))
cat("\npanel_trimmed - proportional flow summary:\n")
print(summary(panel_trimmed$flow_calc_pct))
cat("\nTNA source distribution (panel_trimmed):\n")
print(table(panel_trimmed$tna_source, useNA = "always"))
n_switch <- panel_master %>% group_by(Ticker) %>% arrange(date) %>%
  summarise(n = sum(tna_source != lag(tna_source), na.rm = TRUE), .groups = "drop")
cat("TNA source switches (panel_master, flow set to NA):", sum(n_switch$n), "\n")

# =============================================================================
# VALIDATION: compare calculated vs Bloomberg flows (panel_trimmed only)
#   Validation is informational - performed on panel_trimmed where
#   Bloomberg flow coverage is highest.
# =============================================================================
comparison <- panel_trimmed %>%
  filter(!is.na(flow_calc_pct_win) &
           !is.na(flow_lseg_pct_win) &
           !is_december)

cat("\n--- VALIDATION: CALCULATED vs Bloomberg FLOWS (panel_trimmed) ---\n")
cat("Overlapping fund-months :", nrow(comparison), "\n")
cat("Correlation             :",
    round(cor(comparison$flow_calc_pct_win,
              comparison$flow_lseg_pct_win,
              use = "complete.obs"), 4), "\n")
cat("Mean difference         :",
    round(mean(comparison$flow_calc_pct_win -
                 comparison$flow_lseg_pct_win, na.rm = TRUE), 6), "\n")
cat("RMSE                    :",
    round(sqrt(mean((comparison$flow_calc_pct_win -
                       comparison$flow_lseg_pct_win)^2,
                    na.rm = TRUE)), 6), "\n")

cat("\nBy group:\n")
comparison %>%
  group_by(ap_group) %>%
  summarise(
    n         = n(),
    corr      = round(cor(flow_calc_pct_win, flow_lseg_pct_win,
                          use = "complete.obs"), 4),
    mean_diff = round(mean(flow_calc_pct_win - flow_lseg_pct_win,
                           na.rm = TRUE), 6),
    rmse      = round(sqrt(mean((flow_calc_pct_win - flow_lseg_pct_win)^2,
                                na.rm = TRUE)), 6),
    .groups   = "drop"
  ) %>%
  print()

# =============================================================================
# COVERAGE COMPARISON
# =============================================================================
cat("\n--- FLOW COVERAGE (panel_trimmed) ---\n")
cat("fund-months with Bloomberg flow   :",
    sum(!is.na(panel_trimmed$flow_lseg_pct_win)), "\n")
cat("fund-months with calc. flow  :",
    sum(!is.na(panel_trimmed$flow_calc_pct_win)), "\n")
cat("fund-months with Bloomberg only   :",
    sum(!is.na(panel_trimmed$flow_lseg_pct_win) &
          is.na(panel_trimmed$flow_calc_pct_win)), "\n")
cat("fund-months with calc. only  :",
    sum(is.na(panel_trimmed$flow_lseg_pct_win) &
          !is.na(panel_trimmed$flow_calc_pct_win)), "\n")
cat("fund-months with both        :",
    sum(!is.na(panel_trimmed$flow_lseg_pct_win) &
          !is.na(panel_trimmed$flow_calc_pct_win)), "\n")
