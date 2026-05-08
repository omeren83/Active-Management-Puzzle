# =============================================================================
# FUND FLOW CALCULATION AND VALIDATION                                     v1.1
#
# v1.1 changes:
#   - Net-return derivation comment rewritten: BSW (2010) observe net directly
#     from CRSP and DERIVE gross by adding back ER/12. The LSEG pipeline
#     observes gross only and DERIVES net by subtracting ER/12. Same
#     arithmetic, opposite direction. The convention itself originates in
#     Carhart (1997) and Wermers (2000) and is used throughout the literature.
#
# Computes Sirri-Tufano (1998) flows from TNA and unwinsorised net returns,
# appends results to all three panels (panel_master, panel_incubation,
# panel_trimmed), and validates against LSEG-supplied fund_flow values.
#
# Formula: Flow_{i,t} = TNA_{i,t} - TNA_{i,t-1} * (1 + R_{i,t})
# where TNA = class_assets if available, else total_assets
#       R   = ret_net_raw (unwinsorised approximated net return)
#             = ret_gross_raw - Expense_Ratio / 1200
#             LSEG serves identical index series for gross and net total
#             returns; true net returns are unavailable from the index. The
#             gross-net wedge of Expense_Ratio/12 follows the standard
#             fee-decomposition convention (Carhart 1997; Wermers 2000;
#             Pastor and Stambaugh 2002), applied in the opposite direction
#             to BSW (2010), who observe net and derive gross. This introduces
#             a small, bounded, predictable downward bias in estimated flows
#             proportional to the fund expense ratio.
#
# NOTE: The flow formula is an accounting identity. It uses ret_net_raw
# (unwinsorised) rather than ret_net (winsorised). Plugging clipped returns
# into a balance-sheet identity produces fictitious asset growth and biased
# flow estimates. Winsorisation is applied to the OUTPUT (flow_calc_pct_win),
# not to the inputs of the identity.
#
# New columns appended to all three panels:
#   tna               - TNA in USD millions (class_assets or total_assets)
#   tna_source        - "class" or "total" (audit trail)
#   tna_lag           - lagged TNA
#   flow_calc         - Sirri-Tufano flow in USD millions
#   flow_calc_pct     - proportional flow (flow_calc / tna_lag)
#   is_december       - TRUE for December obs (year-end distribution artifact)
#   flow_calc_pct_win - flow_calc_pct winsorised at 1/99 pct, NA in December
#   flow_lseg_pct     - LSEG fund_flow / tna_lag (for validation only)
#   flow_lseg_pct_win - winsorised LSEG flow (for validation only)
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
# MAIN FUNCTION: compute and append all flow variables to a panel
# =============================================================================
compute_flows <- function(panel) {
  panel %>%
    # --- 1. TNA: class_assets preferred, total_assets as fallback -----------
  mutate(
    tna        = if_else(!is.na(class_assets) & class_assets > 0,
                         class_assets, total_assets),
    tna_source = if_else(!is.na(class_assets) & class_assets > 0,
                         "class", "total")
  ) %>%
    # --- 2. Sirri-Tufano flow -----------------------------------------------
  group_by(Ticker) %>%
    arrange(date) %>%
    mutate(
      tna_lag       = lag(tna),
      # ret_net_raw = ret_gross_raw - Expense_Ratio/1200 (see data_import_and_cleaning.R).
      # Uses approximated net return (unwinsorised) - the flow identity requires
      # actual returns, not clipped values.
      flow_calc     = tna - tna_lag * (1 + ret_net_raw),
      # Proportional flow
      flow_calc_pct = if_else(
        !is.na(tna_lag) & tna_lag > 0,
        flow_calc / tna_lag,
        NA_real_
      )
    ) %>%
    ungroup() %>%
    # --- 3. Winsorise, flag December ----------------------------------------
  mutate(
    is_december       = (format(date, "%m") == "12"),
    flow_calc_pct_win = if_else(is_december, NA_real_,
                                winsorise(flow_calc_pct)),
    # LSEG proportional flow on same TNA base (validation only)
    flow_lseg_pct     = if_else(
      !is.na(tna_lag) & tna_lag > 0 & !is.na(fund_flow),
      fund_flow / tna_lag,
      NA_real_
    ),
    flow_lseg_pct_win = if_else(is_december, NA_real_,
                                winsorise(flow_lseg_pct))
  )
}

# =============================================================================
# APPLY TO ALL THREE PANELS
# =============================================================================
panel_master     <- compute_flows(panel_master)
panel_incubation <- compute_flows(panel_incubation)
panel_trimmed    <- compute_flows(panel_trimmed)

cat("Flow variables appended to all three panels.\n")
cat("\npanel_trimmed - calculated flow summary (USD millions):\n")
print(summary(panel_trimmed$flow_calc))
cat("\npanel_trimmed - proportional flow summary:\n")
print(summary(panel_trimmed$flow_calc_pct))
cat("\nTNA source distribution (panel_trimmed):\n")
print(table(panel_trimmed$tna_source, useNA = "always"))

# =============================================================================
# VALIDATION: compare calculated vs LSEG flows (panel_trimmed only)
#   Validation is informational - performed on panel_trimmed where
#   LSEG flow coverage is highest.
# =============================================================================
comparison <- panel_trimmed %>%
  filter(!is.na(flow_calc_pct_win) &
           !is.na(flow_lseg_pct_win) &
           !is_december)

cat("\n--- VALIDATION: CALCULATED vs LSEG FLOWS (panel_trimmed) ---\n")
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
cat("fund-months with LSEG flow   :",
    sum(!is.na(panel_trimmed$flow_lseg_pct_win)), "\n")
cat("fund-months with calc. flow  :",
    sum(!is.na(panel_trimmed$flow_calc_pct_win)), "\n")
cat("fund-months with LSEG only   :",
    sum(!is.na(panel_trimmed$flow_lseg_pct_win) &
          is.na(panel_trimmed$flow_calc_pct_win)), "\n")
cat("fund-months with calc. only  :",
    sum(is.na(panel_trimmed$flow_lseg_pct_win) &
          !is.na(panel_trimmed$flow_calc_pct_win)), "\n")
cat("fund-months with both        :",
    sum(!is.na(panel_trimmed$flow_lseg_pct_win) &
          !is.na(panel_trimmed$flow_calc_pct_win)), "\n")
