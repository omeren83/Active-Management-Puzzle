# =============================================================================
# GAP AND MISSING-EXPENSE-RATIO DIAGNOSTIC (read-only; changes nothing)
# diagnostics/gap_and_er_check.R  (v1.0, 17 Oct 2026)
#
# Audit items 1 and 3 (claude/final_audit_2026-10-17.md):
#   (3) Returns, flows and tna_lag use row lag(); a fund with a missing month
#       inside its history gets a multi-month return booked as one month.
#   (1) Funds without an expense ratio have NA gross return and drop out of
#       the portfolio sorts, including the net-return series.
#
# Run after Phase A and flow_calculation.R (panel_master and panel_incubation
# in the session, with ret_net_raw, ret_gross_raw, tna_lag, flow_calc_pct).
# Output: gap_er_diagnostic.xlsx in the working directory + console summary.
# =============================================================================

suppressPackageStartupMessages({
  library(dplyr)
  library(writexl)
})

if (!exists("WORKING_DIR")) WORKING_DIR <- getwd()
for (o in c("panel_master", "panel_incubation"))
  if (!exists(o)) stop(o, " not in session: run Phase A and flow_calculation.R first.")

# --- 1. Calendar step between consecutive rows of each fund --------------------
mon_idx <- function(d) as.integer(format(d, "%Y")) * 12L + as.integer(format(d, "%m"))
gaps <- panel_master %>%
  group_by(Ticker) %>%
  arrange(date, .by_group = TRUE) %>%
  mutate(step = mon_idx(date) - lag(mon_idx(date))) %>%
  ungroup()

# Rows that follow a gap (step > 1): their return/flow spans several months
after_gap <- gaps %>% filter(!is.na(step), step > 1L)
has <- function(v) if (v %in% names(after_gap)) sum(!is.na(after_gap[[v]])) else NA_integer_

# --- 2. How many of these rows reach the analysis samples ----------------------
key <- after_gap %>% select(Ticker, date, step)
inc <- panel_incubation %>% inner_join(key, by = c("Ticker", "date"))
inc_perf_act <- inc %>% filter(ap_group == "Active", !excluded_perf)

summary_gap <- data.frame(
  item = c("panel_master fund-months", "rows after an internal gap (step > 1)",
           "  ... funds affected", "  ... with ret_net_raw non-missing",
           "  ... with flow_calc_pct non-missing", "  ... with tna_lag non-missing",
           "incubation rows after a gap", "  ... active, perf-comparison sample",
           "  ... active perf, ret_net_raw non-missing", "  ... funds affected (active perf)"),
  value = c(nrow(panel_master), nrow(after_gap), n_distinct(after_gap$Ticker),
            has("ret_net_raw"), has("flow_calc_pct"), has("tna_lag"),
            nrow(inc), nrow(inc_perf_act), sum(!is.na(inc_perf_act$ret_net_raw)),
            n_distinct(inc_perf_act$Ticker)))
step_dist <- after_gap %>% count(step, name = "rows") %>% arrange(step)
gap_rows  <- after_gap %>%
  select(Ticker, any_of(c("Name", "ap_group")), date, step,
         any_of(c("ret_net_raw", "tna", "tna_lag", "flow_calc_pct"))) %>%
  arrange(desc(step), Ticker, date)

# --- 3. Missing expense ratio: fund-months and funds, by group ----------------
no_er <- panel_incubation %>%
  filter(!excluded_perf, is.na(ret_gross_raw), !is.na(ret_net_raw))
er_by_group <- no_er %>%
  group_by(ap_group) %>%
  summarise(fund_months = n(), funds = n_distinct(Ticker),
            last_month = format(max(date), "%Y-%m"), .groups = "drop")
er_share <- panel_incubation %>%
  filter(!excluded_perf, !is.na(ret_net_raw)) %>%
  group_by(ap_group) %>%
  summarise(all_fund_months = n(), all_funds = n_distinct(Ticker), .groups = "drop") %>%
  left_join(er_by_group, by = "ap_group") %>%
  mutate(share_fund_months_pct = round(100 * fund_months / all_fund_months, 2),
         share_funds_pct = round(100 * funds / all_funds, 2))
# Unflagged (reclassified) vs Bloomberg-flagged funds among those without ER
if ("Actively_Managed_New" %in% names(no_er)) {
  er_flag <- no_er %>%
    mutate(flagged = Actively_Managed_New %in% c("Y", "N")) %>%
    group_by(ap_group, flagged) %>%
    summarise(fund_months = n(), funds = n_distinct(Ticker), .groups = "drop")
} else er_flag <- data.frame(note = "Actively_Managed_New not in panel")

# --- 4. Output -------------------------------------------------------------------
cat("\n=== Internal gaps ===\n"); print(summary_gap, row.names = FALSE)
cat("\nGap length (months between consecutive rows):\n"); print(step_dist, row.names = FALSE)
cat("\n=== Missing expense ratio (performance-comparison sample) ===\n")
print(er_share, row.names = FALSE); print(er_flag, row.names = FALSE)
write_xlsx(list(gap_summary = summary_gap, gap_lengths = step_dist, gap_rows = gap_rows,
                no_er_share = er_share, no_er_by_flag = er_flag),
           file.path(WORKING_DIR, "gap_er_diagnostic.xlsx"))
cat("\nWritten: gap_er_diagnostic.xlsx\n")
