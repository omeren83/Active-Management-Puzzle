# =============================================================================
# AUDIT CHECKS (Oct 2026) - data diagnostics that settle the data-dependent
# findings of the script audit. Read-only: writes audit_checks.xlsx only.
# Run AFTER Phase A (data_import_and_cleaning.R + flow_calculation.R) in the
# same session, from WORKING_DIR. Needs: dplyr, tidyr, readxl, lubridate, writexl.
# =============================================================================
suppressPackageStartupMessages({
  library(dplyr); library(tidyr); library(readxl); library(lubridate); library(writexl)
})
stopifnot(exists("panel_incubation"), exists("panel_master"), exists("parse_col_dates"))
out <- list()
ym  <- function(d) year(d) * 12L + month(d)

# --- C1. Is the "gross" TR index actually net of fees? ----------------------
# Passive funds: annual tracking difference (fund - benchmark) vs expense ratio.
# If the index is NAV-based (net), slope of TD on ER is ~ -1 and mean TD ~ -mean ER.
bench_long <- read_excel("fund_data.xlsx", sheet = "bench_returns", col_types = "text") %>%
  rename(Benchmark_Code = 1) %>%
  pivot_longer(-Benchmark_Code, names_to = "date", values_to = "lvl") %>%
  mutate(date = parse_col_dates(date), lvl = suppressWarnings(as.numeric(lvl))) %>%
  filter(!is.na(date), !is.na(lvl)) %>%
  group_by(Benchmark_Code) %>% arrange(date) %>%
  mutate(b_ret = lvl / lag(lvl) - 1, ym = ym(date)) %>% ungroup() %>%
  select(Benchmark_Code, ym, b_ret)

td <- panel_incubation %>%
  filter(ap_group == "Passive", !excluded_perf, !is.na(ret_gross_raw)) %>%
  mutate(ym = ym(date), ER = suppressWarnings(as.numeric(Expense_Ratio))) %>%
  inner_join(bench_long, by = c("Benchmark_Code", "ym")) %>%
  group_by(Ticker, Name, ER) %>%
  summarise(n = n(), td_ann = 1200 * mean(ret_gross_raw - b_ret, na.rm = TRUE),
            .groups = "drop") %>%
  filter(n >= 36, !is.na(ER), ER < 5)
if (nrow(td) >= 3) {
  fit <- lm(td_ann ~ ER, data = td)
  cat("\n[C1] Passive funds:", nrow(td), "| mean TD (%/yr):", round(mean(td$td_ann), 3),
      "| mean ER:", round(mean(td$ER), 3), "| slope TD~ER:", round(coef(fit)[2], 2),
      "(se", round(sqrt(vcov(fit)[2, 2]), 2), ")\n",
      "     slope ~ -1 => index is NET of fees; slope ~ 0 => index is gross.\n")
} else cat("\n[C1] Too few passive funds matched to a benchmark - check Benchmark_Code / dates.\n")
out$C1_tracking_diff <- arrange(td, desc(ER))

# --- H5. Internal gaps: lag() spans >1 calendar month -----------------------
gaps <- panel_master %>% group_by(Ticker) %>% arrange(date) %>%
  mutate(step = ym(date) - lag(ym(date))) %>% ungroup() %>% filter(!is.na(step))
cat("\n[H5] Fund-month steps > 1 month:", sum(gaps$step > 1), "of", nrow(gaps),
    "| funds affected:", n_distinct(gaps$Ticker[gaps$step > 1]), "\n")
out$H5_gaps <- gaps %>% filter(step > 1) %>% count(Ticker, name = "n_gaps") %>% arrange(desc(n_gaps))

# --- H5b. TNA source switches (class <-> total) -----------------------------
sw <- panel_master %>% group_by(Ticker) %>% arrange(date) %>%
  mutate(switch = tna_source != lag(tna_source)) %>% ungroup()
cat("[H5b] TNA source switches:", sum(sw$switch, na.rm = TRUE), "\n")

# --- H7. Expense ratio range ------------------------------------------------
er <- panel_master %>% distinct(Ticker, Name, ap_group, Expense_Ratio) %>%
  mutate(ER = suppressWarnings(as.numeric(Expense_Ratio)))
cat("\n[H7] ER summary (%):\n"); print(summary(er$ER))
out$H7_er_outliers <- er %>% filter(ER < 0 | ER > 5)
cat("     ER outside [0,5]:", nrow(out$H7_er_outliers), "funds\n")

# --- Dates: day-of-month convention, factor and sentiment end dates ---------
cat("\n[Dates] day-of-month of panel dates:\n"); print(table(day(panel_master$date)))
cat("Last month with MOM factor :", format(max(panel_master$date[!is.na(panel_master$MOM)])), "\n")
if ("SENT_ORTH" %in% names(panel_master))
  cat("Last month with SENT_ORTH  :", format(max(panel_master$date[!is.na(panel_master$SENT_ORTH)])), "\n")
cat("Last month with ret_gross  :", format(max(panel_master$date[!is.na(panel_master$ret_gross)])), "\n")

# --- H10. Winsorisation clipping concentrated in market-wide months? -------
clip <- panel_incubation %>% filter(!is.na(ret_gross_raw)) %>%
  group_by(date) %>% summarise(n_clipped = sum(ret_gross != ret_gross_raw), n = n(),
                               .groups = "drop") %>% arrange(desc(n_clipped))
cat("\n[H10] Top months by # of winsorised fund returns:\n"); print(head(clip, 8))
out$H10_clip_by_month <- clip

# --- D5. Funds with missing inception whose data starts at the pull start ---
inc <- panel_master %>% group_by(Ticker) %>%
  summarise(inc_na = all(is.na(Inception_Date)), first = min(date), .groups = "drop")
cat("\n[D5] Missing inception & first obs at pull start:",
    sum(inc$inc_na & inc$first == min(panel_master$date)), "funds\n")

# --- D13. Active funds in the performance sample (1,964 vs 1,973) -----------
cat("\n[D13] Active, !excluded_perf funds in panel_incubation:",
    n_distinct(panel_incubation$Ticker[panel_incubation$ap_group == "Active" &
                                       !panel_incubation$excluded_perf]), "\n")

write_xlsx(out, "audit_checks.xlsx")
cat("\nWrote audit_checks.xlsx\n")
