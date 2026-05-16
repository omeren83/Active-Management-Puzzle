# =============================================================================
# STRUCTURAL BREAK TEST: BAI-PERRON ON ROLLING ACTIVE FUND ALPHA SERIES
# structural_break_test.R  (v1.2)
#
# v1.2 changes vs v1.1 (figure inline-text strip):
#   fig_breaktest inline narrative text stripped per project-wide convention.
#   Figure title, subtitle (Bai-Perron methodology metadata), and source
#   caption (line/colour legend, HAC details) all moved to the LaTeX
#   \caption{} block in dissertation_main.tex. Only the y-axis label,
#   x-axis date scale, and the in-figure break-date markers (geom_text
#   labels added when has_breaks) remain in the PNG itself. The break-date
#   markers are the one piece of "narrative" that stays in-figure because
#   they pin specific dates to specific positions on the time axis - no
#   amount of LaTeX caption can substitute for that.
#
# v1.1 changes vs v1.0 (Family C audit):
#   No code change. This script reads alpha_rolling.xlsx (produced by
#   alpha_estimation.R), and that input now reflects the v2.7 panel-prep
#   filter filter(!excluded_perf), so the rolling alpha series itself is
#   computed on the performance-comparison subsample defined by
#   flagged_funds.xlsx. The Bai-Perron break dates produced by this script
#   therefore implicitly inherit the new sample definition.
#
#   IMPORTANT: After re-running alpha_estimation.R v2.7, the existing
#   subperiod_analysis.R SUBPERIODS thresholds (Jan 2006, Nov 2011) become
#   stale because the rolling alpha series itself shifts. This script must
#   be re-run BEFORE subperiod_analysis.R, and the new break dates pasted
#   into SUBPERIODS$P1$date_hi and SUBPERIODS$P2$date_hi (and the panel /
#   window / months strings updated accordingly). subperiod_analysis.R v1.5
#   enforces this with a hard stop on the panel_trimmed-era thresholds.
#
# PURPOSE:
#   Formally tests for structural breaks in the monthly cross-sectional mean
#   of 36-month rolling Carhart (1997) four-factor gross alpha, Active funds.
#   This is the same series plotted in Figure 2 (before 12-month smoothing),
#   and motivates the sub-period thresholds for the dissertation appendix.
#
# METHOD:
#   Bai and Perron (1998, Econometrica; 2003, JAE) multiple structural break
#   test, implemented via strucchange::breakpoints(). Confidence intervals
#   use Newey-West HAC standard errors (sandwich::NeweyWest) to account for
#   the substantial serial autocorrelation induced by overlapping 36-month
#   rolling windows.
#
# IMPORTANT NOTE ON SERIES CHOICE:
#   The test is applied to the RAW monthly cross-sectional mean alpha series,
#   NOT the 12-month smoothed series used in Figure 2. Smoothing (trailing
#   mean) introduces MA serial correlation that would inflate the test
#   statistic. The raw series already has substantial overlap-induced
#   autocorrelation; applying HAC inference on top of smoothing would be
#   double-adjusting, so the raw series is the correct input.
#
# INPUTS:   alpha_rolling.xlsx  (produced by alpha_estimation.R)
# OUTPUTS:  breaktest_results.xlsx  (break dates, CIs, F-stats, BIC table)
#           fig_breaktest.png        (annotated alpha series with breaks)
#           [console] summary table
#
# REFERENCES:
#   Bai, J., Perron, P. (1998). Estimating and testing linear models with
#     multiple structural changes. Econometrica, 66(1), 47-78.
#   Bai, J., Perron, P. (2003). Computation and analysis of multiple
#     structural change models. Journal of Applied Econometrics, 18(1), 1-22.
# =============================================================================

library(dplyr)
library(lubridate)
library(readxl)
library(writexl)
library(ggplot2)
library(slider)
library(strucchange)   # Bai-Perron breakpoints(), Fstats(), sctest()
library(sandwich)      # NeweyWest() for HAC CIs on break dates

# Working directory: data folder (same convention as master_pipeline.R)
if (!exists("WORKING_DIR")) WORKING_DIR <- "D:/TEZ/data/R import"
setwd(WORKING_DIR)

# =============================================================================
# CONFIGURATION
# =============================================================================
INPUT_FILE   <- "alpha_rolling.xlsx"
OUTPUT_EXCEL <- "breaktest_results.xlsx"
OUTPUT_FIG   <- "fig_breaktest.png"

# Maximum number of structural breaks to test (BIC will select among 0..MAX_BREAKS)
MAX_BREAKS   <- 5L

# Minimum segment length (months). 36 matches the rolling window width;
# ensures each regime has enough observations for reliable alpha estimation.
MIN_SEG_MONTHS <- 36L

# Newey-West lag for HAC confidence intervals on break dates.
# Rule: floor(4 * (T/100)^(2/9)), standard for monthly financial data.
# Computed dynamically below once T is known.

# =============================================================================
# 1. LOAD AND PREPARE THE SERIES
# =============================================================================
cat("=== 1. Loading alpha_rolling.xlsx ===\n")
alpha_roll <- read_excel(INPUT_FILE)

# alpha_ann in the Excel file is in decimal form (from alpha_estimation.R);
# multiply by 100 to express in percent for interpretable break magnitudes.
alpha_roll <- alpha_roll %>%
  mutate(
    date      = as.Date(date),
    alpha_pct = alpha_ann * 100
  )

# Compute monthly cross-sectional EW mean across Active funds.
# This is the series tested for breaks; it is NOT smoothed.
alpha_ts_raw <- alpha_roll %>%
  filter(ap_group == "Active", !is.na(alpha_pct)) %>%
  group_by(date) %>%
  summarise(mean_alpha = mean(alpha_pct, na.rm = TRUE),
            n_funds    = n(),
            .groups    = "drop") %>%
  arrange(date)

cat(sprintf("Series: %d monthly observations, %s to %s\n",
            nrow(alpha_ts_raw),
            format(min(alpha_ts_raw$date), "%b %Y"),
            format(max(alpha_ts_raw$date), "%b %Y")))
cat(sprintf("Mean fund count per month: %.0f (min %d, max %d)\n",
            mean(alpha_ts_raw$n_funds),
            min(alpha_ts_raw$n_funds),
            max(alpha_ts_raw$n_funds)))

# Also compute 12-month smoothed series for the annotated plot only
# (not for the break test itself).
alpha_ts_raw <- alpha_ts_raw %>%
  mutate(smooth_alpha = slide_index_dbl(
    .x = mean_alpha, .i = floor_date(date, "month"),
    .f = mean, .before = months(11), .complete = TRUE
  ))

# =============================================================================
# 2. CONSTRUCT ts OBJECT AND SET PARAMETERS
# =============================================================================
T_obs     <- nrow(alpha_ts_raw)
start_yr  <- year(min(alpha_ts_raw$date))
start_mo  <- month(min(alpha_ts_raw$date))
h_frac    <- MIN_SEG_MONTHS / T_obs          # minimum segment as fraction of T
nw_lag    <- floor(4 * (T_obs / 100)^(2/9)) # Andrews (1991) rule for monthly data
cat(sprintf("T = %d | min segment = %d months (h = %.3f) | NW lag = %d\n",
            T_obs, MIN_SEG_MONTHS, h_frac, nw_lag))

y_ts <- ts(alpha_ts_raw$mean_alpha,
           start     = c(start_yr, start_mo),
           frequency = 12)

# =============================================================================
# 3. BAI-PERRON: OPTIMAL BREAK POINT ESTIMATION
#
#    Model: y_t = mu_i + e_t  for t in segment i  (breaks in mean only)
#    Selection: OLS-based RSS/BIC across 0..MAX_BREAKS; confidence intervals
#               use the Newey-West sandwich covariance.
# =============================================================================
cat("\n=== 3. Bai-Perron breakpoints() ===\n")

bp <- breakpoints(y_ts ~ 1,
                  h      = h_frac,
                  breaks = MAX_BREAKS)

# Print RSS/BIC table (used to select optimal number of breaks)
bp_summary <- summary(bp)
print(bp_summary)

# Extract optimal break count via BIC.
# AIC(bp, k = log(T)) computes BIC for m = 0..MAX_BREAKS and returns a named
# vector; this is the recommended interface (avoids depending on the internal
# $RSS matrix structure which can differ across strucchange versions).
bic_all   <- AIC(bp, k = log(T_obs))
n_optimal <- as.integer(which.min(bic_all)) - 1L   # 0-indexed
cat(sprintf("\nOptimal number of breaks by BIC: %d\n", n_optimal))

# Refit with optimal break count
bp_opt <- breakpoints(bp, breaks = n_optimal)

# Break dates (index into the ts object -> calendar dates).
# bp_opt$breakpoints is NA when n_optimal == 0; guard against that.
break_indices <- if (n_optimal == 0L) integer(0L) else bp_opt$breakpoints
break_dates   <- alpha_ts_raw$date[break_indices]
cat("Break dates (end of each segment):\n")
print(break_dates)

# =============================================================================
# 4. CONFIDENCE INTERVALS WITH HAC COVARIANCE
#
#    confint.breakpointsfull() with vcov. = sandwich::NeweyWest accounts for
#    the overlap-induced autocorrelation in the cross-sectional mean series.
#    The 95% CI is expressed in index units; we convert to calendar months.
# =============================================================================
cat("\n=== 4. HAC confidence intervals for break dates ===\n")

# Build the NW vcov function scoped to our lag choice
nw_vcov <- function(x, ...) NeweyWest(x, lag = nw_lag, prewhite = FALSE, ...)

if (n_optimal == 0L) {
  # No breaks selected: CI data frame is empty but same structure
  ci_df <- data.frame(break_n  = integer(0),
                      lower_95 = as.Date(character(0)),
                      estimate = as.Date(character(0)),
                      upper_95 = as.Date(character(0)))
  cat("No structural breaks selected by BIC; CI table is empty.\n")
} else {
  # confint() with vcov. requires the original breakpointsfull object (bp),
  # NOT the refitted bp_opt which is class "breakpoints". Pass breaks= here.
  ci_bp <- confint(bp, breaks = n_optimal, vcov. = nw_vcov, level = 0.95)
  
  # Extract CI matrix and convert index positions to calendar dates.
  # ci_bp$confint rows = breaks, cols = [lower, bp, upper] as integer indices.
  ci_mat <- ci_bp$confint
  # Ensure matrix orientation even for the single-break case (returns vector)
  if (!is.matrix(ci_mat)) ci_mat <- matrix(ci_mat, nrow = 1)
  
  ci_dates <- apply(ci_mat, 2, function(idx) {
    idx_safe <- pmax(1L, pmin(T_obs, as.integer(round(idx))))
    alpha_ts_raw$date[idx_safe]
  })
  # apply() drops to vector if ci_mat has one row; restore matrix shape
  if (!is.matrix(ci_dates)) ci_dates <- matrix(ci_dates, nrow = 1)
  
  ci_df <- data.frame(
    break_n  = seq_len(nrow(ci_mat)),
    lower_95 = as.Date(ci_dates[, 1], origin = "1970-01-01"),
    estimate = as.Date(ci_dates[, 2], origin = "1970-01-01"),
    upper_95 = as.Date(ci_dates[, 3], origin = "1970-01-01")
  )
  print(ci_df)
}

# =============================================================================
# 5. SEGMENT-LEVEL REGIME MEANS
# =============================================================================
cat("\n=== 5. Regime mean alpha (%, annualised) ===\n")

# Build segment membership vector
seg_boundaries <- c(0L, break_indices, T_obs)
segment_labels <- rep(NA_integer_, T_obs)
for (s in seq_along(break_indices) + 1L) {
  segment_labels[(seg_boundaries[s - 1L] + 1L):seg_boundaries[s]] <- s - 1L
}
segment_labels[is.na(segment_labels)] <- length(break_indices) + 1L

alpha_ts_raw$segment <- segment_labels

regime_means <- alpha_ts_raw %>%
  group_by(segment) %>%
  summarise(
    start_date  = min(date),
    end_date    = max(date),
    n_months    = n(),
    mean_alpha  = mean(mean_alpha, na.rm = TRUE),
    sd_alpha    = sd(mean_alpha,   na.rm = TRUE),
    .groups     = "drop"
  )
print(regime_means)

# =============================================================================
# 6. STRUCTURAL BREAK F-STATISTICS (supF, meanF, expF)
#
#    Fstats() computes the sequence of F-statistics for a one-time break at
#    each point in [from, to]. sctest() delivers the global test statistics:
#    - supF  (Andrews 1993): supremum of F; tests H0: no break vs H1: one break
#    - meanF (Andrews & Ploberger 1994): mean F; more power against gradual shifts
#    - expF  (Andrews & Ploberger 1994): exponential mean F
#
#    These are single-break tests; the Bai-Perron above covers multiple breaks.
#    Together they form a standard battery (see Perron 2006 survey).
# =============================================================================
cat("\n=== 6. Global F-statistics (single-break null) ===\n")

# Trim 15% from each end for F-sequence stability (standard in the literature)
fs <- Fstats(y_ts ~ 1, from = 0.15, to = 0.85, vcov = nw_vcov)

f_sup  <- sctest(fs, type = "supF")
f_mean <- sctest(fs, type = "aveF")
f_exp  <- sctest(fs, type = "expF")

fstat_df <- data.frame(
  test      = c("supF (Andrews 1993)", "meanF (Andrews & Ploberger 1994)",
                "expF  (Andrews & Ploberger 1994)"),
  statistic = c(f_sup$statistic,  f_mean$statistic,  f_exp$statistic),
  p_value   = c(f_sup$p.value,    f_mean$p.value,    f_exp$p.value)
)
print(fstat_df)

# =============================================================================
# 7. BIC / RSS COMPARISON TABLE (for the appendix)
# =============================================================================
# bp_summary$RSS is a 2 x (MAX_BREAKS+1) matrix produced by summary():
#   row "RSS" = residual sum of squares for 0..MAX_BREAKS breaks
#   row "BIC" = BIC for each model (same values as bic_all above)
rss_all <- as.numeric(bp_summary$RSS["RSS", ])

bic_tbl <- data.frame(
  n_breaks = 0:MAX_BREAKS,
  RSS      = round(rss_all, 4),
  BIC      = round(as.numeric(bic_all), 4)
)
bic_tbl$optimal <- bic_tbl$n_breaks == n_optimal
print(bic_tbl)

# =============================================================================
# 8. ANNOTATED FIGURE: SMOOTHED SERIES + BREAK DATES + 95% CI BANDS
#
#    Uses the 12-month smoothed series (as in Figure 2) for visual consistency.
#    Vertical dashed lines = BP point estimates; shaded bands = 95% HAC CIs.
#    This figure goes into the appendix to justify the sub-period thresholds.
# =============================================================================
cat("\n=== 8. Generating annotated plot ===\n")

# Regime-mean step function (raw-series means, for visual reference)
regime_step <- alpha_ts_raw %>%
  group_by(segment) %>%
  mutate(regime_mean_val = mean(mean_alpha, na.rm = TRUE)) %>%
  ungroup()

# Build annotation layers only when breaks exist
has_breaks <- nrow(ci_df) > 0L

if (has_breaks) {
  break_label_df <- ci_df %>%
    mutate(label = format(estimate, "%b %Y"))
  
  ci_rect_df <- ci_df %>%
    mutate(xmin = as.Date(lower_95),
           xmax = as.Date(upper_95),
           ymin = -Inf, ymax = Inf)
}

p_break <- ggplot(alpha_ts_raw, aes(x = date)) +
  # Zero reference line
  geom_hline(yintercept = 0, colour = "grey60", linetype = "dashed",
             linewidth = 0.4) +
  # Regime mean step function
  geom_step(data = regime_step, aes(y = regime_mean_val),
            colour = "#E74C3C", linetype = "solid", linewidth = 0.6,
            direction = "hv") +
  # Smoothed alpha series (matches Figure 2)
  geom_line(aes(y = smooth_alpha), colour = "#2166AC", linewidth = 0.85,
            na.rm = TRUE) +
  scale_y_continuous(labels = function(x) paste0(x, "%")) +
  scale_x_date(date_breaks = "5 years", date_labels = "%Y") +
  theme_classic(base_size = 11) +
  labs(
    # v1.2: title, subtitle, and source caption moved to the LaTeX
    # \caption{} block. Only the y-axis label and the in-figure
    # break-date markers (added below as geom_text when has_breaks)
    # remain in the figure itself. The break-date text labels are
    # essential because they identify regime transitions on the
    # plot directly; the LaTeX caption can describe them in prose.
    title    = NULL,
    subtitle = NULL,
    caption  = NULL,
    y        = "Annualized Gross Alpha (%)",
    x        = NULL
  ) +
  theme(
    plot.caption = element_text(size = 7.5, colour = "grey45",
                                hjust = 0, margin = margin(t = 8))
  )

# Add break-date layers only when breaks were found
if (has_breaks) {
  p_break <- p_break +
    geom_rect(data = ci_rect_df,
              aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
              fill = "steelblue", alpha = 0.12, inherit.aes = FALSE) +
    geom_vline(data = break_label_df,
               aes(xintercept = as.Date(estimate)),
               colour = "#C0392B", linetype = "dashed", linewidth = 0.65) +
    geom_text(data = break_label_df,
              aes(x = as.Date(estimate), y = Inf, label = label),
              colour = "#C0392B", size = 2.8, vjust = 1.5, hjust = -0.08)
}

ggsave(OUTPUT_FIG, plot = p_break, width = 8.5, height = 4.8, dpi = 300)
cat(sprintf("Written: %s\n", OUTPUT_FIG))

# =============================================================================
# 9. EXPORT TO EXCEL
# =============================================================================
cat("\n=== 9. Writing Excel output ===\n")

# Sheet 1: Break date estimates and 95% HAC CIs
sheet_breaks <- ci_df %>%
  mutate(
    lower_95 = format(lower_95, "%Y-%m-%d"),
    estimate = format(estimate, "%Y-%m-%d"),
    upper_95 = format(upper_95, "%Y-%m-%d")
  )
colnames(sheet_breaks) <- c("Break #", "95% CI Lower", "Point Estimate", "95% CI Upper")

# Sheet 2: Regime means
sheet_regimes <- regime_means %>%
  mutate(start_date = format(start_date, "%Y-%m-%d"),
         end_date   = format(end_date,   "%Y-%m-%d"),
         across(c(mean_alpha, sd_alpha), ~ round(.x, 4)))
colnames(sheet_regimes) <- c("Segment", "Start", "End", "N Months",
                             "Mean Alpha (%)", "SD Alpha (%)")

# Sheet 3: BIC / RSS table
sheet_bic <- bic_tbl %>%
  mutate(across(c(RSS, BIC), ~ round(.x, 4)),
         optimal = ifelse(optimal, "YES", ""))
colnames(sheet_bic) <- c("N Breaks", "RSS", "BIC", "BIC Optimal")

# Sheet 4: Global F-statistics
sheet_fstats <- fstat_df %>%
  mutate(statistic = round(statistic, 4),
         p_value   = round(p_value,   4))
colnames(sheet_fstats) <- c("Test", "Statistic", "p-value")

# Sheet 5: Full monthly series (raw + smoothed)
sheet_series <- alpha_ts_raw %>%
  select(date, mean_alpha, smooth_alpha, n_funds, segment) %>%
  mutate(date = format(date, "%Y-%m-%d"),
         across(c(mean_alpha, smooth_alpha), ~ round(.x, 4)))
colnames(sheet_series) <- c("Date", "Raw Mean Alpha (%)", "Smoothed Alpha (%)",
                            "N Funds", "Segment")

write_xlsx(
  list(
    "Break Dates"   = sheet_breaks,
    "Regime Means"  = sheet_regimes,
    "BIC Table"     = sheet_bic,
    "F-Statistics"  = sheet_fstats,
    "Monthly Series" = sheet_series
  ),
  path = OUTPUT_EXCEL
)
cat(sprintf("Written: %s\n", OUTPUT_EXCEL))

# =============================================================================
# 10. CONSOLE SUMMARY
# =============================================================================
cat("\n", paste(rep("=", 65), collapse = ""), "\n", sep = "")
cat("STRUCTURAL BREAK TEST SUMMARY\n")
cat(paste(rep("=", 65), collapse = ""), "\n", sep = "")
cat(sprintf("Series:       Monthly EW mean active-fund rolling gross alpha\n"))
cat(sprintf("Sample:       %s - %s (%d months)\n",
            format(min(alpha_ts_raw$date), "%b %Y"),
            format(max(alpha_ts_raw$date), "%b %Y"),
            T_obs))
cat(sprintf("Method:       Bai-Perron (1998, 2003) | min seg: %d mo | NW lag: %d\n",
            MIN_SEG_MONTHS, nw_lag))
cat(sprintf("Optimal breaks (BIC): %d\n\n", n_optimal))

cat("Break date estimates (95% HAC CI):\n")
for (i in seq_len(nrow(ci_df))) {
  cat(sprintf("  Break %d:  %s  [%s, %s]\n",
              i,
              format(ci_df$estimate[i], "%b %Y"),
              format(ci_df$lower_95[i], "%b %Y"),
              format(ci_df$upper_95[i], "%b %Y")))
}

cat("\nRegime mean alpha (%, annualised):\n")
for (i in seq_len(nrow(regime_means))) {
  cat(sprintf("  Period %d  %s - %s  (%d mo):  %.2f%% pa\n",
              i,
              format(regime_means$start_date[i], "%b %Y"),
              format(regime_means$end_date[i],   "%b %Y"),
              regime_means$n_months[i],
              regime_means$mean_alpha[i]))
}

cat("\nGlobal F-statistics:\n")
for (i in seq_len(nrow(fstat_df))) {
  cat(sprintf("  %-42s  stat = %6.3f  p = %.4f\n",
              fstat_df$test[i], fstat_df$statistic[i], fstat_df$p_value[i]))
}
cat(paste(rep("=", 65), collapse = ""), "\n", sep = "")
cat("[DONE] structural_break_test.R complete.\n")