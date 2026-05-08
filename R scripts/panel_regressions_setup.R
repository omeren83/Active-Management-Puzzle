# panel_regressions_setup.R                                              v1.3
# =============================================================================
# v1.3 changes vs v1.2 (Family E pre-defense audit):
#   - Default PANEL_CHOICE switched from "trimmed" to "incubation". This
#     aligns the H1-H4 behavioral panel regressions with the rest of the
#     dissertation, which migrated to panel_incubation (Evans 2010 36-month
#     incubation correction, no date cap) some time ago. panel_trimmed
#     remains selectable for robustness checks.
#   - excluded_perf and excluded_h3 boolean flag columns from
#     data_import_and_cleaning.R Step 8c (flagged_funds.xlsx ledger) are
#     now preserved through the transmute and carried into panel_reg.
#     This lets H3-channel scripts (H3_lottery_demand.R, psychological_premium.R)
#     apply filter(!excluded_h3) at panel-prep stage. Behavioral H1, H2, H4
#     scripts ignore these flags (Entire-Analysis exclusions already applied
#     at source, no further filter needed).
#   - Pre-flight check added: if the source panel lacks excluded_perf /
#     excluded_h3, the script errors with an instruction to re-run
#     data_import_and_cleaning.R v1.2+ Step 8c. No silent fallback.
#
# v1.2 changes (inline patch, May 2026):
#   - Lagged-sentiment columns added at end of script (suffix _lag), per
#     Huang, Jiang, Tu, and Zhou (2015, RFS) timing convention.
#
# v1.1 (original):
# =============================================================================
# Builds panel_reg: the regression-ready fund-month panel consumed by the
# four hypothesis-test scripts (H1-H4) of behavioral panel regressions
# against the Berk-Green (2004) rational null.
#
# CONFIG (top of script):
#   PANEL_CHOICE  one of: "trimmed" (default), "incubation", "master"
#   SUBSET        one of: "active"  (default), "passive", "all"
#   MIN_CAT_SIZE  minimum funds per (date, Lipper_Category) cell to compute
#                 a fractional rank (default 5)
#
# DEPENDENCIES (session objects from prior phases):
#   panel_master / panel_incubation / panel_trimmed   (Phase A)
#   behavioral_state_vars                              (Phase I)
#   alpha_rolling.xlsx in WORKING_DIR                  (Phase B; for ActR2)
#
# OUTPUT:
#   panel_reg       global env data frame (one row per fund-month)
#   panel_reg.rds   serialised, in WORKING_DIR
#
# COLUMNS PRODUCED IN panel_reg:
#   IDs:           Ticker, date, yearmo, Lipper_Category, ap_group, Name
#   LHS:           flow (Sirri-Tufano winsorised), is_december (filter flag)
#   Performance:   cumret_lag (12m gross return ending t-1), rank_lag,
#                  R_LOW, R_MID, R_HIGH (proposal Eq 6-8)
#   Controls:      log_TNA, log_Age, ExpRatio, LoadDummy, ret_vol (36m SD),
#                  Turnover, style_flow_lag
#   Activeness:    ActR2 (1 - rolling Carhart R²), ActSkew (36m skewness)
#   State vars:    SENT_ORTH, PLS_SENT, VIX, SKEW, PUT_CALL_RATIO, AAII_BB,
#                  UMCSENT, MD_RATIO, DMD_YOY, plus 8 D_* regime dummies
#
# NOTES:
#   - Ranking is within-Lipper-category, monthly, on lagged 12-month gross
#     return. Funds with NA Lipper_Category are dropped (Sirri-Tufano /
#     Cheng et al. 2025 standard).
#   - Performance and rank are computed AFTER the SUBSET filter is applied,
#     i.e. an active fund's rank is computed within active peers in its
#     Lipper category, not within the full universe. This matches the
#     "investor's choice set within active management" framing of H1-H4.
#   - StyleFlow is leave-one-out (excludes focal fund i) and lagged 1 month.
#     Proposal Section 6.5 ("All controls are lagged one period").
#   - LoadDummy = 1 for funds whose Name carries -A / -B / -C suffix; 0
#     otherwise (regex on Name; ~29% of universe per name-classification
#     diagnostic). Time-invariant.
#   - Return volatility is rolling 36-month SD per proposal Section 6.5
#     (note: Sirri-Tufano 1998 used 12m; we use 36m as committed in the
#     proposal). Skewness is also 36m (proposal Eq 10). Both require >=36
#     monthly observations -> truncates short-lived funds.
#   - Activeness ActR2 = 1 - R² from rolling 36m Carhart regression
#     (Amihud & Goyenko 2013), already produced by alpha_estimation.R as
#     column as_r2 of alpha_rolling.xlsx.
#
# REFERENCES:
#   Sirri E.R. & Tufano P. (1998). J Finance 53(5), 1589-1622.
#   Cheng X. et al. (2025). Financial Management 54(3).
#   Evans R.B. (2010). J Finance 65(4), 1581-1611.
#   Amihud Y. & Goyenko R. (2013). RFS 26(3), 667-694.
#   Proposal Sections 6.2, 6.3.1, 6.3.2, 6.5.
#
# Dependencies: dplyr, tidyr, slider, e1071, lubridate, stringr, readxl
# =============================================================================

suppressPackageStartupMessages({
  library(dplyr); library(tidyr); library(stringr)
  library(slider); library(e1071); library(lubridate)
  library(readxl)
})

# --- 0. Config ---------------------------------------------------------------
# v1.3: default panel switched from "trimmed" to "incubation" to align with
# the rest of the dissertation (alpha_estimation.R, FF_comparison.R,
# subperiod_analysis.R, persistence_testing.R, activeness_analysis.R have
# all been on panel_incubation since the Evans-2010 migration).
PANEL_CHOICE <- "incubation"  # "incubation" (default) | "trimmed" | "master"
SUBSET       <- "active"      # "active"  | "passive"   | "all"
MIN_CAT_SIZE <- 5L            # min funds per (date, Lipper_Category) for rank

if (!exists("WORKING_DIR")) WORKING_DIR <- getwd()
ALPHA_ROLLING_FILE <- file.path(WORKING_DIR, "alpha_rolling.xlsx")
OUT_FILE_RDS       <- file.path(WORKING_DIR, "panel_reg.rds")

# --- 1. Select source panel --------------------------------------------------
panel_obj_name <- switch(
  PANEL_CHOICE,
  "trimmed"    = "panel_trimmed",
  "incubation" = "panel_incubation",
  "master"     = "panel_master",
  stop("PANEL_CHOICE must be 'trimmed', 'incubation', or 'master'")
)
if (!exists(panel_obj_name)) {
  stop("Required session object '", panel_obj_name,
       "' not found. Run Phase A first.")
}
panel <- get(panel_obj_name)
cat("Source panel:", panel_obj_name,
    " | rows =", nrow(panel),
    " | funds =", n_distinct(panel$Ticker), "\n")

# --- 2. Sanity-check required columns ----------------------------------------
required <- c("Ticker", "date", "ret_gross_raw", "tna_lag",
              "flow_calc_pct_win", "is_december", "ap_group",
              "Lipper_Category", "Name", "Inception_Date",
              "Expense_Ratio", "Turnover")
missing_cols <- setdiff(required, names(panel))
if (length(missing_cols)) {
  stop("Missing required column(s) in ", panel_obj_name, ": ",
       paste(missing_cols, collapse = ", "))
}

# v1.3: flagged_funds.xlsx Step 8c flags must be present so downstream
# H3-channel scripts can apply filter(!excluded_h3). Behavioral H1/H2/H4
# scripts ignore these flags but they are preserved in panel_reg for audit.
required_flags <- c("excluded_perf", "excluded_h3")
missing_flags  <- setdiff(required_flags, names(panel))
if (length(missing_flags)) {
  stop("Missing flagged_funds.xlsx Step 8c flag(s) in ", panel_obj_name, ": ",
       paste(missing_flags, collapse = ", "),
       ". Re-run data_import_and_cleaning.R (v1.2 or later) so that the ",
       "flagged_funds.xlsx exclusion ledger is wired into the source panel.")
}

# --- 3. Filter to fund subset ------------------------------------------------
panel <- switch(
  SUBSET,
  "active"  = filter(panel, ap_group == "Active"),
  "passive" = filter(panel, ap_group == "Passive"),
  "all"     = filter(panel, ap_group %in% c("Active", "Passive")),
  stop("SUBSET must be 'active', 'passive', or 'all'")
)
cat("After SUBSET=", SUBSET, ": rows =", nrow(panel),
    " | funds =", n_distinct(panel$Ticker), "\n", sep = "")

# --- 4. Drop funds without Lipper_Category -----------------------------------
n_before <- n_distinct(panel$Ticker)
panel <- filter(panel, !is.na(Lipper_Category))
n_dropped <- n_before - n_distinct(panel$Ticker)
cat("Dropped", n_dropped, "fund(s) with NA Lipper_Category. ",
    "Remaining funds =", n_distinct(panel$Ticker), "\n")

# --- 5. LoadDummy (regex on Name; static per fund) ---------------------------
load_pat <- "[-/ ](A|B|C|A1|A2|CL A|CL B|CL C|CLASS A|CLASS B|CLASS C)$"
load_lookup <- panel %>%
  distinct(Ticker, Name) %>%
  mutate(LoadDummy = as.integer(str_detect(toupper(Name), load_pat))) %>%
  select(Ticker, LoadDummy)
panel <- left_join(panel, load_lookup, by = "Ticker")
cat("LoadDummy: load=", sum(load_lookup$LoadDummy),
    " | other=", sum(load_lookup$LoadDummy == 0), "\n", sep = "")

# --- 6. Per-fund rolling stats (12m return, 36m vol, 36m skew, age) ----------
# Strict-completion windows: any NA input -> NA output (no near-window estimates).
panel <- panel %>%
  arrange(Ticker, date) %>%
  group_by(Ticker) %>%
  mutate(
    cumret_12m   = slide_dbl(ret_gross_raw,
                             ~ prod(1 + .x) - 1,
                             .before = 11, .complete = TRUE),
    ret_vol_36m  = slide_dbl(ret_gross_raw,
                             stats::sd,
                             .before = 35, .complete = TRUE),
    ret_skew_36m = slide_dbl(ret_gross_raw,
                             ~ e1071::skewness(.x, na.rm = FALSE),
                             .before = 35, .complete = TRUE),
    age_months   = as.numeric(difftime(date, as.Date(Inception_Date),
                                       units = "days")) / 30.4375,
    ret_max12_12m = slide_dbl(ret_gross_raw, max,
                              .before = 11, .complete = TRUE)
  ) %>%
  ungroup()

# --- 7. Lag rolling stats by 1 month -----------------------------------------
panel <- panel %>%
  arrange(Ticker, date) %>%
  group_by(Ticker) %>%
  mutate(
    cumret_lag   = lag(cumret_12m,   1),
    ret_vol_lag  = lag(ret_vol_36m,  1),
    ret_skew_lag = lag(ret_skew_36m, 1),
    age_lag      = lag(age_months,   1),
    ret_max12_lag = lag(ret_max12_12m, 1)
  ) %>%
  ungroup()

# --- 8. Within-Lipper-category fractional rank on lagged cumret -------------
# Position-based rank divided by category size with the (rank - 0.5) / N
# Hodges-Lehmann adjustment so values lie in (0, 1).
panel <- panel %>%
  group_by(date, Lipper_Category) %>%
  mutate(
    .n_in_cat = sum(!is.na(cumret_lag)),
    rank_lag  = if_else(
      !is.na(cumret_lag) & .n_in_cat >= MIN_CAT_SIZE,
      (rank(cumret_lag, ties.method = "average", na.last = "keep") - 0.5) /
         .n_in_cat,
      NA_real_
    )
  ) %>%
  ungroup() %>%
  select(-.n_in_cat) %>%
  mutate(
    R_LOW  = pmin(rank_lag, 0.20),
    R_MID  = pmin(pmax(rank_lag - 0.20, 0), 0.60),
    R_HIGH = pmax(rank_lag - 0.80, 0)
  )

# --- 9. Leave-one-out style flow, then lag by 1 month ------------------------
panel <- panel %>%
  group_by(date, Lipper_Category) %>%
  mutate(
    .style_n     = sum(!is.na(flow_calc_pct_win)),
    .style_total = sum(flow_calc_pct_win, na.rm = TRUE),
    style_flow   = if_else(
      !is.na(flow_calc_pct_win) & .style_n > 1,
      (.style_total - flow_calc_pct_win) / (.style_n - 1),
      NA_real_
    )
  ) %>%
  ungroup() %>%
  select(-.style_n, -.style_total) %>%
  arrange(Ticker, date) %>%
  group_by(Ticker) %>%
  mutate(style_flow_lag = lag(style_flow, 1)) %>%
  ungroup()

# --- 10. log_TNA and log_Age (lagged; defensive against zero/negative) ------
# tna_lag is already TNA_{t-1} from flow_calculation.R -- do not lag again.
panel <- panel %>%
  mutate(
    log_TNA = if_else(is.na(tna_lag) | tna_lag <= 0, NA_real_, log(tna_lag)),
    log_Age = if_else(is.na(age_lag) | age_lag <  0, NA_real_,
                      log(1 + age_lag))
  )

# --- 11. ActR2 from alpha_rolling.xlsx, then lag -----------------------------
if (!file.exists(ALPHA_ROLLING_FILE)) {
  warning("alpha_rolling.xlsx not found at ", ALPHA_ROLLING_FILE,
          "; ActR2 set to NA. H3 cannot be estimated until Phase B has run.")
  panel$ActR2 <- NA_real_
} else {
  ar <- read_xlsx(ALPHA_ROLLING_FILE)
  r2_col <- intersect(c("as_r2", "r2", "R2", "r_squared", "adj_r2"),
                      names(ar))
  if (length(r2_col) == 0) {
    stop("alpha_rolling.xlsx has no recognisable R² column. ",
         "Looked for: as_r2, r2, R2, r_squared, adj_r2. Found: ",
         paste(names(ar), collapse = ", "))
  }
  r2_col <- r2_col[1]
  cat("Using R² column from alpha_rolling.xlsx:", r2_col, "\n")
  ar <- ar %>%
    mutate(date = as.Date(date)) %>%
    transmute(Ticker, date, ActR2 = 1 - .data[[r2_col]])
  panel <- left_join(panel, ar, by = c("Ticker", "date"),
                     relationship = "one-to-one")
}
panel <- panel %>%
  arrange(Ticker, date) %>%
  group_by(Ticker) %>%
  mutate(ActR2_lag = lag(ActR2, 1)) %>%
  ungroup()

# --- 12. Merge behavioral state variables by year-month ---------------------
# Panel dates are LSEG trading-calendar EOM (e.g., 1994-12-30 = Friday).
# behavioral_state_vars dates are calendar EOM (e.g., 1994-12-31 = Saturday).
# Joining on raw date drops ~30% of fund-months for months whose calendar EOM
# falls on a weekend. Joining on (year, month) avoids the issue entirely:
# "VIX in December 1994" is the same monthly observation regardless of
# whether we label it Dec 30 or Dec 31.
if (!exists("behavioral_state_vars")) {
  stop("behavioral_state_vars not in session. Run Phase I ",
       "(behavioral_state_variables.R) first.")
}

bsv_full <- behavioral_state_vars %>% mutate(date = as.Date(date))

# Drop pre-existing behavioral columns from panel (data_import_and_cleaning.R
# already merges some sentiment proxies). Phase I values take precedence.
overlap <- intersect(
  setdiff(names(bsv_full), c("date", "yearmo")),
  names(panel)
)
if (length(overlap) > 0) {
  cat("Note: dropping ", length(overlap),
      " pre-existing column(s) from panel that overlap with behavioral ",
      "state vars (Phase I values will be used):\n  ",
      paste(overlap, collapse = ", "), "\n", sep = "")
  panel <- panel %>% select(-all_of(overlap))
}

# Build matching year-month key on both sides
panel <- panel %>%
  mutate(.yearmo_join = year(date) * 100L + month(date))
bsv <- bsv_full %>%
  mutate(.yearmo_join = year(date) * 100L + month(date)) %>%
  select(-date, -any_of("yearmo"))

panel <- left_join(panel, bsv, by = ".yearmo_join",
                   relationship = "many-to-one") %>%
  select(-.yearmo_join)

# --- 13. Final column selection ---------------------------------------------
panel_reg <- panel %>%
  transmute(
    Ticker, date,
    yearmo = year(date) * 100L + month(date),
    Lipper_Category, ap_group, Name,
    flow = flow_calc_pct_win,
    is_december,
    cumret_lag, rank_lag, R_LOW, R_MID, R_HIGH,
    log_TNA, log_Age,
    # Force numeric coercion: Excel/LSEG occasionally serialises numeric
    # fields as character (e.g. when a column has a single non-numeric cell
    # somewhere). Without explicit coercion, fixest treats ExpRatio as a
    # factor and creates ~700 dummies, all collinear with fund FE.
    ExpRatio  = suppressWarnings(as.numeric(Expense_Ratio)),
    LoadDummy = as.integer(LoadDummy),
    ret_vol   = ret_vol_lag,
    Turnover  = suppressWarnings(as.numeric(Turnover)),
    style_flow_lag,
    ActR2    = ActR2_lag,
    ActSkew  = ret_skew_lag,
    MAX12    = ret_max12_lag,
    SENT_ORTH, PLS_SENT, VIX, SKEW, PUT_CALL_RATIO,
    AAII_BB, UMCSENT, MD_RATIO, DMD_YOY, MD_DETREND,
    D_SENT, D_PLS, D_VIX, D_SKEW, D_PCR,
    D_MD, D_MD_LEVEL, D_MD_DETREND,
    D_AAII, D_UMCSENT,
    # v1.3: flagged_funds.xlsx Step 8c flags carried through so that
    # H3-channel scripts can apply filter(!excluded_h3).
    excluded_perf, excluded_h3
  )

# --- 14. Diagnostics ---------------------------------------------------------
cat("\n========================== panel_reg ===============================\n")
cat("rows =", nrow(panel_reg),
    " | funds =", n_distinct(panel_reg$Ticker),
    " | dates =", n_distinct(panel_reg$date),
    " | range =", format(min(panel_reg$date)), "->",
    format(max(panel_reg$date)), "\n")

cat("\nNon-NA observation count per column:\n")
nna <- vapply(panel_reg, function(x) sum(!is.na(x)), integer(1))
print(data.frame(column  = names(nna),
                 n_obs   = unname(nna),
                 pct_obs = round(100 * unname(nna) / nrow(panel_reg), 1),
                 row.names = NULL))

# Approx estimation sample for the H1 baseline (excluding behavioral state)
core_rhs <- c("flow", "rank_lag", "log_TNA", "log_Age", "ExpRatio",
              "LoadDummy", "ret_vol", "Turnover", "style_flow_lag")
est_n <- panel_reg %>%
  filter(!is_december) %>%
  filter(if_all(all_of(core_rhs), ~ !is.na(.))) %>%
  nrow()
cat("\nEst. sample (flow + core controls all non-NA, ex-December): ",
    est_n, " fund-months\n", sep = "")

# =============================================================================
# PATCH for panel_regressions_setup.R                                    v1.2
# =============================================================================
# Add this block to panel_regressions_setup.R AFTER all behavioral state
# variables have been merged into panel_reg, but BEFORE the final cat() summary
# / saveRDS().
#
# This creates lagged versions of every sentiment variable for use as the
# PRIMARY specification in H1/H2 (Huang et al. 2015 timing convention).
# Contemporaneous versions remain available for the timing robustness check.
# =============================================================================

# Lagged behavioral state variables (one-period lag, within fund)
# Following Huang, Jiang, Tu, and Zhou (2015, RFS) timing convention.
panel_reg <- panel_reg %>%
  arrange(Ticker, date) %>%
  group_by(Ticker) %>%
  mutate(
    # Continuous series lagged
    SENT_ORTH_lag      = dplyr::lag(SENT_ORTH,      1),
    PUT_CALL_RATIO_lag = dplyr::lag(PUT_CALL_RATIO, 1),
    AAII_BB_lag        = dplyr::lag(AAII_BB,        1),
    # Regime dummies lagged
    D_SENT_lag         = dplyr::lag(D_SENT,         1),
    D_AAII_lag         = dplyr::lag(D_AAII,         1),
    D_MD_DETREND_lag   = dplyr::lag(D_MD_DETREND,   1),
    D_MD_LEVEL_lag     = dplyr::lag(D_MD_LEVEL,     1),
    D_MD_lag           = dplyr::lag(D_MD,           1)
  ) %>%
  ungroup()

cat("Added lagged sentiment columns (suffix _lag).\n")

# --- 15. Save and expose -----------------------------------------------------
saveRDS(panel_reg, OUT_FILE_RDS)
cat("\nWrote", OUT_FILE_RDS, "\n")

assign("panel_reg", panel_reg, envir = .GlobalEnv)
