# behavioral_state_variables.R                                            v1.2
# =============================================================================
# v1.2 changes (Family E pre-defense audit):
#   - No code changes. This script builds the time-series of behavioral
#     state variables (sentiment, VIX, SKEW, PCR, AAII, UMCSENT, margin
#     debt) from the Sentiment sheet of fund_data.xlsx. It does not consume
#     the fund-level panel (panel_master / panel_incubation / panel_trimmed)
#     and is therefore unaffected by the flagged_funds.xlsx Step 8c
#     exclusion ledger. Audit-stamp bump only.
#
# v1.1 changes vs v1.0:
#   - Added MD_DETREND  = residual of OLS regression of log(MD_RATIO) on a
#     linear time trend, following Daniel-Klos-Pollet (2016 NYU Stern WP) and
#     Rapach-Ringgenberg-Zhou (2016 JFE). Captures elevated commitment net
#     of the secular trend in margin debt / market cap.
#   - Added D_MD_LEVEL   = 1 if MD_RATIO   > Q_66 of in-sample distribution.
#                          Captures accumulated leverage commitment (level).
#   - Added D_MD_DETREND = 1 if MD_DETREND > Q_66 of in-sample distribution.
#                          Methodologically cleanest H2 regime per literature.
#   - Existing D_MD (based on DMD_YOY year-over-year growth) is unchanged
#     and corresponds conceptually to D_MD_GROWTH. Three margin-debt
#     dummies coexist for the 6-column H2 spec.
#
# REFERENCES (added in v1.1):
#   Daniel K., Klos A., Pollet J. (2016). NYU Stern Working Paper.
#   Rapach D.E., Ringgenberg M.C., Zhou G. (2016). JFE 121(1).
# =============================================================================

suppressPackageStartupMessages({
  library(readxl); library(writexl)
  library(dplyr);  library(tidyr); library(lubridate)
})

# --- 0. Config ---------------------------------------------------------------
if (!exists("WORKING_DIR")) WORKING_DIR <- getwd()

INPUT_FILE        <- file.path(WORKING_DIR, "fund_data.xlsx")
INPUT_SHEET       <- "Sentiment"
OUTPUT_FILE       <- file.path(WORKING_DIR, "behavioral_state_vars.xlsx")
REGIME_PERCENTILE <- 0.66

EXPECTED_SERIES <- c(
  "SENT_ORTH","PLS_SENT","VIX","SKEW","PUT_CALL_RATIO",
  "VOL_NYSE","VOL_AMEX","VOL_TOTAL","MARGIN_DEBT","TOTAL_MCAP",
  "AAII_BULL","AAII_BEAR","UMCSENT"
)

# --- 1. Load sheet -----------------------------------------------------------
cat("Loading", INPUT_FILE, "  sheet:", INPUT_SHEET, "\n")

# Case-insensitive sheet resolver
sheet_names <- excel_sheets(INPUT_FILE)
match_idx <- which(tolower(sheet_names) == tolower(INPUT_SHEET))
if (length(match_idx) == 0) {
  stop("Sheet '", INPUT_SHEET, "' not found. Available: ",
       paste(sheet_names, collapse = ", "))
}
actual_sheet <- sheet_names[match_idx[1]]
if (actual_sheet != INPUT_SHEET) {
  cat("Note: requested '", INPUT_SHEET, "', using '", actual_sheet,
      "' (case-insensitive match).\n", sep = "")
}

raw <- suppressWarnings(read_excel(
  INPUT_FILE, sheet = actual_sheet, col_names = FALSE,
  na = c("", "NA", "#N/A N/A", "#N/A Field Not Applicable")
))

# Header row parsing (LSEG wide-by-row)
hdr      <- as.character(unlist(raw[1, ]))
date_hdr <- hdr[-1]

parse_dates <- function(x) {
  # Excel serial as numeric/character is the common LSEG case -- try first.
  # Use tryCatch on as.Date because charToDate raises ERRORS (not warnings)
  # for unparseable strings, and suppressWarnings does NOT catch errors.
  n <- suppressWarnings(as.numeric(x))
  if (!any(is.na(n))) {
    return(as.Date(n, origin = "1899-12-30"))
  }
  d <- tryCatch(
    suppressWarnings(as.Date(x)),
    error = function(e) NULL
  )
  if (!is.null(d) && !any(is.na(d))) return(d)
  stop("Could not parse date headers in row 1. ",
       "First 3 values: ", paste(head(x, 3), collapse = " | "))
}
dates     <- parse_dates(date_hdr)
dates_eom <- ceiling_date(dates, "month") - days(1)

# --- 2. Pivot long, then tidy wide ------------------------------------------
body <- raw[-1, ]
names(body) <- c("Ticker", as.character(dates_eom))

long <- body %>%
  pivot_longer(-Ticker, names_to = "date", values_to = "value") %>%
  mutate(date = as.Date(date), value = as.numeric(value))

panel_raw <- long %>%
  pivot_wider(id_cols = date, names_from = Ticker, values_from = value) %>%
  arrange(date)

cat("Months loaded:", nrow(panel_raw),
    " | Range:", format(min(panel_raw$date)), "->",
    format(max(panel_raw$date)), "\n")

# Coverage report
cat("\nSeries coverage:\n")
present <- intersect(EXPECTED_SERIES, names(panel_raw))
missing <- setdiff(EXPECTED_SERIES, names(panel_raw))
for (v in present) {
  ok <- !is.na(panel_raw[[v]])
  cat(sprintf("  %-15s n = %4d   %s -> %s\n",
              v, sum(ok),
              if (any(ok)) format(min(panel_raw$date[ok])) else "-",
              if (any(ok)) format(max(panel_raw$date[ok])) else "-"))
}
if (length(missing) > 0) {
  cat("  WARNING - expected series not found:",
      paste(missing, collapse = ", "), "\n")
}

# --- 3. Constructed series ---------------------------------------------------
panel <- panel_raw %>%
  mutate(
    yearmo   = year(date) * 100L + month(date),
    MD_RATIO = MARGIN_DEBT / TOTAL_MCAP,
    AAII_BB  = AAII_BULL - AAII_BEAR
  ) %>%
  mutate(DMD_YOY = MD_RATIO / lag(MD_RATIO, 12) - 1)

# --- 3a. NEW: detrended log MD_RATIO (Rapach-Ringgenberg-Zhou methodology) --
# Regress log(MD_RATIO) on a linear time trend; the residual MD_DETREND
# captures elevated leverage commitment net of secular growth in margin
# debt / market cap (hedge fund AUM expansion, equity-lending market
# expansion, etc., per DKP 2016).
md_idx <- !is.na(panel$MD_RATIO) & panel$MD_RATIO > 0
panel$MD_DETREND <- NA_real_
if (sum(md_idx) >= 24L) {
  log_md    <- log(panel$MD_RATIO[md_idx])
  t_seq     <- seq_along(log_md)
  trend_mod <- lm(log_md ~ t_seq)
  panel$MD_DETREND[md_idx] <- residuals(trend_mod)
  cat(sprintf(
    "\nMD_DETREND: trend slope = %+.6f per month  (R^2 = %.3f, n = %d)\n",
    coef(trend_mod)[2], summary(trend_mod)$r.squared, length(log_md)
  ))
} else {
  warning("MD_DETREND: insufficient observations (", sum(md_idx),
          "); column set to NA.")
}

# --- 4. Regime dummies (66th pctile, in-sample threshold) -------------------
make_dummy <- function(x, p = REGIME_PERCENTILE) {
  thr <- quantile(x, probs = p, na.rm = TRUE, names = FALSE)
  d   <- as.integer(x > thr)
  d[is.na(x)] <- NA_integer_
  d
}

panel <- panel %>%
  mutate(
    D_SENT       = make_dummy(SENT_ORTH),
    D_PLS        = make_dummy(PLS_SENT),
    D_VIX        = make_dummy(VIX),
    D_SKEW       = make_dummy(SKEW),
    D_PCR        = make_dummy(PUT_CALL_RATIO),
    D_MD         = make_dummy(DMD_YOY),     # GROWTH spec (unchanged)
    D_MD_LEVEL   = make_dummy(MD_RATIO),    # NEW: level spec
    D_MD_DETREND = make_dummy(MD_DETREND),  # NEW: detrended (DKP/RRZ)
    D_AAII       = make_dummy(AAII_BB),
    D_UMCSENT    = make_dummy(UMCSENT)
  )

# --- 5. Print thresholds -----------------------------------------------------
cat("\nRegime thresholds (66th pctile of in-sample distribution):\n")
thr_vars <- c("SENT_ORTH","PLS_SENT","VIX","SKEW","PUT_CALL_RATIO",
              "DMD_YOY","MD_RATIO","MD_DETREND","AAII_BB","UMCSENT")
thr_vars <- intersect(thr_vars, names(panel))
thr_tbl <- sapply(thr_vars, function(v)
  quantile(panel[[v]], REGIME_PERCENTILE, na.rm = TRUE, names = FALSE))
print(round(thr_tbl, 4))

# --- 6. Order columns and export --------------------------------------------
ordered <- c(
  "date","yearmo",
  "SENT_ORTH","PLS_SENT","VIX","SKEW","PUT_CALL_RATIO",
  "VOL_NYSE","VOL_AMEX","VOL_TOTAL",
  "MARGIN_DEBT","TOTAL_MCAP","MD_RATIO","DMD_YOY","MD_DETREND",
  "AAII_BULL","AAII_BEAR","AAII_BB","UMCSENT",
  "D_SENT","D_PLS","D_VIX","D_SKEW","D_PCR",
  "D_MD","D_MD_LEVEL","D_MD_DETREND",
  "D_AAII","D_UMCSENT"
)
panel <- panel[, intersect(ordered, names(panel))]

write_xlsx(panel, OUTPUT_FILE)
cat(sprintf("\nWrote %s  (%d rows, %d cols)\n",
            OUTPUT_FILE, nrow(panel), ncol(panel)))

# Expose for downstream pipeline scripts
behavioral_state_vars <- panel
assign("behavioral_state_vars", behavioral_state_vars, envir = .GlobalEnv)
