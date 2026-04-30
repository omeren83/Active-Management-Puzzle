# behavioral_state_variables.R                                            v1.0
# =============================================================================
# Constructs monthly behavioral state variables for H1-H4 panel regressions.
#
# INPUT:
#   fund_data.xlsx, sheet "Sentiment" (LSEG wide-by-row layout). Schema:
#     - Column 1 = "Ticker" (= series identifier).
#     - Columns 2..N = month-end date headers (Excel serials).
#     - Each row is one monthly time series. Series rows expected:
#         SENT_ORTH       Baker-Wurgler orthogonalised sentiment (1965-07 -> 2023-12)
#         PLS_SENT        Huang et al. (2015) PLS sentiment       (         -> 2023-12)
#         VIX             CBOE Volatility Index                   (1994-12 -> 2026-02)
#         SKEW            CBOE SKEW Index                         (2005-11 -> 2026-02)
#         PUT_CALL_RATIO  CBOE equity put-call ratio              (         -> 2019-10)
#         VOL_NYSE        NYSE share volume                       (         -> 2026-02)
#         VOL_AMEX        AMEX share volume                       (         -> 2026-02)
#         VOL_TOTAL       Total US equity volume                  (         -> 2026-02)
#         MARGIN_DEBT     FINRA margin debt balances              (         -> 2026-01)
#         TOTAL_MCAP      Total US equity market capitalisation   (         -> 2026-02)
#         AAII_BULL       AAII survey bullish percentage          (1994-12 -> 2026-02)
#         AAII_BEAR       AAII survey bearish percentage          (1994-12 -> 2026-02)
#         UMCSENT         U Michigan Consumer Sentiment Index     (         -> 2026-01)
#
# OUTPUT:
#   behavioral_state_vars.xlsx  in WORKING_DIR. One row per month, columns:
#     - date (Date, end-of-month) ; yearmo (numeric YYYYMM)
#     - All raw level series (above)
#     - Constructed series:
#         MD_RATIO  = MARGIN_DEBT / TOTAL_MCAP                     (Eq A.8.1)
#         DMD_YOY   = MD_RATIO_t / MD_RATIO_{t-12} - 1             (Eq A.8.2)
#         AAII_BB   = AAII_BULL - AAII_BEAR
#     - 0/1 regime dummies at the 66th-pctile threshold of the in-sample
#       distribution (Baker & Wurgler 2007; proposal Section 6.4):
#         D_SENT, D_PLS, D_VIX, D_SKEW, D_PCR, D_MD, D_AAII, D_UMCSENT
#     Sign convention: D_* = 1 when the raw variable is in its top tertile.
#     For VIX and PUT_CALL_RATIO this corresponds to a BEARISH regime
#     (opposite to D_SENT/D_PLS/D_AAII/D_UMCSENT). Regression code must flip
#     signs as appropriate.
#
# REFERENCES:
#   Baker M., Wurgler J. (2006). J Finance 61(4); (2007). JEP 21(2).
#   Huang D., Jiang F., Tu J., Zhou G. (2015). RFS 28(3).
#   Statman M., Thorley S., Vorkink K. (2006). RFS 19(4).
#   Proposal Section 6.4 (Behavioral State Variables); Appendix A.8.
#
# Dependencies: readxl, writexl, dplyr, tidyr, lubridate
# =============================================================================

suppressPackageStartupMessages({
  library(readxl); library(writexl)
  library(dplyr);  library(tidyr); library(lubridate)
})

# --- 0. Config ---------------------------------------------------------------
# WORKING_DIR is set by master_pipeline.R; running standalone uses CWD.
if (!exists("WORKING_DIR")) WORKING_DIR <- getwd()

INPUT_FILE        <- file.path(WORKING_DIR, "fund_data.xlsx")
INPUT_SHEET       <- "Sentiment"
OUTPUT_FILE       <- file.path(WORKING_DIR, "behavioral_state_vars.xlsx")
REGIME_PERCENTILE <- 0.66   # Baker & Wurgler 2007 convention

# Series expected in the input sheet (used only to print a coverage report;
# missing series are tolerated but flagged).
EXPECTED_SERIES <- c(
  "SENT_ORTH","PLS_SENT","VIX","SKEW","PUT_CALL_RATIO",
  "VOL_NYSE","VOL_AMEX","VOL_TOTAL","MARGIN_DEBT","TOTAL_MCAP",
  "AAII_BULL","AAII_BEAR","UMCSENT"
)

# --- 1. Load sheet -----------------------------------------------------------
cat("Loading", INPUT_FILE, "  sheet:", INPUT_SHEET, "\n")

# Resolve sheet name case-insensitively (Excel sheet names are case-sensitive
# in readxl, but Sentiment / sentiment / SENTIMENT are all the same to a human).
available_sheets <- excel_sheets(INPUT_FILE)
match_idx <- which(tolower(available_sheets) == tolower(INPUT_SHEET))
if (length(match_idx) == 0) {
  cat("\nAvailable sheets in", basename(INPUT_FILE), ":\n")
  for (s in available_sheets) cat("  -", s, "\n")
  stop("Sheet '", INPUT_SHEET, "' not found in ", INPUT_FILE,
       ". Add the sheet (or update INPUT_SHEET in the script).")
}
resolved_sheet <- available_sheets[match_idx[1]]
if (resolved_sheet != INPUT_SHEET) {
  cat("Note: requested '", INPUT_SHEET, "', using '", resolved_sheet,
      "' (case-insensitive match).\n", sep = "")
}

# col_names = TRUE: row 1 (Ticker + date headers) becomes column names;
# data rows hold each series. readxl serialises Excel-date headers into
# numeric strings (Excel serial numbers, origin 1899-12-30).
raw <- read_excel(
  INPUT_FILE, sheet = resolved_sheet, col_names = TRUE,
  na = c("", "NA", "#N/A N/A", "#N/A Field Not Applicable")
)

# Parse date column names: handle both Excel serials ("34698") and parsed
# date strings ("1994-12-30") robustly.
parse_one_date <- function(s) {
  # Try ISO date first; if that errors, try Excel serial number.
  d <- tryCatch(as.Date(s), error = function(e) NA, warning = function(w) NA)
  if (!is.na(d)) return(d)
  n <- suppressWarnings(as.numeric(s))
  if (!is.na(n)) return(as.Date(n, origin = "1899-12-30"))
  as.Date(NA)
}
date_hdr   <- names(raw)[-1]
dates_raw  <- as.Date(vapply(date_hdr, parse_one_date, FUN.VALUE = as.Date(NA)))
if (any(is.na(dates_raw))) {
  bad <- date_hdr[is.na(dates_raw)]
  stop("Failed to parse date headers: ", paste(head(bad, 5), collapse = ", "))
}
# Snap to true end-of-month (LSEG headers can be the last trading day).
dates_eom <- ceiling_date(dates_raw, "month") - days(1)

# Rename columns: keep "Ticker", replace date headers with EOM ISO strings.
names(raw) <- c("Ticker", as.character(dates_eom))

# --- 2. Pivot to long, then tidy wide ----------------------------------------
long <- raw %>%
  pivot_longer(-Ticker, names_to = "date", values_to = "value") %>%
  mutate(date = as.Date(date), value = as.numeric(value))

panel_raw <- long %>%
  pivot_wider(id_cols = date, names_from = Ticker, values_from = value) %>%
  arrange(date)

cat("Months loaded:", nrow(panel_raw),
    " | Range:", format(min(panel_raw$date)), "->",
    format(max(panel_raw$date)), "\n")

# Coverage report (and missing-series flag)
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

# --- 4. Regime dummies (66th pctile, fixed in-sample threshold) --------------
make_dummy <- function(x, p = REGIME_PERCENTILE) {
  thr <- quantile(x, probs = p, na.rm = TRUE, names = FALSE)
  d   <- as.integer(x > thr)
  d[is.na(x)] <- NA_integer_
  d
}

panel <- panel %>%
  mutate(
    D_SENT    = make_dummy(SENT_ORTH),
    D_PLS     = make_dummy(PLS_SENT),
    D_VIX     = make_dummy(VIX),       # high VIX     => bearish regime
    D_SKEW    = make_dummy(SKEW),
    D_PCR     = make_dummy(PUT_CALL_RATIO),  # high PCR => bearish regime
    D_MD      = make_dummy(DMD_YOY),
    D_AAII    = make_dummy(AAII_BB),
    D_UMCSENT = make_dummy(UMCSENT)
  )

# Print thresholds (for documentation in dissertation methodology).
cat("\nRegime thresholds (66th pctile of in-sample distribution):\n")
thr_vars <- c("SENT_ORTH","PLS_SENT","VIX","SKEW","PUT_CALL_RATIO",
              "DMD_YOY","AAII_BB","UMCSENT")
thr_vars <- intersect(thr_vars, names(panel))
thr_tbl <- sapply(thr_vars, function(v)
  quantile(panel[[v]], REGIME_PERCENTILE, na.rm = TRUE, names = FALSE))
print(round(thr_tbl, 4))

# --- 5. Order columns and export ---------------------------------------------
ordered <- c(
  "date","yearmo",
  "SENT_ORTH","PLS_SENT","VIX","SKEW","PUT_CALL_RATIO",
  "VOL_NYSE","VOL_AMEX","VOL_TOTAL",
  "MARGIN_DEBT","TOTAL_MCAP","MD_RATIO","DMD_YOY",
  "AAII_BULL","AAII_BEAR","AAII_BB","UMCSENT",
  "D_SENT","D_PLS","D_VIX","D_SKEW","D_PCR",
  "D_MD","D_AAII","D_UMCSENT"
)
panel <- panel[, intersect(ordered, names(panel))]

write_xlsx(panel, OUTPUT_FILE)
cat(sprintf("\nWrote %s  (%d rows, %d cols)\n",
            OUTPUT_FILE, nrow(panel), ncol(panel)))

# Make available as session object for downstream pipeline scripts.
behavioral_state_vars <- panel
