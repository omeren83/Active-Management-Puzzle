# =============================================================================
# FUND DATA IMPORT & PANEL CONSTRUCTION                                    v1.1
#
# v1.1 changes:
#   - Step 8b added: excludes leveraged / derivative-based mutual funds from
#     the Passive universe. Rydex/ProFunds/Direxion daily-reset products are
#     classified as passive (Actively_Managed_New = N) by LSEG but use equity
#     swaps or futures to deliver a constant daily leverage multiple.
#     Diagnostic: 30 passive funds with Turnover > 200% identified; confirmed
#     as Rydex 2x, ProFunds UltraSector, and Direxion Bull/Bear products.
#     Rydex Pure Style funds (Pure Value / Pure Growth, long-only 1x) are
#     explicitly retained despite high reconstitution turnover reported by LSEG.
#     Two additional funds (MOJAX, GENDX) are excluded as active strategies
#     misclassified as passive by LSEG.
#
# Produces three panels:
#   panel_master      ??? no corrections, no date trimming (audit baseline)
#   panel_incubation  ??? Evans (2010) 36-month age filter applied
#   panel_trimmed     ??? Evans filter + 1995-2023 sample period trim
#
# Cleaning applied to all panels:
#   (1) Frozen tail removal ??? drops LSEG forward-filled post-closure obs
#   (2) Empty fund exclusion ??? drops funds with zero valid return obs
#   (3) Confirmed data error exclusion (QWVOX, VALLCEN)
#   (4) Evans (2010) incubation bias correction
#   (5) Winsorisation of monthly returns at 1st / 99th percentile
#   (6) Leveraged / derivative-based passive fund exclusion [NEW v1.1]
#
# Dependencies: readxl, dplyr, tidyr, lubridate
# Evans (2010): Journal of Finance, Vol. LXV, No. 4
# =============================================================================

library(lubridate)
library(readxl)
library(dplyr)
library(tidyr)

FILE          <- "fund_data.xlsx"
DATE_MIN_DATA <- as.Date("1994-12-01")  # include Dec 1994 for lag computation
DATE_MIN      <- as.Date("1995-01-01")  # actual sample start for analysis
DATE_MAX      <- as.Date("2023-12-31")  # sample end
EVANS_MONTHS  <- 36   # Evans (2010): <5% of funds incubated longer than 36 months

# =============================================================================
# HELPER 1: parse date column headers (Excel serial, ISO, Mon-YYYY)
# =============================================================================
parse_col_dates <- function(x) {
  parsed <- suppressWarnings(as.Date(x, format = "%Y-%m-%d"))
  if (!all(is.na(parsed))) return(parsed)
  
  nums <- suppressWarnings(as.numeric(x))
  if (!all(is.na(nums))) return(as.Date(nums, origin = "1899-12-30"))
  
  parsed <- suppressWarnings(as.Date(paste0("01 ", x), format = "%d %b %Y"))
  if (!all(is.na(parsed))) return(parsed)
  
  stop("Could not parse date column headers. Check format in Excel.")
}

# =============================================================================
# HELPER 2: parse a single Inception_Date value
#   Handles Excel serial numbers and LSEG error strings (#N/A N/A etc.)
# =============================================================================
parse_inception_date <- function(x) {
  x[grepl("^#|^\\s*$", x)] <- NA
  nums <- suppressWarnings(as.numeric(x))
  as.Date(ifelse(!is.na(nums), nums, NA), origin = "1899-12-30")
}

# =============================================================================
# HELPER 3: winsorise a numeric vector at given quantile bounds
# =============================================================================
winsorise <- function(x, low = 0.01, high = 0.99) {
  q <- quantile(x, probs = c(low, high), na.rm = TRUE)
  pmax(pmin(x, q[2]), q[1])
}

# =============================================================================
# CONFIRMED DATA ERRORS ??? excluded from all panels
#   QWVOX US Equity  : total return index reaches 113,446 (base = 1 in Dec
#     1994), implying >11,000,000% cumulative return. Impossible for a US
#     domestic equity fund; confirmed LSEG index scaling error.
#   VALLCEN US Equity: single-month return of -75.9% in April 2022 (S&P 500
#     fell -8.7% that month). Fund had $152M AUM; no economic justification.
#     Confirmed LSEG data feed error.
# =============================================================================
DATA_ERROR_TICKERS <- c("QWVOX US Equity", "VALLCEN US Equity")

# =============================================================================
# 1. STATIC DATA
# =============================================================================
static <- read_excel(FILE, sheet = "static") %>%
  mutate(Inception_Date = parse_inception_date(Inception_Date))

cat("Static loaded:", nrow(static), "funds |",
    sum(!is.na(static$Inception_Date)), "with valid Inception_Date\n")

# =============================================================================
# 2. FUND-LEVEL TIME SERIES ??? pivot wide ??? long
# =============================================================================
ts_sheets <- c("gross_return", "net_return", "track_diff",
               "net_assets",   "class_assets", "total_assets",
               "num_of_shares", "fund_flow")

read_fund_ts <- function(sheet) {
  df <- read_excel(FILE, sheet = sheet)
  date_cols <- setdiff(names(df), "Ticker")
  
  df %>%
    pivot_longer(cols = all_of(date_cols), names_to = "date", values_to = sheet) %>%
    mutate(
      date    = parse_col_dates(date),
      !!sheet := as.numeric(.data[[sheet]])
    )
}

fund_ts_list <- lapply(ts_sheets, read_fund_ts)
fund_panel   <- Reduce(function(a, b) full_join(a, b, by = c("Ticker", "date")), fund_ts_list)

cat("Raw panel:", nrow(fund_panel), "rows |", n_distinct(fund_panel$Ticker), "funds\n")

# =============================================================================
# 3. MACRO SHEETS ??? pivot wide ??? long ??? wide
# =============================================================================
read_macro_ts <- function(sheet) {
  df <- read_excel(FILE, sheet = sheet, col_types = "text")
  label_col <- names(df)[1]
  date_cols  <- names(df)[-1]
  
  df %>%
    pivot_longer(cols = all_of(date_cols), names_to = "date", values_to = "value") %>%
    mutate(date = parse_col_dates(date), value = as.numeric(value)) %>%
    pivot_wider(names_from = all_of(label_col), values_from = "value")
}

sentiment_df     <- read_macro_ts("sentiment")
bench_returns_df <- read_macro_ts("bench_returns")
factors_df       <- read_macro_ts("factors")

macro_panel <- sentiment_df %>%
  full_join(bench_returns_df, by = "date") %>%
  full_join(factors_df,       by = "date")

cat("Macro panel:", nrow(macro_panel), "months |",
    ncol(macro_panel) - 1, "variables\n")

# =============================================================================
# 4. FROZEN TAIL REMOVAL (gross_return as master signal)
#    Drops terminal blocks of repeated values ??? LSEG forward-fills closed
#    funds. Only removes repeats AFTER the last genuine price movement,
#    so legitimate mid-life identical consecutive returns are preserved.
#
#    Threshold: requires >= 3 consecutive identical index values at the tail
#    (i.e. >= 2 is_frozen=TRUE obs in the terminal block) before classifying
#    a fund as dead. Once threshold is met, deletion starts at the FIRST
#    repeated value.
#
#    Known limitation: funds dying within 2 months of Dec 2023 may retain
#    1-2 carried obs (LSEG carry truncated before reaching the threshold).
# =============================================================================
gross_clean <- fund_panel %>%
  select(Ticker, date, gross_return) %>%
  filter(!is.na(gross_return)) %>%
  group_by(Ticker) %>%
  arrange(date) %>%
  mutate(is_frozen = (gross_return == lag(gross_return))) %>%
  mutate(
    last_move_date    = max(date[is_frozen == FALSE | is.na(is_frozen)], na.rm = TRUE),
    terminal_frozen_n = sum(is_frozen == TRUE & date > last_move_date, na.rm = TRUE)
  ) %>%
  filter(
    !(is_frozen == TRUE &
        date > last_move_date &
        terminal_frozen_n >= 2)
  ) %>%
  select(-is_frozen, -last_move_date, -terminal_frozen_n) %>%
  ungroup()

# Data-driven effective closure dates ??? byproduct of frozen tail removal
closure_dates <- gross_clean %>%
  group_by(Ticker) %>%
  summarise(effective_closure = max(date), .groups = "drop")

# Empty fund exclusion
valid_tickers <- gross_clean %>%
  group_by(Ticker) %>%
  summarise(n_obs = n(), .groups = "drop") %>%
  filter(n_obs > 0) %>%
  pull(Ticker)

cat("After frozen tail removal:", length(valid_tickers), "funds |",
    nrow(gross_clean), "obs\n")
cat("Empty funds removed:", n_distinct(fund_panel$Ticker) - length(valid_tickers), "\n")

# Sync all series to the cleaned gross_return timeline
fund_panel_clean <- fund_panel %>%
  semi_join(gross_clean, by = c("Ticker", "date")) %>%
  filter(Ticker %in% valid_tickers) %>%
  filter(!Ticker %in% DATA_ERROR_TICKERS)

cat("Data error funds removed:", length(DATA_ERROR_TICKERS),
    paste0("(", paste(DATA_ERROR_TICKERS, collapse = ", "), ")"), "\n")

# =============================================================================
# 5. ASSEMBLE BASE PANEL (cleaned, no date trim, no Evans filter)
# =============================================================================
base_panel <- fund_panel_clean %>%
  left_join(static,      by = "Ticker") %>%
  left_join(macro_panel, by = "date") %>%
  arrange(Ticker, date)

# =============================================================================
# 6. EVANS (2010) INCUBATION FILTER
#    Remove first 36 months of each fund's return history.
#    Cutoff = Inception_Date + 36m; first observed date used as fallback
#    when Inception_Date is missing.
#    Reference: Evans (2010), JF Vol. LXV No. 4, p.1581
# =============================================================================

first_obs <- base_panel %>%
  group_by(Ticker) %>%
  summarise(first_obs_date = min(date), .groups = "drop")

evans_cutoff <- static %>%
  select(Ticker, Inception_Date) %>%
  left_join(first_obs, by = "Ticker") %>%
  mutate(
    ref_date     = if_else(!is.na(Inception_Date), Inception_Date, first_obs_date),
    evans_cutoff = ref_date %m+% months(EVANS_MONTHS)
  ) %>%
  select(Ticker, evans_cutoff)

# =============================================================================
# 7. PRODUCE THREE PANELS (pre-classification)
# =============================================================================

# Panel 1: Master ??? no incubation correction, no date trimming
panel_master <- base_panel

# Panel 2: Incubation-corrected ??? Evans 36-month filter, no date trim
panel_incubation <- base_panel %>%
  left_join(evans_cutoff, by = "Ticker") %>%
  filter(date >= evans_cutoff) %>%
  select(-evans_cutoff)

# Panel 3: Trimmed ??? Evans filter + 1995-2023 sample period
panel_trimmed <- panel_incubation %>%
  filter(date >= DATE_MIN_DATA & date <= DATE_MAX)

# =============================================================================
# 8. ACTIVE/PASSIVE CLASSIFICATION
#    Y = Active, N = Passive, everything else = Unknown
# =============================================================================
classify_ap <- function(panel) {
  panel %>%
    mutate(
      ap_group = case_when(
        Actively_Managed_New == "Y" ~ "Active",
        Actively_Managed_New == "N" ~ "Passive",
        TRUE                        ~ "Unknown"
      )
    )
}

panel_master     <- classify_ap(panel_master)
panel_incubation <- classify_ap(panel_incubation)
panel_trimmed    <- classify_ap(panel_trimmed)

cat("\nActive/passive classification applied to all panels.\n")
cat("panel_trimmed distribution:\n")
print(table(panel_trimmed$ap_group, useNA = "always"))

# =============================================================================
# 8b. EXCLUDE LEVERAGED / DERIVATIVE-BASED PRODUCTS FROM PASSIVE UNIVERSE
#
#     Background: daily-reset leveraged mutual funds (Rydex, ProFunds,
#     Direxion) are classified as passive by LSEG (Actively_Managed_New = N)
#     because they mechanically track an index. However, they use equity swaps
#     or futures to deliver a constant daily leverage multiple, resulting in
#     annual turnover of 200-4000% and severe volatility decay over multi-year
#     holding periods (Avellaneda & Zhang, 2010). Their inclusion in the
#     passive benchmark contaminated fee-quintile and flow-performance analyses.
#
#     Diagnostic: 30 passive funds with Turnover > 200% were identified; all
#     confirmed as leveraged/derivative products on manual review.
#
#     Exclusion criteria (Passive group only):
#       (a) Keyword pattern in fund Name column ??? catches explicit leverage
#           indicators and all ProFunds share classes (the entire ProFunds
#           lineup in this universe is leveraged; "profund" is unambiguous)
#       (b) Hard-coded misclassifications ??? MOJAX (tactical active fund) and
#           GENDX (Gotham long/short quant) classified as Passive in LSEG
#
#     Explicit retention (overrides keyword match):
#       Rydex Pure Style funds (Pure Value / Pure Growth) are long-only 1x
#       products tracking S&P Pure Style indices. Their high reported turnover
#       reflects index reconstitution, not derivative rolling. Retained.
#
#     References: Avellaneda & Zhang (2010, SIAM J. Financial Math.);
#     Cremers & Petajisto (2009, RFS); Amihud & Goyenko (2013, RFS).
# =============================================================================

LEVERAGED_KEYWORDS <- paste(
  c("2x",
    "3x",
    "1\\.5x",
    "ultra",       # ProFunds UltraSector, Rydex Ultra
    "inverse",
    "bear market",
    "bull 1",      # Direxion "BULL 125", "BULL 150X" etc.
    "profund"),    # all ProFunds in this universe are leveraged
  collapse = "|"
)

# Confirmed active strategies misclassified as passive by LSEG
ACTIVE_MISLABELLED <- c("MOJAX US Equity", "GENDX US Equity")

# Long-only 1x Rydex Pure Style funds ??? exempt from keyword filter
PURE_STYLE_RETAIN <- c(
  "RYAVX US Equity",  # Rydex S&P Mid-Cap 400 Pure Value
  "RYZAX US Equity",  # Rydex S&P 500 Pure Value
  "RYAZX US Equity",  # Rydex S&P Small-Cap 600 Pure Value
  "RYWAX US Equity",  # Rydex S&P Small-Cap 600 Growth
  "RYAWX US Equity"   # Rydex S&P 500 Pure Growth
)

exclude_leveraged <- function(panel) {
  panel %>%
    filter(
      !(ap_group == "Passive" &
          !Ticker %in% PURE_STYLE_RETAIN &
          (grepl(LEVERAGED_KEYWORDS, Name, ignore.case = TRUE) |
             Ticker %in% ACTIVE_MISLABELLED))
    )
}

n_pass_before <- n_distinct(panel_trimmed$Ticker[panel_trimmed$ap_group == "Passive"])

panel_master     <- exclude_leveraged(panel_master)
panel_incubation <- exclude_leveraged(panel_incubation)
panel_trimmed    <- exclude_leveraged(panel_trimmed)

n_pass_after <- n_distinct(panel_trimmed$Ticker[panel_trimmed$ap_group == "Passive"])
cat("\nLeveraged/derivative filter:\n")
cat("  Passive funds before:", n_pass_before, "\n")
cat("  Passive funds after :", n_pass_after, "\n")
cat("  Removed            :", n_pass_before - n_pass_after, "\n")

# =============================================================================
# 9. MONTHLY PERCENTAGE RETURNS FROM INDEX LEVELS
#    gross_return is the LSEG total return index level (not a percentage).
#    ret_gross_raw: unwinsorised gross decimal return (index_t/index_{t-1} - 1).
#    ret_gross: winsorised gross return (Step 10 below).
#
#    NET RETURN APPROXIMATION:
#    LSEG serves identical total return index series for both the gross and net
#    return fields. True net returns are therefore unavailable from the index.
#    Following Barras, Scaillet and Wermers (2010, JF), net returns are
#    approximated by deducting one-twelfth of the static annual expense ratio
#    from each monthly gross return:
#        ret_net_raw = ret_gross_raw - Expense_Ratio / 1200
#    Expense_Ratio is in percentage terms (e.g. 1.0 = 1%), so dividing by 1200
#    converts to a monthly decimal deduction. Where Expense_Ratio is missing or
#    unparseable, ret_net_raw is NA ??? no imputation is applied.
#
#    IMPORTANT: ret_gross_raw and ret_net_raw (unwinsorised) are preserved on
#    the panel for use in the Sirri-Tufano flow identity in flow_calculation.R.
#    Winsorisation is applied to the OUTPUT only (Step 10).
#
#    First observation per fund is NA by construction (no lagged index).
# =============================================================================
compute_returns <- function(panel) {
  panel %>%
    group_by(Ticker) %>%
    arrange(date) %>%
    mutate(
      ret_gross_raw = gross_return / lag(gross_return) - 1,
      fee_monthly   = suppressWarnings(as.numeric(Expense_Ratio) / 1200),
      ret_net_raw   = ret_gross_raw - fee_monthly,
      ret_gross     = ret_gross_raw,
      ret_net       = ret_net_raw
    ) %>%
    select(-fee_monthly) %>%
    ungroup()
}

panel_master     <- compute_returns(panel_master)
panel_incubation <- compute_returns(panel_incubation)
panel_trimmed    <- compute_returns(panel_trimmed)

cat("\nMonthly returns computed for all panels.\n")
cat("panel_trimmed ret_gross_raw summary:\n")
print(summary(panel_trimmed$ret_gross_raw))
cat("panel_trimmed ret_net_raw summary (expense-adjusted approximation):\n")
print(summary(panel_trimmed$ret_net_raw))

# =============================================================================
# 10. WINSORISE MONTHLY RETURNS
#     ret_gross and ret_net winsorised at 1st/99th percentile.
#     ret_gross_raw and ret_net_raw remain untouched for the flow identity.
#     Standard practice: Carhart (1997), Fama & French (2010).
# =============================================================================
winsorise_returns <- function(panel) {
  panel %>%
    mutate(
      ret_gross = winsorise(ret_gross_raw),
      ret_net   = winsorise(ret_net_raw)
    )
}

panel_master     <- winsorise_returns(panel_master)
panel_incubation <- winsorise_returns(panel_incubation)
panel_trimmed    <- winsorise_returns(panel_trimmed)

cat("\nReturns winsorised at 1st/99th percentile for all panels.\n")
cat("panel_trimmed ret_gross summary post-winsorisation:\n")
print(summary(panel_trimmed$ret_gross))
cat("Raw (unwinsorised) ret_gross_raw preserved for flow formula:\n")
print(summary(panel_trimmed$ret_gross_raw))

# =============================================================================
# 11. SUMMARY
# =============================================================================
summarise_panel <- function(panel, label) {
  cat("\n---", label, "---\n")
  cat("Dimensions   :", nrow(panel), "rows x", ncol(panel), "columns\n")
  cat("Date range   :", format(min(panel$date)), "to", format(max(panel$date)), "\n")
  cat("Unique funds :", n_distinct(panel$Ticker), "\n")
  cat("  Active     :", n_distinct(panel$Ticker[panel$ap_group == "Active"]),   "\n")
  cat("  Passive    :", n_distinct(panel$Ticker[panel$ap_group == "Passive"]),  "\n")
  cat("  Unknown    :", n_distinct(panel$Ticker[panel$ap_group == "Unknown"]),  "\n")
  cat("Unique dates :", n_distinct(panel$date), "\n")
  obs_dist <- panel %>%
    group_by(Ticker) %>%
    summarise(n = n(), .groups = "drop") %>%
    pull(n)
  cat("Obs per fund : min =", min(obs_dist),
      "| median =", median(obs_dist),
      "| max =", max(obs_dist), "\n")
}

summarise_panel(panel_master,     "PANEL MASTER (no corrections)")
summarise_panel(panel_incubation, "PANEL INCUBATION (Evans 36m filter)")
summarise_panel(panel_trimmed,    "PANEL TRIMMED (Evans + 1995-2023)")