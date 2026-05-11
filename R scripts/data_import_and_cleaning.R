# =============================================================================
# FUND DATA IMPORT & PANEL CONSTRUCTION                                    v1.2
#
# v1.2 changes:
#   - Step 8c added: applies the flagged_funds.xlsx exclusion ledger.
#       (a) "Exclude from Entire Analysis" tickers are dropped from all three
#           panels at source. This subsumes the prior DATA_ERROR_TICKERS
#           hardcode and adds PASSIVE_INDEX, GLOBAL_MANDATE, EM_MANDATE,
#           LONG_SHORT, BEAR_MARKET, MARKET_NEUTRAL exclusions.
#       (b) Two boolean flag columns are added to surviving observations:
#             excluded_perf  TRUE for funds in the "Exclude from Perf
#                            Comparison" sheet (used by aggregate alphas,
#                            bootstrap, FDR, persistence, sub-period,
#                            robust factor models, portfolio sorts)
#             excluded_h3    TRUE for funds in the "Exclude from H3 Only"
#                            sheet OR with the SECTOR_FUND flag in the
#                            performance-comparison sheet (used by H3,
#                            activeness analyses)
#       The DATA_ERROR_TICKERS hardcode is removed; the same exclusion is
#       now driven entirely by flagged_funds.xlsx (DATA_ERROR flag).
#   - parse_inception_date: now accepts ISO-string dates as well as Excel
#     serial numbers, mirroring parse_col_dates. Prevents silent NA-coercion
#     of valid dates after a locale or Excel reformat (Issue A.3).
#   - Net-return approximation comment rewritten: BSW (2010) observe net
#     directly from CRSP and DERIVE gross by adding back ER/12. The LSEG
#     pipeline observes gross only and DERIVES net by subtracting ER/12.
#     The arithmetic wedge is identical; the direction is reversed. The
#     convention itself goes back to Carhart (1997) and Wermers (2000)
#     and is described correctly in those terms (Issue A.2).
#
# v1.1 changes (retained):
#   - Step 8b: leveraged / derivative-based passive fund exclusion.
#     Mostly redundant given Step 8c (PASSIVE_INDEX in flagged_funds.xlsx
#     covers the universe), but retained as a defensive net for any
#     inverse / 2x / 3x products that might be missed from the workbook.
#     Pure Style Rydex retention list also retained but is now of no effect
#     because RYAVX, RYZAX, RYAZX, RYWAX, RYAWX are flagged as PASSIVE_INDEX
#     in flagged_funds.xlsx and thereby excluded by Step 8c.
#
# Produces three panels:
#   panel_master      - no incubation correction, no date trimming
#   panel_incubation  - Evans (2010) 36-month age filter applied
#   panel_trimmed     - Evans filter + 1995-2023 sample period trim
#
# Each panel carries the new boolean columns excluded_perf and excluded_h3
# so downstream scripts can apply the appropriate scope-specific filter:
#     panel_*  %>% filter(!excluded_perf)   # for performance/alpha tables
#     panel_*  %>% filter(!excluded_h3)     # for H3 / activeness analyses
# Behavioral H1/H2/H4 panel regressions need no further filter; the
# Entire-Analysis exclusion is already applied at source.
#
# Cleaning applied to all panels:
#   (1) Frozen tail removal - drops LSEG forward-filled post-closure obs
#   (2) Empty fund exclusion - drops funds with zero valid return obs
#   (3) Evans (2010) incubation bias correction
#   (4) Winsorisation of monthly returns at 1st / 99th percentile
#   (5) Leveraged / derivative-based passive fund exclusion (defensive)
#   (6) flagged_funds.xlsx Entire-Analysis exclusion + flag columns [NEW v1.2]
#
# Dependencies: readxl, dplyr, tidyr, lubridate
# Evans (2010): Journal of Finance, Vol. LXV, No. 4
# =============================================================================

library(lubridate)
library(readxl)
library(dplyr)
library(tidyr)

FILE          <- "fund_data.xlsx"
FLAGGED_FILE  <- "flagged_funds.xlsx"   # exclusion ledger (8c)
DATE_MIN_DATA <- as.Date("1994-12-01")  # include Dec 1994 for lag computation
DATE_MIN      <- as.Date("1995-01-01")  # actual sample start for analysis
DATE_MAX      <- as.Date("2023-12-31")  # sample end (panel_trimmed)
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
#   Handles Excel serial numbers, ISO strings, and LSEG error markers.
#   Falls back gracefully if Excel reformats inception dates as text.
# =============================================================================
parse_inception_date <- function(x) {
  x[grepl("^#|^\\s*$", x)] <- NA
  # Try ISO first - preserves real dates if Excel writes them as text
  parsed <- suppressWarnings(as.Date(as.character(x), format = "%Y-%m-%d"))
  if (any(!is.na(parsed))) return(parsed)
  # Fall back to Excel serial number
  nums <- suppressWarnings(as.numeric(x))
  as.Date(nums, origin = "1899-12-30")
}

# =============================================================================
# HELPER 3: winsorise a numeric vector at given quantile bounds
# =============================================================================
winsorise <- function(x, low = 0.01, high = 0.99) {
  q <- quantile(x, probs = c(low, high), na.rm = TRUE)
  pmax(pmin(x, q[2]), q[1])
}

# =============================================================================
# 1. STATIC DATA
# =============================================================================
static <- read_excel(FILE, sheet = "static") %>%
  mutate(Inception_Date = parse_inception_date(Inception_Date))

cat("Static loaded:", nrow(static), "funds |",
    sum(!is.na(static$Inception_Date)), "with valid Inception_Date\n")

# =============================================================================
# 2. FUND-LEVEL TIME SERIES - pivot wide -> long
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
# 3. MACRO SHEETS - pivot wide -> long -> wide
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
#    Drops terminal blocks of repeated values - LSEG forward-fills closed
#    funds. Only removes repeats AFTER the last genuine price movement,
#    so legitimate mid-life identical consecutive returns are preserved.
#
#    Threshold: requires >= 3 consecutive identical index values at the tail
#    (i.e. >= 2 is_frozen=TRUE obs in the terminal block) before classifying
#    a fund as dead. Once threshold is met, deletion starts at the FIRST
#    repeated value.
#
#    Known limitation: funds dying within 2 months of the data pull date
#    may retain 1-2 carried obs (LSEG carry truncated before reaching the
#    threshold).
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

# Data-driven effective closure dates - byproduct of frozen tail removal
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
  filter(Ticker %in% valid_tickers)

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

# Panel 1: Master - no incubation correction, no date trimming
panel_master <- base_panel

# Panel 2: Incubation-corrected - Evans 36-month filter, no date trim
panel_incubation <- base_panel %>%
  left_join(evans_cutoff, by = "Ticker") %>%
  filter(date >= evans_cutoff) %>%
  select(-evans_cutoff)

# Panel 3: Trimmed - Evans filter + 1995-2023 sample period
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
cat("panel_trimmed distribution (pre-Step-8b/8c):\n")
print(table(panel_trimmed$ap_group, useNA = "always"))

# =============================================================================
# 8b. EXCLUDE LEVERAGED / DERIVATIVE-BASED PRODUCTS FROM PASSIVE UNIVERSE
#     [Largely redundant after v1.2 Step 8c but retained as defensive net]
#
#     Background: daily-reset leveraged mutual funds (Rydex, ProFunds,
#     Direxion) are classified as passive by LSEG (Actively_Managed_New = N)
#     because they mechanically track an index. However, they use equity swaps
#     or futures to deliver a constant daily leverage multiple, resulting in
#     annual turnover of 200-4000% and severe volatility decay over multi-year
#     holding periods (Avellaneda & Zhang, 2010).
#
#     Diagnostic: 30 passive funds with Turnover > 200% were identified; all
#     confirmed as leveraged/derivative products on manual review. Most appear
#     in flagged_funds.xlsx with the BEAR_MARKET or PASSIVE_INDEX flag and
#     are therefore caught by Step 8c. This step remains as a name-pattern
#     defensive layer for any leveraged products not yet in the workbook.
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

# Note: ACTIVE_MISLABELLED and PURE_STYLE_RETAIN constants removed in v1.2.
# Both are now superseded by flagged_funds.xlsx in Step 8c:
#   - MOJAX, GENDX are flagged PASSIVE_INDEX in Entire Analysis (excluded)
#   - RYAVX/RYZAX/RYAZX/RYWAX/RYAWX (Pure Style) are also flagged PASSIVE_INDEX
#     in flagged_funds.xlsx and therefore excluded. The user-curated workbook
#     supersedes the previous explicit-retention logic.

exclude_leveraged <- function(panel) {
  panel %>%
    filter(
      !(ap_group == "Passive" &
          grepl(LEVERAGED_KEYWORDS, Name, ignore.case = TRUE))
    )
}

n_pass_before <- n_distinct(panel_trimmed$Ticker[panel_trimmed$ap_group == "Passive"])

panel_master     <- exclude_leveraged(panel_master)
panel_incubation <- exclude_leveraged(panel_incubation)
panel_trimmed    <- exclude_leveraged(panel_trimmed)

n_pass_after <- n_distinct(panel_trimmed$Ticker[panel_trimmed$ap_group == "Passive"])
cat("\nLeveraged/derivative filter (Step 8b - defensive):\n")
cat("  Passive funds before:", n_pass_before, "\n")
cat("  Passive funds after :", n_pass_after, "\n")
cat("  Removed             :", n_pass_before - n_pass_after, "\n")

# =============================================================================
# 8c. APPLY flagged_funds.xlsx EXCLUSION LEDGER                       [NEW v1.2]
#
#     flagged_funds.xlsx is the canonical, user-curated exclusion workbook.
#     It encodes the dissertation's three-tier scope discipline:
#
#       (i)  Exclude from Entire Analysis  (438 funds): dropped at source.
#            Includes PASSIVE_INDEX (passives), GLOBAL_MANDATE / EM_MANDATE
#            (non-US), LONG_SHORT, MARKET_NEUTRAL, BEAR_MARKET (violate the
#            long-only assumption), and DATA_ERROR (the two confirmed LSEG
#            errors QWVOX, VALLCEN).
#
#       (ii) Exclude from Perf Comparison  (585 funds): NOT dropped here.
#            Tagged on the surviving panel with excluded_perf = TRUE so
#            that aggregate-alpha, bootstrap, FDR, persistence, sub-period,
#            robust factor model, and portfolio-sort scripts can apply
#            filter(!excluded_perf). This is the FF (2010)-style restriction
#            to diversified long-only funds with style-matched benchmarks
#            for performance comparison.
#
#       (iii) Exclude from H3 Only  (245 funds): tagged with excluded_h3 = TRUE.
#            For H3 (lottery demand) and the activeness proxy, where Cremers
#            and Petajisto (2009) require unambiguous size/style benchmarks.
#            The composite excluded_h3 column is the union of "Exclude from
#            H3 Only" and SECTOR_FUND-tagged funds in the Perf Comparison
#            sheet (sector funds are excluded from both performance and H3).
#
#     Behavioral H1/H2/H4 panel regressions need no further filter beyond
#     what is applied at source by (i): they run on the full surviving
#     universe (~3,326 funds pre-Evans).
#
#     This block subsumes the prior DATA_ERROR_TICKERS hardcode.
# =============================================================================

# Read all three flag sheets
flagged_entire <- read_excel(FLAGGED_FILE, sheet = "Exclude from Entire Analysis")
flagged_perf   <- read_excel(FLAGGED_FILE, sheet = "Exclude from Perf Comparison")
flagged_h3     <- read_excel(FLAGGED_FILE, sheet = "Exclude from H3 Only")

# Defensive: workbook ticker column may be named "Bloomberg Ticker" or "Ticker"
get_tickers <- function(df) {
  col <- intersect(c("Bloomberg Ticker", "Ticker"), names(df))[1]
  if (is.na(col)) stop("flagged_funds.xlsx: no Ticker / Bloomberg Ticker column.")
  unique(df[[col]])
}

tickers_entire <- get_tickers(flagged_entire)
tickers_perf   <- get_tickers(flagged_perf)
tickers_h3     <- get_tickers(flagged_h3)

# SECTOR_FUND in Perf Comparison contributes to excluded_h3 too (per dissertation)
sector_in_perf <- if ("Flag(s)" %in% names(flagged_perf)) {
  pf_col <- if ("Bloomberg Ticker" %in% names(flagged_perf)) "Bloomberg Ticker" else "Ticker"
  flagged_perf[[pf_col]][grepl("SECTOR_FUND", flagged_perf[["Flag(s)"]], fixed = TRUE)]
} else character(0)

tickers_h3_full <- unique(c(tickers_h3, sector_in_perf))

# Apply at source to all three panels
n_before <- list(
  master     = n_distinct(panel_master$Ticker),
  incubation = n_distinct(panel_incubation$Ticker),
  trimmed    = n_distinct(panel_trimmed$Ticker)
)

# Snapshot panels BEFORE Entire-Analysis drop, for descriptive
# Table 4.1 only (universe composition). All downstream analyses
# continue to use the post-8c panels.
panel_master_pre8c     <- panel_master
panel_incubation_pre8c <- panel_incubation
panel_trimmed_pre8c    <- panel_trimmed

panel_master     <- panel_master     %>% filter(!Ticker %in% tickers_entire)
panel_incubation <- panel_incubation %>% filter(!Ticker %in% tickers_entire)
panel_trimmed    <- panel_trimmed    %>% filter(!Ticker %in% tickers_entire)

panel_master     <- panel_master     %>% filter(!Ticker %in% tickers_entire)
panel_incubation <- panel_incubation %>% filter(!Ticker %in% tickers_entire)
panel_trimmed    <- panel_trimmed    %>% filter(!Ticker %in% tickers_entire)

# Tag remaining funds with the two flag columns
add_flag_cols <- function(panel) {
  panel %>%
    mutate(
      excluded_perf = Ticker %in% tickers_perf,
      excluded_h3   = Ticker %in% tickers_h3_full
    )
}

panel_master     <- add_flag_cols(panel_master)
panel_incubation <- add_flag_cols(panel_incubation)
panel_trimmed    <- add_flag_cols(panel_trimmed)

n_after <- list(
  master     = n_distinct(panel_master$Ticker),
  incubation = n_distinct(panel_incubation$Ticker),
  trimmed    = n_distinct(panel_trimmed$Ticker)
)

cat("\n--- Step 8c: flagged_funds.xlsx applied ---\n")
cat(sprintf("  Entire Analysis tickers: %d\n", length(tickers_entire)))
cat(sprintf("  Perf Comparison tickers: %d (tagged excluded_perf)\n",
            length(tickers_perf)))
cat(sprintf("  H3 (incl SECTOR_FUND) :  %d (tagged excluded_h3)\n",
            length(tickers_h3_full)))
cat("  Funds dropped at source:\n")
for (k in names(n_before)) {
  cat(sprintf("    panel_%-10s : %5d -> %5d  (-%d)\n",
              k, n_before[[k]], n_after[[k]],
              n_before[[k]] - n_after[[k]]))
}

# Diagnostic: flag distribution within surviving panel_trimmed
cat("\n  panel_trimmed flag-column counts (post-source-filter):\n")
cat(sprintf("    excluded_perf = TRUE : %d funds\n",
            n_distinct(panel_trimmed$Ticker[panel_trimmed$excluded_perf])))
cat(sprintf("    excluded_h3   = TRUE : %d funds\n",
            n_distinct(panel_trimmed$Ticker[panel_trimmed$excluded_h3])))

# =============================================================================
# 9. MONTHLY PERCENTAGE RETURNS FROM INDEX LEVELS
#    gross_return is the LSEG total return index level (not a percentage).
#    ret_gross_raw: unwinsorised gross decimal return (index_t/index_{t-1} - 1).
#    ret_gross: winsorised gross return (Step 10 below).
#
#    NET RETURN APPROXIMATION (direction reversed from BSW 2010):
#    LSEG serves identical total return index series for both the gross and
#    net return fields. True net returns are therefore unavailable from the
#    LSEG index. We approximate net returns from gross by deducting one-twelfth
#    of the static annual expense ratio per month:
#         ret_net_raw = ret_gross_raw - Expense_Ratio / 1200
#    Expense_Ratio is in percentage terms (e.g. 1.0 = 1%), so dividing by 1200
#    converts to a monthly decimal deduction. Where Expense_Ratio is missing
#    or unparseable, ret_net_raw is NA - no imputation is applied.
#
#    The arithmetic gross-net wedge of Expense_Ratio/12 follows the standard
#    fee-decomposition convention in the mutual fund literature - originating
#    in Carhart (1997) and Wermers (2000), adopted in Pastor and Stambaugh
#    (2002) and used throughout BSW (2010). BSW (2010) and most CRSP-based
#    studies observe NET returns and DERIVE gross by ADDING back the wedge.
#    Our pipeline observes gross only and DERIVES net by SUBTRACTING the
#    wedge - the same arithmetic, applied in the opposite direction.
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
cat("panel_trimmed ret_net_raw summary (gross - ER/12 approximation):\n")
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
  cat("  excluded_perf : ", n_distinct(panel$Ticker[panel$excluded_perf]), "\n")
  cat("  excluded_h3   : ", n_distinct(panel$Ticker[panel$excluded_h3]),   "\n")
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
