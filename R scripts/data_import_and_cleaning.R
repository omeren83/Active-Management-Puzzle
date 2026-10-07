# =============================================================================
# FUND DATA IMPORT & PANEL CONSTRUCTION                                    v1.4
#
# v1.4 changes vs v1.3 (pipeline audit, Oct 2026):
#   - RETURN INDEX IS NET OF FEES. The LSEG total-return index (sheet
#     "gross_return") is NAV-based and therefore net of the expense ratio.
#     Verified against published calendar-year NAV returns (MTCBX, TEMTX,
#     HSLAX, 2021-2025: 15 of 15 fund-years agree to rounding). Step 9 now
#     sets ret_net_raw = index return and ret_gross_raw = ret_net_raw + ER/1200
#     (gross = net + ER/12, as in Fama & French 2010 and BSW 2010). v1.3 had
#     the direction reversed, so its "net" series deducted fees twice.
#   - NO POOLED WINSORISATION OF RETURNS. Pooled 1/99 cut-offs clipped whole
#     market months (91% of funds in Oct 2008), creating spurious factor-model
#     residuals. Step 10 now (a) removes data errors with a documented screen
#     (factor-of-two rule + terminal-print rule), written to return_screen.xlsx,
#     and (b) uses screened raw returns as ret_gross / ret_net. Per-month
#     cross-sectional 1/99 winsorised series (ret_gross_cs, ret_net_cs) are
#     kept for robustness only. Flows keep their own winsorisation.
#   - Expense_Ratio cleaned at source: values outside [0, 5]% set to NA.
#   - Step 8b: ProFunds abbreviations ("PRFND", "ULTRSCT", ...) and
#     "leveraged" now caught in all ap_groups (Bloomberg truncates names, so
#     v1.3 missed UltraSector ProFunds, one of them labelled Active).
#   - Join-uniqueness assertions added (static, fund panel, base panel).
#   - Duplicated Step 8c filter lines removed.
#
# v1.3 changes vs v1.2 (filter-methodology revision, May 2026):
#   - flagged_funds.xlsx ledger updated: PASSIVE_INDEX (313 funds, formerly
#     Tier 0) and H3_EXCLUDED (90 funds, formerly Tier 2) flags retired.
#     Passive index funds now survive Step 8c and appear in descriptive
#     statistics, aggregate alpha, portfolio sorts, and flow figures.
#     Equity Income / Specialty Diversified / Specialty Miscellaneous
#     categories (formerly H3_EXCLUDED) rejoin H3 and activeness analyses.
#     Rationale: PASSIVE_INDEX was over-exclusion -- the active-vs-passive
#     performance contrast central to the puzzle requires the full passive
#     universe in descriptive and aggregate tables. H3_EXCLUDED's original
#     justification (Cremers-Petajisto 2009 benchmark-misassignment) applied
#     to active share, not to the 1-R^2 proxy used here, which is computed
#     from Carhart factors only and is invariant to fund-specific benchmark
#     assignment. Both groups remain absent from active-only analyses via
#     pre-existing ap_group == "Active" guards in the relevant scripts.
#   - SECTOR_FUND (149) and COVERED_CALL_OVERLAY (6) remain in the H3-only
#     scope; SECTOR_FUND rationale rewritten to attribute inflation of H3
#     lottery proxies to Carhart factor orthogonality rather than to
#     benchmark misassignment. See flagged_funds.xlsx Legend sheet.
#   - No code change in this script. The Step 8c wiring is identical; only
#     the workbook contents change. Header counts in Step 8c documentation
#     updated below.
#
# v1.2 changes:
#   - Step 8c added: applies the flagged_funds.xlsx exclusion ledger.
#       (a) "Exclude from Entire Analysis" tickers are dropped from all three
#           panels at source. This subsumes the prior DATA_ERROR_TICKERS
#           hardcode and adds GLOBAL_MANDATE, EM_MANDATE, LONG_SHORT,
#           BEAR_MARKET, MARKET_NEUTRAL, DATA_ERROR exclusions.
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
#     Retained as a defensive name-pattern net for inverse / 2x / 3x
#     products. Note: as of v1.3 PASSIVE_INDEX is no longer in the workbook,
#     so the bulk of passive funds now SURVIVE Step 8c. The keyword filter
#     here still removes leveraged products by name pattern, regardless of
#     workbook contents. The Pure Style Rydex retention list (RYAVX, RYZAX,
#     RYAZX, RYWAX, RYAWX) is now a no-op since no PASSIVE_INDEX flag exists
#     to override; those funds pass through to the active/passive panel and
#     are classified by their LSEG ap_group label.
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
#   (4) Return data-error screen (v1.4; replaces pooled winsorisation)
#   (5) Leveraged / derivative-based fund exclusion (name patterns)
#   (6) flagged_funds.xlsx Entire-Analysis exclusion + flag columns [NEW v1.2]
#
# Dependencies: readxl, dplyr, tidyr, lubridate, writexl
# Evans (2010): Journal of Finance, Vol. LXV, No. 4
# =============================================================================

library(lubridate)
library(readxl)
library(dplyr)
library(tidyr)
library(writexl)

FILE          <- "fund_data.xlsx"
FLAGGED_FILE  <- "flagged_funds.xlsx"   # exclusion ledger (8c)
DATE_MIN_DATA <- as.Date("1994-12-01")  # include Dec 1994 for lag computation
DATE_MIN      <- as.Date("1995-01-01")  # actual sample start for analysis
DATE_MAX      <- as.Date("2023-12-31")  # sample end (panel_trimmed)
EVANS_MONTHS  <- 36   # Evans (2010): <5% of funds incubated longer than 36 months

# Return data-error screen (Step 10). Ratio = (1 + r_fund) / (1 + r_median),
# where r_median is the cross-sectional median fund return that month.
SCREEN_RATIO_ANY  <- 2      # any month: flag if ratio > 2 or < 1/2
SCREEN_LOG_FINAL  <- 0.30   # final month of a fund that dies before data end: flag if |log(ratio)| > 0.30
SCREEN_FILE       <- "return_screen.xlsx"   # flagged list, for inspection
# Fund-months confirmed genuine on inspection: "Ticker|YYYY-MM" (kept unscreened)
RETURN_SCREEN_KEEP <- character(0)

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
stopifnot(!anyDuplicated(static$Ticker))   # one static row per fund

cat("Static loaded:", nrow(static), "funds |",
    sum(!is.na(static$Inception_Date)), "with valid Inception_Date\n")

# Expense ratio (% p.a.): numeric; values outside [0, 5] are data errors -> NA
static <- static %>%
  mutate(.er_raw       = suppressWarnings(as.numeric(Expense_Ratio)),
         .er_bad       = !is.na(.er_raw) & (.er_raw < 0 | .er_raw > 5),
         Expense_Ratio = if_else(.er_bad, NA_real_, .er_raw))
cat("Expense_Ratio outside [0, 5]% set to NA:", sum(static$.er_bad), "funds\n")
if (any(static$.er_bad))
  print(as.data.frame(static[static$.er_bad, c("Ticker", "Name", ".er_raw")]),
        row.names = FALSE)
static <- select(static, -.er_raw, -.er_bad)

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
stopifnot(!anyDuplicated(fund_panel[c("Ticker", "date")]))   # one row per fund-month

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
stopifnot(!anyDuplicated(base_panel[c("Ticker", "date")]))   # joins added no rows

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
#     [Defensive name-pattern net; load-bearing as of v1.3]
#
#     Background: daily-reset leveraged mutual funds (Rydex, ProFunds,
#     Direxion) are classified as passive by LSEG (Actively_Managed_New = N)
#     because they mechanically track an index. However, they use equity swaps
#     or futures to deliver a constant daily leverage multiple, resulting in
#     annual turnover of 200-4000% and severe volatility decay over multi-year
#     holding periods (Avellaneda & Zhang, 2010).
#
#     Diagnostic: 30 passive funds with Turnover > 200% were identified; all
#     confirmed as leveraged/derivative products on manual review. The
#     BEAR_MARKET flag in flagged_funds.xlsx catches the inverse / bear
#     subset at Step 8c. As of v1.3 the PASSIVE_INDEX flag is retired, so
#     the leveraged-long products (Rydex Ultra, ProFunds Ultra, Direxion
#     Bull 1.x/2x/3x) are no longer caught at source by Step 8c and rely
#     on the name-pattern filter below. The keyword list is therefore
#     load-bearing, not merely defensive.
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
# As of v1.3, with PASSIVE_INDEX retired from flagged_funds.xlsx:
#   - MOJAX, GENDX (LSEG-flagged "Active" but functionally pure index trackers)
#     are no longer in any exclusion sheet. They pass through to the analysis
#     panel classified by their LSEG ap_group label. Their downstream effect
#     is small (low fund count, low TNA share) and falls within the noise
#     of LSEG classification accuracy.
#   - RYAVX/RYZAX/RYAZX/RYWAX/RYAWX (Pure Style Rydex passives) similarly
#     pass through, classified ap_group == "Passive" by LSEG, and contribute
#     to the passive cohort in descriptive and aggregate tables.
# The user-curated workbook continues to govern Step 8c; the v1.2 inline
# retention logic remains superseded.

# v1.4: Bloomberg truncates fund names ("ULTRSCTR PRFND", "PROFND", "US PF-INV"),
# so the patterns above miss several UltraSector ProFunds. These abbreviations, plus
# "leveraged", are unambiguous and are applied to ALL ap_groups (one ProFund,
# IDPIX, is labelled Active by LSEG). "ultra" stays Passive-only because
# genuine active funds use it (e.g. American Century Ultra, Wasatch Ultra Growth).
LEVERAGED_ANY_GROUP <- "prfnd|prfund|profnd|profund|pf-inv|leveraged"

exclude_leveraged <- function(panel) {
  panel %>%
    filter(
      !(ap_group == "Passive" &
          grepl(LEVERAGED_KEYWORDS, Name, ignore.case = TRUE)),
      !grepl(LEVERAGED_ANY_GROUP, Name, ignore.case = TRUE)
    )
}

cat("\nStep 8b removes (panel_master, all groups):\n")
print(as.data.frame(panel_master %>%
  distinct(Ticker, Name, ap_group) %>%
  filter((ap_group == "Passive" & grepl(LEVERAGED_KEYWORDS, Name, ignore.case = TRUE)) |
           grepl(LEVERAGED_ANY_GROUP, Name, ignore.case = TRUE)) %>%
  arrange(ap_group, Name)), row.names = FALSE)

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
#       (i)  Exclude from Entire Analysis  (125 funds, v1.3): dropped at
#            source. Covers GLOBAL_MANDATE / EM_MANDATE (non-US), LONG_SHORT,
#            MARKET_NEUTRAL, BEAR_MARKET (violate the long-only assumption),
#            and DATA_ERROR (the two confirmed LSEG errors QWVOX, VALLCEN).
#            v1.3 note: PASSIVE_INDEX (313 funds, formerly the bulk of this
#            tier) has been retired; passive index funds now survive Step 8c
#            and appear in descriptive, aggregate, and portfolio-sort
#            tables. Active-only analyses (alpha estimation, bootstrap,
#            persistence, H1-H4) are unaffected because they apply their
#            own ap_group == "Active" guards downstream.
#
#       (ii) Exclude from Perf Comparison  (292 funds, v1.3): NOT dropped
#            here. Tagged on the surviving panel with excluded_perf = TRUE
#            so that aggregate-alpha, bootstrap, FDR, persistence,
#            sub-period, robust factor model, and portfolio-sort scripts
#            can apply filter(!excluded_perf). After PASSIVE_INDEX retirement
#            this sheet is dominated by SECTOR_FUND (169) and the long-short
#            / market-neutral / global-mandate residuals.
#
#       (iii) Exclude from H3 Only  (155 funds, v1.3): tagged with
#            excluded_h3 = TRUE. As of v1.3 contains SECTOR_FUND (149) and
#            COVERED_CALL_OVERLAY (6) only. H3_EXCLUDED (90 funds in
#            Equity Income / Specialty Diversified / Specialty Miscellaneous
#            Lipper categories) was retired: its original Cremers-Petajisto
#            (2009) benchmark-misassignment rationale applied to active
#            share, but the activeness proxy used here is 1-R^2 from a
#            Carhart four-factor regression (Amihud-Goyenko 2013), which
#            does not use any fund-specific benchmark. The composite
#            excluded_h3 column is the union of "Exclude from H3 Only" and
#            SECTOR_FUND-tagged funds in the Perf Comparison sheet (sector
#            funds are excluded from both performance and H3).
#
#     Behavioral H1/H2/H4 panel regressions need no further filter beyond
#     what is applied at source by (i): they run on the full surviving
#     active universe (SUBSET = "active" in panel_regressions_setup.R).
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
# 9. MONTHLY RETURNS FROM THE TOTAL-RETURN INDEX
#    gross_return is the LSEG total-return index LEVEL. Despite the sheet name,
#    it is NAV-based and therefore NET of the expense ratio (v1.4; verified
#    against published calendar-year NAV returns of MTCBX, TEMTX and HSLAX,
#    2021-2025, 15 of 15 fund-years agree to rounding). The "net_return" sheet
#    is the same series (identical in 99.6% of cells).
#
#    ret_net_raw   = index_t / index_{t-1} - 1          (what investors earn)
#    ret_gross_raw = ret_net_raw + Expense_Ratio / 1200  (before expenses)
#
#    Gross = net + ER/12 is the convention of Fama & French (2010) and
#    BSW (2010). Expense_Ratio is in percent (1.0 = 1%) and is a static
#    end-of-sample snapshot. Where it is missing, ret_gross_raw is NA and
#    ret_net_raw is unaffected (no imputation). Returns exclude sales loads,
#    as in CRSP-based studies.
#
#    First observation per fund in each panel is NA (no lagged index).
# =============================================================================
compute_returns <- function(panel) {
  panel %>%
    group_by(Ticker) %>%
    arrange(date) %>%
    mutate(
      fee_monthly   = suppressWarnings(as.numeric(Expense_Ratio)) / 1200,
      ret_net_raw   = gross_return / lag(gross_return) - 1,  # NAV-based: net
      ret_gross_raw = ret_net_raw + fee_monthly               # add back ER/12
    ) %>%
    select(-fee_monthly) %>%
    ungroup()
}

panel_master     <- compute_returns(panel_master)
panel_incubation <- compute_returns(panel_incubation)
panel_trimmed    <- compute_returns(panel_trimmed)

cat("\nMonthly returns computed for all panels (index = net of fees).\n")

# =============================================================================
# 10. RETURN DATA-ERROR SCREEN (replaces pooled winsorisation, v1.4)
#     Pooled 1/99 cut-offs clipped whole market months rather than outliers
#     (91% of funds in Oct 2008) and created spurious factor-model residuals.
#     Carhart (1997), Fama & French (2010), BSW (2010) and Kosowski et al.
#     (2006) do not winsorise fund returns. Instead, data errors are removed:
#
#     ratio_it = (1 + r_it) / (1 + median_t), median over all funds in month t
#       Rule A (any month):  ratio > 2 or ratio < 1/2 - a long-only fund
#                            cannot double or halve relative to the median fund
#       Rule B (final month): |log ratio| > 0.30 in the LAST month of a fund
#                            that stops reporting before the data end -
#                            liquidation/merger prints in the LSEG index
#
#     Flags are computed once on panel_master (full fund histories, so "final
#     month" is the true last month) and applied to all panels. Flagged months
#     get NA in ret_net_raw and ret_gross_raw, so they also drop out of flows.
#     The list is written to SCREEN_FILE; months confirmed genuine can be
#     restored via RETURN_SCREEN_KEEP.
#
#     ret_gross / ret_net (used by all performance scripts) = screened raw
#     returns. ret_gross_cs / ret_net_cs = per-month cross-sectional 1/99
#     winsorisation of the same series, for robustness only.
# =============================================================================
screen_tbl <- panel_master %>%
  filter(!is.na(ret_net_raw)) %>%
  group_by(date) %>%
  mutate(med_t = median(ret_net_raw)) %>%
  ungroup() %>%
  group_by(Ticker) %>%
  # final month only if the fund stops reporting before the data end
  mutate(is_final = date == max(date) & max(date) < max(panel_master$date)) %>%
  ungroup() %>%
  mutate(
    ratio  = (1 + ret_net_raw) / (1 + med_t),
    rule_A = ratio > SCREEN_RATIO_ANY | ratio < 1 / SCREEN_RATIO_ANY,
    rule_B = is_final & !rule_A & abs(log(pmax(ratio, 1e-9))) > SCREEN_LOG_FINAL,
    key    = paste0(Ticker, "|", format(date, "%Y-%m")),
    kept   = key %in% RETURN_SCREEN_KEEP
  ) %>%
  filter(rule_A | rule_B) %>%
  transmute(Ticker, Name, ap_group, date, ret_net_raw, median_t = med_t,
            ratio, rule = if_else(rule_A, "A: ratio outside [1/2, 2]",
                                  "B: final-month print"),
            action = if_else(kept, "kept (RETURN_SCREEN_KEEP)", "set to NA")) %>%
  arrange(ap_group, Ticker, date)

screen_drop <- screen_tbl %>% filter(action == "set to NA") %>% select(Ticker, date)

cat("\n--- Step 10: return data-error screen (panel_master) ---\n")
cat("  Flagged fund-months:", nrow(screen_tbl),
    "| set to NA:", nrow(screen_drop), "\n")
print(as.data.frame(count(screen_tbl, ap_group, rule)), row.names = FALSE)
write_xlsx(list(flagged = screen_tbl), SCREEN_FILE)
cat("  Flagged list written to", SCREEN_FILE, "\n")

apply_return_screen <- function(panel) {
  bad <- paste(panel$Ticker, panel$date) %in% paste(screen_drop$Ticker, screen_drop$date)
  panel %>%
    mutate(ret_net_raw   = if_else(bad, NA_real_, ret_net_raw),
           ret_gross_raw = if_else(bad, NA_real_, ret_gross_raw),
           # main performance series: screened, not winsorised
           ret_net   = ret_net_raw,
           ret_gross = ret_gross_raw) %>%
    # robustness series: per-month cross-sectional 1/99 winsorisation
    group_by(date) %>%
    mutate(ret_net_cs   = winsorise(ret_net_raw),
           ret_gross_cs = winsorise(ret_gross_raw)) %>%
    ungroup()
}

panel_master     <- apply_return_screen(panel_master)
panel_incubation <- apply_return_screen(panel_incubation)
panel_trimmed    <- apply_return_screen(panel_trimmed)

cat("\npanel_incubation ret_net (screened) summary:\n")
print(summary(panel_incubation$ret_net))
cat("panel_incubation ret_gross (screened, = net + ER/12) summary:\n")
print(summary(panel_incubation$ret_gross))

# =============================================================================
# 11. SUMMARY
# =============================================================================
summarise_panel <- function(panel, label, panel_pre8c = NULL) {
  cat("\n---", label, "---\n")
  cat("Dimensions   :", nrow(panel), "rows x", ncol(panel), "columns\n")
  cat("Date range   :", format(min(panel$date)), "to", format(max(panel$date)), "\n")
  
  # If a pre-8c snapshot is supplied, report both universe and analytical counts
  if (!is.null(panel_pre8c)) {
    n_universe   <- n_distinct(panel_pre8c$Ticker)
    n_analytical <- n_distinct(panel$Ticker)
    cat("Unique funds : ", n_analytical, " analytical / ", n_universe,
        " universe (Step 8c dropped ", n_universe - n_analytical, ")\n", sep = "")
    cat("  Universe   :",
        n_distinct(panel_pre8c$Ticker[panel_pre8c$ap_group == "Active"]),  "Active /",
        n_distinct(panel_pre8c$Ticker[panel_pre8c$ap_group == "Passive"]), "Passive /",
        n_distinct(panel_pre8c$Ticker[panel_pre8c$ap_group == "Unknown"]), "Unknown\n")
    cat("  Analytical :",
        n_distinct(panel$Ticker[panel$ap_group == "Active"]),  "Active /",
        n_distinct(panel$Ticker[panel$ap_group == "Passive"]), "Passive /",
        n_distinct(panel$Ticker[panel$ap_group == "Unknown"]), "Unknown\n")
  } else {
    cat("Unique funds :", n_distinct(panel$Ticker), "\n")
    cat("  Active     :", n_distinct(panel$Ticker[panel$ap_group == "Active"]),  "\n")
    cat("  Passive    :", n_distinct(panel$Ticker[panel$ap_group == "Passive"]), "\n")
    cat("  Unknown    :", n_distinct(panel$Ticker[panel$ap_group == "Unknown"]), "\n")
  }
  
  cat("  Tagged (retained in panel, filtered downstream):\n")
  cat("    excluded_perf flag : ", n_distinct(panel$Ticker[panel$excluded_perf]), "\n")
  cat("    excluded_h3 flag   : ", n_distinct(panel$Ticker[panel$excluded_h3]),   "\n")
  cat("Unique dates :", n_distinct(panel$date), "\n")
  
  obs_dist <- panel %>%
    group_by(Ticker) %>%
    summarise(n = n(), .groups = "drop") %>%
    pull(n)
  cat("Obs per fund : min =", min(obs_dist),
      "| median =", median(obs_dist),
      "| max =", max(obs_dist), "\n")
}

summarise_panel(panel_master,     "PANEL MASTER (post-Step-8c analytical)",     panel_master_pre8c)
summarise_panel(panel_incubation, "PANEL INCUBATION (Evans 36m + post-Step-8c)", panel_incubation_pre8c)
summarise_panel(panel_trimmed,    "PANEL TRIMMED (Evans + 1995-2023 + post-Step-8c)", panel_trimmed_pre8c)
