# =============================================================================
# ACTIVENESS-CONDITIONED PERSISTENCE                                        v1.2
#
# v1.2 changes vs v1.1 (filter-methodology revision, May 2026):
#   No code change. flagged_funds.xlsx ledger updated upstream: H3_EXCLUDED
#   flag retired from "Exclude from H3 Only" (90 funds in Equity Income,
#   Specialty Diversified, and Specialty/Miscellaneous Lipper categories
#   rejoin the activeness-conditioned persistence sample). Rationale: the
#   activeness measure used here -- formation-window 1-R^2 from a Carhart
#   four-factor regression (Amihud and Goyenko 2013) -- does not consume
#   any fund-specific benchmark, so the Cremers-Petajisto (2009) benchmark-
#   misassignment argument that motivated the H3_EXCLUDED tag does not
#   apply. The post-revision !excluded_h3 universe contains SECTOR_FUND
#   (149) and COVERED_CALL_OVERLAY (6) only.
#
# v1.1 changes vs v1.0 (Family D pre-defense audit):
#   - H3 / activeness subsample filter added at the panel-prep stage:
#     ap <- panel_incubation %>% filter(!excluded_h3) %>% rename(...) %>% ...
#     Per data_import_and_cleaning.R v1.2 Step 8c, activeness analyses must
#     restrict to the !excluded_h3 subsample so that the formation-window
#     1-R^2 measure and the within-cohort tercile breakpoints are not
#     contaminated by funds in the "Exclude from H3 Only" sheet of
#     flagged_funds.xlsx.
#     [Legacy v1.1 note, SUPERSEDED by v1.2: under the original ledger this
#     sheet listed Equity Income, Specialty Diversified, Specialty/
#     Miscellaneous, sector funds, and covered-call overlays. The first
#     three categories were retired in v1.2; only sector and covered-call
#     funds remain in scope.]
#   - Sample-source sentence in fn_text footnote updated to append
#     "H3 / activeness subsample per flagged\_funds.xlsx".
#
# v1.0 (original):
#
# 10 x 3 bivariate sort: 10 alpha deciles x 3 activeness terciles.
# Activeness measure: formation-window 1 - R^2 (cohort-local, NOT lifetime),
# following Amihud & Goyenko (2013, RFS).
# Inference: parametric Carhart with Newey-West (12-month lag); no bootstrap.
#
# Companion to persistence_testing.R: identical cohort definitions, formation
# (36m) / holding (12m) windows, formation NW lag (3m), and active-fund-only
# Evans-corrected source panel. The only methodological additions are:
#   (1) retain R^2 from each fund's formation regression,
#   (2) within each cohort, sort funds first into terciles by 1 - R^2, then
#       within tercile sort into deciles by formation alpha t-statistic,
#   (3) build 30 (T,D) cell portfolios + 3 within-tercile spreads.
#
# Outputs:
#   activeness_persistence_results.xlsx   per-cell estimates + cohort-level
#                                         cell counts (diagnostic) + summary.
#   table_activeness_persistence.tex      3-subpanel longtable, 33 rows x 10
#                                         cols, footnotesize portrait fit.
#
# Requires panel_incubation in session (run data_import_and_cleaning.R first).
# Dependencies: dplyr, tidyr, lubridate, writexl, knitr, kableExtra, stringr.
# =============================================================================

library(dplyr)
library(tidyr)
library(lubridate)
library(writexl)
library(knitr)
library(kableExtra)
library(stringr)

# =============================================================================
# 0. CONFIGURATION
# =============================================================================
FORM_MONTHS  <- 36L
HOLD_MONTHS  <- 12L
MIN_OBS_FORM <- 24L
NW_LAG_FORM  <- 3L
NW_LAG_HOLD  <- 12L
N_DECILES    <- 10L
N_TERCILES   <- 3L

# Diagnostic threshold: warn if any (cohort, T, D) cell has fewer than this
# number of funds. Tail cells (D1, D10) within T3 (most active) in early
# cohorts are the realistic binding constraint.
MIN_CELL_FUNDS <- 5L

# Cohort definitions (mirror persistence_testing.R Section 0)
COHORT_DEFS <- data.frame(
  cohort_id = 1:8,
  rank_date = as.Date(c("1997-12-31", "2001-12-31", "2005-12-31",
                        "2009-12-31", "2013-12-31", "2017-12-31",
                        "2021-12-31", "2025-12-31")),
  hold_months = c(12L, 12L, 12L, 12L, 12L, 12L, 12L, 2L),
  stringsAsFactors = FALSE
) %>%
  mutate(form_lo = rank_date %m-% months(FORM_MONTHS - 1L) %>% floor_date("month"),
         form_hi = rank_date %>% floor_date("month"),
         hold_lo = (rank_date + 1) %>% floor_date("month"),
         hold_hi = ((rank_date + 1) %>% floor_date("month")) %m+%
           months(hold_months - 1L))

USABLE_COHORTS <- COHORT_DEFS$cohort_id[COHORT_DEFS$hold_months == HOLD_MONTHS]

# =============================================================================
# 1. HELPERS (lifted from persistence_testing.R for consistency)
# =============================================================================
fast_ols <- function(y, X) {
  tryCatch({
    XtX  <- crossprod(X)
    beta <- solve(XtX, crossprod(X, y))
    e    <- as.vector(y - X %*% beta)
    n    <- length(y); k <- ncol(X)
    ss_r <- sum(e^2); ss_t <- sum((y - mean(y))^2)
    list(beta = as.vector(beta), e = e,
         r2 = if (ss_t > 0) 1 - ss_r / ss_t else NA_real_,
         adj_r2 = if (ss_t > 0) 1 - (ss_r / (n - k)) / (ss_t / (n - 1)) else NA_real_,
         n = n, k = k)
  }, error = function(err) NULL)
}

nw_se <- function(X, e, lag) {
  T <- nrow(X); k <- ncol(X)
  lag <- min(lag, T - 1L)
  tryCatch({
    XtX_inv <- solve(crossprod(X))
    scores  <- X * as.vector(e)
    S       <- crossprod(scores) / T
    if (lag > 0L) {
      for (j in seq_len(lag)) {
        w  <- 1 - j / (lag + 1)
        Gj <- crossprod(scores[(j + 1):T, , drop = FALSE],
                        scores[1:(T - j),  , drop = FALSE]) / T
        S  <- S + w * (Gj + t(Gj))
      }
    }
    sqrt(pmax(diag(T * XtX_inv %*% S %*% XtX_inv), 0))
  }, error = function(err) rep(NA_real_, k))
}

fmt <- function(x, d = 3) {
  ifelse(is.na(x) | is.nan(x), "--",
         formatC(round(as.numeric(x), d), format = "f", digits = d))
}

add_stars <- function(val_str, t_stat) {
  if (is.na(val_str) || val_str %in% c("--", "")) return(val_str)
  t_abs <- suppressWarnings(abs(as.numeric(t_stat)))
  if (is.na(t_abs)) return(val_str)
  stars <- if      (t_abs >= 2.576) "$^{***}$"
  else if (t_abs >= 1.960) "$^{**}$"
  else if (t_abs >= 1.645) "$^{*}$"
  else                     ""
  paste0(val_str, stars)
}

# One-tailed p-value in the direction of the observed t (matches Table 18 p^C)
fmt_p <- function(p) {
  if (is.na(p)) return("--")
  if (p < 0.001) return("$<$.001")
  formatC(round(as.numeric(p), 3), format = "f", digits = 3)
}

# Append a note paragraph AFTER \end{longtable} (not inside the body).
# Format mirrors clean_latex() floating-table branch: \begin{singlespace}
# wrapper ensures the note renders single-spaced (the document default is
# \doublespacing via setspace). No "Notes:" prefix and no \textit{} wrapping —
# matches Table 4.8 (BSW decomposition) caption style per SBE convention.
longtable_note <- function(s, note) {
  note_para <- paste0(
    "\\begin{singlespace}\\footnotesize\\noindent\n", note, "\n",
    "\\end{singlespace}\n\n"
  )
  parts <- strsplit(s, "\\end{longtable}", fixed = TRUE)[[1]]
  paste0(parts[1], "\\end{longtable}\n", note_para,
         if (length(parts) > 1)
           paste0(parts[-1], collapse = "\\end{longtable}") else "")
}

# Wrap longtable in footnotesize + tight tabcolsep for portrait fit
wrap_lt_small <- function(s, tabcolsep = "4pt") {
  # \setlength{\LTpost}{0pt} is scoped to the brace group, neutralizing the
  # 36pt \LTpost set globally in the preamble for this table only.
  opener <- paste0("{\\setlength{\\tabcolsep}{", tabcolsep,
                   "}\\setlength{\\LTpost}{0pt}\\footnotesize\n\\begin{longtable}")
  parts_open <- strsplit(s, "\\begin{longtable}", fixed = TRUE)[[1]]
  s <- paste0(parts_open[1], opener,
              if (length(parts_open) > 1) parts_open[2] else "")
  parts_close <- strsplit(s, "\\end{longtable}", fixed = TRUE)[[1]]
  paste0(parts_close[1], "\\end{longtable}\n}",
         if (length(parts_close) > 1)
           paste0(parts_close[-1], collapse = "\\end{longtable}") else "")
}

# =============================================================================
# 2. PANEL RESTRICTION
# =============================================================================
cat("=== 1. Data preparation ===\n")
if (!exists("panel_incubation"))
  stop("panel_incubation not found in session. Run data_import_and_cleaning.R ",
       "(and flow_calculation.R) first.")

ap <- panel_incubation %>%
  filter(!excluded_h3) %>%             # v1.1: H3 / activeness subsample
  rename(mkt_rf = MKT_RF, smb = SMB, hml = HML, mom = MOM, rf = RF,
         exp_r  = Expense_Ratio) %>%
  mutate(excess_ret = ret_gross - rf,
         exp_r      = suppressWarnings(as.numeric(exp_r)),
         ap_group   = gsub("Agtive", "Active", ap_group),
         date       = floor_date(date, "month")) %>%
  filter(ap_group == "Active",
         !is.na(excess_ret), !is.na(mkt_rf), !is.na(smb),
         !is.na(hml), !is.na(mom)) %>%
  select(Ticker, date, excess_ret, ret_gross, mkt_rf, smb, hml, mom, rf,
         exp_r) %>%
  arrange(Ticker, date)

factors_ts <- ap %>%
  distinct(date, mkt_rf, smb, hml, mom, rf) %>%
  arrange(date)

cat("  Active fund-months:", nrow(ap),
    "| Active funds:",      n_distinct(ap$Ticker),
    "| Factor months:",     nrow(factors_ts), "\n")

# =============================================================================
# 3. PER-COHORT FORMATION REGRESSION + 10x3 CELL ASSIGNMENT
# =============================================================================
cat("=== 2. Per-cohort formation regressions and cell assignment ===\n")

cohort_returns_list <- list()
cohort_diag_list    <- list()

for (cid in USABLE_COHORTS) {
  cd <- COHORT_DEFS[COHORT_DEFS$cohort_id == cid, ]
  cat(sprintf("  Cohort %d: form %s--%s, hold %s--%s\n",
              cid, format(cd$form_lo), format(cd$form_hi),
              format(cd$hold_lo), format(cd$hold_hi)))

  form_panel <- ap %>% filter(date >= cd$form_lo, date <= cd$form_hi)
  hold_panel <- ap %>% filter(date >= cd$hold_lo, date <= cd$hold_hi)

  form_by_fund <- split(form_panel, form_panel$Ticker)

  # Per-fund formation Carhart regression; retain R^2 (= 1 - 1mR2 below)
  run_form <- function(tk) {
    d <- form_by_fund[[tk]]; n <- nrow(d)
    if (n < MIN_OBS_FORM) return(NULL)
    y <- d$excess_ret
    X <- cbind(1, d$mkt_rf, d$smb, d$hml, d$mom)
    fit <- fast_ols(y, X); if (is.null(fit)) return(NULL)
    se  <- nw_se(X, fit$e, NW_LAG_FORM)
    t_alpha <- fit$beta[1] / se[1]
    data.frame(
      Ticker        = tk,
      n_form        = n,
      alpha_f       = fit$beta[1],
      t_alpha_f     = t_alpha,
      r2_f          = fit$r2,
      one_minus_r2  = 1 - fit$r2,
      stringsAsFactors = FALSE
    )
  }
  form_est <- bind_rows(lapply(names(form_by_fund), run_form)) %>%
    filter(is.finite(one_minus_r2), is.finite(t_alpha_f))

  if (nrow(form_est) < N_TERCILES * N_DECILES) {
    warning(sprintf("Cohort %d: only %d funds passed formation filter; skipping.",
                    cid, nrow(form_est)))
    next
  }

  # Tercile by 1-R^2 (T1 = least active, T3 = most active)
  # Decile by alpha t-stat WITHIN tercile (D1 = top, D10 = bottom -> flip ntile)
  form_est <- form_est %>%
    mutate(tercile = ntile(one_minus_r2, N_TERCILES)) %>%
    group_by(tercile) %>%
    mutate(decile = N_DECILES + 1L - ntile(t_alpha_f, N_DECILES)) %>%
    ungroup()

  # Diagnostic: cell counts per cohort
  cohort_diag_list[[as.character(cid)]] <-
    form_est %>% count(tercile, decile, name = "n_funds") %>%
    mutate(cohort_id = cid)

  # Build holding-period EW gross excess return per (T, D) cell
  hold_by_fund <- split(hold_panel, hold_panel$Ticker)

  for (tval in seq_len(N_TERCILES)) {
    for (dval in seq_len(N_DECILES)) {
      tickers <- form_est$Ticker[form_est$tercile == tval &
                                 form_est$decile  == dval]
      if (length(tickers) == 0L) next
      cell_panel <- bind_rows(lapply(tickers, function(tk) {
        hd <- hold_by_fund[[tk]]
        if (is.null(hd) || nrow(hd) == 0L) return(NULL)
        hd[, c("Ticker", "date", "excess_ret", "exp_r")]
      }))
      if (nrow(cell_panel) == 0L) next
      ew <- cell_panel %>%
        group_by(date) %>%
        summarise(ew_excess = mean(excess_ret, na.rm = TRUE),
                  ew_exp    = mean(exp_r,      na.rm = TRUE),
                  n_alive   = n(),
                  .groups   = "drop") %>%
        mutate(tercile = tval, decile = dval, cohort_id = cid)
      key <- paste(cid, tval, dval, sep = "_")
      cohort_returns_list[[key]] <- ew
    }
  }
}

# =============================================================================
# 4. POOLED HOLDING-PERIOD CARHART REGRESSIONS
# =============================================================================
cat("=== 3. Pooled holding-period Carhart regressions ===\n")

all_returns <- bind_rows(cohort_returns_list) %>%
  inner_join(factors_ts %>% select(date, mkt_rf, smb, hml, mom),
             by = "date") %>%
  arrange(tercile, decile, date)

run_holding <- function(sub) {
  if (nrow(sub) < 12L) return(NULL)
  y <- sub$ew_excess
  X <- cbind(1, sub$mkt_rf, sub$smb, sub$hml, sub$mom)
  fit <- fast_ols(y, X); if (is.null(fit)) return(NULL)
  se <- nw_se(X, fit$e, NW_LAG_HOLD)
  t_stats <- fit$beta / se
  # One-tailed p-value in direction of observed t-stat (matches Table 18 p^C)
  p_alpha <- pnorm(-abs(t_stats[1]))
  list(n_months    = nrow(sub),
       alpha_m     = fit$beta[1], t_alpha = t_stats[1], p_alpha = p_alpha,
       b_mkt = fit$beta[2], t_mkt = t_stats[2],
       b_smb = fit$beta[3], t_smb = t_stats[3],
       b_hml = fit$beta[4], t_hml = t_stats[4],
       b_mom = fit$beta[5], t_mom = t_stats[5],
       adj_r2 = fit$adj_r2)
}

cell_estimates <- list()
for (tval in seq_len(N_TERCILES)) {
  for (dval in seq_len(N_DECILES)) {
    sub <- all_returns %>% filter(tercile == tval, decile == dval)
    res <- run_holding(sub)
    if (is.null(res)) {
      warning(sprintf("Cell (T%d, D%d): regression failed or n_months < 12; ",
                      tval, dval),
              "row will contain NAs.")
      cell_estimates[[paste(tval, dval, sep = "_")]] <- data.frame(
        tercile = tval, decile = dval,
        n_months = NA_integer_, n_funds_avg = NA_real_,
        alpha_m = NA_real_, t_alpha = NA_real_, p_alpha = NA_real_,
        b_mkt = NA_real_, t_mkt = NA_real_,
        b_smb = NA_real_, t_smb = NA_real_,
        b_hml = NA_real_, t_hml = NA_real_,
        b_mom = NA_real_, t_mom = NA_real_,
        adj_r2 = NA_real_, exp_ratio = NA_real_,
        stringsAsFactors = FALSE
      )
      next
    }
    cell_estimates[[paste(tval, dval, sep = "_")]] <- data.frame(
      tercile = tval, decile = dval,
      n_months = res$n_months,
      n_funds_avg = mean(sub$n_alive),
      alpha_m = res$alpha_m, t_alpha = res$t_alpha, p_alpha = res$p_alpha,
      b_mkt = res$b_mkt, t_mkt = res$t_mkt,
      b_smb = res$b_smb, t_smb = res$t_smb,
      b_hml = res$b_hml, t_hml = res$t_hml,
      b_mom = res$b_mom, t_mom = res$t_mom,
      adj_r2 = res$adj_r2,
      exp_ratio = mean(sub$ew_exp, na.rm = TRUE),
      stringsAsFactors = FALSE
    )
  }
}

# Within-tercile D1 - D10 spread regressions
spread_estimates <- list()
for (tval in seq_len(N_TERCILES)) {
  d1 <- all_returns %>% filter(tercile == tval, decile == 1L) %>%
    select(date, ew1 = ew_excess)
  d10 <- all_returns %>% filter(tercile == tval, decile == N_DECILES) %>%
    select(date, ew10 = ew_excess)
  spr <- inner_join(d1, d10, by = "date") %>%
    mutate(spread = ew1 - ew10) %>%
    inner_join(factors_ts %>% select(date, mkt_rf, smb, hml, mom),
               by = "date")
  if (nrow(spr) < 12L) next
  y <- spr$spread
  X <- cbind(1, spr$mkt_rf, spr$smb, spr$hml, spr$mom)
  fit <- fast_ols(y, X); if (is.null(fit)) next
  se <- nw_se(X, fit$e, NW_LAG_HOLD)
  t_stats <- fit$beta / se
  p_alpha <- pnorm(-abs(t_stats[1]))
  spread_estimates[[as.character(tval)]] <- data.frame(
    tercile = tval, decile = 11L,
    n_months = nrow(spr), n_funds_avg = NA_real_,
    alpha_m = fit$beta[1], t_alpha = t_stats[1], p_alpha = p_alpha,
    b_mkt = fit$beta[2], t_mkt = t_stats[2],
    b_smb = fit$beta[3], t_smb = t_stats[3],
    b_hml = fit$beta[4], t_hml = t_stats[4],
    b_mom = fit$beta[5], t_mom = t_stats[5],
    adj_r2 = fit$adj_r2, exp_ratio = NA_real_,
    stringsAsFactors = FALSE
  )
}

estimates <- bind_rows(c(cell_estimates, spread_estimates)) %>%
  arrange(tercile, decile)
rownames(estimates) <- NULL

# =============================================================================
# 5. DIAGNOSTICS: cell sizes and pooled month coverage
# =============================================================================
cat("=== 4. Diagnostics ===\n")

cell_counts_by_cohort <- bind_rows(cohort_diag_list) %>%
  arrange(tercile, decile, cohort_id)

# Min and mean fund counts per (T, D) across cohorts; expected total months = 84
EXPECTED_MONTHS <- length(USABLE_COHORTS) * HOLD_MONTHS

cell_summary <- cell_counts_by_cohort %>%
  group_by(tercile, decile) %>%
  summarise(min_funds_cohort   = min(n_funds),
            mean_funds_cohort  = mean(n_funds),
            n_cohorts_present  = n(),
            n_cohorts_thin     = sum(n_funds < MIN_CELL_FUNDS),
            .groups = "drop") %>%
  left_join(estimates %>% select(tercile, decile, n_months_actual = n_months),
            by = c("tercile", "decile")) %>%
  mutate(months_lost = EXPECTED_MONTHS - replace_na(n_months_actual, 0L))

thin <- cell_summary %>% filter(min_funds_cohort < MIN_CELL_FUNDS)
if (nrow(thin) > 0L) {
  cat(sprintf("  WARNING: %d (T, D) cells have at least one cohort with < %d funds:\n",
              nrow(thin), MIN_CELL_FUNDS))
  print(as.data.frame(thin))
} else {
  cat(sprintf("  All (T, D) cells have at least %d funds in every cohort.\n",
              MIN_CELL_FUNDS))
}

short_pool <- cell_summary %>% filter(months_lost > 0L)
if (nrow(short_pool) > 0L) {
  cat(sprintf("  WARNING: %d cells have fewer than %d pooled monthly obs (expected = 7 cohorts x 12 months).\n",
              nrow(short_pool), EXPECTED_MONTHS))
  print(as.data.frame(short_pool %>% select(tercile, decile, n_months_actual, months_lost)))
}

# =============================================================================
# 6. WRITE XLSX
# =============================================================================
cat("=== 5. Writing activeness_persistence_results.xlsx ===\n")

sheets <- list(
  estimates              = estimates,
  cell_counts_by_cohort  = cell_counts_by_cohort,
  cell_summary           = cell_summary,
  config                 = data.frame(
    parameter = c("FORM_MONTHS", "HOLD_MONTHS", "MIN_OBS_FORM",
                  "NW_LAG_FORM", "NW_LAG_HOLD", "N_DECILES", "N_TERCILES",
                  "MIN_CELL_FUNDS", "USABLE_COHORTS", "ACTIVENESS_MEASURE",
                  "ACTIVENESS_DEFINITION"),
    value     = c(FORM_MONTHS, HOLD_MONTHS, MIN_OBS_FORM,
                  NW_LAG_FORM, NW_LAG_HOLD, N_DECILES, N_TERCILES,
                  MIN_CELL_FUNDS,
                  paste(USABLE_COHORTS, collapse = ","),
                  "1 - R^2 (Carhart 4-factor, formation window)",
                  "Cohort-local: ranked within each cohort's 36-month formation period")
  )
)
write_xlsx(sheets, "activeness_persistence_results.xlsx")
cat("Written: activeness_persistence_results.xlsx\n")

# =============================================================================
# 7. BUILD LATEX TABLE (3 sub-panels, 11 rows each = 33 rows; 10 columns)
# =============================================================================
cat("=== 6. Building table_activeness_persistence.tex ===\n")

fmt_alpha_pct <- function(a_m, t) {
  if (is.na(a_m)) return("--")
  add_stars(fmt(a_m * 100, 3), t)
}

build_panel_rows <- function(est) {
  est <- est %>% arrange(decile)
  rows <- list()
  for (i in seq_len(nrow(est))) {
    r <- est[i, ]
    lbl <- if (r$decile == 11L)        "D1$-$D10"
    else if (r$decile == 1L)           "D1 (High)"
    else if (r$decile == N_DECILES)    "D10 (Low)"
    else                                paste0("D", r$decile)
    rows[[i]] <- data.frame(
      Decile  = lbl,
      Alpha   = fmt_alpha_pct(r$alpha_m, r$t_alpha),
      tAlpha  = paste0("(", fmt(r$t_alpha, 2), ")"),
      pC      = fmt_p(r$p_alpha),
      bM      = add_stars(fmt(r$b_mkt, 3), r$t_mkt),
      bS      = add_stars(fmt(r$b_smb, 3), r$t_smb),
      bH      = add_stars(fmt(r$b_hml, 3), r$t_hml),
      bO      = add_stars(fmt(r$b_mom, 3), r$t_mom),
      R2      = fmt(r$adj_r2, 3),
      ExpR    = if (is.na(r$exp_ratio)) "--" else fmt(r$exp_ratio, 2),
      stringsAsFactors = FALSE
    )
  }
  do.call(rbind, rows)
}

panel_titles <- c(
  "Panel T1: Lowest activeness tercile (lowest formation $1-R^2$)",
  "Panel T2: Middle activeness tercile",
  "Panel T3: Highest activeness tercile (highest formation $1-R^2$)"
)

panel_tables <- lapply(seq_len(N_TERCILES), function(tval) {
  build_panel_rows(estimates %>% filter(tercile == tval))
})

ROWS_PER_PANEL <- nrow(panel_tables[[1]])
full_data <- bind_rows(panel_tables)
rownames(full_data) <- NULL

pack_tab <- data.frame(
  label = panel_titles,
  start = (seq_along(panel_tables) - 1L) * ROWS_PER_PANEL + 1L,
  end   = seq_along(panel_tables) * ROWS_PER_PANEL,
  stringsAsFactors = FALSE
)

fn_text <- paste(
  "Activeness-conditioned persistence test: $10 \\times 3$ bivariate sort.",
  "At each cohort ranking date, all eligible active funds are sorted into",
  "three terciles by formation-period $1 - R^2$ from a 36-month",
  "\\textcite{Carhart1997} four-factor regression (T1 = lowest activeness,",
  "T3 = highest), and within each tercile into ten deciles by formation-period",
  "alpha $t$-statistic (Newey-West, 3-month lag). D1 contains the highest-ranked",
  "funds, D10 the lowest. Equal-weighted holding-period (12-month) gross",
  "excess returns are pooled across the seven non-overlapping cohorts of",
  "Table~\\ref{tab:persistence} (Jan 1995--Feb 2026) and regressed on the",
  "\\textcite{Carhart1997} four factors with Newey-West $t$-statistics",
  "(12-month lag).",
  "$\\hat{\\alpha}$: monthly, \\%.",
  "$p^C$: one-tailed parametric $p$-value from the holding-period alpha",
  "$t$-statistic under normality, in the direction of the observed effect.",
  "ExpR: mean expense ratio (\\%) of cell constituents, averaged across cohorts.",
  "D1$-$D10 is the within-tercile long-short spread.",
  "Significance stars on $\\hat{\\alpha}$ and factor loadings reflect Newey-West",
  "$t$-statistics: $^{*}$, $^{**}$, $^{***}$ at 10\\%, 5\\%, 1\\%.",
  "The activeness measure is the formation-window $1 - R^2$ following",
  "\\textcite{AmihudGoyenko2013}; tercile breakpoints are computed within each",
  "cohort to avoid look-ahead contamination.",
  "Bootstrap inference (cf.\\ Table~\\ref{tab:persistence}) is omitted because",
  "this is a secondary conditional test; non-normality of decile-portfolio",
  "returns is most severe in unconditional tail sorts, which conditioning on",
  "activeness already attenuates.",
  "Sample: Active funds in the \\textcite{Evans2010}-corrected",
  "panel\\_incubation, Jan 1995--Feb 2026; H3 / activeness subsample",
  "per flagged\\_funds.xlsx."
)

k <- full_data %>%
  kbl(format    = "latex",
      booktabs  = TRUE,
      linesep   = "",
      escape    = FALSE,
      longtable = TRUE,
      caption   = "Activeness-Conditioned Persistence Test",
      label     = "activeness_persistence",
      col.names = c("Decile",
                    "$\\hat{\\alpha}$", "$t(\\hat{\\alpha})$", "$p^C$",
                    "$\\beta_{\\text{MKT}}$", "$\\beta_{\\text{SMB}}$",
                    "$\\beta_{\\text{HML}}$", "$\\beta_{\\text{MOM}}$",
                    "$\\bar{R}^2$", "ExpR"),
      align     = c("l", rep("r", 9)),
      row.names = FALSE) %>%
  kable_styling(latex_options = c("hold_position", "repeat_header"))

for (i in seq_len(nrow(pack_tab))) {
  k <- k %>%
    pack_rows(pack_tab$label[i], pack_tab$start[i], pack_tab$end[i],
              bold = FALSE, italic = TRUE,
              hline_before = (i > 1), hline_after = FALSE,
              escape = FALSE)
}

s <- as.character(k)
s <- longtable_note(s, fn_text)
s <- wrap_lt_small(s, tabcolsep = "4pt")

writeLines(s, "table_activeness_persistence.tex")
cat("Written: table_activeness_persistence.tex\n")

# =============================================================================
# 8. FINAL SUMMARY
# =============================================================================
cat("\n=== ACTIVENESS-CONDITIONED PERSISTENCE COMPLETE ===\n")
for (tval in seq_len(N_TERCILES)) {
  est <- estimates %>% filter(tercile == tval)
  d1  <- est[est$decile == 1L,        ]
  d10 <- est[est$decile == N_DECILES, ]
  sp  <- est[est$decile == 11L,       ]
  if (nrow(d1) == 0L || nrow(d10) == 0L) next
  cat(sprintf("  T%d: D1 alpha = %s%% (t=%s, p=%s); D10 alpha = %s%% (t=%s, p=%s); D1-D10 alpha = %s%% (t=%s)\n",
              tval,
              fmt(d1$alpha_m * 100, 3), fmt(d1$t_alpha, 2), fmt_p(d1$p_alpha),
              fmt(d10$alpha_m * 100, 3), fmt(d10$t_alpha, 2), fmt_p(d10$p_alpha),
              if (nrow(sp) > 0) fmt(sp$alpha_m * 100, 3) else "--",
              if (nrow(sp) > 0) fmt(sp$t_alpha, 2) else "--"))
}
cat("\nOutputs:\n")
cat("  activeness_persistence_results.xlsx\n")
cat("  table_activeness_persistence.tex\n")
