# =============================================================================
# PERSISTENCE TESTING                                                      v1.2


# Changes from v1.1:
# Panel C/D/E sub-period title strings realigned to Sep 2011 partition

# Changes from v1.0 (Family D pre-defense audit):
#   - Performance-comparison subsample filter added to the panel-prep stage:
#     ap <- panel_incubation %>% filter(!excluded_perf) %>% rename(...) %>% ...
#     Per the flagged_funds.xlsx exclusion ledger wired in by
#     data_import_and_cleaning.R v1.2 Step 8c, persistence testing is a
#     performance-comparison exercise and therefore must restrict to the
#     !excluded_perf subsample. Without this filter, funds in the "Exclude
#     from Perf Comparison" sheet (data errors, daily-reset leveraged
#     products, sector funds, etc.) would contaminate decile cutoffs and
#     pooled holding-period decile alphas.
#   - Sample-source sentence in fn_text footnote updated to append
#     "performance-comparison subsample per flagged\_funds.xlsx".
#
# v1.0 (original):
#
# Carhart (1997) parametric + Kosowski, Timmermann, Wermers & White (2006,
# hereafter KTWW) bootstrap persistence tests on 36-month formation /
# 12-month holding non-overlapping cohorts. Implements the design specified
# in persistence_testing_methodology.docx.
#
# STRUCTURE:
#   - 8 cohorts total; cohort 8 (partial) excluded. 7 non-overlapping cohorts
#     for Panel A (full-sample, t-stat rank) and Panel B (full-sample, raw-
#     alpha rank, robustness). Panels C/D/E/F restrict to regime-consistent
#     cohort subsets.
#   - Active funds only, gross returns, Evans-corrected panel_incubation.
#   - Formation: 36 months, NW_LAG = 3, min 24 obs per methodology 2.3.
#   - Holding:   12 months, pooled across cohorts, NW_LAG = 12 (capped at
#                (T-1)/2 for small panels), equal-weighted decile portfolios.
#   - Bootstrap: B = 10000 iterations, shared tau(t) within each cohort to
#                preserve cross-sectional correlation (KTWW convention).
#                NA-propagation naturally handles within-holding deaths.
#
# REUSE / DEPENDENCIES:
#   - Consumes panel_incubation from the in-session data pipeline
#     (data_import_and_cleaning.R + flow_calculation.R must be run first).
#   - Does NOT re-import alpha_rolling.xlsx / alpha_fullperiod.xlsx because
#     those artefacts lack fund-level factor loadings and holding-period
#     residuals required here. Formation regressions are re-estimated on
#     each cohort's 36-month window -- cheap (~5s serial).
#   - Bootstrap caching via digest (same pattern as subperiod_analysis.R v1.2)
#     reduces re-run wall time from ~3 min to ~10 seconds.
#
# EFFICIENCY (key design choice):
#   The factor-component contribution to pseudo decile returns is invariant
#   across bootstrap iterations (loadings are fixed from formation). Only the
#   residual term depends on the resampling index tau. We therefore precompute
#   FacMat[i,t] = beta_i*MKT_t + s_i*SMB_t + h_i*HML_t + p_i*MOM_t once per
#   cohort, and at each iteration simply index ResMat[:, tau(t)] (column
#   permutation) and take row means within deciles. This reduces per-iteration
#   cost from O(funds x 12 x 4 multiplications) to O(funds x 12 indexing).
#
# OUTPUTS:
#   persistence_results.xlsx   - all panels' raw numeric results + audit
#                                (alpha, t, bootstrap quantiles, loadings,
#                                 moments, expense ratio) across 6 panels.
#   table_persistence.tex      - single longtable, 6 panels x 11 rows each,
#                                 small-font landscape-ready layout.
#   persistence_bootstrap_cache_{A..F}.rds - one cache per panel.
#
# REFERENCES:
#   Carhart, M.M. (1997). On persistence in mutual fund performance. JF.
#   Kosowski, Timmermann, Wermers & White (2006). Can mutual fund 'stars'
#     really pick stocks? JF 61, 2551-2595.
#   Fama & French (2010). Luck versus skill. JF 65, 1915-1947.
#   Evans (2010). Mutual fund incubation. JF 65, 1581-1611.
#
# Dependencies: dplyr, tidyr, lubridate, writexl, parallel, knitr,
#               kableExtra, stringr, digest
# =============================================================================

library(dplyr)
library(tidyr)
library(lubridate)
library(writexl)
library(parallel)
library(knitr)
library(kableExtra)
library(stringr)
library(digest)

# =============================================================================
# 0. CONFIGURATION
# =============================================================================

# Formation / holding window parameters (methodology Section 2.1 - 2.3)
FORM_MONTHS      <- 36L
HOLD_MONTHS      <- 12L
MIN_OBS_FORM     <- 24L
NW_LAG_FORM      <- 3L    # methodology Section 2.3
NW_LAG_HOLD      <- 12L   # methodology Section 3.4 (capped to (T-1)/2 in fits)
N_DECILES        <- 10L

# Bootstrap parameters
B_RUNS           <- 10000L
BOOT_SEED        <- 42L
N_CORES          <- max(1L, detectCores() - 1L)
USE_BOOT_CACHE   <- TRUE
BOOT_CACHE_DIR   <- file.path(".", paste0("cache_", Sys.info()[["nodename"]]))
dir.create(BOOT_CACHE_DIR, showWarnings = FALSE)  # no-op if already exists

# Cohort ranking dates -- end of year k; formation = 36m ending on the ranking
# date, holding = 12m starting the first of the following month. Cohort 8
# carries 2 holding-period months in the panel_incubation window (through
# Feb 2026) and is excluded from inference per methodology Section 2.2.
COHORT_DEFS <- data.frame(
  cohort_id     = 1:8,
  rank_date     = as.Date(c("1997-12-31", "2001-12-31", "2005-12-31",
                            "2009-12-31", "2013-12-31", "2017-12-31",
                            "2021-12-31", "2025-12-31")),
  hold_months   = c(12L, 12L, 12L, 12L, 12L, 12L, 12L, 2L),
  stringsAsFactors = FALSE
) %>%
  mutate(form_lo   = rank_date %m-% months(FORM_MONTHS - 1L) %>% floor_date("month"),
         form_hi   = rank_date %>% floor_date("month"),
         hold_lo   = (rank_date + 1) %>% floor_date("month"),
         hold_hi   = ((rank_date + 1) %>% floor_date("month")) %m+%
           months(hold_months - 1L))

USABLE_COHORTS <- COHORT_DEFS$cohort_id[COHORT_DEFS$hold_months == HOLD_MONTHS]

# Panel definitions -- Panel A/B use all 7 usable cohorts; C/D/E restrict by
# regime-based formation-period assignment; F uses the two boundary-straddling
# cohorts (3 and 5) per methodology Section 5.
PANELS <- list(
  A = list(label = "A",
           title = "Panel A: Full sample, t-statistic ranking",
           cohorts = USABLE_COHORTS, rank_var = "t_stat"),
  B = list(label = "B",
           title = "Panel B: Full sample, raw-alpha ranking (robustness)",
           cohorts = USABLE_COHORTS, rank_var = "alpha"),
  C = list(label = "C",
           title = "Panel C: P1 sub-period (Jan 1995--Dec 2005)",
           cohorts = c(1L, 2L, 3L), rank_var = "t_stat"),
  D = list(label = "D",
           title = "Panel D: P2 sub-period (Jan 2006--Sep 2011)",
           cohorts = c(4L),           rank_var = "t_stat"),
  E = list(label = "E",
           title = "Panel E: P3 sub-period (Oct 2011--Feb 2026)",
           cohorts = c(5L, 6L, 7L),   rank_var = "t_stat"),
  F = list(label = "F",
           title = "Panel F: Cross-regime (boundary-straddling cohorts 3, 5)",
           cohorts = c(3L, 5L),       rank_var = "t_stat")
)

# =============================================================================
# 1. HELPERS (kernels mirror alpha_estimation.R v2.5 / subperiod_analysis.R v1.2)
# =============================================================================

fast_ols <- function(y, X) {
  tryCatch({
    XtX  <- crossprod(X)
    beta <- solve(XtX, crossprod(X, y))
    e    <- as.vector(y - X %*% beta)
    n    <- length(y); k <- ncol(X)
    ss_r <- sum(e^2); ss_t <- sum((y - mean(y))^2)
    list(beta = as.vector(beta), e = e,
         r2 = 1 - ss_r / ss_t,
         adj_r2 = 1 - (ss_r / (n - k)) / (ss_t / (n - 1)),
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

# Skewness / excess kurtosis (biased moment estimators, matches e1071 default)
sk_moment <- function(x) {
  x <- x[!is.na(x)]; n <- length(x)
  if (n < 4L) return(c(skew = NA_real_, exkurt = NA_real_))
  m  <- mean(x); s2 <- mean((x - m)^2)
  if (s2 <= 0) return(c(skew = NA_real_, exkurt = NA_real_))
  s  <- sqrt(s2)
  c(skew   = mean((x - m)^3) / s^3,
    exkurt = mean((x - m)^4) / s^4 - 3)
}

# Jarque-Bera p-value (manual; no tseries dependency)
jb_pvalue <- function(x) {
  mm <- sk_moment(x); n  <- sum(!is.na(x))
  if (any(is.na(mm)) || n < 4L) return(NA_real_)
  JB <- n * (mm["skew"]^2 / 6 + mm["exkurt"]^2 / 24)
  pchisq(JB, df = 2, lower.tail = FALSE) %>% as.numeric()
}

# Formatting
fmt <- function(x, d = 3) {
  ifelse(is.na(x) | is.nan(x), "--",
         formatC(round(as.numeric(x), d), format = "f", digits = d))
}
fmt1 <- function(x) fmt(x, 1)
fmt2 <- function(x) fmt(x, 2)

# Significance stars on a formatted value (|t| thresholds 1.645/1.960/2.576)
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

# LaTeX cleaners (lifted from portfolio_sorts.R)
clean_latex <- function(x, resize = TRUE, small = FALSE) {
  # Phase 2.4 SBE consistency pass (May 2026).
  # Replaces kableExtra's default output with the savebox+minipage pattern
  # for floating tables, or @{\extracolsep{\fill}} stretched longtable layout.
  # Args resize/small retained for signature compatibility, no effect now.

  find_brace_end <- function(s, brace_start) {
    depth <- 0L
    n <- nchar(s)
    for (i in brace_start:n) {
      ch <- substr(s, i, i)
      if (ch == "{") depth <- depth + 1L
      else if (ch == "}") {
        depth <- depth - 1L
        if (depth == 0L) return(i)
      }
    }
    NA_integer_
  }

  # Generic kableExtra fixes.
  x <- gsub("\\\\end[{]threeparttable[}][}]", "\\\\end{threeparttable}", x)
  x <- gsub("\\begin{table}[!h]", "\\begin{table}[H]", x, fixed = TRUE)

  # ---- Longtable branch (Phase 2.3 stretching + Phase 2.4 12pt gap) -------
  if (grepl("\\\\begin\\{longtable\\}", x)) {
    if (!grepl("@\\{\\\\extracolsep\\{\\\\fill\\}\\}", x, perl = TRUE)) {
      x <- sub(
        "(\\\\begin\\{longtable\\}(?:\\[[^]]*\\])?)\\{",
        "\\1{@{\\\\extracolsep{\\\\fill}}",
        x, perl = TRUE
      )
    }
    cap_pattern <- paste0(
      "(\\\\caption(?:\\[[^]]*\\])?",
      "\\{(?:[^{}]|\\{(?:[^{}]|\\{[^{}]*\\})*\\})*\\}",
      ")\\\\\\\\(?!\\[)"
    )
    x <- gsub(cap_pattern, "\\1\\\\\\\\[12pt]", x, perl = TRUE)
    return(x)
  }

  # ---- Floating-table branch (savebox + minipage) -------------------------
  cap_anchor <- regexpr("\\\\caption(?=\\{)", x, perl = TRUE)
  if (cap_anchor == -1) return(x)
  cap_start <- as.integer(cap_anchor)
  brace_start <- cap_start + attr(cap_anchor, "match.length")
  cap_end <- find_brace_end(x, brace_start)
  if (is.na(cap_end)) return(x)
  caption_text <- substring(x, cap_start, cap_end)

  tab_m <- regexpr(
    "(?s)\\\\begin\\{tabular\\}(?:\\[[^]]*\\])?\\{[^}]*\\}.*?\\\\end\\{tabular\\}",
    x, perl = TRUE
  )
  if (tab_m == -1) return(x)
  tabular <- substring(x, tab_m, tab_m + attr(tab_m, "match.length") - 1L)

  notes_m <- regexpr(
    "(?s)\\\\begin\\{tablenotes\\}\\s*\\\\item\\s+(.*?)\\\\end\\{tablenotes\\}",
    x, perl = TRUE
  )
  notes <- ""
  if (notes_m != -1) {
    full_block <- substring(x, notes_m, notes_m + attr(notes_m, "match.length") - 1L)
    inner <- sub("^(?s)\\\\begin\\{tablenotes\\}\\s*\\\\item\\s+", "",
                 full_block, perl = TRUE)
    inner <- sub("(?s)\\\\end\\{tablenotes\\}\\s*$", "", inner, perl = TRUE)
    notes <- trimws(inner)
  }

  out <- paste0(
    "\\begin{table}[H]\n",
    "\\centering\n",
    "\\sbox{\\tabletempbox}{%\n",
    "\\footnotesize\n",
    tabular, "%\n",
    "}\n",
    "\\setlength{\\tabletempwidth}{\\wd\\tabletempbox}\n",
    "\\ifdim\\tabletempwidth>\\linewidth\\setlength{\\tabletempwidth}{\\linewidth}\\fi\n",
    "\\begin{minipage}{\\tabletempwidth}\n",
    "\\captionsetup{width=\\linewidth}\n",
    caption_text, "\n",
    "\\ifdim\\wd\\tabletempbox>\\linewidth\n",
    "  \\resizebox{\\linewidth}{!}{\\usebox{\\tabletempbox}}\n",
    "\\else\n",
    "  \\usebox{\\tabletempbox}\n",
    "\\fi\n"
  )
  if (nzchar(notes)) {
    out <- paste0(out,
      "\\par\\medskip\n",
      "\\begin{singlespace}\\footnotesize\\noindent\n",
      notes, "\n",
      "\\end{singlespace}\n"
    )
  }
  out <- paste0(out, "\\end{minipage}\n\\end{table}\n")
  out
}

# Append a note paragraph AFTER \end{longtable} (portfolio_sorts.R pattern).
# Append a note paragraph AFTER \end{longtable} (not inside the body).
# Format mirrors clean_latex() floating-table branch: \begin{singlespace}
# wrapper ensures the note renders single-spaced (the document default is
# \doublespacing via setspace). No "Notes:" prefix and no \textit{} wrapping —
# matches Table 4.8 (BSW decomposition) caption style per SBE convention.
longtable_note <- function(s, note) {
  note_para <- paste0(
    "\\vspace{-\\baselineskip}\n",
    "\\begin{singlespace}\\footnotesize\\noindent\n", note, "\n",
    "\\end{singlespace}\n\n"
  )
  parts <- strsplit(s, "\\end{longtable}", fixed = TRUE)[[1]]
  paste0(parts[1], "\\end{longtable}\n", note_para,
         if (length(parts) > 1)
           paste0(parts[-1], collapse = "\\end{longtable}") else "")
}

# Wrap longtable in footnotesize + tight tabcolsep.
wrap_lt_small <- function(s, tabcolsep = "2pt") {
  opener <- paste0("{\\setlength{\\tabcolsep}{", tabcolsep,
                   "}\\footnotesize\n\\begin{longtable}")
  parts_open <- strsplit(s, "\\begin{longtable}", fixed = TRUE)[[1]]
  s <- paste0(parts_open[1], opener,
              if (length(parts_open) > 1) parts_open[2] else "")
  parts_close <- strsplit(s, "\\end{longtable}", fixed = TRUE)[[1]]
  paste0(parts_close[1], "\\end{longtable}\n}",
         if (length(parts_close) > 1)
           paste0(parts_close[-1], collapse = "\\end{longtable}") else "")
}

# =============================================================================
# 2. PANEL RESTRICTION & FACTOR SERIES
# =============================================================================
cat("=== 1. Data preparation ===\n")

if (!exists("panel_incubation"))
  stop("panel_incubation not found in session. Run data_import_and_cleaning.R ",
       "(and flow_calculation.R) first.")

# Active funds only, gross returns, per methodology Section 2.3.
# v1.1: filter(!excluded_perf) restricts to the performance-comparison
# subsample (flagged_funds.xlsx Step 8c).
ap <- panel_incubation %>%
  filter(!excluded_perf) %>%
  rename(mkt_rf = MKT_RF, smb = SMB, hml = HML, mom = MOM, rf = RF,
         exp_r  = Expense_Ratio) %>%
  mutate(excess_ret = ret_gross - rf,
         exp_r      = suppressWarnings(as.numeric(exp_r)),
         ap_group   = gsub("Agtive", "Active", ap_group),
         # Normalise date to first-of-month. Cohort bounds and hold_calendar
         # are month-start; if the source panel stores end-of-month dates
         # (LSEG Excel header convention varies), naive date comparisons
         # silently drop observations at the last day of the boundary month.
         # This alignment is idempotent if dates are already month-start.
         date       = floor_date(date, "month")) %>%
  filter(ap_group == "Active",
         !is.na(excess_ret), !is.na(mkt_rf), !is.na(smb),
         !is.na(hml), !is.na(mom)) %>%
  select(Ticker, date, excess_ret, ret_gross, mkt_rf, smb, hml, mom, rf,
         exp_r) %>%
  arrange(Ticker, date)

# Factor time series (common to all regressions)
factors_ts <- ap %>%
  distinct(date, mkt_rf, smb, hml, mom, rf) %>%
  arrange(date)

cat("  Active fund-months:", nrow(ap),
    " | Active funds:",      n_distinct(ap$Ticker),
    " | Factor months:",     nrow(factors_ts), "\n")

# Sanity: last available month must cover the last usable holding window
last_obs <- max(factors_ts$date)
for (cid in USABLE_COHORTS) {
  cd <- COHORT_DEFS[COHORT_DEFS$cohort_id == cid, ]
  if (last_obs < cd$hold_hi)
    warning(sprintf("Cohort %d holding ends %s but factors end %s.",
                    cid, format(cd$hold_hi), format(last_obs)))
}

# =============================================================================
# 3. PER-COHORT FORMATION REGRESSION + RANK ASSIGNMENT
# =============================================================================
# For each cohort: run Carhart 4-factor OLS+NW on each fund's 36-month
# formation window; retain loadings, formation alpha, alpha t-stat, expense
# ratio. Then compute the 12 projection residuals for the holding window
# using formation loadings (KTWW Step 3). Returns one list per cohort.
# =============================================================================
cat("=== 2. Per-cohort formation regressions ===\n")

estimate_cohort <- function(cd) {
  cat(sprintf("  Cohort %d: form %s--%s, hold %s--%s\n",
              cd$cohort_id, format(cd$form_lo), format(cd$form_hi),
              format(cd$hold_lo), format(cd$hold_hi)))
  
  # Formation panel (must have >= MIN_OBS_FORM within [form_lo, form_hi])
  form_panel <- ap %>% filter(date >= cd$form_lo, date <= cd$form_hi)
  hold_panel <- ap %>% filter(date >= cd$hold_lo, date <= cd$hold_hi)
  
  form_by_fund <- split(form_panel, form_panel$Ticker)
  
  # Per-fund formation regression
  run_form <- function(tk) {
    d <- form_by_fund[[tk]]; n <- nrow(d)
    if (n < MIN_OBS_FORM) return(NULL)
    y <- d$excess_ret
    X <- cbind(1, d$mkt_rf, d$smb, d$hml, d$mom)
    fit <- fast_ols(y, X); if (is.null(fit)) return(NULL)
    se  <- nw_se(X, fit$e, NW_LAG_FORM)
    t_alpha <- fit$beta[1] / se[1]
    data.frame(
      Ticker    = tk,
      n_form    = n,
      alpha_f   = fit$beta[1],       # monthly, decimal (pre-annualised)
      t_alpha_f = t_alpha,
      b_mkt     = fit$beta[2],
      b_smb     = fit$beta[3],
      b_hml     = fit$beta[4],
      b_mom     = fit$beta[5],
      exp_r     = d$exp_r[1],
      stringsAsFactors = FALSE
    )
  }
  form_est <- bind_rows(lapply(names(form_by_fund), run_form))
  if (nrow(form_est) == 0L) return(NULL)
  
  # Holding-period residual projection: for each fund that (a) has formation
  # estimates and (b) has at least one holding-period obs, compute 12
  # projection residuals using formation loadings. Missing-month residuals
  # remain NA (propagates correctly through portfolio aggregation + bootstrap).
  hold_calendar <- seq.Date(cd$hold_lo, cd$hold_hi, by = "month")
  stopifnot(length(hold_calendar) == HOLD_MONTHS)
  fac_hold <- factors_ts %>%
    filter(date %in% hold_calendar) %>%
    arrange(date)
  if (nrow(fac_hold) != HOLD_MONTHS) {
    warning(sprintf("Cohort %d: only %d holding-period factor months found; skipping.",
                    cd$cohort_id, nrow(fac_hold)))
    return(NULL)
  }
  
  hold_by_fund <- split(hold_panel, hold_panel$Ticker)
  ticker_set   <- intersect(form_est$Ticker, names(hold_by_fund))
  # Funds with formation estimates but no holding observations at all are
  # dropped here (cannot enter the portfolio at any time in the holding
  # window, so they contribute no inference).
  form_est     <- form_est[form_est$Ticker %in% ticker_set, , drop = FALSE]
  
  n_funds <- nrow(form_est)
  # Factor-component matrix: [n_funds x HOLD_MONTHS] fixed across bootstrap
  # iterations. Entry i,t = b_mkt_i * MKT_t + b_smb_i * SMB_t + ...
  fac_comp <- as.matrix(form_est[, c("b_mkt", "b_smb", "b_hml", "b_mom")]) %*%
    t(as.matrix(fac_hold[, c("mkt_rf", "smb", "hml", "mom")]))
  rownames(fac_comp) <- form_est$Ticker
  colnames(fac_comp) <- as.character(fac_hold$date)
  
  # Residual matrix: [n_funds x HOLD_MONTHS]; NA where the fund lacks an
  # observation in the corresponding calendar month.
  res_mat <- matrix(NA_real_, nrow = n_funds, ncol = HOLD_MONTHS,
                    dimnames = list(form_est$Ticker, as.character(hold_calendar)))
  # Excess-return matrix (used for actual-decile portfolio aggregation)
  exr_mat <- matrix(NA_real_, nrow = n_funds, ncol = HOLD_MONTHS,
                    dimnames = list(form_est$Ticker, as.character(hold_calendar)))
  
  for (i in seq_len(n_funds)) {
    tk <- form_est$Ticker[i]
    hd <- hold_by_fund[[tk]]
    if (is.null(hd) || nrow(hd) == 0L) next
    # Align by date
    idx <- match(as.character(hd$date), as.character(hold_calendar))
    ok  <- !is.na(idx)
    if (!any(ok)) next
    # Excess return in holding month
    exr_mat[i, idx[ok]] <- hd$excess_ret[ok]
    # Projection residual = excess return - factor component
    res_mat[i, idx[ok]] <- hd$excess_ret[ok] - fac_comp[i, idx[ok]]
  }
  
  list(
    cohort_id   = cd$cohort_id,
    rank_date   = cd$rank_date,
    form_lo     = cd$form_lo, form_hi = cd$form_hi,
    hold_lo     = cd$hold_lo, hold_hi = cd$hold_hi,
    hold_dates  = hold_calendar,
    form_est    = form_est,       # per-fund formation loadings, alpha, t, exp_r
    fac_hold    = fac_hold,        # factor series over holding window
    fac_comp    = fac_comp,        # [n_funds x 12] factor-based return
    res_mat     = res_mat,         # [n_funds x 12] holding residuals
    exr_mat     = exr_mat          # [n_funds x 12] actual excess returns
  )
}

cohorts <- lapply(seq_len(nrow(COHORT_DEFS)), function(i) {
  if (!(COHORT_DEFS$cohort_id[i] %in% USABLE_COHORTS)) return(NULL)
  estimate_cohort(COHORT_DEFS[i, , drop = FALSE])
})
names(cohorts) <- paste0("C", COHORT_DEFS$cohort_id)
cohorts <- cohorts[!sapply(cohorts, is.null)]

# =============================================================================
# 4. DECILE ASSIGNMENT PER PANEL / COHORT
# =============================================================================
# For each (panel, cohort): rank funds by the panel's ranking variable
# (t-stat for A/C/D/E/F; raw alpha for B), break into deciles (D1 = top).
# The "top fund" and "bottom fund" are additionally tracked as decile-less
# references per methodology Section 2.4 (KTWW), but deciles are the primary
# analysis unit and are what the LaTeX table reports.
# =============================================================================

assign_deciles <- function(coh_obj, rank_var) {
  v <- if (rank_var == "t_stat") coh_obj$form_est$t_alpha_f
  else                      coh_obj$form_est$alpha_f
  ok <- !is.na(v)
  dec <- rep(NA_integer_, length(v))
  if (sum(ok) < N_DECILES) return(dec)
  # D1 = highest rank; so descending rank gives D1 first.
  # ntile with descending order: use -v for the ranking.
  dec[ok] <- as.integer(dplyr::ntile(-v[ok], N_DECILES))
  dec
}

# =============================================================================
# 5. ACTUAL HOLDING-PERIOD DECILE PORTFOLIO ALPHAS
# =============================================================================
# For a given panel:
#   (a) collect deciles across panel's cohorts;
#   (b) per decile, stack pooled holding-period equal-weighted excess returns
#       across cohorts into a single time series of length 12 * K_panel;
#   (c) run Carhart 4-factor OLS + NW SE to obtain alpha and all loadings.
# Spread return (D1 - D10) is computed by row-wise subtraction of the two
# decile series; because both are already excess returns (by construction of
# res/exr matrices), subtraction cancels RF and the spread should be regressed
# on factors directly (no further RF treatment).
# =============================================================================

# Aggregate a decile's actual excess return series across cohorts in panel.
# Returns a tibble {cohort_id, hold_idx, date, ret_ew_ex, n_funds_alive,
# exp_ratio_avg}. Funds with NA at a given month are excluded from the mean.
build_actual_decile_series <- function(panel, decile, cohorts_by_id) {
  pieces <- list()
  for (cid in panel$cohorts) {
    coh <- cohorts_by_id[[as.character(cid)]]
    if (is.null(coh)) next
    dec <- assign_deciles(coh, panel$rank_var)
    in_dec <- which(dec == decile)
    if (length(in_dec) == 0L) next
    # Subset excess-return matrix to the decile members
    em <- coh$exr_mat[in_dec, , drop = FALSE]
    er <- coh$form_est$exp_r[in_dec]
    # Monthly cross-sectional mean (equal-weight, NA-exclusion = live-members)
    mu <- colMeans(em, na.rm = TRUE)
    n_alive <- colSums(!is.na(em))
    pieces[[length(pieces) + 1L]] <- tibble(
      cohort_id       = cid,
      hold_idx        = seq_len(HOLD_MONTHS),
      date            = coh$hold_dates,
      ret_ew_ex       = mu,
      n_funds_alive   = as.integer(n_alive),
      exp_ratio_avg   = if (length(er)) mean(er, na.rm = TRUE) else NA_real_
    )
  }
  bind_rows(pieces)
}

# Carhart regression on the pooled decile excess-return series.
# subtract_rf = FALSE because the series is already excess (we constructed it
# as fund excess returns then averaged). Returns all coefficients + NW t-stats.
run_carhart_on_decile <- function(series, factors_ts) {
  d <- series %>%
    left_join(factors_ts, by = "date") %>%
    filter(!is.na(ret_ew_ex), !is.na(mkt_rf))
  n <- nrow(d)
  na_row <- data.frame(
    n_months = n,
    alpha_m = NA_real_, t_alpha = NA_real_, p_alpha_1t = NA_real_,
    b_mkt = NA_real_, t_mkt = NA_real_,
    b_smb = NA_real_, t_smb = NA_real_,
    b_hml = NA_real_, t_hml = NA_real_,
    b_mom = NA_real_, t_mom = NA_real_,
    adj_r2 = NA_real_,
    exret_mean = NA_real_, exret_sd = NA_real_,
    skew = NA_real_, exkurt = NA_real_, jb_p = NA_real_
  )
  if (n < 12L) return(na_row)
  y  <- d$ret_ew_ex
  X  <- cbind(1, d$mkt_rf, d$smb, d$hml, d$mom)
  fit <- fast_ols(y, X); if (is.null(fit)) return(na_row)
  se  <- nw_se(X, fit$e, NW_LAG_HOLD)
  t_alpha <- fit$beta[1] / se[1]
  # One-tailed parametric p-value (right tail if alpha > 0, left if < 0).
  # Reported directly as "the chance that the observed alpha's sign arose
  # under the zero-true-alpha null", two-sided rescaled to one-sided in the
  # direction of the point estimate (methodology Section 3.4).
  p_1t <- pnorm(-abs(t_alpha))
  mm   <- sk_moment(y)
  list(
    n_months = n,
    alpha_m  = fit$beta[1], t_alpha = t_alpha, p_alpha_1t = p_1t,
    b_mkt = fit$beta[2], t_mkt = fit$beta[2] / se[2],
    b_smb = fit$beta[3], t_smb = fit$beta[3] / se[3],
    b_hml = fit$beta[4], t_hml = fit$beta[4] / se[4],
    b_mom = fit$beta[5], t_mom = fit$beta[5] / se[5],
    adj_r2     = fit$adj_r2,
    exret_mean = mean(y), exret_sd = sd(y),
    skew       = mm["skew"], exkurt = mm["exkurt"],
    jb_p       = jb_pvalue(y)
  ) %>% as.data.frame()
}

# Build actual stats for all 10 deciles + spread (D1 - D10) for one panel.
build_actual_panel <- function(panel, cohorts_by_id, factors_ts) {
  out <- list()
  series_by_dec <- vector("list", N_DECILES)
  n_funds_per_dec <- integer(N_DECILES)
  exp_ratio_per_dec <- numeric(N_DECILES)
  
  # NA-filled stats row with the correct column schema. Emitted whenever a
  # decile has no valid pooled series (e.g. a small-cohort panel where the
  # single cohort yields too few eligible formation alphas for ntile to
  # produce 10 groups). Guarantees `estimates` always carries the `decile`
  # column downstream, so arrange(decile) and the xlsx/LaTeX writers work
  # uniformly across panels.
  empty_stats <- function() data.frame(
    n_months   = 0L,
    alpha_m    = NA_real_, t_alpha = NA_real_, p_alpha_1t = NA_real_,
    b_mkt = NA_real_, t_mkt = NA_real_,
    b_smb = NA_real_, t_smb = NA_real_,
    b_hml = NA_real_, t_hml = NA_real_,
    b_mom = NA_real_, t_mom = NA_real_,
    adj_r2     = NA_real_,
    exret_mean = NA_real_, exret_sd = NA_real_,
    skew = NA_real_, exkurt = NA_real_, jb_p = NA_real_
  )
  
  for (d in seq_len(N_DECILES)) {
    s <- build_actual_decile_series(panel, d, cohorts_by_id)
    series_by_dec[[d]] <- s
    if (nrow(s) == 0L) {
      # No members in this decile across all panel cohorts.
      est <- empty_stats()
      est$decile    <- d
      est$n_funds   <- NA_real_
      est$exp_ratio <- NA_real_
      out[[paste0("D", d)]] <- est
      next
    }
    # Count unique fund-membership (sum of alive-counts weighted by cohort count)
    n_funds_per_dec[d] <- sum(s$n_funds_alive, na.rm = TRUE) /
      max(length(unique(s$cohort_id)), 1L)
    # Average constituent expense ratio across cohorts
    exp_ratio_per_dec[d] <- mean(s$exp_ratio_avg, na.rm = TRUE)
    est <- run_carhart_on_decile(s, factors_ts)
    est$decile     <- d
    est$n_funds    <- n_funds_per_dec[d]
    est$exp_ratio  <- exp_ratio_per_dec[d]
    out[[paste0("D", d)]] <- est
  }
  
  # Spread D1 - D10: emit NA row if either end is missing, so `estimates`
  # always has an 11-th row (decile == 11L) for the spread.
  s1  <- series_by_dec[[1]]
  s10 <- series_by_dec[[N_DECILES]]
  if (!is.null(s1) && nrow(s1) > 0L && !is.null(s10) && nrow(s10) > 0L) {
    sp <- inner_join(
      s1  %>% select(date, r1  = ret_ew_ex),
      s10 %>% select(date, r10 = ret_ew_ex),
      by = "date"
    ) %>% transmute(cohort_id = NA_integer_, hold_idx = NA_integer_,
                    date, ret_ew_ex = r1 - r10,
                    n_funds_alive = NA_integer_, exp_ratio_avg = NA_real_)
    est_sp <- run_carhart_on_decile(sp, factors_ts)
  } else {
    est_sp <- empty_stats()
  }
  est_sp$decile    <- 11L  # spread marker
  est_sp$n_funds   <- NA_real_
  est_sp$exp_ratio <- NA_real_
  out[["D1-D10"]]  <- est_sp
  
  list(estimates = bind_rows(out),
       series_by_dec = series_by_dec)
}

# =============================================================================
# 6. BOOTSTRAP NULL DISTRIBUTION (KTWW Step 5)
# =============================================================================
# Vectorised per-iteration logic:
#   - Each cohort draws tau_k in {1..HOLD_MONTHS}^HOLD_MONTHS (i.i.d. with
#     replacement). The SAME tau_k is applied to all funds in cohort k,
#     preserving cross-sectional residual correlation.
#   - Pseudo fund excess return at calendar month t, cohort k:
#       r^b_{i,t} = fac_comp[i,t] + res_mat[i, tau_k(t)]
#     NA entries in res_mat (funds dead at index tau_k(t)) propagate to NA
#     and are excluded from the decile mean.
#   - Within each decile d: mean across fund rows at each calendar month t.
#   - Stack decile series across cohorts into one 12 * K_panel vector.
#   - Run Carhart with NW SE -> record t_alpha^b for decile d.
# =============================================================================

# Inputs prepared per panel: a list of cohort blocks, each providing:
#   fac_comp[n_funds x HOLD_MONTHS]
#   res_mat [n_funds x HOLD_MONTHS]
#   dec_idx [n_funds]   (decile membership in 1..10, NA = skip)
#   fac_ts  [HOLD_MONTHS x 5]   (date, mkt_rf, smb, hml, mom)
#
# Bootstrap kernel: one iteration returns a numeric vector of length
# (N_DECILES + 1) of simulated t_alpha per decile (plus spread D1 - D10).

# Worker: produces t_alpha^b for all deciles + spread in one panel.
# bs_blocks is a list, one element per cohort in the panel, each a list with:
#   fac_comp, res_mat, dec_idx, fac_mat (factors as numeric matrix [12 x 5])
one_boot_iter <- function(run_id, bs_blocks, nw_lag_hold, min_obs_hold) {
  # For each cohort: draw tau_k, reindex res_mat columns, build pseudo
  # excess-return matrix = fac_comp + res_resampled.
  K <- length(bs_blocks)
  # Use ncol() per block to avoid relying on an exported global.
  H_each  <- vapply(bs_blocks, function(b) ncol(b$res_mat), integer(1))
  total_T <- sum(H_each)
  n_dec   <- 10L   # decile count; stable design choice
  # Pre-allocate: for each decile, we'll build a concatenated vector of
  # length sum(H_each) (one per calendar position in panel).
  dec_series <- vector("list", n_dec)
  for (d in seq_len(n_dec)) dec_series[[d]] <- numeric(total_T)
  factor_stack <- matrix(0, nrow = total_T, ncol = 4)
  
  offset <- 0L
  for (k in seq_len(K)) {
    blk <- bs_blocks[[k]]
    H   <- H_each[k]
    tau <- sample.int(H, size = H, replace = TRUE)
    pseudo <- blk$fac_comp + blk$res_mat[, tau, drop = FALSE]
    for (d in seq_len(n_dec)) {
      rows <- which(blk$dec_idx == d)
      if (length(rows) == 0L) {
        dec_series[[d]][offset + seq_len(H)] <- NA_real_
      } else if (length(rows) == 1L) {
        dec_series[[d]][offset + seq_len(H)] <- pseudo[rows, ]
      } else {
        dec_series[[d]][offset + seq_len(H)] <- colMeans(pseudo[rows, , drop = FALSE],
                                                         na.rm = TRUE)
      }
    }
    factor_stack[offset + seq_len(H), ] <- as.matrix(blk$fac_mat)
    offset <- offset + H
  }
  
  # Spread D1 - D10
  spread_series <- dec_series[[1]] - dec_series[[n_dec]]
  
  # Regress each decile (and spread) on Carhart factors
  t_sim <- numeric(n_dec + 1L)
  for (d in seq_len(n_dec + 1L)) {
    y <- if (d <= n_dec) dec_series[[d]] else spread_series
    ok <- is.finite(y) & is.finite(factor_stack[, 1])
    if (sum(ok) < min_obs_hold) { t_sim[d] <- NA_real_; next }
    yk <- y[ok]; Xk <- cbind(1, factor_stack[ok, , drop = FALSE])
    Tn <- length(yk); L <- min(nw_lag_hold, Tn - 1L)
    t_sim[d] <- tryCatch({
      XtX_inv <- solve(crossprod(Xk))
      beta    <- XtX_inv %*% crossprod(Xk, yk)
      e       <- as.vector(yk - Xk %*% beta)
      scores  <- Xk * e
      S       <- crossprod(scores) / Tn
      if (L > 0L) {
        for (j in seq_len(L)) {
          w  <- 1 - j / (nw_lag_hold + 1)
          Gj <- crossprod(scores[(j + 1):Tn, , drop = FALSE],
                          scores[1:(Tn - j),  , drop = FALSE]) / Tn
          S  <- S + w * (Gj + t(Gj))
        }
      }
      vc <- (Tn * XtX_inv %*% S %*% XtX_inv)[1, 1]
      beta[1] / sqrt(max(vc, 0))
    }, error = function(e) NA_real_)
  }
  t_sim
}

# Build per-panel bootstrap blocks (one element per cohort).
build_bs_blocks <- function(panel, cohorts_by_id) {
  blks <- list()
  for (cid in panel$cohorts) {
    coh <- cohorts_by_id[[as.character(cid)]]
    if (is.null(coh)) next
    dec <- assign_deciles(coh, panel$rank_var)
    blks[[length(blks) + 1L]] <- list(
      cohort_id = cid,
      fac_comp  = coh$fac_comp,
      res_mat   = coh$res_mat,
      dec_idx   = dec,
      # factor matrix for the holding window, 4 columns: MKT_RF, SMB, HML, MOM
      fac_mat   = as.matrix(coh$fac_hold[, c("mkt_rf", "smb", "hml", "mom")])
    )
  }
  blks
}

# Run the full B-iteration bootstrap (cached). Returns matrix
# [B_RUNS x (N_DECILES + 1)] of simulated t_alpha per decile + spread.
run_bootstrap <- function(panel, cohorts_by_id, B = B_RUNS) {
  bs_blocks <- build_bs_blocks(panel, cohorts_by_id)
  if (length(bs_blocks) == 0L) {
    warning(sprintf("Panel %s: no cohorts usable for bootstrap.", panel$label))
    return(matrix(NA_real_, nrow = 0L, ncol = N_DECILES + 1L))
  }
  
  # Cache key: invalidated by any change to panel composition, residuals,
  # deciles, or bootstrap parameters.
  cache_key <- digest::digest(list(
    panel_label = panel$label,
    rank_var    = panel$rank_var,
    cohorts     = panel$cohorts,
    # hash of the decile assignments + residual matrix across cohorts
    blocks      = lapply(bs_blocks, function(b)
      list(dec = b$dec_idx,
           fac_comp = round(b$fac_comp, 8),
           res_mat  = round(b$res_mat,  8),
           fac_mat  = round(b$fac_mat,  8))),
    B           = B,
    seed        = BOOT_SEED,
    nw_hold     = NW_LAG_HOLD,
    n_dec       = N_DECILES
  ))
  cache_file <- file.path(BOOT_CACHE_DIR,
                          sprintf("persistence_bootstrap_cache_%s.rds",
                                  panel$label))
  if (USE_BOOT_CACHE && file.exists(cache_file)) {
    cached <- tryCatch(readRDS(cache_file), error = function(e) NULL)
    if (!is.null(cached) && identical(cached$key, cache_key)) {
      cat(sprintf("  [cache hit] Panel %s: loaded from %s\n",
                  panel$label, cache_file))
      return(cached$sim_matrix)
    }
    cat(sprintf("  [cache miss] Panel %s: key changed; recomputing.\n",
                panel$label))
  }
  
  cat(sprintf("  Bootstrapping panel %s (B = %d, cores = %d) ...\n",
              panel$label, B, N_CORES))
  t0 <- Sys.time()
  
  # Workers
  cl <- makeCluster(N_CORES)
  clusterExport(cl,
                c("bs_blocks", "one_boot_iter", "N_DECILES",
                  "HOLD_MONTHS", "NW_LAG_HOLD"),
                envir = environment())
  clusterSetRNGStream(cl, BOOT_SEED)
  sim_list <- parLapply(cl, seq_len(B), one_boot_iter,
                        bs_blocks, NW_LAG_HOLD, min_obs_hold = 12L)
  stopCluster(cl)
  
  sim_matrix <- do.call(rbind, sim_list)
  cat(sprintf("  Panel %s wall time: %.1f sec\n", panel$label,
              as.numeric(difftime(Sys.time(), t0, units = "secs"))))
  
  if (USE_BOOT_CACHE) {
    saveRDS(list(key = cache_key, sim_matrix = sim_matrix), cache_file)
    cat(sprintf("  [cached] Saved panel %s to %s\n",
                panel$label, cache_file))
  }
  sim_matrix
}

# =============================================================================
# 7. ASSEMBLE PER-PANEL RESULTS
# =============================================================================
# For each panel:
#   - compute actual decile + spread Carhart estimates,
#   - run bootstrap (cached),
#   - compute right/left-tail bootstrap p-values relative to actual t,
#   - assemble one data.frame with all columns needed for the LaTeX table.
# =============================================================================
cat("\n=== 3. Per-panel estimation + bootstrap ===\n")

# Index cohorts by id for O(1) lookup (keys are character cohort_id).
cohorts_by_id <- list()
for (nm in names(cohorts)) {
  cid <- cohorts[[nm]]$cohort_id
  cohorts_by_id[[as.character(cid)]] <- cohorts[[nm]]
}

results_per_panel <- list()

for (pnm in names(PANELS)) {
  panel <- PANELS[[pnm]]
  cat(sprintf("\n--- Panel %s: %s ---\n", panel$label, panel$title))
  cat(sprintf("  Cohorts in panel: %s\n",
              paste(panel$cohorts, collapse = ", ")))
  
  # Diagnostic: per-cohort fund count with valid formation-period ranking
  # variable, and distribution across deciles after assignment. Helps pin
  # down a Panel D-style insufficient-funds situation before the bootstrap.
  for (cid in panel$cohorts) {
    coh <- cohorts_by_id[[as.character(cid)]]
    if (is.null(coh)) {
      cat(sprintf("    Cohort %d: MISSING (estimate_cohort returned NULL)\n", cid))
      next
    }
    dec <- assign_deciles(coh, panel$rank_var)
    n_valid <- sum(!is.na(dec))
    dec_counts <- if (n_valid > 0L) table(dec, useNA = "no") else integer(0)
    cat(sprintf("    Cohort %d: %d valid-ranking funds; decile sizes: %s\n",
                cid, n_valid,
                if (length(dec_counts) == 0L) "(all NA -- deciles not formed)"
                else paste(sprintf("%d=%d", as.integer(names(dec_counts)),
                                   as.integer(dec_counts)), collapse = " ")))
  }
  
  # 7a. Actual decile estimates
  act <- build_actual_panel(panel, cohorts_by_id, factors_ts)
  est <- act$estimates
  
  # 7b. Bootstrap null distribution
  sim <- run_bootstrap(panel, cohorts_by_id)
  
  # 7c. Bootstrap p-values for each decile + spread (column order: D1..D10, Spread)
  est$p_boot_right <- NA_real_
  est$p_boot_left  <- NA_real_
  if (nrow(sim) > 0L) {
    for (d in seq_len(N_DECILES + 1L)) {
      dec_id <- if (d <= N_DECILES) d else 11L
      actual_t <- est$t_alpha[est$decile == dec_id]
      if (length(actual_t) == 0L || is.na(actual_t)) next
      sim_col <- sim[, d]
      sim_col <- sim_col[is.finite(sim_col)]
      if (length(sim_col) == 0L) next
      est$p_boot_right[est$decile == dec_id] <- mean(sim_col > actual_t)
      est$p_boot_left [est$decile == dec_id] <- mean(sim_col < actual_t)
    }
  }
  
  est$panel_label <- panel$label
  est$panel_title <- panel$title
  results_per_panel[[pnm]] <- list(
    panel      = panel,
    estimates  = est,
    sim_matrix = sim,
    series     = act$series_by_dec
  )
}

# =============================================================================
# 8. WRITE NUMERIC RESULTS TO XLSX (AUDIT / DOWNSTREAM USE)
# =============================================================================
cat("\n=== 4. Writing persistence_results.xlsx ===\n")

sheets <- list()
for (pnm in names(results_per_panel)) {
  sheets[[paste0("panel_", pnm)]] <-
    results_per_panel[[pnm]]$estimates %>%
    arrange(decile) %>%
    mutate(decile_label = ifelse(decile == 11L, "D1-D10",
                                 paste0("D", decile))) %>%
    select(panel_label, decile_label, decile, n_months, n_funds,
           alpha_m, t_alpha, p_alpha_1t, p_boot_right, p_boot_left,
           b_mkt, t_mkt, b_smb, t_smb, b_hml, t_hml, b_mom, t_mom,
           adj_r2, exret_mean, exret_sd, skew, exkurt, jb_p, exp_ratio)
}

# Cohort summary (diagnostic)
sheets$cohort_summary <- do.call(rbind, lapply(cohorts, function(c) {
  data.frame(
    cohort_id = c$cohort_id,
    form_lo   = c$form_lo, form_hi = c$form_hi,
    hold_lo   = c$hold_lo, hold_hi = c$hold_hi,
    n_funds   = nrow(c$form_est),
    pct_full_hold = 100 * mean(rowSums(!is.na(c$res_mat)) == HOLD_MONTHS)
  )
}))

# Panel composition
sheets$panel_config <- do.call(rbind, lapply(PANELS, function(p) {
  data.frame(panel = p$label, title = p$title,
             cohorts = paste(p$cohorts, collapse = ","),
             rank_var = p$rank_var)
}))

write_xlsx(sheets, "persistence_results.xlsx")
cat("Written: persistence_results.xlsx\n")

# =============================================================================
# 9. BUILD LATEX TABLE
# =============================================================================
# Single longtable: 6 panels x 11 rows each (10 deciles + spread).
# Column set (14 cols) -- tight enough for landscape small-font layout:
#   Decile, alpha_m (%), t(alpha), p^C_{1t}, p^R_B, p^L_B,
#   b_MKT, b_SMB, b_HML, b_MOM, adj R2, ExpR, JB_p
# Skew / ExKurt / ExRet / SD preserved in the xlsx for audit.
# =============================================================================
cat("\n=== 5. Building table_persistence.tex ===\n")

# Format p-values compactly (< 0.001 style)
fmt_p <- function(p) {
  if (is.na(p)) return("--")
  if (p < 0.001) return("$<$.001")
  formatC(round(as.numeric(p), 3), format = "f", digits = 3)
}

# Convert monthly alpha (decimal) to percent and apply stars
fmt_alpha_pct <- function(a_m, t) {
  if (is.na(a_m)) return("--")
  add_stars(fmt(a_m * 100, 3), t)
}

# Build one panel's 11 rows (D1..D10 + D1-D10)
build_panel_rows <- function(est) {
  est <- est %>% arrange(decile)
  rows <- list()
  for (i in seq_len(nrow(est))) {
    r  <- est[i, ]
    lbl <- if (r$decile == 11L) "D1$-$D10"
    else if (r$decile == 1L)  "D1 (High)"
    else if (r$decile == N_DECILES) "D10 (Low)"
    else                      paste0("D", r$decile)
    rows[[i]] <- data.frame(
      Decile  = lbl,
      Alpha   = fmt_alpha_pct(r$alpha_m, r$t_alpha),
      t_Alpha = paste0("(", fmt(r$t_alpha, 2), ")"),
      pC      = fmt_p(r$p_alpha_1t),
      pR      = fmt_p(r$p_boot_right),
      pL      = fmt_p(r$p_boot_left),
      bM      = add_stars(fmt(r$b_mkt, 3), r$t_mkt),
      bS      = add_stars(fmt(r$b_smb, 3), r$t_smb),
      bH      = add_stars(fmt(r$b_hml, 3), r$t_hml),
      bO      = add_stars(fmt(r$b_mom, 3), r$t_mom),
      R2      = fmt(r$adj_r2, 3),
      ExpR    = if (is.na(r$exp_ratio)) "--" else fmt(r$exp_ratio, 2),
      JB      = fmt_p(r$jb_p),
      stringsAsFactors = FALSE
    )
  }
  do.call(rbind, rows)
}

panel_tables <- lapply(results_per_panel,
                       function(r) build_panel_rows(r$estimates))
ROWS_PER_PANEL <- nrow(panel_tables[[1]])
full_data <- bind_rows(panel_tables)
rownames(full_data) <- NULL

pack_tab <- data.frame(
  label = sapply(results_per_panel, function(r) r$panel$title),
  start = (seq_along(panel_tables) - 1L) * ROWS_PER_PANEL + 1L,
  end   = seq_along(panel_tables) * ROWS_PER_PANEL,
  stringsAsFactors = FALSE
)

fn_text <- paste(
  "\\textcite{KosowskiTimmermannWermersWhite2006} bootstrap persistence test on 36-month formation,",
  "12-month holding non-overlapping cohorts, ranked by formation-period",
  "\\textcite{Carhart1997} four-factor alpha $t$-statistic (Newey-West, 3-month",
  "lag) except Panel B, which ranks by raw alpha as a robustness check.",
  "Equal-weighted decile portfolios pooled across cohorts; alpha and factor",
  "loadings estimated jointly on pooled holding-period returns by OLS with",
  "Newey-West $t$-statistics (12-month lag).",
  "$\\hat{\\alpha}$: monthly, \\%.",
  "$p^C$: one-tailed parametric $p$-value from the holding-period alpha",
  "$t$-statistic under normality.",
  "$p^R_B$, $p^L_B$: right- and left-tail bootstrap $p$-values",
  "($B = 10{,}000$), computed as the fraction of simulated decile",
  "$t(\\hat{\\alpha})$ under the zero-true-alpha null that exceed (fall below)",
  "the observed $t$-statistic.",
  "$\\text{JB}_p$: Jarque-Bera $p$-value for the decile-portfolio holding-",
  "period excess return distribution. Where $\\text{JB}_p < 0.05$, the",
  "bootstrap $p$-values are the valid inferential basis.",
  "ExpR: mean expense ratio (\\%) of decile constituents, averaged across",
  "cohorts. D1 $-$ D10 is the long-short spread.",
  "Significance stars on $\\hat{\\alpha}$ and factor loadings reflect",
  "Newey-West $t$-statistics: $^{*}$, $^{**}$, $^{***}$ at 10\\%, 5\\%, 1\\%.",
  "Sample: Active funds in the \\textcite{Evans2010}-corrected",
  "panel\\_incubation, Jan 1995--Feb 2026; performance-comparison subsample",
  "per flagged\\_funds.xlsx. Panel D uses a single cohort",
  "(12 months) and should be interpreted with caution."
)

k <- full_data %>%
  kbl(format    = "latex",
      booktabs  = TRUE,
      linesep   = "",
      escape    = FALSE,
      longtable = TRUE,
      caption   = "Persistence in Active Mutual Fund Performance: Carhart and KTWW Bootstrap Tests",
      label     = "persistence",
      col.names = c("Decile",
                    "$\\hat{\\alpha}$", "$t(\\hat{\\alpha})$",
                    "$p^C$", "$p^R_B$", "$p^L_B$",
                    "$\\beta_{\\text{MKT}}$", "$\\beta_{\\text{SMB}}$",
                    "$\\beta_{\\text{HML}}$", "$\\beta_{\\text{MOM}}$",
                    "$\\bar{R}^2$", "ExpR", "$\\text{JB}_p$"),
      align     = c("l", rep("r", 12))) %>%
  kable_styling(latex_options = c("hold_position", "repeat_header"))

for (i in seq_len(nrow(pack_tab))) {
  k <- k %>%
    pack_rows(pack_tab$label[i], pack_tab$start[i], pack_tab$end[i],
              bold = FALSE, italic = TRUE,
              hline_before = (i > 1), hline_after = FALSE)
}

s <- as.character(k)
s <- longtable_note(s, fn_text)
s <- wrap_lt_small(s, tabcolsep = "2pt")

writeLines(s, "table_persistence.tex")
cat("Written: table_persistence.tex\n")

# =============================================================================
# 10. FINAL SUMMARY
# =============================================================================
cat("\n=== PERSISTENCE TESTING COMPLETE ===\n")
for (pnm in names(results_per_panel)) {
  r <- results_per_panel[[pnm]]
  est <- r$estimates
  d1  <- est[est$decile == 1L, ]
  d10 <- est[est$decile == N_DECILES, ]
  sp  <- est[est$decile == 11L, ]
  cat(sprintf("  Panel %s: D1 alpha = %s (t=%s, p^R_B=%s); D10 alpha = %s (t=%s, p^L_B=%s); D1-D10 t=%s\n",
              r$panel$label,
              fmt(d1$alpha_m * 100, 3), fmt(d1$t_alpha, 2), fmt_p(d1$p_boot_right),
              fmt(d10$alpha_m * 100, 3), fmt(d10$t_alpha, 2), fmt_p(d10$p_boot_left),
              fmt(sp$t_alpha, 2)))
}
cat("\nOutputs:\n")
cat("  persistence_results.xlsx  (audit + downstream numeric data)\n")
cat("  table_persistence.tex     (single longtable, 6 panels)\n")
cat("  persistence_bootstrap_cache_*.rds  (per-panel caches)\n")