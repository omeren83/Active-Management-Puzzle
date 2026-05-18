# =============================================================================
# ACTIVENESS ANALYSIS - DEGREE-OF-ACTIVENESS QUINTILE SORTS                v1.3
#
# v1.3 changes vs v1.2 (filter-methodology revision, May 2026):
#   No code change. flagged_funds.xlsx ledger updated upstream: H3_EXCLUDED
#   flag retired from "Exclude from H3 Only" (90 funds across the Equity
#   Income, Specialty Diversified, and Specialty/Miscellaneous Lipper
#   categories rejoin the activeness sample). Rationale: the H3_EXCLUDED
#   tag was originally justified by Cremers and Petajisto (2009) on the
#   ground that funds lacking unambiguous size/style benchmarks contaminate
#   active-share measurement. The activeness proxies used here -- 1-R^2
#   from a Carhart four-factor regression (Amihud-Goyenko 2013) and
#   tracking error against the fund's assigned benchmark -- are conceptually
#   distinct: 1-R^2 does not use any fund-specific benchmark at all, and
#   tracking error already has its own SECTOR_FUND guard. The benchmark-
#   misassignment argument therefore does not justify dropping these
#   categories from the activeness analyses.
#   The post-revision !excluded_h3 universe contains SECTOR_FUND (149) and
#   COVERED_CALL_OVERLAY (6) only; see v1.2 enumeration below for the
#   superseded composition.
#
# v1.2 changes vs v1.1 (Family D pre-defense audit):
#   - H3 / activeness subsample filter wired in at the panel-prep stage.
#     Per data_import_and_cleaning.R v1.2 Step 8c, the activeness scripts
#     must restrict to the !excluded_h3 subsample so that funds in the
#     "Exclude from H3 Only" sheet of flagged_funds.xlsx do not contaminate
#     the activeness measures or the quintile cutoffs.
#     [Legacy v1.2 note, SUPERSEDED by v1.3: under the original ledger this
#     sheet listed Equity Income, Specialty Diversified, Specialty/
#     Miscellaneous, sector funds, and covered-call overlays. The first
#     three categories were retired in v1.3; only sector and covered-call
#     funds remain.]
#     Implementation:
#       (i)  An h3_eligible Ticker lookup is built from panel_incubation %>%
#            filter(!excluded_h3) %>% distinct(Ticker) at the start of
#            Section 2 (after pre-flight checks).
#       (ii) alpha_rolling.xlsx and alpha_fullperiod.xlsx are restricted to
#            this universe via semi_join. Without this step, the 1-R^2 and
#            quintile cutoffs would be computed on a universe that includes
#            !excluded_h3 = TRUE funds (because alpha_estimation.R applies
#            only !excluded_perf, not !excluded_h3).
#       (iii) te_input and port_base panel_incubation pulls add filter(!excluded_h3).
#       (iv) factors_ts (line ~415 region) is left unfiltered because it
#            takes distinct(date, MKT_RF, ..., RF) and these factor values
#            are constant within a date.
#   - BSW (2010) misattribution corrected. The footnote sentence describing
#     the gross-to-net derivation previously attributed the "deduct one-twelfth
#     of the static annual expense ratio" arithmetic to BarrasScailletWermers
#     (2010); the correct attribution is Carhart (1997) and Wermers (2000).
#     BSW (2010) use the same arithmetic in the OPPOSITE direction (they
#     observe net from CRSP and derive gross). Fix mirrors the corrections
#     already applied to FF_comparison.R, subperiod_analysis.R, alpha_reporting.R,
#     descriptive_statistics.R, and build_robust_tables.R.
#   - Sample-source sentences in fn_char, fn_base_alpha, and fn_mr footnotes
#     appended with "H3 / activeness subsample per flagged\_funds.xlsx" to
#     match the convention adopted across performance-comparison footnotes.
#
# v1.1 changes vs v1.0:
#   Adds Patton & Timmermann (2010, JFE) monotonic relation (MR) bootstrap test
#   for both activeness measures, applied to all four panel combinations
#   (Active EW Gross / VW Gross / EW Net / VW Net). Rationale: a significant
#   Q5-Q1 spread does not by itself imply the alpha gradient is monotonically
#   increasing or decreasing across all five quintiles -- the pattern could be
#   U-shaped or non-monotone with the endpoints dominating. The MR test
#   bootstraps the joint distribution of inter-quintile differences under the
#   least-favorable null (all alphas equal) and tests whether the gradient is
#   monotone in either direction. Cremers & Petajisto (2009, RFS), Amihud &
#   Goyenko (2013, RFS), and Berk & van Binsbergen (2015, JFE) all report MR
#   test results, so this addition aligns activeness_analysis.R with the canon.
#   New output: table_activeness_monotonicity.tex (8 rows: 2 measures x 4 panels)
#   and a new 'monotonicity' sheet in activeness_alphas.xlsx. Runtime addition
#   ~30-60 seconds for B = 1000 bootstrap iterations across the 8 cells.
#
# v1.0 (original):
#
# Companion to portfolio_sorts.R. Tests whether the degree of activeness within
# the actively managed universe is rewarded in the cross-section of factor-
# adjusted returns. Two activeness proxies are used:
#   (a) 1 - R^2 from the rolling 36-month Carhart (1997) four-factor
#       regression. Following Amihud & Goyenko (2013, RFS); higher 1-R^2 means
#       returns are less explained by systematic factors and therefore reflect
#       greater idiosyncratic / active deviation.
#   (b) Annualised Tracking Error against the fund's assigned benchmark index,
#       sqrt(12) * sd(ret_gross_i - ret_bench_b(i)). Following the
#       interpretation of Cremers & Petajisto (2009, RFS).
#
# Quintile assignment is fund-level (time-series average of activeness),
# matching the static-fee precedent in portfolio_sorts.R Tables 14-16. Active
# funds only; passive sorts are uninformative by construction.
#
# Inputs:
#   panel_incubation (session)        Evans-corrected panel from Phase A. Provides
#                                     ret_gross, ret_net, tna_lag, Expense_Ratio,
#                                     Turnover, Benchmark_Code (from static),
#                                     and the merged factor series.
#   alpha_rolling.xlsx                as_r2 = R^2 from rolling Carhart regressions
#                                     (alpha_estimation.R).
#   alpha_fullperiod.xlsx             Fund-level eligibility (ap_group, n_obs).
#   fund_data.xlsx, sheet bench_returns
#                                     Benchmark INDEX LEVELS (not returns) with
#                                     benchmark codes on rows, dates on columns.
#                                     Returns computed here as level_t/level_{t-1}-1.
#
# Outputs:
#   activeness_inputs.xlsx            Per-fund activeness measures + quintiles.
#   activeness_returns.xlsx           Monthly portfolio returns per quintile.
#   activeness_alphas.xlsx            Carhart regression results per quintile.
#   table_activeness_chars.tex        Characteristics by activeness quintile.
#   table_activeness_alpha_r2.tex     Alpha by 1-R^2 quintile (4 panels).
#   table_activeness_alpha_te.tex     Alpha by Tracking Error quintile (4 panels).
#   table_activeness_monotonicity.tex Patton-Timmermann MR test results (1 table,
#                                     8 rows = 2 measures x 4 panel combinations).
#
# Dependencies: dplyr, tidyr, readxl, writexl, lubridate, knitr, kableExtra,
#               stringr, zoo
# =============================================================================

library(dplyr)
library(tidyr)
library(readxl)
library(writexl)
library(lubridate)
library(knitr)
library(kableExtra)
library(stringr)
library(zoo)

# =============================================================================
# 0. CONFIG
# =============================================================================
N_QUINT       <- 5L
NW_LAG        <- 6L
MIN_OBS       <- 24L     # min monthly obs for a Carhart regression
MIN_OBS_ACT   <- 24L     # min monthly obs to assign a fund an activeness value

# Patton-Timmermann (2010) MR test bootstrap settings
MR_B          <- 1000L   # number of bootstrap iterations per panel
MR_SEED       <- 42L     # reproducibility

FILE          <- "fund_data.xlsx"
ROLLING_FILE  <- "alpha_rolling.xlsx"
FULLPER_FILE  <- "alpha_fullperiod.xlsx"

# =============================================================================
# 1. HELPERS  (lifted from portfolio_sorts.R / alpha_reporting.R for self-
#              containment so this script can run independently)
# =============================================================================

fmt <- function(x, d = 3) {
  ifelse(is.na(x) | is.nan(x), "--",
         formatC(round(as.numeric(x), d), format = "f", digits = d,
                 big.mark = ","))
}

# Weighted mean with NA handling
wm <- function(x, w) {
  v <- !is.na(x) & !is.na(w) & w > 0
  if (sum(v) == 0L) return(NA_real_)
  sum(x[v] * w[v]) / sum(w[v])
}

# Quintile assignment that tolerates NA
safe_ntile <- function(x, n = N_QUINT) {
  out   <- rep(NA_integer_, length(x))
  valid <- !is.na(x)
  if (sum(valid) < n) return(out)
  out[valid] <- dplyr::ntile(x[valid], n)
  out
}

# Significance star helper - matches portfolio_sorts.R |t| thresholds
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

# kableExtra LaTeX cleanup - identical to portfolio_sorts.R
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

write_tex <- function(s, fn, resize = TRUE, small = FALSE) {
  writeLines(clean_latex(s, resize = resize, small = small), fn)
  cat("Written:", fn, "\n")
}

# Footnote paragraph after \end{longtable} - identical to portfolio_sorts.R
# Append a note paragraph AFTER \end{longtable} (not inside the body).
# Format mirrors clean_latex() floating-table branch: \begin{singlespace}
# wrapper ensures the note renders single-spaced (the document default is
# \doublespacing via setspace). No "Note:" prefix and no \textit{} wrapping —
# matches Table 4.8 (BSW decomposition) caption style per SBE convention.
longtable_note <- function(s, note, n_cols) {
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

# Tight tabcolsep + footnotesize wrapper for compact body text
wrap_lt_small <- function(s, tabcolsep = "3pt") {
  opener <- paste0(
    "{\\setlength{\\tabcolsep}{", tabcolsep, "}\\footnotesize\n",
    "\\captionsetup{font=normalsize}\\begin{longtable}"
  )
  parts_open <- strsplit(s, "\\begin{longtable}", fixed = TRUE)[[1]]
  s <- paste0(parts_open[1], opener,
              if (length(parts_open) > 1) parts_open[2] else "")
  parts_close <- strsplit(s, "\\end{longtable}", fixed = TRUE)[[1]]
  paste0(parts_close[1], "\\end{longtable}\n}",
         if (length(parts_close) > 1)
           paste0(parts_close[-1], collapse = "\\end{longtable}") else "")
}

# OLS / NW / model runner - identical to portfolio_sorts.R
fast_ols <- function(y, X) {
  tryCatch({
    XtX  <- crossprod(X)
    beta <- solve(XtX, crossprod(X, y))
    e    <- as.vector(y - X %*% beta)
    n    <- length(y); k <- ncol(X)
    list(beta = as.vector(beta), e = e,
         r2   = 1 - sum(e^2) / sum((y - mean(y))^2),
         n = n, k = k)
  }, error = function(err) NULL)
}

nw_se <- function(X, e, lag) {
  T <- nrow(X)
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
  }, error = function(err) rep(NA_real_, ncol(X)))
}

# Run CAPM, FF3, Carhart on a single return column.
# subtract_rf: TRUE for individual portfolios; FALSE for long-short spreads
# (RF cancels in self-financing construction).
run_models <- function(ret_col, port_df, factors_df, subtract_rf = TRUE) {
  d <- port_df %>%
    select(date, ret = all_of(ret_col)) %>%
    left_join(factors_df, by = "date") %>%
    filter(!is.na(ret), !is.na(MKT_RF), !is.na(RF)) %>%
    mutate(excess = if (subtract_rf) ret - RF else ret)

  n <- nrow(d)
  na_row <- data.frame(
    n_months   = n,
    alpha_capm = NA_real_, t_capm = NA_real_,
    alpha_ff3  = NA_real_, t_ff3  = NA_real_,
    alpha_car  = NA_real_, t_car  = NA_real_,
    b_mkt = NA_real_, t_mkt = NA_real_,
    b_smb = NA_real_, t_smb = NA_real_,
    b_hml = NA_real_, t_hml = NA_real_,
    b_mom = NA_real_, t_mom = NA_real_,
    adj_r2 = NA_real_
  )
  if (n < MIN_OBS) return(na_row)

  y  <- d$excess
  X1 <- cbind(1, d$MKT_RF)
  f1 <- fast_ols(y, X1); if (is.null(f1)) return(na_row)
  s1 <- nw_se(X1, f1$e, NW_LAG)

  X3 <- cbind(1, d$MKT_RF, d$SMB, d$HML)
  f3 <- fast_ols(y, X3); if (is.null(f3)) return(na_row)
  s3 <- nw_se(X3, f3$e, NW_LAG)

  X4 <- cbind(1, d$MKT_RF, d$SMB, d$HML, d$MOM)
  f4 <- fast_ols(y, X4); if (is.null(f4)) return(na_row)
  s4 <- nw_se(X4, f4$e, NW_LAG)

  data.frame(
    n_months   = n,
    alpha_capm = f1$beta[1] * 12,  t_capm = f1$beta[1] / s1[1],
    alpha_ff3  = f3$beta[1] * 12,  t_ff3  = f3$beta[1] / s3[1],
    alpha_car  = f4$beta[1] * 12,  t_car  = f4$beta[1] / s4[1],
    b_mkt = f4$beta[2],  t_mkt = f4$beta[2] / s4[2],
    b_smb = f4$beta[3],  t_smb = f4$beta[3] / s4[3],
    b_hml = f4$beta[4],  t_hml = f4$beta[4] / s4[4],
    b_mom = f4$beta[5],  t_mom = f4$beta[5] / s4[5],
    adj_r2 = 1 - (1 - f4$r2) * (n - 1) / (n - 5)
  )
}

# Patton & Timmermann (2010, JFE) monotonic relation (MR) bootstrap test.
# 
# Tests whether the Carhart alphas are monotonic across the five quintiles.
# Two one-sided tests (computed jointly):
#   - p_up:  H_1 is "alphas increasing"  (alpha_q > alpha_{q-1} for all q)
#            Test stat J_up = min_q (alpha_q - alpha_{q-1}); reject for large J_up
#   - p_dn:  H_1 is "alphas decreasing"  (alpha_q < alpha_{q-1} for all q)
#            Test stat J_dn = min_q (alpha_{q-1} - alpha_q); reject for large J_dn
#
# Bootstrap procedure follows Patton & Timmermann (2010), Section 3:
#   1. Estimate Carhart alpha_q from each quintile's full-sample regression.
#   2. Recenter excess returns: r_q,t* = r_q,t - alpha_q. This imposes the
#      least-favorable null where every quintile has alpha = 0 (a single
#      point in the "all alphas equal" null space).
#   3. Resample calendar months with replacement, preserving cross-sectional
#      factor dependence. In each iteration re-estimate Carhart alphas
#      from the recentered, resampled data, recompute J_up^bs and J_dn^bs.
#   4. p-values:  p_up = mean(J_up^bs >= J_up_obs)
#                 p_dn = mean(J_dn^bs >= J_dn_obs)
#
# Returns a one-row data.frame summarising the test for the given panel.
# Set subtract_rf = TRUE for the alpha test (standard).
mr_test <- function(port_df, factors_df, ret_col, B = MR_B, seed = MR_SEED,
                    subtract_rf = TRUE) {
  qs <- 1:N_QUINT
  
  na_out <- data.frame(
    n_months  = NA_integer_,
    alpha_q1  = NA_real_, alpha_q5 = NA_real_,
    spread    = NA_real_,
    J_up      = NA_real_, p_up = NA_real_,
    J_dn      = NA_real_, p_dn = NA_real_,
    p_min     = NA_real_, B = B
  )
  
  # 1. Per-quintile excess return series + full-sample Carhart alphas.
  q_excess <- list()  # date -> excess return per quintile
  alphas   <- numeric(N_QUINT)
  for (q in qs) {
    d <- port_df %>%
      filter(quintile == q) %>%
      select(date, ret = all_of(ret_col)) %>%
      left_join(factors_df, by = "date") %>%
      filter(!is.na(ret), !is.na(MKT_RF), !is.na(RF)) %>%
      mutate(excess = if (subtract_rf) ret - RF else ret)
    if (nrow(d) < MIN_OBS) return(na_out)
    
    y <- d$excess
    X <- cbind(1, d$MKT_RF, d$SMB, d$HML, d$MOM)
    fit <- fast_ols(y, X)
    if (is.null(fit)) return(na_out)
    alphas[q] <- fit$beta[1]
    
    q_excess[[q]] <- d %>% select(date, excess)
  }
  
  # 2. Align all five quintiles on common dates and recenter excess returns
  #    by subtracting alpha_q (imposes alpha = 0 LFC for the null).
  common_dates <- Reduce(intersect, lapply(q_excess, function(x) as.character(x$date)))
  common_dates <- as.Date(common_dates)
  if (length(common_dates) < MIN_OBS) return(na_out)
  
  fac <- factors_df %>%
    filter(date %in% common_dates) %>%
    arrange(date) %>%
    select(date, MKT_RF, SMB, HML, MOM, RF)
  
  ex_mat <- matrix(NA_real_, nrow = nrow(fac), ncol = N_QUINT)  # rows = dates, cols = quintiles
  for (q in qs) {
    qd <- q_excess[[q]] %>% filter(date %in% common_dates) %>% arrange(date)
    ex_mat[, q] <- qd$excess - alphas[q]   # recentered to alpha = 0
  }
  
  T <- nrow(fac)
  X_full <- cbind(1, fac$MKT_RF, fac$SMB, fac$HML, fac$MOM)
  
  # 3. Observed test statistics (annualised, % units for table display).
  diffs_obs <- diff(alphas) * 12 * 100  # alpha_2-alpha_1, ..., alpha_5-alpha_4
  J_up_obs  <- min(diffs_obs)
  J_dn_obs  <- min(-diffs_obs)
  
  # 4. Bootstrap loop. Inline OLS for speed (avoids fast_ols overhead).
  set.seed(seed)
  J_up_bs <- numeric(B)
  J_dn_bs <- numeric(B)
  for (b in seq_len(B)) {
    idx     <- sample.int(T, size = T, replace = TRUE)
    X_bs    <- X_full[idx, , drop = FALSE]
    XtX_inv <- tryCatch(solve(crossprod(X_bs)), error = function(e) NULL)
    if (is.null(XtX_inv)) {
      J_up_bs[b] <- NA_real_; J_dn_bs[b] <- NA_real_; next
    }
    a_bs <- numeric(N_QUINT)
    fail <- FALSE
    for (q in qs) {
      y_bs   <- ex_mat[idx, q]
      beta_q <- XtX_inv %*% crossprod(X_bs, y_bs)
      if (any(is.na(beta_q))) { fail <- TRUE; break }
      a_bs[q] <- beta_q[1]
    }
    if (fail) {
      J_up_bs[b] <- NA_real_; J_dn_bs[b] <- NA_real_; next
    }
    d_bs       <- diff(a_bs) * 12 * 100
    J_up_bs[b] <- min(d_bs)
    J_dn_bs[b] <- min(-d_bs)
  }
  
  p_up <- mean(J_up_bs >= J_up_obs, na.rm = TRUE)
  p_dn <- mean(J_dn_bs >= J_dn_obs, na.rm = TRUE)
  
  data.frame(
    n_months = T,
    alpha_q1 = alphas[1] * 12 * 100,
    alpha_q5 = alphas[N_QUINT] * 12 * 100,
    spread   = (alphas[N_QUINT] - alphas[1]) * 12 * 100,
    J_up     = J_up_obs,
    p_up     = p_up,
    J_dn     = J_dn_obs,
    p_dn     = p_dn,
    p_min    = pmin(p_up, p_dn),
    B        = B
  )
}

# Excel column-header date parser - matches data_import_and_cleaning.R
parse_col_dates <- function(x) {
  parsed <- suppressWarnings(as.Date(x, format = "%Y-%m-%d"))
  if (!all(is.na(parsed))) return(parsed)
  nums <- suppressWarnings(as.numeric(x))
  if (!all(is.na(nums))) return(as.Date(nums, origin = "1899-12-30"))
  parsed <- suppressWarnings(as.Date(paste0("01 ", x), format = "%d %b %Y"))
  if (!all(is.na(parsed))) return(parsed)
  stop("Could not parse benchmark date column headers.")
}

# =============================================================================
# 2. INPUTS
# =============================================================================
cat("=== 2. Loading inputs ===\n")

# Pre-flight checks
if (!exists("panel_incubation"))
  stop("panel_incubation not in session. Run Phase A (data_import_and_cleaning.R) first.")
if (!"Benchmark_Code" %in% names(panel_incubation))
  stop("panel_incubation has no Benchmark_Code column. Verify the static sheet ",
       "of fund_data.xlsx contains Benchmark_Code and re-run Phase A.")

# Factor matrix used by run_models (date + four factors + RF)
# Note: factor values are constant within a date across the fund cross-section,
# so distinct(date, ...) is invariant under filter(!excluded_h3); the filter
# is therefore omitted here to avoid a redundant pass.
factors_ts <- panel_incubation %>%
  distinct(date, MKT_RF, SMB, HML, MOM, RF) %>%
  filter(!is.na(MKT_RF)) %>%
  arrange(date)

# v1.2: Build the H3 / activeness fund-eligibility lookup. Used to restrict
# alpha_rolling.xlsx and alpha_fullperiod.xlsx (both produced by
# alpha_estimation.R, which applies only !excluded_perf) to the
# !excluded_h3 subsample, so that quintile cutoffs and the activeness
# measures are computed on the correct universe.
h3_eligible <- panel_incubation %>%
  filter(!excluded_h3) %>%
  distinct(Ticker)

# Per-fund-month rolling R^2 from alpha_estimation.R
alpha_roll <- read_excel(ROLLING_FILE) %>%
  select(Ticker, date, ap_group, as_r2) %>%
  mutate(date = as.Date(date)) %>%
  semi_join(h3_eligible, by = "Ticker")    # v1.2: !excluded_h3 restriction

# Full-period fund table - used only for Active eligibility filter
alpha_full <- read_excel(FULLPER_FILE) %>%
  select(Ticker, ap_group, n_obs) %>%
  semi_join(h3_eligible, by = "Ticker")    # v1.2: !excluded_h3 restriction

cat("  H3-eligible Tickers :", nrow(h3_eligible), "\n")
cat("  alpha_rolling rows  :", nrow(alpha_roll), "\n")
cat("  alpha_fullperiod n  :", nrow(alpha_full), "\n")

# =============================================================================
# 3. BENCHMARK INDEX LEVELS -> RETURNS
#    The bench_returns sheet stores TOTAL RETURN INDEX LEVELS, not returns.
#    Pivot long, parse date headers, then compute level_t / level_{t-1} - 1
#    within each Benchmark_Code group.
# =============================================================================
cat("=== 3. Benchmark levels -> returns ===\n")

bench_raw    <- read_excel(FILE, sheet = "bench_returns", col_types = "text")
bench_id_col <- names(bench_raw)[1]

bench_long <- bench_raw %>%
  rename(Benchmark_Code = !!bench_id_col) %>%
  pivot_longer(-Benchmark_Code, names_to = "date", values_to = "bench_level") %>%
  mutate(
    date        = parse_col_dates(date),
    bench_level = suppressWarnings(as.numeric(bench_level))
  ) %>%
  filter(!is.na(date), !is.na(bench_level), !is.na(Benchmark_Code)) %>%
  group_by(Benchmark_Code) %>%
  arrange(date) %>%
  mutate(bench_ret = bench_level / lag(bench_level) - 1) %>%
  ungroup() %>%
  filter(!is.na(bench_ret))

cat("  Unique benchmark codes:", n_distinct(bench_long$Benchmark_Code), "\n")
cat("  Date range            :", format(min(bench_long$date)),
    "to", format(max(bench_long$date)), "\n")

# =============================================================================
# 4. ACTIVENESS MEASURES PER FUND (Active funds only)
#    (a) mean_1mR2: time-series average of (1 - as_r2) from rolling Carhart.
#    (b) te_ann   : annualised sd of (ret_gross - bench_ret) over fund's life.
#
#    Active funds are eligible for (a) iff they have >= MIN_OBS_ACT rolling
#    observations. They are eligible for (b) iff they additionally have a
#    non-empty, non-error Benchmark_Code that matches at least MIN_OBS_ACT
#    benchmark observations. Funds without a matchable benchmark are dropped
#    from the TE sort only and remain in the R^2 sort.
# =============================================================================
cat("=== 4. Per-fund activeness measures ===\n")

# (a) 1 - R^2  (Amihud & Goyenko, 2013)
act_r2 <- alpha_roll %>%
  filter(ap_group == "Active", !is.na(as_r2)) %>%
  group_by(Ticker) %>%
  summarise(
    n_roll      = n(),
    mean_1mR2   = mean(1 - as_r2,   na.rm = TRUE),
    median_1mR2 = median(1 - as_r2, na.rm = TRUE),
    .groups     = "drop"
  ) %>%
  filter(n_roll >= MIN_OBS_ACT)

# (b) Tracking Error  (Cremers & Petajisto 2009 interpretation)
# v1.2: filter(!excluded_h3) restricts to the H3 / activeness subsample.
te_input <- panel_incubation %>%
  filter(!excluded_h3) %>%
  filter(ap_group == "Active", !is.na(ret_gross),
         !is.na(Benchmark_Code), Benchmark_Code != "",
         !grepl("^#|^N/A", Benchmark_Code)) %>%
  select(Ticker, date, Benchmark_Code, ret_gross) %>%
  inner_join(bench_long %>% select(Benchmark_Code, date, bench_ret),
             by = c("Benchmark_Code", "date")) %>%
  mutate(active_return = ret_gross - bench_ret)

act_te <- te_input %>%
  group_by(Ticker, Benchmark_Code) %>%
  summarise(
    n_te    = n(),
    te_ann  = sd(active_return,   na.rm = TRUE) * sqrt(12),
    mean_ar = mean(active_return, na.rm = TRUE) * 12,
    .groups = "drop"
  ) %>%
  filter(n_te >= MIN_OBS_ACT)

cat("  Active funds with R^2 measure :", nrow(act_r2), "\n")
cat("  Active funds with TE measure  :", nrow(act_te), "\n")

# Combine into one fund-level table; assign quintiles within each measure.
fund_activeness <- alpha_full %>%
  filter(ap_group == "Active") %>%
  left_join(act_r2 %>% select(Ticker, mean_1mR2), by = "Ticker") %>%
  left_join(act_te %>% select(Ticker, Benchmark_Code, te_ann), by = "Ticker") %>%
  mutate(
    q_R2 = safe_ntile(mean_1mR2, N_QUINT),
    q_TE = safe_ntile(te_ann,    N_QUINT)
  )

# Quick diagnostic: cross-tab of quintile assignments
cat("  Quintile distribution (R^2) :", paste(table(fund_activeness$q_R2,
                                                  useNA = "always"),
                                             collapse = " / "), "\n")
cat("  Quintile distribution (TE)  :", paste(table(fund_activeness$q_TE,
                                                  useNA = "always"),
                                             collapse = " / "), "\n")

# =============================================================================
# 5. PORT_BASE: ATTACH QUINTILES TO FUND-MONTH ROWS
#    Active funds only. Carries gross/net returns, lagged TNA, fee, turnover,
#    and the two activeness values + quintile labels.
# =============================================================================
cat("=== 5. Building port_base ===\n")

port_base <- panel_incubation %>%
  filter(!excluded_h3) %>%             # v1.2: H3 / activeness subsample
  filter(ap_group == "Active") %>%
  select(Ticker, date, ap_group, ret_gross, ret_net, tna_lag,
         Expense_Ratio, Turnover) %>%
  mutate(fee_sort = suppressWarnings(as.numeric(Expense_Ratio))) %>%
  inner_join(
    fund_activeness %>% select(Ticker, mean_1mR2, te_ann, q_R2, q_TE),
    by = "Ticker"
  )

cat("  port_base rows:", nrow(port_base),
    "| funds:",        n_distinct(port_base$Ticker), "\n")

# =============================================================================
# 6. PORTFOLIO RETURN CONSTRUCTION
# =============================================================================
cat("=== 6. Building quintile portfolios ===\n")

# Builds monthly EW/VW gross & net returns per (date, quintile), plus
# cross-sectional means of TNA, ER, Turnover, and the activeness measure.
build_port <- function(data, q_col, sort_name, value_col) {
  data %>%
    filter(!is.na(.data[[q_col]]), !is.na(ret_gross)) %>%
    group_by(date, quintile = .data[[q_col]]) %>%
    summarise(
      ret_ew_gross = mean(ret_gross, na.rm = TRUE),
      ret_ew_net   = mean(ret_net,   na.rm = TRUE),
      ret_vw_gross = wm(ret_gross, tna_lag),
      ret_vw_net   = wm(ret_net,   tna_lag),
      n_funds      = n(),
      mean_tna     = mean(tna_lag,  na.rm = TRUE),
      mean_er      = mean(fee_sort, na.rm = TRUE),
      mean_turn    = mean(suppressWarnings(as.numeric(Turnover)), na.rm = TRUE),
      mean_act     = mean(.data[[value_col]], na.rm = TRUE),
      .groups      = "drop"
    ) %>%
    mutate(sort_type = sort_name)
}

port_r2 <- build_port(port_base,
                      "q_R2", "Activeness_R2", "mean_1mR2")
port_te <- build_port(port_base %>% filter(!is.na(q_TE)),
                      "q_TE", "Activeness_TE", "te_ann")

# =============================================================================
# 7. REGRESSIONS
#    For each (quintile q, weighting wt, return type gn), regress the monthly
#    portfolio (excess) return on CAPM / FF3 / Carhart factors with NW(6) SEs.
#    Q5-Q1 spread (q=6) uses subtract_rf=FALSE because RF cancels in
#    self-financing construction.
# =============================================================================
cat("=== 7. Regressions ===\n")

make_spread <- function(port_df, ret_col) {
  d5 <- port_df %>% filter(quintile == 5L) %>% select(date, r5 = all_of(ret_col))
  d1 <- port_df %>% filter(quintile == 1L) %>% select(date, r1 = all_of(ret_col))
  inner_join(d5, d1, by = "date") %>%
    transmute(date, !!ret_col := r5 - r1)
}

regress_active <- function(port_df, sort_name) {
  rows <- list()
  qs   <- sort(unique(port_df$quintile[!is.na(port_df$quintile)]))

  for (q in c(qs, 6L)) {
    for (wt in c("EW", "VW")) {
      for (gn in c("gross", "net")) {
        rcol <- paste0("ret_", tolower(wt), "_", gn)
        d    <- if (q == 6L) make_spread(port_df, rcol) else
                              port_df %>% filter(quintile == q)
        r    <- tryCatch(
          run_models(rcol, d, factors_ts, subtract_rf = (q != 6L)),
          error = function(e) NULL
        )
        if (!is.null(r))
          rows[[paste(q, wt, gn)]] <-
            r %>% mutate(quintile    = as.integer(q),
                         weighting   = wt,
                         return_type = gn,
                         sort_type   = sort_name)
      }
    }
  }
  bind_rows(rows)
}

alpha_r2 <- regress_active(port_r2, "Activeness_R2")
alpha_te <- regress_active(port_te, "Activeness_TE")

# =============================================================================
# 7.5. PATTON-TIMMERMANN (2010) MONOTONIC RELATION TEST
#
# For each (sort_type x weighting x return_type) combination, run mr_test()
# on the corresponding monthly portfolio return series. Uses subtract_rf=TRUE
# throughout (testing alpha, not raw return spreads). B = MR_B iterations
# bootstrap calendar months with replacement under the LFC null
# (all alphas equal, recentered to zero).
# =============================================================================
cat("=== 7.5. Patton-Timmermann monotonicity tests (B =",
    MR_B, ") ===\n")

run_mr_panels <- function(port_df, sort_name) {
  out <- list()
  for (wt in c("EW", "VW")) {
    for (gn in c("gross", "net")) {
      rcol <- paste0("ret_", tolower(wt), "_", gn)
      r    <- mr_test(port_df, factors_ts, rcol)
      out[[paste(wt, gn)]] <- r %>%
        mutate(sort_type   = sort_name,
               weighting   = wt,
               return_type = gn) %>%
        select(sort_type, weighting, return_type, everything())
    }
  }
  bind_rows(out)
}

mr_t0 <- Sys.time()
mr_r2 <- run_mr_panels(port_r2, "Activeness_R2")
mr_te <- run_mr_panels(port_te, "Activeness_TE")
mr_all <- bind_rows(mr_r2, mr_te)
cat("  MR test wall time:",
    round(as.numeric(difftime(Sys.time(), mr_t0, units = "secs")), 1),
    "sec across", nrow(mr_all), "panels\n")

# =============================================================================
# 8. EXCEL EXPORT
# =============================================================================
cat("=== 8. Excel export ===\n")

write_xlsx(
  list(
    fund_level = fund_activeness,
    by_R2      = act_r2,
    by_TE      = act_te
  ),
  "activeness_inputs.xlsx"
)
write_xlsx(
  list(
    all = bind_rows(port_r2, port_te),
    R2  = port_r2,
    TE  = port_te
  ),
  "activeness_returns.xlsx"
)
write_xlsx(
  list(
    all          = bind_rows(alpha_r2, alpha_te),
    R2           = alpha_r2,
    TE           = alpha_te,
    monotonicity = mr_all
  ),
  "activeness_alphas.xlsx"
)
cat("Written: activeness_inputs.xlsx\n")
cat("Written: activeness_returns.xlsx\n")
cat("Written: activeness_alphas.xlsx\n")

# =============================================================================
# 9. CHARACTERISTICS TABLE
#    Two panels stacked vertically (R^2 and TE). One row per quintile.
#    Columns: Mean Activeness, N Avg, Mean TNA, Mean ER, Mean Turn,
#    EW Gross monthly mean return, VW Gross monthly mean return, EW Sharpe.
# =============================================================================
cat("=== 9. Characteristics table ===\n")

# Annualised Sharpe of EW gross excess return - matches portfolio_sorts.R
sharpe_ann <- function(r_ew_gross, rf) {
  ex <- r_ew_gross - rf
  v  <- !is.na(ex)
  if (sum(v) < MIN_OBS) return(NA_real_)
  sd_ex <- sd(ex[v]); if (is.na(sd_ex) || sd_ex == 0) return(NA_real_)
  sqrt(12) * mean(ex[v]) / sd_ex
}

compute_sharpe_lookup <- function(port_df) {
  port_df %>%
    inner_join(factors_ts %>% select(date, RF), by = "date") %>%
    filter(!is.na(quintile)) %>%
    group_by(sort_type, quintile) %>%
    summarise(ew_sharpe = sharpe_ann(ret_ew_gross, RF), .groups = "drop")
}

sharpe_lu <- bind_rows(
  compute_sharpe_lookup(port_r2),
  compute_sharpe_lookup(port_te)
)

make_char_panel <- function(port_df) {
  sort_nm <- unique(port_df$sort_type)
  port_df %>%
    filter(!is.na(quintile)) %>%
    group_by(quintile) %>%
    summarise(
      Mean_Act  = fmt(mean(mean_act,         na.rm = TRUE), 3),
      N_Avg     = fmt(mean(n_funds,          na.rm = TRUE), 0),
      Mean_TNA  = fmt(mean(mean_tna,         na.rm = TRUE), 0),
      Mean_ER   = fmt(mean(mean_er,          na.rm = TRUE), 2),
      Mean_Turn = fmt(mean(mean_turn,        na.rm = TRUE), 1),
      EW_Gross  = fmt(mean(ret_ew_gross*100, na.rm = TRUE)),
      VW_Gross  = fmt(mean(ret_vw_gross*100, na.rm = TRUE)),
      .groups   = "drop"
    ) %>%
    left_join(
      sharpe_lu %>% filter(sort_type == sort_nm) %>%
        select(quintile, ew_sharpe),
      by = "quintile"
    ) %>%
    mutate(Q = paste0("Q", quintile),
           EW_Sharpe = fmt(ew_sharpe, 2)) %>%
    select(Q, Mean_Act, N_Avg, Mean_TNA, Mean_ER, Mean_Turn,
           EW_Gross, VW_Gross, EW_Sharpe)
}

CHAR_NCOLS <- 9L
char_r2  <- make_char_panel(port_r2)
char_te  <- make_char_panel(port_te)
char_all <- bind_rows(char_r2, char_te)
n_panel  <- 5L

fn_char <- paste(
  "Cross-sectional means of monthly fund characteristics by activeness",
  "quintile, actively managed funds only. Q1 = lowest activeness; Q5 =",
  "highest activeness. Activeness is fund-level: time-series average of",
  "$1-R^2$ from a 36-month rolling \\textcite{Carhart1997} regression",
  "(Panel A; \\citealt{AmihudGoyenko2013}), or annualised Tracking Error",
  "against the fund's assigned benchmark index",
  "(Panel B; following \\citealt{CremersPetajisto2009}).",
  "Mean Act.: cross-sectional mean of the activeness measure within the",
  "quintile (decimal for $1-R^2$; annualised decimal for Tracking Error).",
  "$N$ Avg.: average number of funds in the quintile portfolio per month.",
  "Mean TNA: average lagged fund total net assets (USD millions). Mean ER:",
  "average static annual expense ratio (\\%). Mean Turn: average annual",
  "turnover (\\%). EW Gross / VW Gross: time-series mean of monthly",
  "equal-weighted / lagged-TNA-weighted gross returns (\\%, monthly).",
  "EW Sharpe: annualised Sharpe ratio of the equal-weighted gross excess",
  "return. Sample: Incubation-corrected panel (Evans 2010), no date cap;",
  "H3 / activeness subsample per flagged\\_funds.xlsx."
)

latex_char <- char_all %>%
  kbl(
    format    = "latex",
    booktabs  = TRUE,
    longtable = TRUE,
    linesep   = "",
    escape    = FALSE,
    caption   = "Activeness Quintile Portfolios: Characteristics",
    label     = "activeness_chars",
    col.names = c("Quintile", "Mean Act.", "$N$ Avg.",
                  "Mean TNA", "Mean ER", "Mean Turn",
                  "EW Gross", "VW Gross", "EW Sharpe"),
    align     = c("l", rep("r", 8))
  ) %>%
  kable_styling(latex_options = c("hold_position", "repeat_header")) %>%
  pack_rows("Panel A: $1-R^2$ Activeness", 1, n_panel,
            bold = FALSE, italic = FALSE,
            hline_before = FALSE, hline_after = FALSE,
            escape = FALSE) %>%
  pack_rows("Panel B: Tracking Error Activeness", n_panel + 1, 2 * n_panel,
            bold = FALSE, italic = FALSE,
            hline_before = TRUE, hline_after = FALSE,
            escape = FALSE)

s_char <- as.character(latex_char)
s_char <- longtable_note(s_char, fn_char, CHAR_NCOLS)
s_char <- wrap_lt_small(s_char, tabcolsep = "3pt")

write_tex(s_char, "table_activeness_chars.tex", resize = FALSE)

# =============================================================================
# 10. ALPHA TABLES (one per activeness measure, four panels each)
#     Panels (in order): Active EW Gross, Active VW Gross, Active EW Net,
#     Active VW Net. Each panel has 6 quintile rows + 6 t-stat rows = 12.
# =============================================================================
cat("=== 10. Alpha tables ===\n")

q_labels_r2 <- c("Q1 (Closet Indexer)", "Q2", "Q3", "Q4",
                 "Q5 (Most Active)",     "Q5$-$Q1")
q_labels_te <- c("Q1 (Lowest TE)",       "Q2", "Q3", "Q4",
                 "Q5 (Highest TE)",      "Q5$-$Q1")

D_NCOLS <- 9L

# Build a 12-row block (6 quintiles x [coef + t-stat]) for one panel.
build_block <- function(alpha_df, wt, gn, q_labels) {
  qs   <- c(1:5, 6L)
  rows <- list()
  for (i in seq_along(qs)) {
    q <- qs[i]
    r <- alpha_df %>% filter(quintile == q, weighting == wt, return_type == gn)
    if (nrow(r) == 0) {
      rows[[paste0("c", i)]] <- data.frame(
        Q = q_labels[i], a_c = "--", a_f = "--", a_r = "--",
        b_m = "--", b_s = "--", b_h = "--", b_o = "--", ar2 = "--",
        stringsAsFactors = FALSE)
      rows[[paste0("t", i)]] <- data.frame(
        Q = "t(coef)", a_c = "", a_f = "", a_r = "",
        b_m = "", b_s = "", b_h = "", b_o = "", ar2 = "",
        stringsAsFactors = FALSE)
      next
    }
    rows[[paste0("c", i)]] <- data.frame(
      Q   = q_labels[i],
      a_c = add_stars(fmt(r$alpha_capm * 100), r$t_capm),
      a_f = add_stars(fmt(r$alpha_ff3  * 100), r$t_ff3),
      a_r = add_stars(fmt(r$alpha_car  * 100), r$t_car),
      b_m = add_stars(fmt(r$b_mkt),           r$t_mkt),
      b_s = add_stars(fmt(r$b_smb),           r$t_smb),
      b_h = add_stars(fmt(r$b_hml),           r$t_hml),
      b_o = add_stars(fmt(r$b_mom),           r$t_mom),
      ar2 = fmt(r$adj_r2),
      stringsAsFactors = FALSE
    )
    rows[[paste0("t", i)]] <- data.frame(
      Q   = "t(coef)",
      a_c = paste0("(", fmt(r$t_capm, 2), ")"),
      a_f = paste0("(", fmt(r$t_ff3,  2), ")"),
      a_r = paste0("(", fmt(r$t_car,  2), ")"),
      b_m = paste0("(", fmt(r$t_mkt,  2), ")"),
      b_s = paste0("(", fmt(r$t_smb,  2), ")"),
      b_h = paste0("(", fmt(r$t_hml,  2), ")"),
      b_o = paste0("(", fmt(r$t_mom,  2), ")"),
      ar2 = "",
      stringsAsFactors = FALSE
    )
  }
  tbl <- do.call(rbind, rows); rownames(tbl) <- NULL
  tbl
}

build_alpha_table <- function(alpha_df, q_labels, cap, lab, fn_text) {
  panels <- list(
    list(label = "Panel A: Active --- EW Gross", wt = "EW", gn = "gross"),
    list(label = "Panel B: Active --- VW Gross", wt = "VW", gn = "gross"),
    list(label = "Panel C: Active --- EW Net",   wt = "EW", gn = "net"),
    list(label = "Panel D: Active --- VW Net",   wt = "VW", gn = "net")
  )
  blocks  <- lapply(panels, function(p) build_block(alpha_df, p$wt, p$gn, q_labels))
  full    <- bind_rows(blocks); rownames(full) <- NULL
  rows_pp <- 12L

  pack_tab <- data.frame(
    label = vapply(panels, `[[`, character(1), "label"),
    start = (seq_along(panels) - 1) * rows_pp + 1,
    end   = seq_along(panels) * rows_pp,
    stringsAsFactors = FALSE
  )

  k <- full %>%
    kbl(
      format    = "latex",
      booktabs  = TRUE,
      linesep   = "",
      escape    = FALSE,
      longtable = TRUE,
      caption   = cap,
      label     = lab,
      col.names = c("Quintile",
                    "$\\alpha_{\\text{CAPM}}$",
                    "$\\alpha_{\\text{FF3}}$",
                    "$\\alpha_{\\text{Car}}$",
                    "$\\beta_{\\text{MKT}}$",
                    "$\\beta_{\\text{SMB}}$",
                    "$\\beta_{\\text{HML}}$",
                    "$\\beta_{\\text{MOM}}$",
                    "$\\bar{R}^2$"),
      align     = c("l", rep("r", 8))
    ) %>%
    kable_styling(latex_options = c("hold_position", "repeat_header"))

  for (i in seq_len(nrow(pack_tab)))
    k <- k %>%
      pack_rows(pack_tab$label[i], pack_tab$start[i], pack_tab$end[i],
                bold = FALSE, italic = FALSE,
                hline_before = (i > 1), hline_after = FALSE)

  s <- as.character(k)
  s <- longtable_note(s, fn_text, D_NCOLS)
  s <- wrap_lt_small(s, tabcolsep = "2pt")
  s
}

# Shared footnote prelude
fn_base_alpha <- paste(
  "Monthly EW and VW portfolio returns regressed on CAPM, Fama-French",
  "three-factor, and \\textcite{Carhart1997} four-factor models.",
  "Alphas annualised ($\\times 12$, \\%). Q5$-$Q1: long-",
  "short spread; the risk-free rate is omitted from the spread return because",
  "it cancels in the self-financing construction:",
  "$(r_5 - R_f) - (r_1 - R_f) = r_5 - r_1$.",
  "EW: equal-weighted; VW: lagged-TNA-weighted. Net returns are computed by",
  "deducting one-twelfth of the static annual expense ratio from each fund's",
  "monthly gross return, following \\textcite{Carhart1997} and \\textcite{Wermers2000}.",
  "Newey-West $t$-statistics (6-month lag) in parentheses below each coefficient.",
  "$^{*}$, $^{**}$, $^{***}$: significant at 10\\%, 5\\%, 1\\% respectively.",
  "Sample: Incubation-corrected panel (Evans 2010), no date cap;",
  "H3 / activeness subsample per flagged\\_funds.xlsx. Active funds",
  "only. Quintile assignment is fund-level using the time-series average of",
  "the activeness measure across each fund's history (minimum 24 monthly obs.)."
)

fn_r2 <- paste(fn_base_alpha,
  "Activeness measured as $1-R^2$ from the 36-month rolling Carhart regression",
  "of fund excess gross returns on the four factors, following",
  "\\textcite{AmihudGoyenko2013}. A higher $1-R^2$ indicates returns less",
  "explained by systematic factor exposures and therefore more idiosyncratic,",
  "indicative of greater active deviation."
)

fn_te <- paste(fn_base_alpha,
  "Activeness measured as annualised Tracking Error against the fund's assigned",
  "benchmark index, $\\sqrt{12}\\cdot\\mathrm{sd}(r_{i,t}^{\\text{gross}}-r_{b(i),t})$,",
  "where $b(i)$ is the benchmark code recorded for fund $i$ in the static",
  "metadata. Following the interpretation of \\textcite{CremersPetajisto2009},",
  "higher tracking error indicates greater portfolio deviation from the",
  "assigned benchmark. Funds with missing or unmatched benchmark codes are",
  "excluded from this sort only."
)

s_r2 <- build_alpha_table(alpha_r2, q_labels_r2,
  "Alpha and Factor Loadings by Activeness Quintile ($1-R^2$, \\%, Annualised)",
  "activeness_alpha_r2", fn_r2)
s_te <- build_alpha_table(alpha_te, q_labels_te,
  "Alpha and Factor Loadings by Activeness Quintile (Tracking Error, \\%, Annualised)",
  "activeness_alpha_te", fn_te)

writeLines(s_r2, "table_activeness_alpha_r2.tex")
writeLines(s_te, "table_activeness_alpha_te.tex")
cat("Written: table_activeness_alpha_r2.tex\n")
cat("Written: table_activeness_alpha_te.tex\n")

# =============================================================================
# 11. MONOTONICITY TEST TABLE
#     One compact 8-row table summarising MR test results across both
#     activeness measures and all four panel combinations. Rendered as Tabular
#     (not longtable) since 8 rows fits on a single page comfortably.
# =============================================================================
cat("=== 11. Monotonicity test table ===\n")

MR_NCOLS <- 8L

# Format an MR p-value with significance star.
fmt_p <- function(p) {
  if (is.na(p)) return("--")
  star <- if (p <= 0.01) "$^{***}$"
  else if (p <= 0.05) "$^{**}$"
  else if (p <= 0.10) "$^{*}$"
  else                ""
  paste0(formatC(round(p, 3), format = "f", digits = 3), star)
}

# Row format: panel label | alpha_Q1 | alpha_Q5 | spread | J_up | p_up | J_dn | p_dn
panel_short <- function(wt, gn) {
  wtxt <- if (wt == "EW") "EW" else "VW"
  gtxt <- if (gn == "gross") "Gross" else "Net"
  paste0(wtxt, " ", gtxt)
}

mr_display <- mr_all %>%
  rowwise() %>%
  mutate(
    Panel    = panel_short(weighting, return_type),
    aQ1      = fmt(alpha_q1, 3),
    aQ5      = fmt(alpha_q5, 3),
    Spread   = fmt(spread, 3),
    Jup_str  = fmt(J_up, 3),
    pup_str  = fmt_p(p_up),
    Jdn_str  = fmt(J_dn, 3),
    pdn_str  = fmt_p(p_dn)
  ) %>%
  ungroup() %>%
  select(sort_type, Panel, aQ1, aQ5, Spread,
         Jup_str, pup_str, Jdn_str, pdn_str)

mr_display_no_sort <- mr_display %>% select(-sort_type)

fn_mr <- paste(
  "\\\\textcite{PattonTimmermann2010} monotonic relation (MR) bootstrap test",
  "for the Carhart alpha gradient across the five activeness quintiles,",
  "separately for each activeness measure and for each weighting",
  "$\\\\times$ return-type combination (Active funds only). Test statistics:",
  "$J_{\\\\uparrow} = \\\\min_{q \\\\geq 2}(\\\\alpha_q - \\\\alpha_{q-1})$",
  "for the increasing-pattern alternative,",
  "$J_{\\\\downarrow} = \\\\min_{q \\\\geq 2}(\\\\alpha_{q-1} - \\\\alpha_q)$",
  "for the decreasing-pattern alternative.",
  "$p_{\\\\uparrow}$ and $p_{\\\\downarrow}$ are bootstrap p-values from",
  paste0("$B = ", MR_B, "$"), "calendar-month resamples under the",
  "least-favorable null configuration (each quintile's excess return series",
  "recentered by subtracting its full-sample Carhart alpha, imposing",
  "$\\\\alpha_q = 0$ for all $q$). Reject the null of no monotone pattern",
  "when the corresponding p-value is small.",
  "$\\\\alpha_{\\\\text{Q1}}$, $\\\\alpha_{\\\\text{Q5}}$, and",
  "$\\\\alpha_{\\\\text{Q5}}-\\\\alpha_{\\\\text{Q1}}$ are annualised Carhart alphas (\\\\%).",
  "$J_{\\\\uparrow}$ and $J_{\\\\downarrow}$ are reported in annualised \\\\%",
  "terms. $^{*}$, $^{**}$, $^{***}$ on the p-values: significant at 10\\\\%,",
  "5\\\\%, 1\\\\%. Implementation follows the recentering procedure of",
  "\\\\textcite{CremersPetajisto2009} and \\\\textcite{AmihudGoyenko2013}.",
  "Sample: Incubation-corrected panel (Evans 2010), no date cap;",
  "H3 / activeness subsample per flagged\\\\_funds.xlsx."
)

n_panels_per_measure <- 4L

mr_kable <- mr_display_no_sort %>%
  kbl(
    format    = "latex",
    booktabs  = TRUE,
    linesep   = "",
    escape    = FALSE,
    caption   = "Patton-Timmermann Monotonic Relation Test for Activeness Quintile Alphas",
    label     = "activeness_monotonicity",
    col.names = c("Panel",
                  "$\\alpha_{\\text{Q1}}$",
                  "$\\alpha_{\\text{Q5}}$",
                  "$\\alpha_{\\text{Q5}}-\\alpha_{\\text{Q1}}$",
                  "$J_{\\uparrow}$",
                  "$p_{\\uparrow}$",
                  "$J_{\\downarrow}$",
                  "$p_{\\downarrow}$"),
    align     = c("l", rep("r", 7))
  ) %>%
  kable_styling(latex_options = "hold_position") %>%
  pack_rows("Panel A: $1-R^2$ Activeness", 1, n_panels_per_measure,
            bold = FALSE, italic = FALSE,
            hline_before = FALSE, hline_after = FALSE,
            escape = FALSE) %>%
  pack_rows("Panel B: Tracking Error Activeness",
            n_panels_per_measure + 1, 2L * n_panels_per_measure,
            bold = FALSE, italic = FALSE,
            hline_before = TRUE, hline_after = FALSE,
            escape = FALSE) %>%
  footnote(
    general        = fn_mr,
    general_title  = "",
    escape         = FALSE,
    threeparttable = TRUE
  )

mr_str <- as.character(mr_kable)
write_tex(mr_str, "table_activeness_monotonicity.tex",
          resize = TRUE, small = FALSE)

# =============================================================================
# 12. SUMMARY DIAGNOSTICS  (printed; no file output)
# =============================================================================
cat("\n=== Summary diagnostics ===\n")

print_quintile_diag <- function(port_df, label) {
  d <- port_df %>%
    filter(!is.na(quintile)) %>%
    group_by(quintile) %>%
    summarise(
      N_avg     = mean(n_funds),
      mean_act  = mean(mean_act, na.rm = TRUE),
      ann_ret   = mean(ret_ew_gross, na.rm = TRUE) * 12 * 100,
      .groups   = "drop"
    )
  cat("\n", label, "\n", sep = "")
  print(as.data.frame(d), row.names = FALSE)
}

print_quintile_diag(port_r2, "1-R^2 Activeness Quintiles (sanity check):")
print_quintile_diag(port_te, "Tracking Error Activeness Quintiles (sanity check):")

# MR test summary printout: one row per panel, both measures.
cat("\nPatton-Timmermann MR test summary (alpha gradient):\n")
mr_print <- mr_all %>%
  mutate(
    panel = paste0(weighting, " ", return_type),
    p_up  = round(p_up, 3),
    p_dn  = round(p_dn, 3),
    p_min = round(p_min, 3)
  ) %>%
  select(sort_type, panel, alpha_q1, alpha_q5, spread, p_up, p_dn, p_min) %>%
  mutate(across(c(alpha_q1, alpha_q5, spread), ~ round(.x, 3)))
print(as.data.frame(mr_print), row.names = FALSE)

cat("\n[SUCCESS] activeness_analysis.R v1.2 complete.\n")
