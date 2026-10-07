# =============================================================================
# SUB-PERIOD ANALYSIS                                                      v1.6
#
# v1.6 changes vs v1.5 (pipeline audit, Oct 2026):
#   (a) Sub-period boundaries are no longer hand-typed. Set SUBPERIOD_BREAKS
#       (last month of P1 and of P2, "YYYY-MM"); the script checks that each
#       is a break date in breaktest_results.xlsx and stops otherwise. Window
#       labels and month counts are built from the data; P3 ends at the last
#       month with factor data (Jan 2026), so labels match the T in the
#       tables. The v1.5 stale-date guard and stale status notes are removed.
#   (b) Bootstrap kernel: months enter in DRAW order (match(samp, t_idx));
#       draws made on the main process from BOOT_SEED (independent of
#       N_CORES); cache key carries BOOT_KERNEL.
#   (c) BSW tails from the same Student-t p-values as pi0; population
#       estimates at GAMMA_STAR = 0.45 (BSW 2010, eq. 8).
#   (d) Table D.3 labels follow the numbers in both tails; fmt_prob().
#   (e) Table D.2 N column counts portfolio constituents.
#   Gross and net portfolios use the same fund-months (rows need ret_gross).
#
# v1.5 changes vs v1.4 (Family C audit):
#   (a) filter(!excluded_perf) added to the panel-prep stage of
#       estimate_subperiod() so all three sub-periods are estimated on the
#       performance-comparison subsample defined by flagged_funds.xlsx
#       (matches the main-text and FF subperiod analysis after the Family B
#       audit). Without this filter, Tables D.2-D.6 would describe a
#       strictly larger fund population than Tables 5-10b in the main text.
#   (b) Mean-TNA bug at line 291 fixed. Same pattern as alpha_estimation.R
#       v2.7 and FF_comparison.R v1.3: d$tna was NULL because panel_incubation
#       carries TNA as class_assets / total_assets. Now uses
#       coalesce(class_assets, total_assets). The mean_tna column in
#       subperiod_results.xlsx had been NaN; no Appendix D table currently
#       consumes it (Tables D.2-D.6 use FF portfolio regression with live
#       panel TNA), but the field is now correctly populated for any
#       downstream VW weighting.
#   (c) The runtime warning at the panel-prep stage (formerly self-flagged
#       in v1.3 / v1.4) has been strengthened to STOP execution rather than
#       merely warn when the SUBPERIODS thresholds match the panel_trimmed
#       era values. After the v1.2 cleaning pipeline added the perf-comparison
#       filter, the rolling alpha series itself changes; the Bai-Perron
#       break dates therefore MUST be re-estimated by structural_break_test.R
#       before this script can be meaningfully run. Override available via
#       SKIP_STALE_DATE_CHECK <- TRUE for cases where the user has manually
#       verified break stability under the new universe.
#   (d) Five Appendix D footnote strings updated to acknowledge the
#       performance-comparison subsample (Tables D.2, D.3, D.4, D.5, D.6).
#       Matches the convention established in alpha_reporting.R v8.2.
#   (e) BSW (2010) net-return citation in Table D.2 footnote (line 700)
#       corrected. Convention attributed to Carhart (1997) and Wermers (2000),
#       the originators; BSW (2010) shares the arithmetic but applies it in
#       the reverse direction.
#
# v1.4 changes vs v1.3:
#   (1) Footnote correction (Tables D.2--D.6): the hardcoded sample label
#       "Sample: Trimmed (1995--2023) Evans-corrected panel." has been replaced
#       with "Sample: Incubation-corrected panel (Evans 2010); no date cap." in
#       all five Appendix D tables. The old Trimmed label was a legacy string
#       from v1.2; it became factually inconsistent after the v1.3 panel switch
#       to panel_incubation (Dec 1994 -- Feb 2026) and became visibly
#       inconsistent after commit 418d8c5 extended P3 to Jan 2026, because the
#       Panel C header then read "Dec 2011--Jan 2026, 170 months" next to a
#       footnote claiming "1995--2023". Five strings replaced; no other
#       behavior change; the label now matches the convention already in use
#       in portfolio_sorts.R for Tables 11--13.
#   (2) P3 endpoint extended from 2023-12-31 to 2026-01-31 in SUBPERIODS (this
#       change was first landed in commit 418d8c5; retained here so the file
#       remains internally consistent with the committed state).
#   (3) Initial-release docstring P3 entry updated: "145 months" --> "170
#       months". Cosmetic; tables rebuilt automatically from SUBPERIODS.
#
# v1.3 change vs v1.2:
#   Panel source switched from panel_trimmed to panel_incubation (see
#   alpha_estimation.R v2.6 for rationale). Extends coverage through February
#   2026 instead of capping at 2023.
#
#
# v1.2 change vs v1.1:
#   Added bootstrap caching layer to avoid re-running the 3x10,000-iteration
#   FF bootstrap every time the script is re-run. On first execution, bootstrap
#   results per sub-period are saved to `subperiod_bootstrap_cache_{P1,P2,P3}.rds`
#   along with a hash of the inputs (fund set, date range, B_RUNS, NW_LAG,
#   seed). On subsequent runs, if the hash matches, the cached result is loaded
#   and the bootstrap is skipped -- reducing wall time from ~30 minutes to
#   ~30 seconds when iterating on tables. Set USE_BOOT_CACHE <- FALSE or
#   delete the cache files to force re-computation. Also trimmed Table D.2
#   footnote (unrelated, see v1.1b below).
#
# v1.1 change vs v1.0:
#   Table D.2 (`table_subperiod_perf_aggregate.tex`) switched from per-fund
#   alpha --> cross-sectional weighted mean (static mean-TNA weight) to the
#   FF (2010) portfolio regression methodology, consistent with the main-body
#   Table 7 refactor (alpha_reporting.R v8.0) and the FF replication refactor
#   (FF_comparison.R v1.2). Per sub-period: 5 groups x 2 rows (coef + NW
#   t-stat) with significance stars. Section 2e port_agg extended from 2
#   groups (Active, Passive) to all 5 groups; regress_agg_one extended to
#   also compute net-return Carhart alpha (alpha_car_net, t_car_net) since
#   Table D.2 Net columns need them. Table D.6 (Active vs Passive model
#   comparison) is unchanged because it filters alpha_agg at point of use.
#
# v1.0 initial release:
#
# Standalone script producing Appendix D.2 tables: parallels of main-text
# Tables 5, 6, 7, 8 and 10 estimated separately within each of three
# sub-periods identified by the Bai-Perron structural break test (Appendix: sub-period analysis).
#
# Sub-periods: three regimes split at the two break dates in SUBPERIOD_BREAKS,
# chosen from structural_break_test.R output (breaktest_results.xlsx).
#
# Outputs (five LaTeX files, one per parallel):
#   table_subperiod_perf_aggregate.tex    parallels Table 5
#   table_subperiod_bootstrap_tails.tex   parallels Table 6
#   table_subperiod_pi0_estimate.tex      parallels Table 7
#   table_subperiod_bsw_decomposition.tex parallels Table 8
#   table_subperiod_port_agg_alpha.tex    parallels Table 10
#
# REQUIRES IN SESSION:
#   panel_incubation -- built by data_import_and_cleaning.R + flow_calculation.R
#                       (must contain ret_gross, ret_net, tna, tna_lag, MKT_RF,
#                       SMB, HML, MOM, RF, ap_group, Expense_Ratio, date, Ticker).
#
# Estimation kernels mirror alpha_estimation.R v2.6, alpha_reporting.R v7.8,
# and portfolio_sorts.R v1.3 exactly, so sub-period numbers are directly
# comparable to the main-text tables.
#
# Bootstrap compute cost: three bootstraps (one per sub-period). Each uses a
# smaller fund universe than the full sample, so total wall time is usually
# close to the single full-sample bootstrap. Parallel via makeCluster.
# =============================================================================

library(dplyr)
library(tidyr)
library(lubridate)
library(writexl)
library(parallel)
library(knitr)
library(kableExtra)
library(stringr)
library(digest)   # used by bootstrap cache (v1.2)

# =============================================================================
# 0. CONFIGURATION
# =============================================================================
# Last month of P1 and of P2 ("YYYY-MM"). Each must be a Bai-Perron break date
# (window-end dating) in breaktest_results.xlsx, sheet "Break Dates". Update
# after re-running structural_break_test.R; the script stops if they are not
# among the detected breaks. P1 starts at the first month with returns and
# factors; P3 ends at the last such month.
SUBPERIOD_BREAKS <- c("2006-03", "2010-08")
BREAKTEST_FILE   <- "breaktest_results.xlsx"

MIN_OBS_FULL  <- 24L
NW_LAG_FULL   <- 6L
NW_LAG_PORT   <- 6L
B_RUNS        <- 10000L
MIN_OBS_BS    <- 8L
BOOT_SEED     <- 42L
N_CORES       <- max(1L, detectCores() - 1L)
PCTS          <- c(1, 2, 3, 4, 5, 10, 20, 30, 40, 50,
                   60, 70, 80, 90, 95, 96, 97, 98, 99)
GAMMA_GRID    <- c(0.05, 0.10, 0.15, 0.20, 0.25,
                   0.30, 0.35, 0.40, 0.45, 0.50)
LAMBDA_STOREY <- 0.5
GAMMA_STAR    <- 0.45   # BSW (2010) eq. 8: gamma* for pi_A-/pi_A+

# Prob. Luck (percent of B runs): 0 -> "0.0%", below 0.05 -> "<0.1%"
fmt_prob <- function(x) {
  v <- suppressWarnings(as.numeric(x))
  ifelse(is.na(v), "--",
    ifelse(v == 0,   "0.0\\%",
    ifelse(v < 0.05, "$<$0.1\\%",
           paste0(formatC(round(v, 1), format = "f", digits = 1), "\\%"))))
}

# Bootstrap caching: saves ~25 minutes when re-running the script after the
# bootstrap has already completed once. Set FALSE to force re-computation.
USE_BOOT_CACHE <- TRUE
BOOT_KERNEL    <- "v2.8-draw-order"  # part of the cache key; bump on resampling changes
BOOT_CACHE_DIR <- file.path(".", paste0("cache_", Sys.info()[["nodename"]]))
dir.create(BOOT_CACHE_DIR, showWarnings = FALSE)  # no-op if already exists

# =============================================================================
# 1. HELPER FUNCTIONS  (kernels identical to alpha_estimation.R v2.5)
# =============================================================================

fast_ols <- function(y, X) {
  tryCatch({
    XtX  <- crossprod(X)
    beta <- solve(XtX, crossprod(X, y))
    e    <- as.vector(y - X %*% beta)
    n    <- length(y); k <- ncol(X)
    s2   <- sum(e^2) / (n - k)
    list(beta   = as.vector(beta), e = e,
         se_ols = sqrt(pmax(diag(s2 * solve(XtX)), 0)),
         r2     = 1 - sum(e^2) / sum((y - mean(y))^2),
         sigma  = sqrt(s2), n = n, k = k)
  }, error = function(err) NULL)
}

nw_se <- function(X, e, lag) {
  T <- nrow(X); k <- ncol(X)
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

fmt <- function(x, digits = 3) {
  if (is.null(x) || length(x) == 0L || is.na(x) || x == "NaN") return("--")
  val <- suppressWarnings(as.numeric(x))
  if (is.na(val)) return("--")
  formatC(round(val, digits), format = "f", digits = digits)
}
fmt1 <- function(x) formatC(round(as.numeric(x), 1), format = "f", digits = 1)

# TNA-weighted cross-sectional mean
wm_alpha <- function(x, w) {
  v <- !is.na(x) & !is.na(w) & w > 0
  if (sum(v) == 0L) return(NA_real_)
  sum(x[v] * w[v]) / sum(w[v])
}

# Significance stars -- matches portfolio_sorts.R / FF_comparison.R
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

# kableExtra LaTeX cleaner -- identical to alpha_reporting.R v7.8
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
  x <- gsub("\\begin{table}[!h]", "\\begin{table}[!htbp]", x, fixed = TRUE)

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
    "(?s)\\\\begin\\{tablenotes\\}.*?\\\\item\\s+(.*?)\\\\end\\{tablenotes\\}",
    x, perl = TRUE
  )
  notes <- ""
  if (notes_m != -1) {
    full_block <- substring(x, notes_m, notes_m + attr(notes_m, "match.length") - 1L)
    inner <- sub("^(?s)\\\\begin\\{tablenotes\\}.*?\\\\item\\s+", "",
                 full_block, perl = TRUE)
    inner <- sub("(?s)\\\\end\\{tablenotes\\}\\s*$", "", inner, perl = TRUE)
    notes <- trimws(inner)
  }

  out <- paste0(
    "\\begin{table}[!htbp]\n",
    "\\centering\n",
    "\\sbox{\\tabletempbox}{%\n",
    "\\footnotesize\n",
    "\\setlength{\\tabcolsep}{4pt}%\n",   # T2.10: tighter column padding inside sbox; reduces natural width below \linewidth so \resizebox is rarely triggered
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

# Phase B helper: extract tablenotes from a threeparttable float and re-emit
# them AFTER \end{table} as a flowing paragraph (NOT a minipage --- minipage
# is unbreakable and overflowed page bottoms when notes were long).
threeparttable_note_after <- function(s) {
  note_rx <- "\\\\begin\\{tablenotes\\}.*?\\\\end\\{tablenotes\\}"
  nb <- regmatches(s, regexpr(note_rx, s, perl = TRUE))
  if (!length(nb)) return(s)
  ni <- nb
  ni <- sub("^\\\\begin\\{tablenotes\\}(\\[para\\])?\\s*\n?", "", ni, perl = TRUE)
  ni <- sub("\\\\end\\{tablenotes\\}\\s*$", "", ni, perl = TRUE)
  ni <- sub("^\\\\footnotesize\\s*\n?", "", ni, perl = TRUE)
  ni <- sub("^\\\\item\\s*", "", ni, perl = TRUE)
  ni <- trimws(ni)
  s <- gsub(note_rx, "", s, perl = TRUE)
  s <- gsub("\\\\begin\\{threeparttable\\}\\s*\n?", "", s)
  s <- gsub("\\\\end\\{threeparttable\\}\\}?\\s*\n?", "", s)
  note_block <- paste0("\\end{table}\n",
                       "{\\footnotesize\\noindent\\textit{Note:} ",
                       ni, "\\par}\n")
  sub("\\end{table}", note_block, s, fixed = TRUE)
}

# =============================================================================
# 2. PER-SUBPERIOD ESTIMATION
# =============================================================================
# Restricts panel_source to [date_lo, date_hi], then computes per-fund
# Carhart alphas, FF bootstrap, Storey pi_0, BSW decomposition, and aggregate
# Active/Passive portfolio alphas. Returns all inputs needed by the 5 table
# builders for one sub-period.

estimate_subperiod <- function(sp, panel_source) {
  cat("\n=== Sub-period ", sp$label, " (", sp$window, ") ===\n", sep = "")
  
  # --- 2a. Panel restriction (local copy) ---
  ap <- panel_source %>%
    filter(!excluded_perf) %>%   # v1.5: performance-comparison subsample
    filter(date >= sp$date_lo, date <= sp$date_hi) %>%
    rename(mkt_rf = MKT_RF, smb = SMB, hml = HML,
           mom = MOM, rf = RF, exp_r = Expense_Ratio) %>%
    mutate(excess_ret = ret_gross - rf,
           exp_r      = suppressWarnings(as.numeric(exp_r)),
           ap_group   = gsub("Agtive", "Active", ap_group)) %>%
    filter(!is.na(excess_ret), !is.na(mkt_rf), !is.na(smb),
           !is.na(hml), !is.na(mom)) %>%
    arrange(Ticker, date)
  
  cat("  Fund-months: ", nrow(ap),
      " | Distinct funds: ", n_distinct(ap$Ticker), "\n", sep = "")
  
  ap_split <- split(ap, ap$Ticker)
  
  # --- 2b. Per-fund full-period Carhart alphas ---
  run_full <- function(tk) {
    d <- ap_split[[tk]]; n <- nrow(d)
    if (n < MIN_OBS_FULL) return(NULL)
    y   <- d$excess_ret
    X   <- cbind(1, d$mkt_rf, d$smb, d$hml, d$mom)
    fit <- fast_ols(y, X); if (is.null(fit)) return(NULL)
    se  <- nw_se(X, fit$e, NW_LAG_FULL)
    # v1.5: mean_tna uses coalesce(class_assets, total_assets). v1.4
    # referenced d$tna which was NULL (column does not exist on
    # panel_incubation). Same fix as alpha_estimation.R v2.7.
    tv  <- coalesce(d$class_assets, d$total_assets)
    data.frame(
      Ticker        = tk,
      ap_group      = d$ap_group[1],
      n_obs         = n,
      alpha_m       = fit$beta[1],
      alpha_ann     = fit$beta[1] * 12,
      alpha_t_nw    = fit$beta[1] / se[1],
      alpha_p_nw    = 2 * pt(-abs(fit$beta[1] / se[1]), n - 5),
      exp_ratio     = d$exp_r[1],
      alpha_net_ann = (fit$beta[1] * 12) - (d$exp_r[1] / 100),
      mean_tna      = mean(tv[!is.na(tv) & tv > 0], na.rm = TRUE)
    )
  }
  alpha_full <- bind_rows(lapply(names(ap_split), run_full)) %>%
    mutate(across(c(alpha_ann, alpha_net_ann), ~ .x * 100))
  cat("  Per-fund alphas:", nrow(alpha_full), "\n")
  
  # --- 2c. Fama-French (2010) bootstrap -- NW HAC SEs, identical to v2.5 ---
  active_fp <- alpha_full %>% filter(ap_group == "Active",
                                     !is.na(alpha_t_nw))
  all_dates <- sort(unique(ap$date)); T_total <- length(all_dates)
  date_idx  <- setNames(seq_len(T_total), as.character(all_dates))
  
  bs_data <- lapply(active_fp$Ticker, function(tk) {
    a_hat <- active_fp$alpha_ann[active_fp$Ticker == tk] / 1200  # decimal monthly
    d     <- ap_split[[tk]]
    list(t_idx   = date_idx[as.character(d$date)],
         y_tilde = d$excess_ret - a_hat,
         X_fac   = cbind(d$mkt_rf, d$smb, d$hml, d$mom))
  })
  names(bs_data) <- active_fp$Ticker
  
  # samp: one pre-drawn vector of T_total month indices (with replacement).
  one_boot_run <- function(samp, bs_data, pcts_probs, min_obs, nw_lag) {
    t_sim <- vapply(bs_data, function(d) {
      # Fund rows for the drawn months in DRAW order; do not sort (adjacent
      # duplicates would look like autocorrelation to Newey-West).
      keep <- match(samp, d$t_idx); keep <- keep[!is.na(keep)]
      if (length(keep) < min_obs) return(NA_real_)
      y <- d$y_tilde[keep]
      X <- cbind(1, d$X_fac[keep, , drop = FALSE])
      tryCatch({
        XtX     <- crossprod(X); XtX_inv <- solve(XtX)
        beta    <- XtX_inv %*% crossprod(X, y)
        e       <- as.vector(y - X %*% beta)
        Tn      <- length(y)
        scores  <- X * e
        S       <- crossprod(scores) / Tn
        L       <- min(nw_lag, Tn - 1L)
        if (L > 0L) {
          for (j in seq_len(L)) {
            w  <- 1 - j / (nw_lag + 1)
            Gj <- crossprod(scores[(j + 1):Tn, , drop = FALSE],
                            scores[1:(Tn - j),  , drop = FALSE]) / Tn
            S  <- S + w * (Gj + t(Gj))
          }
        }
        vc_alpha <- (Tn * XtX_inv %*% S %*% XtX_inv)[1, 1]
        beta[1] / sqrt(max(vc_alpha, 0))
      }, error = function(e) NA_real_)
    }, numeric(1L), USE.NAMES = FALSE)
    quantile(t_sim[!is.na(t_sim)], probs = pcts_probs, na.rm = TRUE)
  }
  
  # Cache key: hash inputs that determine bootstrap output. If any changes
  # (fund set, date range, B_RUNS, NW_LAG, seed), cache is invalidated.
  cache_key <- digest::digest(list(
    tickers   = sort(active_fp$Ticker),
    alphas    = active_fp$alpha_ann[order(active_fp$Ticker)],
    date_lo   = sp$date_lo,
    date_hi   = sp$date_hi,
    T_total   = T_total,
    B_RUNS    = B_RUNS,
    NW_LAG    = NW_LAG_FULL,
    MIN_OBS   = MIN_OBS_BS,
    seed      = BOOT_SEED,
    pcts      = PCTS,
    kernel    = BOOT_KERNEL
  ))
  cache_file <- file.path(BOOT_CACHE_DIR,
                          paste0("subperiod_bootstrap_cache_", sp$label, ".rds"))
  
  cached_ok <- FALSE
  if (USE_BOOT_CACHE && file.exists(cache_file)) {
    cached <- tryCatch(readRDS(cache_file), error = function(e) NULL)
    if (!is.null(cached) && identical(cached$key, cache_key)) {
      cat("  [cache hit] Loading bootstrap from", cache_file, "\n")
      sim_matrix <- cached$sim_matrix
      cached_ok  <- TRUE
    } else {
      cat("  [cache miss] Key changed; recomputing.\n")
    }
  }
  
  if (!cached_ok) {
    cat("  Bootstrap (B =", B_RUNS, ", cores =", N_CORES, ") ...\n")
    # All month draws made here from one seed: independent of N_CORES
    set.seed(BOOT_SEED)
    samp_list <- replicate(B_RUNS, sample.int(T_total, T_total, replace = TRUE),
                           simplify = FALSE)
    cl <- makeCluster(N_CORES)
    boot_t0 <- Sys.time()
    boot_results <- parLapply(cl, samp_list, one_boot_run,
                              bs_data, PCTS / 100,
                              MIN_OBS_BS, NW_LAG_FULL)
    stopCluster(cl)
    cat("  Bootstrap wall time:",
        round(as.numeric(difftime(Sys.time(), boot_t0,
                                  units = "secs")), 1), "sec\n")
    sim_matrix <- do.call(rbind, boot_results)
    if (USE_BOOT_CACHE) {
      saveRDS(list(key = cache_key, sim_matrix = sim_matrix), cache_file)
      cat("  [cached] Saved to", cache_file, "\n")
    }
  }
  
  actual_pct <- as.numeric(quantile(active_fp$alpha_t_nw, PCTS / 100))
  boot_summary <- data.frame(
    percentile       = PCTS,
    t_alpha_actual   = actual_pct,
    t_alpha_sim_mean = colMeans(sim_matrix),
    pct_runs_below   = colMeans(sweep(sim_matrix, 2,
                                      actual_pct, "<")) * 100
  )
  
  # --- 2d. Storey pi_0 + BSW decomposition ---
  active_pi0 <- alpha_full %>% filter(ap_group == "Active",
                                      !is.na(alpha_p_nw))
  total_n    <- nrow(active_pi0)
  num_above  <- sum(active_pi0$alpha_p_nw > LAMBDA_STOREY, na.rm = TRUE)
  pi_0_val   <- min(1.0, num_above / (total_n * (1 - LAMBDA_STOREY)))
  cat("  pi_0 estimate:", round(pi_0_val * 100, 1), "% (N =",
      total_n, ")\n")
  
  t_stats <- active_pi0$alpha_t_nw
  p_vals  <- active_pi0$alpha_p_nw   # same Student-t p-values as pi_0
  bsw_df <- do.call(rbind, lapply(GAMMA_GRID, function(g) {
    S_neg    <- mean(p_vals < g & t_stats < 0)
    S_pos    <- mean(p_vals < g & t_stats > 0)
    F_luck   <- pi_0_val * g / 2
    data.frame(
      gamma           = round(g * 100),
      S_neg_pct       = S_neg  * 100,
      S_pos_pct       = S_pos  * 100,
      F_luck_pct      = F_luck * 100,
      T_unskilled_pct = (S_neg - F_luck) * 100,
      T_skilled_pct   = (S_pos - F_luck) * 100
    )
  }))
  
  # --- 2e. Aggregate portfolio alphas (5 groups, gross + net) ---
  # Monthly EW/VW portfolio returns for Active, Passive, Unknown, A+P, Full.
  # Feeds both Table D.2 (5-group aggregate alpha) and Table D.6 (Active vs
  # Passive model comparison; filtered at point of use).
  build_port_for <- function(data, grp_label) {
    data %>%
      filter(!is.na(ret_gross)) %>%
      group_by(date) %>%
      summarise(
        ret_ew_gross = mean(ret_gross, na.rm = TRUE),
        ret_ew_net   = mean(ret_net,   na.rm = TRUE),
        ret_vw_gross = wm_alpha(ret_gross, tna_lag),
        ret_vw_net   = wm_alpha(ret_net,   tna_lag),
        .groups = "drop"
      ) %>%
      mutate(ap_group = grp_label)
  }
  port_agg <- bind_rows(
    build_port_for(ap %>% filter(ap_group == "Active"),                "Active"),
    build_port_for(ap %>% filter(ap_group == "Passive"),               "Passive"),
    build_port_for(ap %>% filter(ap_group == "Unknown"),               "Unknown"),
    build_port_for(ap %>% filter(ap_group %in% c("Active","Passive")), "Active + Passive"),
    build_port_for(ap,                                                  "Full Sample")
  )
  factors_ts <- ap %>%
    distinct(date, mkt_rf, smb, hml, mom, rf) %>%
    rename(MKT_RF = mkt_rf, SMB = smb, HML = hml, MOM = mom, RF = rf) %>%
    arrange(date)
  
  run_models <- function(ret_col, port_df, fac_df, subtract_rf = TRUE) {
    d <- port_df %>%
      select(date, ret = all_of(ret_col)) %>%
      left_join(fac_df, by = "date") %>%
      filter(!is.na(ret), !is.na(MKT_RF), !is.na(RF)) %>%
      mutate(excess = if (subtract_rf) ret - RF else ret)
    n <- nrow(d)
    na_row <- data.frame(
      n_months = n,
      alpha_capm = NA_real_, t_capm = NA_real_,
      alpha_ff3  = NA_real_, t_ff3  = NA_real_,
      alpha_car  = NA_real_, t_car  = NA_real_
    )
    if (n < MIN_OBS_FULL) return(na_row)
    y  <- d$excess
    X1 <- cbind(1, d$MKT_RF); f1 <- fast_ols(y, X1)
    if (is.null(f1)) return(na_row)
    s1 <- nw_se(X1, f1$e, NW_LAG_PORT)
    X3 <- cbind(1, d$MKT_RF, d$SMB, d$HML); f3 <- fast_ols(y, X3)
    if (is.null(f3)) return(na_row)
    s3 <- nw_se(X3, f3$e, NW_LAG_PORT)
    X4 <- cbind(1, d$MKT_RF, d$SMB, d$HML, d$MOM); f4 <- fast_ols(y, X4)
    if (is.null(f4)) return(na_row)
    s4 <- nw_se(X4, f4$e, NW_LAG_PORT)
    data.frame(
      n_months   = n,
      alpha_capm = f1$beta[1] * 12, t_capm = f1$beta[1] / s1[1],
      alpha_ff3  = f3$beta[1] * 12, t_ff3  = f3$beta[1] / s3[1],
      alpha_car  = f4$beta[1] * 12, t_car  = f4$beta[1] / s4[1]
    )
  }
  
  # Now also computes net-return Carhart alpha (needed for Table D.2 Net cols).
  regress_agg_one <- function(grp, wt) {
    d     <- port_agg %>% filter(ap_group == grp)
    g_col <- if (wt == "EW") "ret_ew_gross" else "ret_vw_gross"
    n_col <- if (wt == "EW") "ret_ew_net"   else "ret_vw_net"
    rg    <- run_models(g_col, d, factors_ts)
    rn    <- run_models(n_col, d, factors_ts)
    rg %>% mutate(alpha_car_net = rn$alpha_car, t_car_net = rn$t_car,
                  ap_group = grp, weighting = wt)
  }
  
  agg_groups_sp <- c("Active", "Passive", "Unknown", "Active + Passive", "Full Sample")
  alpha_agg <- bind_rows(
    lapply(agg_groups_sp,
           function(g) bind_rows(regress_agg_one(g, "EW"),
                                 regress_agg_one(g, "VW")))
  ) %>%
    mutate(across(c(alpha_capm, alpha_ff3, alpha_car, alpha_car_net), ~ .x * 100))
  
  # Portfolio constituents per group (N column of Table D.2)
  n_port <- function(rows) n_distinct(ap$Ticker[rows & !is.na(ap$ret_gross)])
  n_funds <- c(
    "Active"           = n_port(ap$ap_group == "Active"),
    "Passive"          = n_port(ap$ap_group == "Passive"),
    "Unknown"          = n_port(ap$ap_group == "Unknown"),
    "Active + Passive" = n_port(ap$ap_group %in% c("Active", "Passive")),
    "Full Sample"      = n_port(rep(TRUE, nrow(ap)))
  )

  list(
    label        = sp$label,
    window       = sp$window,
    panel        = sp$panel,
    n_funds      = n_funds,
    alpha_full   = alpha_full,
    boot_summary = boot_summary,
    pi_0         = pi_0_val,
    pi_0_n       = total_n,
    bsw_df       = bsw_df,
    alpha_agg    = alpha_agg
  )
}

# =============================================================================
# 3. RUN THREE SUB-PERIOD ESTIMATIONS
# =============================================================================

if (!exists("panel_incubation"))
  stop("panel_incubation not found in session. Run data_import_and_cleaning.R ",
       "(and flow_calculation.R) first.")

# --- Build SUBPERIODS from SUBPERIOD_BREAKS and the data --------------------
# Boundaries must be detected break dates (window-end dating of the rolling
# alpha series; see structural_break_test.R).
.bd <- readxl::read_excel(BREAKTEST_FILE, sheet = "Break Dates")
.bd <- format(as.Date(.bd[["Point Estimate"]]), "%Y-%m")
if (!all(SUBPERIOD_BREAKS %in% .bd))
  stop("SUBPERIOD_BREAKS (", paste(SUBPERIOD_BREAKS, collapse = ", "),
       ") are not all among the break dates in ", BREAKTEST_FILE, " (",
       paste(.bd, collapse = ", "), "). Choose the boundaries from the current ",
       "structural_break_test.R output and update SUBPERIOD_BREAKS.", call. = FALSE)
stopifnot(length(SUBPERIOD_BREAKS) == 2L, SUBPERIOD_BREAKS[1] < SUBPERIOD_BREAKS[2])

# Months with returns and all four factors define the sample span
.months <- panel_incubation %>%
  filter(!excluded_perf, !is.na(ret_gross), !is.na(MKT_RF), !is.na(SMB),
         !is.na(HML), !is.na(MOM), !is.na(RF)) %>%
  distinct(date) %>% arrange(date) %>% pull(date)
.ym     <- format(.months, "%Y-%m")
.first  <- floor_date(min(.months), "month")
.last   <- ceiling_date(max(.months), "month") - days(1)
.b1     <- ceiling_date(as.Date(paste0(SUBPERIOD_BREAKS[1], "-01")), "month") - days(1)
.b2     <- ceiling_date(as.Date(paste0(SUBPERIOD_BREAKS[2], "-01")), "month") - days(1)

make_sp <- function(lbl, letter, lo, hi) {
  n_m <- sum(.months >= lo & .months <= hi)            # months that enter the regressions
  win <- paste0(format(lo, "%b %Y"), "--", format(hi, "%b %Y"))
  list(label   = lbl,
       window  = win,
       panel   = paste0("Panel ", letter, ": ", lbl, " (", win, ", ", n_m, " months)"),
       date_lo = lo,
       date_hi = hi)
}
SUBPERIODS <- list(
  P1 = make_sp("P1", "A", .first,         .b1),
  P2 = make_sp("P2", "B", .b1 + days(1),  .b2),
  P3 = make_sp("P3", "C", .b2 + days(1),  .last)
)
cat("\nSub-periods (from SUBPERIOD_BREAKS):\n")
for (.sp in SUBPERIODS) cat("  ", .sp$panel, "\n")
rm(.bd, .months, .ym, .first, .last, .b1, .b2, .sp)

results <- lapply(SUBPERIODS, estimate_subperiod,
                  panel_source = panel_incubation)

# Persist numeric outputs for audit / Excel inspection
write_xlsx(
  list(
    P1_alpha_full   = results$P1$alpha_full,
    P2_alpha_full   = results$P2$alpha_full,
    P3_alpha_full   = results$P3$alpha_full,
    P1_boot_summary = results$P1$boot_summary,
    P2_boot_summary = results$P2$boot_summary,
    P3_boot_summary = results$P3$boot_summary,
    P1_bsw          = results$P1$bsw_df,
    P2_bsw          = results$P2$bsw_df,
    P3_bsw          = results$P3$bsw_df,
    P1_alpha_agg    = results$P1$alpha_agg,
    P2_alpha_agg    = results$P2$alpha_agg,
    P3_alpha_agg    = results$P3$alpha_agg,
    pi0_summary     = data.frame(
      period = c("P1", "P2", "P3"),
      window = sapply(results, `[[`, "window"),
      pi_0   = sapply(results, `[[`, "pi_0"),
      n      = sapply(results, `[[`, "pi_0_n"),
      stringsAsFactors = FALSE
    )
  ),
  "subperiod_results.xlsx"
)
cat("\nWritten: subperiod_results.xlsx\n")

# =============================================================================
# 4. TABLE D.2 -- AGGREGATE PORTFOLIO ALPHA BY GROUP AND SUB-PERIOD
# =============================================================================
# FF (2010) portfolio regression methodology (v2.0 refactor). Per sub-period,
# 5 groups x 2 rows (coef + t-stat) = 10 rows.
# =============================================================================
cat("\n=== 4. Table D.2 (perf aggregate, FF-style portfolio regression) ===\n")

# N column: portfolio constituents per group (r$n_funds, from estimate_subperiod).

make_d2_block <- function(alpha_agg, n_funds) {
  out <- list()
  for (grp in c("Active", "Passive", "Unknown", "Active + Passive", "Full Sample")) {
    eg <- alpha_agg %>% filter(ap_group == grp, weighting == "EW")
    vg <- alpha_agg %>% filter(ap_group == grp, weighting == "VW")
    if (nrow(eg) == 0L || nrow(vg) == 0L) next
    out[[paste0(grp, "_c")]] <- data.frame(
      Group    = grp,
      N        = formatC(n_funds[[grp]], format = "d", big.mark = ","),
      T        = formatC(eg$n_months,    format = "d"),
      EW_Gross = add_stars(fmt(eg$alpha_car),     eg$t_car),
      VW_Gross = add_stars(fmt(vg$alpha_car),     vg$t_car),
      EW_Net   = add_stars(fmt(eg$alpha_car_net), eg$t_car_net),
      VW_Net   = add_stars(fmt(vg$alpha_car_net), vg$t_car_net),
      stringsAsFactors = FALSE
    )
    out[[paste0(grp, "_t")]] <- data.frame(
      Group    = "t(coef)",
      N        = "",
      T        = "",
      EW_Gross = paste0("(", fmt(eg$t_car,     2), ")"),
      VW_Gross = paste0("(", fmt(vg$t_car,     2), ")"),
      EW_Net   = paste0("(", fmt(eg$t_car_net, 2), ")"),
      VW_Net   = paste0("(", fmt(vg$t_car_net, 2), ")"),
      stringsAsFactors = FALSE
    )
  }
  df <- do.call(rbind, out)
  rownames(df) <- NULL
  df
}

perf_data <- bind_rows(lapply(results,
                              function(r) make_d2_block(r$alpha_agg, r$n_funds)))
rownames(perf_data) <- NULL

# 10 rows per sub-period panel (5 groups * 2 rows).
ROWS_PP <- 10L
perf_packs <- data.frame(
  label = sapply(results, `[[`, "panel"),
  start = (seq_along(results) - 1L) * ROWS_PP + 1L,
  end   = seq_along(results) * ROWS_PP,
  stringsAsFactors = FALSE
)

fn_d2 <- paste(
  "Annualised \\\\textcite{Carhart1997} four-factor alpha (\\\\%) from portfolio",
  "regressions of the monthly aggregate return of each group on the market,",
  "size, value, and momentum factors, estimated separately within each",
  "sub-period, following \\\\textcite{FamaFrench2010}.",
  "EW: equal-weighted; VW: lagged-TNA-weighted",
  "$w_{i,t-1} = \\\\text{TNA}_{i,t-1} / \\\\sum_j \\\\text{TNA}_{j,t-1}$.",
  "Net returns are NAV-based (net of expenses); gross returns add back one-twelfth",
  "of the annual expense ratio each month \\\\parencite{FamaFrench2010}.",
  "Newey-West $t$-stats (6-month lag) in parentheses;",
  "$^{*}$, $^{**}$, $^{***}$: 10\\\\%, 5\\\\%, 1\\\\%.",
  "$N$: unique funds; $T$: months in the regression.",
  "Sub-period boundaries are two of the Bai-Perron break dates (Appendix~\\\\ref{app:subperiod}).",
  "Sample: Incubation-corrected panel (Evans 2010), no date cap; performance-comparison subsample per flagged\\\\_funds.xlsx."
)

latex_d2 <- perf_data %>%
  kbl(format    = "latex",
      row.names = FALSE,
      booktabs  = TRUE,
      linesep   = "",
      escape    = FALSE,
      caption   = "Aggregate Portfolio Alpha by Group and Sub-Period (\\%, Annualised)",
      label     = "subperiod_perf_aggregate",
      col.names = c("Group", "$N$", "$T$",
                    "EW", "VW", "EW", "VW"),
      align     = "lrrrrrr") %>%
  kable_styling(latex_options = "hold_position") %>%
  add_header_above(c(" " = 3, "Gross Alpha" = 2, "Net Alpha" = 2), bold = FALSE) %>%
  footnote(general        = fn_d2,
           general_title  = "",
           escape         = FALSE,
           threeparttable = TRUE)

# Horizontal rule after Unknown's t-stat row within each panel.
# Unknown occupies rows 5-6 (coef + t-stat) within a 10-row panel,
# so the rule goes after (panel_start + 5) = panel_start + 5L.
for (i in seq_len(nrow(perf_packs))) {
  latex_d2 <- latex_d2 %>%
    pack_rows(perf_packs$label[i], perf_packs$start[i], perf_packs$end[i],
              bold = FALSE, italic = TRUE,
              hline_before = (i > 1), hline_after = FALSE) %>%
    row_spec(perf_packs$start[i] + 5L, hline_after = TRUE)
}

writeLines(clean_latex(as.character(latex_d2), resize = TRUE),
           "table_subperiod_perf_aggregate.tex")
cat("Written: table_subperiod_perf_aggregate.tex\n")

# =============================================================================
# 5. TABLE D.3 -- FF BOOTSTRAP PERCENTILES  (parallels Table 6)
# =============================================================================
cat("\n=== 5. Table D.3 (bootstrap tails, parallels Table 6) ===\n")

build_boot_block <- function(boot_summary) {
  boot_summary %>%
    filter(percentile %in% c(1, 5, 10, 50, 90, 95, 99)) %>%
    mutate(
      Pct       = paste0(percentile, "\\%"),
      Actual_t  = sapply(t_alpha_actual,   fmt, digits = 3),
      Sim_Mean  = sapply(t_alpha_sim_mean, fmt, digits = 3),
      Prob_Luck = fmt_prob(pct_runs_below),
      # Same rule in both tails: the label follows the reported probability
      Interpretation = case_when(
        pct_runs_below < 5  ~ "Worse than luck",
        pct_runs_below > 95 ~ "Better than luck",
        TRUE                ~ "Consistent with zero skill"
      )
    ) %>%
    select(Pct, Actual_t, Sim_Mean, Prob_Luck, Interpretation)
}

boot_data <- bind_rows(lapply(results,
                              function(r) build_boot_block(r$boot_summary)))
rownames(boot_data) <- NULL

BOOT_ROWS_PP <- 7L
boot_packs <- data.frame(
  label = sapply(results, `[[`, "panel"),
  start = (seq_along(results) - 1L) * BOOT_ROWS_PP + 1L,
  end   = seq_along(results) * BOOT_ROWS_PP,
  stringsAsFactors = FALSE
)

n_active_bs_vec <- sapply(results, function(r)
  sum(r$alpha_full$ap_group == "Active" &
        !is.na(r$alpha_full$alpha_t_nw)))

fn_d3 <- paste(
  "Bootstrap procedure follows \\\\textcite{FamaFrench2010}, applied separately",
  "within each sub-period. For each fund, estimated monthly alpha is subtracted",
  "from the excess return series to construct a zero-alpha null return.",
  "In each of $B = 10{,}000$ bootstrap iterations, calendar months (within the",
  "sub-period window) are resampled with replacement and kept in the order drawn,",
  "the same draw applying to all funds, preserving cross-sectional",
  "factor return dependence. The \\\\textcite{Carhart1997} four-factor model is",
  "re-estimated on each resampled series.",
  "\\\\textit{Actual} $t(\\\\hat{\\\\alpha})$: percentile of the empirical",
  "$t$-statistic distribution across active funds in the sub-period.",
  "\\\\textit{Sim.\\\\ Mean}: average of that percentile across all iterations.",
  "\\\\textit{Prob.\\\\ Luck}: fraction of iterations in which the simulated",
  "percentile falls below the actual value; values below 5\\\\% (above 95\\\\%)",
  "indicate that the actual percentile is worse (better) than zero-alpha luck",
  "would produce; 0.0\\\\% means no iteration and $<$0.1\\\\% means 1 to 4 of 10,000.",
  "Newey-West standard errors with a 6-month lag are used throughout.",
  paste0("Active fund counts: P1 $N = ", formatC(n_active_bs_vec[1], format = "d", big.mark = ","),
         "$; P2 $N = ", formatC(n_active_bs_vec[2], format = "d", big.mark = ","),
         "$; P3 $N = ", formatC(n_active_bs_vec[3], format = "d", big.mark = ","), "$."),
  "Sample: Incubation-corrected panel (Evans 2010), no date cap; performance-comparison subsample per flagged\\\\_funds.xlsx."
)

latex_d3 <- boot_data %>%
  kbl(format    = "latex",
      row.names = FALSE,
      booktabs  = TRUE,
      escape    = FALSE,
      linesep   = "",
      caption   = "Fama--French (2010) Bootstrap: Actual vs.\\ Simulated $t(\\hat{\\alpha})$ Percentiles by Sub-Period",
      label     = "subperiod_bootstrap_tails",
      col.names = c("Percentile", "Actual $t(\\hat{\\alpha})$",
                    "Sim.\\ Mean", "Prob.\\ Luck", "Interpretation"),
      align     = c("r", "r", "r", "r", "l")) %>%
  kable_styling(latex_options = "hold_position") %>%
  column_spec(5, width = "13em") %>%
  footnote(general        = fn_d3,
           general_title  = "",
           escape         = FALSE,
           threeparttable = TRUE)

for (i in seq_len(nrow(boot_packs))) {
  latex_d3 <- latex_d3 %>%
    pack_rows(boot_packs$label[i], boot_packs$start[i], boot_packs$end[i],
              bold = FALSE, italic = TRUE,
              hline_before = (i > 1), hline_after = FALSE)
}

d3_str <- as.character(latex_d3)
# Strip any \addlinespace kableExtra may have inserted (bug-fix rule from
# alpha_reporting.R v7.6 -- empty replacement, not "\n").
d3_str <- gsub("\\\\addlinespace[^\n]*\n", "", d3_str)
d3_str <- threeparttable_note_after(d3_str)  # PHASE B: move note outside float
writeLines(clean_latex(d3_str, resize = FALSE, small = TRUE),
           "table_subperiod_bootstrap_tails.tex")
cat("Written: table_subperiod_bootstrap_tails.tex\n")

# =============================================================================
# 6. TABLE D.4 -- STOREY pi_0 BY SUB-PERIOD  (parallels Table 7)
# =============================================================================
# One row per sub-period; mirrors alpha_reporting.R v7.8 Table 7 column layout:
# Metric | Estimate | N | lambda | Interpretation
# =============================================================================
cat("\n=== 6. Table D.4 (pi_0 estimate, parallels Table 7) ===\n")

interp_pi0 <- function(p) {
  if (p > 0.90) "Industry dominated by luck"
  else if (p > 0.75) "Heterogeneous skill; majority zero-alpha"
  else if (p > 0.50) "Moderate skill heterogeneity"
  else "Substantial skilled-fund presence"
}

pi0_data <- data.frame(
  Period         = sapply(results, function(r)
    paste0(r$label, " (", r$window, ")")),
  Estimate       = sapply(results, function(r)
    paste0(formatC(r$pi_0 * 100, format = "f", digits = 1), "\\%")),
  N              = sapply(results, function(r) formatC(r$pi_0_n, format = "d", big.mark = ",")),
  Lambda         = formatC(LAMBDA_STOREY, format = "f", digits = 1),
  Interpretation = sapply(results, function(r) interp_pi0(r$pi_0)),
  stringsAsFactors = FALSE
)
rownames(pi0_data) <- NULL

fn_d4 <- paste(
  "The proportion of true zero-alpha active funds ($\\\\hat{\\\\pi}_0$) estimated",
  "separately within each sub-period, following \\\\textcite{Storey2002} and",
  "\\\\textcite{BarrasScailletWermers2010}, using $p$-values from Newey-West",
  "$t$-tests on sub-period \\\\textcite{Carhart1997} four-factor alphas.",
  "The estimator is $\\\\hat{\\\\pi}_0 = |\\\\{p_i > \\\\lambda\\\\}| \\\\,/\\\\, [N(1-\\\\lambda)]$,",
  "where $\\\\lambda = 0.5$ is the standard tuning parameter.",
  "$N$: number of active funds with $\\\\geq 24$ monthly observations within the",
  "sub-period. Passive and Unknown-classified funds are excluded.",
  "Sample: Incubation-corrected panel (Evans 2010), no date cap; performance-comparison subsample per flagged\\\\_funds.xlsx.",
  "The four-way decomposition of skilled, unskilled, and lucky fund proportions",
  "implied by these estimates is reported in",
  "Table~\\\\ref{tab:subperiod_bsw_decomposition}."
)

latex_d4 <- pi0_data %>%
  kbl(format    = "latex",
      row.names = FALSE,
      booktabs  = TRUE,
      escape    = FALSE,
      caption   = "Aggregate Skill Estimate by Sub-Period: Proportion of True Zero-Alpha Active Funds",
      label     = "subperiod_pi0_estimate",
      col.names = c("Sub-Period", "$\\hat{\\pi}_0$", "$N$",
                    "$\\lambda$", "Interpretation"),
      align     = c("l", "r", "r", "r", "l")) %>%
  kable_styling(latex_options = "hold_position") %>%
  column_spec(1, width = "13em") %>%
  column_spec(5, width = "14em") %>%
  footnote(general        = fn_d4,
           general_title  = "",
           escape         = FALSE,
           threeparttable = TRUE)

writeLines(clean_latex(as.character(latex_d4), resize = TRUE),
           "table_subperiod_pi0_estimate.tex")
cat("Written: table_subperiod_pi0_estimate.tex\n")

# =============================================================================
# 7. TABLE D.5 -- BSW FOUR-WAY DECOMPOSITION  (parallels Table 8)
# =============================================================================
cat("\n=== 7. Table D.5 (BSW decomposition, parallels Table 8) ===\n")

build_bsw_block <- function(bsw_df) {
  bsw_df %>%
    mutate(
      gamma_fmt       = paste0(formatC(gamma, format = "f", digits = 0), "\\%"),
      S_neg_fmt       = sapply(S_neg_pct,       fmt1),
      S_pos_fmt       = sapply(S_pos_pct,       fmt1),
      F_luck_fmt      = sapply(F_luck_pct,      fmt1),
      T_unskilled_fmt = sapply(T_unskilled_pct, fmt1),
      T_skilled_fmt   = sapply(T_skilled_pct,   fmt1)
    ) %>%
    select(gamma_fmt, S_neg_fmt, S_pos_fmt, F_luck_fmt,
           T_unskilled_fmt, T_skilled_fmt)
}

bsw_data <- bind_rows(lapply(results,
                             function(r) build_bsw_block(r$bsw_df)))
rownames(bsw_data) <- NULL

BSW_ROWS_PP  <- length(GAMMA_GRID)   # 10 rows per panel

bsw_packs <- data.frame(
  label = sapply(results, `[[`, "panel"),
  start = (seq_along(results) - 1L) * BSW_ROWS_PP + 1L,
  end   = seq_along(results) * BSW_ROWS_PP,
  stringsAsFactors = FALSE
)

fn_d5 <- paste(
  "\\\\textcite{BarrasScailletWermers2010} four-way decomposition, estimated",
  "separately within each sub-period.",
  "$S^-_\\\\gamma$ ($S^+_\\\\gamma$): fraction of active funds with significantly",
  "negative (positive) Newey-West $t(\\\\hat{\\\\alpha})$ at two-sided level $\\\\gamma$,",
  "using sub-period \\\\textcite{Carhart1997} alphas and the same Student-$t$ $p$-values as $\\\\hat{\\\\pi}_0$.",
  "$F_\\\\gamma = \\\\hat{\\\\pi}_0 \\\\cdot \\\\gamma/2$: expected false discoveries per tail,",
  "with $\\\\hat{\\\\pi}_0$ the sub-period \\\\textcite{Storey2002} estimator at",
  "$\\\\lambda = 0.5$ (Table~\\\\ref{tab:subperiod_pi0_estimate}).",
  "$T^\\\\pm_\\\\gamma = S^\\\\pm_\\\\gamma - F_\\\\gamma$: genuinely unskilled ($-$) or skilled ($+$).",
  paste0("The row at $\\\\gamma^* = ", formatC(GAMMA_STAR, format = "f", digits = 2),
         "$ gives population-level"),
  "$\\\\hat{\\\\pi}^-_A$, $\\\\hat{\\\\pi}^+_A$ (BSW 2010, eq.~8). Negative $T^+_\\\\gamma$ means right-tail",
  "significance does not exceed the false-discovery rate.",
  "Percentages of the sub-period active-fund universe.",
  "Sample: Incubation-corrected panel (Evans 2010), no date cap; performance-comparison subsample per flagged\\\\_funds.xlsx."
)

latex_d5 <- bsw_data %>%
  kbl(format    = "latex",
      row.names = FALSE,
      booktabs  = TRUE,
      escape    = FALSE,
      linesep   = "",
      caption   = "BSW (2010) Four-Way Decomposition by Sub-Period: Proportions of Skilled, Unskilled, and Lucky Funds (\\%)",
      label     = "subperiod_bsw_decomposition",
      col.names = c("$\\gamma$", "$S^-_\\gamma$", "$S^+_\\gamma$",
                    "$F_\\gamma$", "$T^-_\\gamma$", "$T^+_\\gamma$"),
      align     = c("r", "r", "r", "r", "r", "r")) %>%
  kable_styling(latex_options = "hold_position") %>%
  add_header_above(c(" " = 1,
                     "Observed Tails"   = 2,
                     "False Disc."      = 1,
                     "True Proportions" = 2),
                   escape = FALSE, bold = FALSE) %>%
  footnote(general        = fn_d5,
           general_title  = "",
           escape         = FALSE,
           threeparttable = TRUE)

for (i in seq_len(nrow(bsw_packs))) {
  latex_d5 <- latex_d5 %>%
    pack_rows(bsw_packs$label[i], bsw_packs$start[i], bsw_packs$end[i],
              bold = FALSE, italic = TRUE,
              hline_before = (i > 1), hline_after = FALSE)
}

d5_str <- as.character(latex_d5)
d5_str <- gsub("\\\\addlinespace[^\n]*\n", "", d5_str)
d5_str <- threeparttable_note_after(d5_str)  # PHASE B: move note outside float
writeLines(clean_latex(d5_str, resize = FALSE, small = TRUE),
           "table_subperiod_bsw_decomposition.tex")
cat("Written: table_subperiod_bsw_decomposition.tex\n")

# =============================================================================
# 8. TABLE D.6 -- ACTIVE VS PASSIVE AGGREGATE PORTFOLIO ALPHA
#                  (parallels Table 10 / portfolio_sorts.R table_port_agg_alpha)
# =============================================================================
# Row structure per panel (8 rows):
#   Active  (EW) coefficient | t(coef)
#   Active  (VW) coefficient | t(coef)
#   Passive (EW) coefficient | t(coef)
#   Passive (VW) coefficient | t(coef)
# =============================================================================
cat("\n=== 8. Table D.6 (portfolio agg alpha, parallels Table 10) ===\n")

make_d6_block <- function(alpha_agg) {
  out <- list()
  for (grp in c("Active", "Passive")) {
    for (wt in c("EW", "VW")) {
      r     <- alpha_agg %>% filter(ap_group == grp, weighting == wt)
      label <- paste0(grp, " (", wt, ")")
      if (nrow(r) == 0) next
      out[[paste0(label, "_c")]] <- data.frame(
        Portfolio = label,
        a_capm = add_stars(fmt(r$alpha_capm), r$t_capm),
        a_ff3  = add_stars(fmt(r$alpha_ff3),  r$t_ff3),
        a_car  = add_stars(fmt(r$alpha_car),  r$t_car),
        stringsAsFactors = FALSE
      )
      out[[paste0(label, "_t")]] <- data.frame(
        Portfolio = "t(coef)",
        a_capm = paste0("(", fmt(r$t_capm, 2), ")"),
        a_ff3  = paste0("(", fmt(r$t_ff3,  2), ")"),
        a_car  = paste0("(", fmt(r$t_car,  2), ")"),
        stringsAsFactors = FALSE
      )
    }
  }
  df <- do.call(rbind, out)
  rownames(df) <- NULL
  df
}

d6_data <- bind_rows(lapply(results,
                            function(r) make_d6_block(r$alpha_agg)))
rownames(d6_data) <- NULL

D6_ROWS_PP <- 8L  # 4 portfolios * (coef + t-stat)
d6_packs <- data.frame(
  label = sapply(results, `[[`, "panel"),
  start = (seq_along(results) - 1L) * D6_ROWS_PP + 1L,
  end   = seq_along(results) * D6_ROWS_PP,
  stringsAsFactors = FALSE
)

fn_d6 <- paste(
  "Monthly EW and VW aggregate portfolio returns regressed on CAPM,",
  "Fama-French three-factor, and \\\\textcite{Carhart1997} four-factor models,",
  "estimated separately within each sub-period.",
  "Alphas annualised ($\\\\times 12$) and expressed as \\\\%.",
  "Newey-West $t$-statistics (6-month lag) in parentheses.",
  "$^{*}$, $^{**}$, $^{***}$: significant at 10\\\\%, 5\\\\%, 1\\\\% respectively.",
  "Sample: Incubation-corrected panel (Evans 2010), no date cap; performance-comparison subsample per flagged\\\\_funds.xlsx."
)

latex_d6 <- d6_data %>%
  kbl(format    = "latex",
      row.names = FALSE,
      booktabs  = TRUE,
      linesep   = "",
      escape    = FALSE,
      caption   = "Active vs.\\ Passive Aggregate Portfolio Alpha by Sub-Period (\\%, Annualised)",
      label     = "subperiod_port_agg_alpha",
      col.names = c("Portfolio",
                    "$\\alpha_{\\text{CAPM}}$",
                    "$\\alpha_{\\text{FF3}}$",
                    "$\\alpha_{\\text{Car}}$"),
      align     = c("l", "r", "r", "r")) %>%
  kable_styling(latex_options = "hold_position") %>%
  footnote(general        = fn_d6,
           general_title  = "",
           escape         = FALSE,
           threeparttable = TRUE)

for (i in seq_len(nrow(d6_packs))) {
  latex_d6 <- latex_d6 %>%
    pack_rows(d6_packs$label[i], d6_packs$start[i], d6_packs$end[i],
              bold = FALSE, italic = TRUE,
              hline_before = (i > 1), hline_after = FALSE)
}

writeLines(clean_latex(as.character(latex_d6), resize = FALSE, small = FALSE),
           "table_subperiod_port_agg_alpha.tex")
cat("Written: table_subperiod_port_agg_alpha.tex\n")

# =============================================================================
# 9. SUMMARY
# =============================================================================
cat("\n=== SUB-PERIOD ANALYSIS COMPLETE ===\n")
for (r in results) {
  cat(sprintf("  %s (%s): N_active=%d, pi_0=%.1f%%\n",
              r$label, r$window,
              sum(r$alpha_full$ap_group == "Active" &
                    !is.na(r$alpha_full$alpha_t_nw)),
              r$pi_0 * 100))
}
cat("\nLaTeX outputs (move into your tables/ subdirectory):\n")
cat("  table_subperiod_perf_aggregate.tex      parallels Table 5\n")
cat("  table_subperiod_bootstrap_tails.tex     parallels Table 6\n")
cat("  table_subperiod_pi0_estimate.tex        parallels Table 7\n")
cat("  table_subperiod_bsw_decomposition.tex   parallels Table 8\n")
cat("  table_subperiod_port_agg_alpha.tex      parallels Table 10\n")