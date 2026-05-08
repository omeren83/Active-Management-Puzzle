# =============================================================================
# ROBUSTNESS ALPHA ESTIMATION: FF6 AND CARHART + PS LIQUIDITY (v1.3)
#
# v1.3 changes vs v1.2 (Family B audit):
#   filter(!excluded_perf) added to the ap_robust panel-prep stage so the
#   FF6 and C5 robustness specifications are estimated on the same
#   performance-comparison subsample as the main-text Carhart specification
#   (alpha_estimation.R v2.7). Without this filter, the Appendix E
#   robustness tables would describe a strictly larger universe than the
#   main-text tables, producing a spurious composition difference between
#   specifications. Per dissertation §4.7 the perf-comparison subsample
#   excludes 585 funds total, 149 of which are SECTOR_FUND-only flags
#   (the remainder are caught by Step 8c at source).
#
# v1.2 (carried forward) implementation
# -----
# Purpose
# -------
# Re-estimates the full-period alpha regressions and the Fama-French (2010)
# bootstrap + BSW decomposition under two alternative factor models, for the
# Appendix E robustness section. The main-text specification remains the
# Carhart (1997) four-factor model produced by alpha_estimation.R (v2.6).
#
# Specifications estimated here
# -----------------------------
#   FF6 : MKT_RF + SMB + HML + RMW + CMA + MOM
#         Fama-French (2015) five-factor + Carhart momentum.
#   C5  : MKT_RF + SMB + HML + MOM + PSL
#         Carhart (1997) four-factor + Pastor-Stambaugh (2003) traded liquidity.
#
# v1.1 changes vs v1.0
# --------------------
#   (a) Per-spec sample filter.  v1.0 required all factors across both specs
#       non-missing, which incorrectly truncated FF6 to the PSL coverage window.
#       v1.1 filters separately per spec so each uses its maximum sample.
#   (b) Defensive -99 -> NA conversion for PSL.  Pastor-Stambaugh's raw file
#       uses -99 as the missing-value sentinel for traded liquidity before
#       1968-01.  If imported to the factors sheet without cleaning, these
#       would enter regressions as -9900% monthly returns.  Cleaned at panel
#       prep regardless of whether the user cleaned them in Excel.
#   (c) Summary now reports observation count and date range per spec so
#       sample-window differences are visible to the reader.
#
# Scope
# -----
# This script deliberately reproduces ONLY the components affected by factor
# choice:  full-period alphas  ->  cross-sectional moments (Table 5)  ->
# bootstrap (Table 6) and BSW decomposition (Tables 7-8).  It does NOT recompute
# rolling alphas (Figure 2) or the performance ranks (flow regressions): those
# downstream objects use Carhart factors by design in the main text.
#
# Data notes
# ----------
# PSL (Pastor-Stambaugh 2003 traded liquidity factor, LIQ_V column in
# liq_data_1962_YYYY.txt on Pastor's Booth page) is in DECIMAL form, matching
# the existing factors sheet convention.  No percent / decimal conversion
# required.  As of 2026-04, PSL is released through December 2024, so the C5
# specification is estimated on 1994-12 -- 2024-12 rather than the full
# 1994-12 -- 2026-02 window; note accordingly in the appendix footnote.
#
# SMB convention
# --------------
# Existing SMB in the factors sheet is the FF3/Carhart SMB (single 2x3 size-BM
# sort).  French's FF5 SMB is slightly different (average across size-BM,
# size-OP, size-Inv sorts).  Correlation is >0.99 in most subsamples (Fama and
# French, 2015).  Keeping a single SMB definition across all three specifications
# isolates the marginal contribution of RMW / CMA / PSL and is defensible; this
# choice is noted in the appendix.
#
# Outputs
# -------
#   alpha_fullperiod_FF6.xlsx     |   alpha_fullperiod_C5.xlsx
#   bootstrap_results_FF6.xlsx    |   bootstrap_results_C5.xlsx
#   robust_alpha_summary.xlsx     |   side-by-side EW/VW vs Carhart baseline
#
# Prerequisites
# -------------
#   - data_import_and_cleaning.R has been sourced; panel_incubation exists with
#     new columns RMW, CMA, PSL populated from the factors sheet.
#   - alpha_estimation.R (v2.6) has been run; alpha_fullperiod.xlsx exists for
#     the side-by-side baseline comparison.
# =============================================================================

library(dplyr)
library(lubridate)
library(writexl)
library(readxl)
library(parallel)

# --- 0. CONFIGURATION --------------------------------------------------------

FACTOR_RF    <- "RF"
LIPPER_COL   <- "Lipper_Category"
EXP_COL      <- "Expense_Ratio"

MIN_OBS_FULL <- 24L
NW_LAG_FULL  <- 6L
B_RUNS       <- 10000L
MIN_OBS_BS   <- 8L
BOOT_SEED    <- 42L

N_CORES      <- max(1L, detectCores() - 1L)

PCTS <- c(1, 2, 3, 4, 5, 10, 20, 30, 40, 50,
          60, 70, 80, 90, 95, 96, 97, 98, 99)

GAMMA_GRID    <- c(0.05, 0.10, 0.15, 0.20, 0.25, 0.30, 0.35, 0.40, 0.45, 0.50)
LAMBDA_STOREY <- 0.5

# Factor specifications: list of character vectors of factor column names as
# they appear in panel_incubation after the pivot in data_import_and_cleaning.R.
SPECS <- list(
  FF6 = c("MKT_RF", "SMB", "HML", "RMW", "CMA", "MOM"),
  C5  = c("MKT_RF", "SMB", "HML", "MOM", "PSL")
)

# --- 1. HELPERS --------------------------------------------------------------

# OLS with explicit design matrix; returns coefficient vector, residuals, and
# scalar summaries.  Used by full-period regressions only.
fast_ols <- function(y, X) {
  tryCatch({
    XtX  <- crossprod(X)
    beta <- solve(XtX, crossprod(X, y))
    e    <- as.vector(y - X %*% beta)
    n    <- length(y); k <- ncol(X); s2 <- sum(e^2) / (n - k)
    list(beta = as.vector(beta), e = e,
         se_ols = sqrt(pmax(diag(s2 * solve(XtX)), 0)),
         r2 = 1 - sum(e^2) / sum((y - mean(y))^2),
         sigma = sqrt(s2), n = n, k = k)
  }, error = function(err) NULL)
}

# Newey-West HAC standard errors with Bartlett kernel.
nw_se <- function(X, e, lag) {
  T <- nrow(X); k <- ncol(X)
  tryCatch({
    XtX_inv <- solve(crossprod(X))
    scores  <- X * as.vector(e)
    S       <- crossprod(scores) / T
    if (lag > 0) {
      for (j in seq_len(lag)) {
        w  <- 1 - j / (lag + 1)
        Gj <- crossprod(scores[(j + 1):T, , drop = FALSE],
                        scores[1:(T - j),   , drop = FALSE]) / T
        S  <- S + w * (Gj + t(Gj))
      }
    }
    sqrt(pmax(diag(T * XtX_inv %*% S %*% XtX_inv), 0))
  }, error = function(err) rep(NA_real_, k))
}

# --- 2. PANEL PREP (shared across specs) -------------------------------------
cat("=== 1. Preparing panel with extended factor set ===\n")

# All factor columns needed across specs, deduped.
ALL_FACTORS_NEEDED <- unique(unlist(SPECS))

# Sanity check: required columns present in panel_incubation.
missing_cols <- setdiff(ALL_FACTORS_NEEDED, names(panel_incubation))
if (length(missing_cols) > 0L) {
  stop("panel_incubation missing factor columns: ",
       paste(missing_cols, collapse = ", "),
       ".  Add these rows to the 'factors' sheet of fund_data.xlsx and ",
       "re-run data_import_and_cleaning.R.")
}

# Build working panel: excess return on gross-of-fee return, standard cleaning.
# v1.1: Do NOT filter on all factors here -- filter is applied per-spec in the
# loop below so each specification uses its maximum available sample.
# Defensive -99 -> NA conversion for PSL (Pastor-Stambaugh raw sentinel for
# missing traded liquidity pre-1968).  Applied universally in case other
# columns inherit the same sentinel from source files.
ap_robust <- panel_incubation %>%
  filter(!excluded_perf) %>%      # v1.3: performance-comparison subsample
  rename(rf     = all_of(FACTOR_RF),
         lipper = all_of(LIPPER_COL),
         exp_r  = all_of(EXP_COL)) %>%
  mutate(excess_ret = ret_gross - rf,
         exp_r      = suppressWarnings(as.numeric(exp_r)),
         across(all_of(ALL_FACTORS_NEEDED),
                ~ ifelse(. == -99, NA_real_, .))) %>%
  filter(!is.na(excess_ret)) %>%
  arrange(Ticker, date)

cat("Funds entering robust estimation:", n_distinct(ap_robust$Ticker),
    " |  Fund-months:", nrow(ap_robust), "\n")

# Diagnostic: report factor coverage (non-missing months, date range).
cat("Factor coverage summary:\n")
for (f in ALL_FACTORS_NEEDED) {
  idx <- !is.na(ap_robust[[f]])
  if (any(idx)) {
    d_rng <- range(ap_robust$date[idx])
    cat(sprintf("  %-7s  n_obs = %6d  |  %s -- %s\n",
                f, sum(idx),
                format(d_rng[1], "%Y-%m"),
                format(d_rng[2], "%Y-%m")))
  } else {
    cat(sprintf("  %-7s  (no non-missing observations)\n", f))
  }
}

# --- 3. FULL-PERIOD REGRESSION (parameterised on factor set) -----------------

# Runs a single fund's full-period regression under a given factor spec.
# Returns one-row data.frame or NULL if obs < MIN_OBS_FULL.
run_full_period_spec <- function(tk, factor_cols) {
  d <- ap_split[[tk]]; n <- nrow(d)
  if (n < MIN_OBS_FULL) return(NULL)
  y <- d$excess_ret
  X <- cbind(1, as.matrix(d[, factor_cols, drop = FALSE]))
  colnames(X) <- c("Intercept", factor_cols)
  fit <- fast_ols(y, X); if (is.null(fit)) return(NULL)
  se_nw_vec <- nw_se(X, fit$e, lag = NW_LAG_FULL)
  k_total   <- ncol(X)                        # intercept + factors
  data.frame(
    Ticker        = tk,
    ap_group      = d$ap_group[1],
    lipper        = d$lipper[1],
    n_obs         = n,
    alpha_m       = fit$beta[1],
    alpha_ann     = fit$beta[1] * 12,
    alpha_t_nw    = fit$beta[1] / se_nw_vec[1],
    alpha_p_nw    = 2 * pt(-abs(fit$beta[1] / se_nw_vec[1]), n - k_total),
    exp_ratio     = d$exp_r[1],
    alpha_net_ann = (fit$beta[1] * 12) - (d$exp_r[1] / 100),
    # v1.2 fix: panel_incubation carries TNA as class_assets (primary) and
    # total_assets (fallback). d$tna in v1.1 silently returned NULL (no such
    # column), making mean_tna = NaN in the output xlsx and breaking VW weights
    # in build_robust_tables.R. Fixed to coalesce(class_assets, total_assets).
    mean_tna      = {tv <- coalesce(d$class_assets, d$total_assets);
    mean(tv[!is.na(tv) & tv > 0], na.rm = TRUE)}
  )
}

# --- 4. BOOTSTRAP (parameterised on factor set) ------------------------------

# Inner bootstrap worker: one resample draw of calendar months, returns a vector
# of simulated t-stats (one per fund) summarised at the PCTS percentiles.
# NW HAC SE used symmetrically with actual full-period regressions (v2.5
# symmetry fix, carried over from alpha_estimation.R).
one_boot_run <- function(run_id, bs_data, T_total, pcts_probs, min_obs, nw_lag) {
  samp     <- sample.int(T_total, size = T_total, replace = TRUE)
  samp_tab <- tabulate(samp, nbins = T_total)
  t_sim <- vapply(bs_data, function(d) {
    keep <- rep(seq_along(d$t_idx), times = samp_tab[d$t_idx])
    if (length(keep) < min_obs) return(NA_real_)
    y <- d$y_tilde[keep]
    X <- cbind(1, d$X_fac[keep, , drop = FALSE])
    tryCatch({
      XtX     <- crossprod(X)
      XtX_inv <- solve(XtX)
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

# --- 5. MAIN LOOP OVER SPECIFICATIONS ---------------------------------------

# Container for final side-by-side summary.
summary_rows <- list()

for (spec_name in names(SPECS)) {
  factor_cols <- SPECS[[spec_name]]
  cat("\n===============================================================\n")
  cat("  Specification:", spec_name, " (",
      paste(factor_cols, collapse = " + "), ")\n")
  cat("===============================================================\n")
  
  # v1.1: Per-spec filter -- keep fund-months where ALL factors used by THIS
  # spec are non-missing.  Previously a shared filter across specs incorrectly
  # truncated FF6 to the PSL coverage window.
  ap_spec <- ap_robust %>%
    filter(if_all(all_of(factor_cols), ~ !is.na(.)))
  ap_split <- split(ap_spec, ap_spec$Ticker)
  d_rng_spec <- range(ap_spec$date)
  cat(sprintf("  Panel: %d fund-month obs | %d funds | %s -- %s\n",
              nrow(ap_spec), length(ap_split),
              format(d_rng_spec[1], "%Y-%m"),
              format(d_rng_spec[2], "%Y-%m")))
  
  # 5a. Full-period regressions
  cat("  [a] Full-period OLS + NW...\n")
  alpha_full <- bind_rows(
    lapply(names(ap_split), run_full_period_spec, factor_cols = factor_cols)
  )
  cat("      Funds estimated:", nrow(alpha_full), "\n")
  
  # 5b. Bootstrap prep (active funds only)
  active_fp <- alpha_full %>%
    filter(ap_group == "Active", !is.na(alpha_t_nw))
  all_cal_dates <- sort(unique(ap_spec$date))
  T_total       <- length(all_cal_dates)
  date_idx_map  <- setNames(seq_len(T_total), as.character(all_cal_dates))
  
  bs_data <- lapply(active_fp$Ticker, function(tk) {
    a_hat <- active_fp$alpha_m[active_fp$Ticker == tk]
    d     <- ap_split[[tk]]
    list(
      t_idx   = date_idx_map[as.character(d$date)],
      y_tilde = d$excess_ret - a_hat,
      X_fac   = as.matrix(d[, factor_cols, drop = FALSE])
    )
  })
  names(bs_data) <- active_fp$Ticker
  
  # 5c. Bootstrap execution
  cat("  [b] Bootstrap (", B_RUNS, "runs, NW lag =", NW_LAG_FULL, ")...\n")
  cl <- makeCluster(N_CORES)
  clusterExport(cl,
                c("bs_data", "T_total", "one_boot_run",
                  "MIN_OBS_BS", "NW_LAG_FULL"),
                envir = environment())
  clusterSetRNGStream(cl, BOOT_SEED)
  boot_t0 <- Sys.time()
  boot_results <- parLapply(cl, seq_len(B_RUNS), one_boot_run,
                            bs_data, T_total, PCTS / 100,
                            MIN_OBS_BS, NW_LAG_FULL)
  stopCluster(cl)
  cat("      Wall time:",
      round(as.numeric(difftime(Sys.time(), boot_t0, units = "secs")), 1),
      "sec\n")
  sim_matrix <- do.call(rbind, boot_results)
  
  bootstrap_summary <- data.frame(
    percentile        = PCTS,
    t_alpha_actual    = as.numeric(quantile(active_fp$alpha_t_nw, PCTS / 100)),
    t_alpha_sim_mean  = colMeans(sim_matrix),
    pct_runs_below    = colMeans(sweep(sim_matrix, 2,
                                       as.numeric(quantile(active_fp$alpha_t_nw, PCTS / 100)),
                                       "<")) * 100
  )
  
  # 5d. BSW gamma-grid decomposition (same as alpha_estimation.R Section 7)
  cat("  [c] BSW gamma-grid decomposition...\n")
  p_vals_bsw <- 2 * pnorm(-abs(active_fp$alpha_t_nw))
  n_bsw      <- sum(!is.na(p_vals_bsw))
  pi0_bsw    <- min(1.0,
                    sum(p_vals_bsw > LAMBDA_STOREY, na.rm = TRUE) /
                      (n_bsw * (1 - LAMBDA_STOREY)))
  t_stats_bsw <- active_fp$alpha_t_nw
  
  bsw_df <- do.call(rbind, lapply(GAMMA_GRID, function(g) {
    t_thresh <- qnorm(1 - g / 2)
    S_neg    <- mean(t_stats_bsw < -t_thresh, na.rm = TRUE)
    S_pos    <- mean(t_stats_bsw >  t_thresh, na.rm = TRUE)
    F_luck   <- pi0_bsw * g / 2
    data.frame(
      gamma           = g * 100,
      S_neg_pct       = round(S_neg    * 100, 4),
      S_pos_pct       = round(S_pos    * 100, 4),
      F_luck_pct      = round(F_luck   * 100, 4),
      T_unskilled_pct = round((S_neg - F_luck) * 100, 4),
      T_skilled_pct   = round((S_pos - F_luck) * 100, 4)
    )
  }))
  pi_A_minus <- bsw_df$T_unskilled_pct[bsw_df$gamma == 20]
  pi_A_plus  <- bsw_df$T_skilled_pct[bsw_df$gamma == 20]
  cat(sprintf("      pi0 = %.1f%% | pi_A-_gamma*=20 = %.1f%% | pi_A+_gamma*=20 = %.1f%%\n",
              pi0_bsw * 100, pi_A_minus, pi_A_plus))
  
  bsw_meta <- data.frame(
    spec       = spec_name,
    lambda     = LAMBDA_STOREY,
    pi0_pct    = round(pi0_bsw * 100, 4),
    n_active   = n_bsw,
    gamma_star = 20,
    pi_A_minus = pi_A_minus,
    pi_A_plus  = pi_A_plus
  )
  
  # 5e. Save spec-specific outputs
  write_xlsx(alpha_full,
             sprintf("alpha_fullperiod_%s.xlsx", spec_name))
  write_xlsx(
    list(
      summary           = bootstrap_summary,
      fund_talpha       = active_fp,
      bsw_decomposition = bsw_df,
      bsw_meta          = bsw_meta
    ),
    sprintf("bootstrap_results_%s.xlsx", spec_name)
  )
  
  # 5f. Accumulate summary row (pooled EW / VW, gross and net, active only)
  active_full <- alpha_full %>% filter(ap_group == "Active")
  ew_gross <- mean(active_full$alpha_ann,     na.rm = TRUE)
  vw_wts   <- active_full$mean_tna
  vw_wts   <- ifelse(is.na(vw_wts) | vw_wts <= 0, NA_real_, vw_wts)
  vw_gross <- weighted.mean(active_full$alpha_ann, w = vw_wts, na.rm = TRUE)
  ew_net   <- mean(active_full$alpha_net_ann, na.rm = TRUE)
  vw_net   <- weighted.mean(active_full$alpha_net_ann, w = vw_wts, na.rm = TRUE)
  
  summary_rows[[spec_name]] <- data.frame(
    spec           = spec_name,
    factors        = paste(factor_cols, collapse = " + "),
    date_start     = format(d_rng_spec[1], "%Y-%m"),
    date_end       = format(d_rng_spec[2], "%Y-%m"),
    n_months       = length(unique(ap_spec$date)),
    n_fund_months  = nrow(ap_spec),
    n_active       = sum(active_full$ap_group == "Active"),
    ew_alpha_gross = ew_gross,
    vw_alpha_gross = vw_gross,
    ew_alpha_net   = ew_net,
    vw_alpha_net   = vw_net,
    pi0_pct        = round(pi0_bsw * 100, 2),
    pi_A_minus_pct = pi_A_minus,
    pi_A_plus_pct  = pi_A_plus
  )
}

# --- 6. SIDE-BY-SIDE SUMMARY WITH CARHART BASELINE ---------------------------
cat("\n=== 6. Building side-by-side summary vs Carhart baseline ===\n")

# Read Carhart baseline from alpha_estimation.R (v2.6) output if present.
baseline_row <- tryCatch({
  car <- read_excel("alpha_fullperiod.xlsx") %>%
    filter(ap_group == "Active")
  vw_w <- car$mean_tna
  vw_w <- ifelse(is.na(vw_w) | vw_w <= 0, NA_real_, vw_w)
  # Pi0 and BSW pop. est. from bootstrap_results.xlsx if available.
  bsw_meta_car <- tryCatch(
    read_excel("bootstrap_results.xlsx", sheet = "bsw_meta"),
    error = function(e) NULL
  )
  pi0_car <- if (!is.null(bsw_meta_car)) bsw_meta_car$pi0_pct[1] else NA_real_
  pim_car <- if (!is.null(bsw_meta_car)) bsw_meta_car$pi_A_minus[1] else NA_real_
  pip_car <- if (!is.null(bsw_meta_car)) bsw_meta_car$pi_A_plus[1] else NA_real_
  # Carhart sample window mirrors ap_robust minus any PSL filter -- use the
  # full range of panel dates for which MKT_RF/SMB/HML/MOM are non-missing.
  car_dates <- ap_robust %>%
    filter(if_all(all_of(c("MKT_RF", "SMB", "HML", "MOM")), ~ !is.na(.))) %>%
    pull(date)
  d_rng_car <- range(car_dates)
  data.frame(
    spec           = "Carhart4",
    factors        = "MKT_RF + SMB + HML + MOM",
    date_start     = format(d_rng_car[1], "%Y-%m"),
    date_end       = format(d_rng_car[2], "%Y-%m"),
    n_months       = length(unique(car_dates)),
    n_fund_months  = length(car_dates),
    n_active       = nrow(car),
    ew_alpha_gross = mean(car$alpha_ann,     na.rm = TRUE),
    vw_alpha_gross = weighted.mean(car$alpha_ann, w = vw_w, na.rm = TRUE),
    ew_alpha_net   = mean(car$alpha_net_ann, na.rm = TRUE),
    vw_alpha_net   = weighted.mean(car$alpha_net_ann, w = vw_w, na.rm = TRUE),
    pi0_pct        = pi0_car,
    pi_A_minus_pct = pim_car,
    pi_A_plus_pct  = pip_car
  )
}, error = function(e) {
  cat("  Note: alpha_fullperiod.xlsx not found; baseline row omitted.\n")
  NULL
})

summary_df <- bind_rows(baseline_row, summary_rows$FF6, summary_rows$C5)

# Convert decimal alphas to annual percent for presentation and rename to make
# units explicit (jury-readable Excel).
alpha_cols <- c("ew_alpha_gross", "vw_alpha_gross", "ew_alpha_net", "vw_alpha_net")
for (nm in alpha_cols) {
  summary_df[[nm]] <- round(summary_df[[nm]] * 100, 3)
}
names(summary_df)[match(alpha_cols, names(summary_df))] <-
  c("ew_alpha_gross_pct_yr", "vw_alpha_gross_pct_yr",
    "ew_alpha_net_pct_yr",   "vw_alpha_net_pct_yr")

write_xlsx(summary_df, "robust_alpha_summary.xlsx")

cat("\n--- Side-by-side pooled alphas (annual percent) ---\n")
print(summary_df)

cat("\n=== ROBUSTNESS ESTIMATION COMPLETE ===\n")
cat("Outputs:\n")
cat("  alpha_fullperiod_FF6.xlsx / alpha_fullperiod_C5.xlsx\n")
cat("  bootstrap_results_FF6.xlsx / bootstrap_results_C5.xlsx\n")
cat("  robust_alpha_summary.xlsx (side-by-side vs Carhart)\n")