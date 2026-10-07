# =============================================================================
# ALPHA ESTIMATION, ROLLING REGRESSIONS
# AND FAMA-FRENCH (2010) BOOTSTRAP SIMULATION (v2.8 - pipeline audit)
#
# v2.8 changes vs v2.7 (pipeline audit, Oct 2026):
#   (a) BOOTSTRAP KERNEL. v2.7 rebuilt each resampled series in calendar order
#       with duplicated months adjacent (rep(seq_along(t_idx), times = ...)).
#       Newey-West read the duplicates as autocorrelation and shrank simulated
#       t-statistics (SD ~0.8 instead of ~1), so "worse/better than luck" was
#       found too easily. Months now enter in draw order (match(samp, t_idx)).
#   (b) RNG independent of core count: all month draws are made on the main
#       process from BOOT_SEED and passed to the workers.
#   (c) Cache key carries BOOT_KERNEL; bump it whenever resampling code changes.
#   (d) BSW: pi0 and the tail counts S-/S+ use the same Student-t (n-5) NW
#       p-values (alpha_p_nw) as alpha_reporting.R; v2.7 used normal p-values.
#   (e) BSW population estimates pi_A-/pi_A+ at GAMMA_STAR = 0.45. BSW (2010,
#       eq. 8) take them at a "sufficiently high" gamma* chosen by MSE and note
#       that pre-set 0.35 or 0.45 give similar results; 0.20 is only the level
#       they use to display the tails. At 0.20, pi0 + pi_A- + pi_A+ < 1.
#   (f) Rolling window defined on a calendar-month index (t-35..t). Date
#       arithmetic on last-business-day dates dropped a month in some windows.
#   (g) Section 5 (rank construction, rank_data.xlsx) removed: unused downstream
#       (panel_regressions_setup.R builds its own ranks).
#   Inputs: ret_gross is now true gross (net index + ER/12, data_import v1.4);
#   funds with no expense ratio have no gross return and drop out here.
#
# v2.7 changes vs v2.6 (Family B audit):
#   (a) filter(!excluded_perf) added to the ap panel-prep stage. This restricts
#       full-period regressions, rolling regressions, rank construction, and
#       the bootstrap+BSW decomposition to the performance-comparison subsample
#       defined by flagged_funds.xlsx (sector funds, benchmark-mismatched
#       products, and entire-analysis exclusions all removed). Per dissertation
#       §4.7 the implied sample is approximately 3,179 funds pre-Evans, of
#       which ~2,400 active are eligible after the per-spec MIN_OBS_FULL=24
#       cut. Tables 5 (cross-sectional moments), 6 (bootstrap tails), 7
#       (pi0), 8 (BSW decomposition), 9 (BSW gamma grid), and Figure 3
#       (luck-vs-skill PDFs/CDFs) all change downstream.
#   (b) Mean-TNA computation in run_full_period() corrected. v2.6 referenced
#       d$tna which was NULL (the column is named class_assets/total_assets
#       in panel_incubation; tna is added later by flow_calculation.R). The
#       resulting NaN mean_tna in alpha_fullperiod.xlsx broke VW weights in
#       build_robust_tables.R, requiring the patch_mean_tna.R workaround.
#       v2.7 uses coalesce(class_assets, total_assets) directly. patch_mean_tna.R
#       is no longer required and has been marked DEPRECATED.
#
# v2.6 change vs v2.5:
#   Source panel switched from panel_trimmed (Evans-corrected, 1995-2023 cap)
#   to panel_incubation (Evans-corrected, full December 1994-February 2026
#   range). The 2023 date cap is dropped to avoid artificially truncating the
#   sample; panel_trimmed is retained in session for the upcoming H1-H4 panel
#   regressions that require sentiment/margin series with restricted coverage.
#   This change affects every downstream output that uses alpha_estimation.R's
#   artefacts: Figure 2 (rolling alphas), Table 5 (cross-sectional alpha
#   moments), Table 6 (bootstrap tails), Table 7 (pi0), Table 8 (BSW
#   decomposition), Figure 3 (CDF/PDF). Numerical results will differ from
#   v2.5; in particular, the tail of the sample (2024-2026) enters the rolling
#   series and the Bai-Perron structural break test should be re-run on the
#   updated alpha_rolling.xlsx output before selecting sub-period thresholds.
#
# v2.5 change vs v2.4:
#   BUG FIX: Section 6 bootstrap (one_boot_run) now computes Newey-West HAC
#   standard errors with the same 6-month lag used for the full-period
#   regressions, instead of OLS standard errors. The previous version produced
#   an apples-to-oranges comparison: actual t-stats in Table 9 / Figure 3 were
#   NW-based while simulated t-stats from the bootstrap were OLS-based, so the
#   "Prob. Luck" column was comparing distributions estimated under different
#   SE assumptions. The fix restores symmetry: both actual and simulated t-stats
#   now use Newey-West SEs throughout. Compute cost approximately 2.6x the v2.4
#   bootstrap (measured locally; depends on parallelism). All other Tables 7-10b
#   are numerically unchanged because pi0/BSW are derived from alpha_p_nw, not
#   from the bootstrap.
#
# v2.4 change vs v2.3:
#   run_full_period() appends mean_tna (time-series mean of strictly positive
#   monthly TNA, USD millions) to alpha_fullperiod.xlsx.  Enables VW cross-
#   sectional averages in alpha_reporting.R Tables 7 and 8.
# Optimized for high-performance multicore systems.
#
# v2.3 change: Section 7 adds BSW (2010) gamma-grid four-way decomposition.
#   - Storey (2002) pi0 computed from active fund t-statistics.
#   - Skilled / Unskilled / Lucky proportions reported at gamma = 0.05 to 0.50.
#   - bsw_df and bsw_meta saved to new sheets in bootstrap_results.xlsx.
# =============================================================================

library(dplyr)
library(lubridate)
library(writexl)
library(parallel)
library(zoo)

# --- 0. CONFIGURATION ---
FACTOR_MKT   <- "MKT_RF"          
FACTOR_SMB   <- "SMB"             
FACTOR_HML   <- "HML"             
FACTOR_MOM   <- "MOM"             
FACTOR_RF    <- "RF"              
LIPPER_COL   <- "Lipper_Category" 
EXP_COL      <- "Expense_Ratio"   

MIN_OBS_FULL <- 24L               
MIN_OBS_ROLL <- 24L               
NW_LAG_FULL  <- 6L                
NW_LAG_ROLL  <- 2L                

B_RUNS       <- 10000L            
MIN_OBS_BS   <- 8L                
BOOT_SEED    <- 42L               
USE_PARALLEL <- TRUE              
N_CORES      <- max(1L, detectCores() - 1L)
USE_BOOT_CACHE <- TRUE
BOOT_KERNEL  <- "v2.8-draw-order"  # part of the cache key; bump on resampling changes
BOOT_CACHE_DIR <- file.path(".", paste0("cache_", Sys.info()[["nodename"]]))
dir.create(BOOT_CACHE_DIR, showWarnings = FALSE)  # no-op if already exists

PCTS <- c(1, 2, 3, 4, 5, 10, 20, 30, 40, 50, 60, 70, 80, 90, 95, 96, 97, 98, 99)

# BSW gamma grid following Barras, Scaillet & Wermers (2010), Table III
GAMMA_GRID   <- c(0.05, 0.10, 0.15, 0.20, 0.25, 0.30, 0.35, 0.40, 0.45, 0.50)
LAMBDA_STOREY <- 0.5              # Storey (2002) tuning parameter
GAMMA_STAR    <- 0.45             # BSW (2010) eq. 8: gamma* for pi_A-/pi_A+

# --- 1. DATA PREPARATION ---
cat("=== 1. Data preparation ===\n")
ap <- panel_incubation %>%
  filter(!excluded_perf) %>%      # v2.7: performance-comparison subsample
  rename(mkt_rf = all_of(FACTOR_MKT), smb = all_of(FACTOR_SMB), hml = all_of(FACTOR_HML),
         mom = all_of(FACTOR_MOM), rf = all_of(FACTOR_RF), lipper = all_of(LIPPER_COL),
         exp_r  = all_of(EXP_COL)) %>%
  mutate(ret_rank = ret_gross, excess_ret = ret_gross - rf,   # ret_rank: rolling vol input
         exp_r = suppressWarnings(as.numeric(exp_r)),
         # v2.7: derive tna locally so this script does not depend on
         # flow_calculation.R having been run first.
         tna_local = coalesce(class_assets, total_assets)) %>%
  filter(!is.na(excess_ret), !is.na(mkt_rf), !is.na(smb), !is.na(hml), !is.na(mom)) %>%
  arrange(Ticker, date)

cat("  Funds entering estimation:", n_distinct(ap$Ticker),
    " |  Fund-months:", nrow(ap), "\n")

ap_split <- split(ap, ap$Ticker)

# --- 2. HELPER FUNCTIONS ---
fast_ols <- function(y, X) {
  tryCatch({
    XtX <- crossprod(X); beta <- solve(XtX, crossprod(X, y))
    e <- as.vector(y - X %*% beta); n <- length(y); k <- ncol(X); s2 <- sum(e^2)/(n-k)
    list(beta=as.vector(beta), e=e, se_ols=sqrt(pmax(diag(s2*solve(XtX)),0)), 
         r2=1-sum(e^2)/sum((y-mean(y))^2), sigma=sqrt(s2), n=n, k=k)
  }, error = function(err) NULL)
}

nw_se <- function(X, e, lag) {
  T <- nrow(X); k <- ncol(X)
  tryCatch({
    XtX_inv <- solve(crossprod(X)); scores <- X * as.vector(e); S <- crossprod(scores)/T
    if (lag > 0) {
      for (j in seq_len(lag)) {
        w <- 1 - j/(lag+1); Gj <- crossprod(scores[(j+1):T,,drop=F], scores[1:(T-j),,drop=F])/T
        S <- S + w*(Gj + t(Gj))
      }
    }
    sqrt(pmax(diag(T * XtX_inv %*% S %*% XtX_inv), 0))
  }, error = function(err) rep(NA_real_, k))
}

# --- 3. FULL-PERIOD REGRESSIONS ---
cat("=== 3. Running Full-Period OLS + NW ===\n")
run_full_period <- function(tk) {
  d <- ap_split[[tk]]; n <- nrow(d)
  if (n < MIN_OBS_FULL) return(NULL)
  y <- d$excess_ret; X <- cbind(1, d$mkt_rf, d$smb, d$hml, d$mom)
  fit <- fast_ols(y, X); if (is.null(fit)) return(NULL)
  se_nw_vec <- nw_se(X, fit$e, lag = NW_LAG_FULL)
  data.frame(Ticker=tk, ap_group=d$ap_group[1], lipper=d$lipper[1], n_obs=n,
             alpha_m=fit$beta[1], alpha_ann=fit$beta[1]*12, alpha_t_nw=fit$beta[1]/se_nw_vec[1],
             alpha_p_nw=2*pt(-abs(fit$beta[1]/se_nw_vec[1]), n-5),
             exp_ratio=d$exp_r[1], alpha_net_ann=(fit$beta[1]*12)-(d$exp_r[1]/100),
             # v2.7: mean_tna uses tna_local (coalesce of class_assets and
             # total_assets, computed in Section 1). v2.6 referenced d$tna
             # which was NULL, producing NaN and requiring patch_mean_tna.R.
             mean_tna=mean(d$tna_local[!is.na(d$tna_local) & d$tna_local > 0], na.rm=TRUE))
}
alpha_full <- bind_rows(lapply(names(ap_split), run_full_period))

# --- 4. PARALLEL ROLLING REGRESSIONS ---
cat("=== 4. Running Parallel Rolling Regressions ===\n")
run_rolling_parallel <- function(tk, ap_list, MIN_OBS, NW_LAG) {
  library(lubridate)
  d <- ap_list[[tk]]; n <- nrow(d); if (n < MIN_OBS) return(NULL)
  dates <- d$date; excess <- d$excess_ret; ret_raw <- d$ret_rank
  mi    <- year(dates) * 12L + month(dates)            # calendar-month index
  X_all <- cbind(1, d$mkt_rf, d$smb, d$hml, d$mom)
  res <- vector("list", n)
  for (i in seq_len(n)) {
    win <- which(mi >= mi[i] - 35L & mi <= mi[i])      # months t-35..t
    if (length(win) < MIN_OBS) next
    y <- excess[win]; X <- X_all[win, , drop=F]; n_w <- length(win)
    tryCatch({
      XtX <- crossprod(X); beta <- solve(XtX, crossprod(X, y))
      e <- as.vector(y - X %*% beta); scores <- X * e; S <- crossprod(scores)/n_w
      if(NW_LAG > 0) {
        for(j in 1:NW_LAG) {
          w <- 1 - j/(NW_LAG+1); Gj <- crossprod(scores[(j+1):n_w,,drop=F], scores[1:(n_w-j),,drop=F])/n_w
          S <- S + w*(Gj + t(Gj))
        }
      }
      se_nw <- sqrt(diag(n_w * solve(XtX) %*% S %*% solve(XtX))[1])
      res[[i]] <- list(Ticker=tk, date=dates[i], ap_group=d$ap_group[1],
                       alpha_ann=beta[1]*12, alpha_t_nw=beta[1]/se_nw,
                       as_r2=1 - (sum(e^2)/sum((y-mean(y))^2)),
                       ret_vol_roll=sd(ret_raw[win])*sqrt(12))
    }, error = function(e) NULL)
  }
  do.call(rbind, lapply(res, as.data.frame))
}

cl <- makeCluster(N_CORES); clusterEvalQ(cl, library(lubridate))
clusterExport(cl, c("ap_split", "MIN_OBS_ROLL", "NW_LAG_ROLL", "run_rolling_parallel"))
alpha_roll <- bind_rows(parLapply(cl, names(ap_split), run_rolling_parallel, ap_split, MIN_OBS_ROLL, NW_LAG_ROLL))
stopCluster(cl)

# --- 6. FAMA-FRENCH BOOTSTRAP ---
# v2.5: simulated t-statistics now use Newey-West HAC standard errors with the
# same lag (NW_LAG_FULL = 6) used for the actual full-period regressions in
# Section 3. This restores symmetry between actual and simulated t-stat
# distributions in Table 9 / Figure 3. Compute cost ~2.6x v2.4.
cat("=== 6. Fama-French (2010) bootstrap (NW SE, lag =", NW_LAG_FULL, ") ===\n")
active_fp <- alpha_full %>% filter(ap_group == "Active", !is.na(alpha_t_nw))
all_cal_dates <- sort(unique(ap$date)); T_total <- length(all_cal_dates)
date_idx_map  <- setNames(seq_len(T_total), as.character(all_cal_dates))
bs_data <- lapply(active_fp$Ticker, function(tk) {
  a_hat <- active_fp$alpha_m[active_fp$Ticker == tk]; d <- ap_split[[tk]]
  list(t_idx = date_idx_map[as.character(d$date)], y_tilde = d$excess_ret - a_hat, X_fac = cbind(d$mkt_rf, d$smb, d$hml, d$mom))
})
names(bs_data) <- active_fp$Ticker
# NW SE for the alpha intercept (returns scalar SE for the first regressor).
# Inlined here so workers don't need access to nw_se() from the global env.
# samp: one pre-drawn vector of T_total month indices (with replacement).
one_boot_run <- function(samp, bs_data, pcts_probs, min_obs, nw_lag) {
  t_sim <- vapply(bs_data, function(d) {
    # Fund rows for the drawn months, in DRAW order (i.i.d. under H0); months
    # the fund did not exist are skipped. Do not sort: adjacent duplicates
    # would look like autocorrelation to Newey-West.
    keep <- match(samp, d$t_idx); keep <- keep[!is.na(keep)]
    if (length(keep) < min_obs) return(NA_real_)
    y <- d$y_tilde[keep]; X <- cbind(1, d$X_fac[keep, , drop = FALSE])
    tryCatch({
      XtX     <- crossprod(X)
      XtX_inv <- solve(XtX)
      beta    <- XtX_inv %*% crossprod(X, y)
      e       <- as.vector(y - X %*% beta)
      Tn      <- length(y)
      # Newey-West HAC variance with Bartlett kernel weights
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
cache_key <- digest::digest(list(
  tickers = sort(active_fp$Ticker),
  alphas  = active_fp$alpha_ann[order(active_fp$Ticker)],
  T_total = T_total,
  B_RUNS  = B_RUNS,
  NW_LAG  = NW_LAG_FULL,
  MIN_OBS = MIN_OBS_BS,
  seed    = BOOT_SEED,
  pcts    = PCTS,
  kernel  = BOOT_KERNEL
))
cache_file <- file.path(BOOT_CACHE_DIR, "fullperiod_bootstrap_cache.rds")

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
  # All month draws made here from one seed: results do not depend on N_CORES
  set.seed(BOOT_SEED)
  samp_list <- replicate(B_RUNS, sample.int(T_total, T_total, replace = TRUE), simplify = FALSE)
  cl <- makeCluster(N_CORES)
  boot_t0 <- Sys.time()
  boot_results <- parLapply(cl, samp_list, one_boot_run, bs_data, PCTS/100, MIN_OBS_BS, NW_LAG_FULL); stopCluster(cl)
  cat("  Bootstrap wall time:", round(as.numeric(difftime(Sys.time(), boot_t0, units = "secs")), 1), "sec\n")
  sim_matrix <- do.call(rbind, boot_results)
  if (USE_BOOT_CACHE) {
    saveRDS(list(key = cache_key, sim_matrix = sim_matrix), cache_file)
    cat("  [cached] Saved to", cache_file, "\n")
  }
}
bootstrap_summary <- data.frame(percentile=PCTS, t_alpha_actual=as.numeric(quantile(active_fp$alpha_t_nw, PCTS/100)), t_alpha_sim_mean=colMeans(sim_matrix), pct_runs_below=colMeans(sweep(sim_matrix, 2, as.numeric(quantile(active_fp$alpha_t_nw, PCTS/100)), "<"))*100)

# --- 7. BSW GAMMA-GRID DECOMPOSITION ---
# Barras, Scaillet & Wermers (2010, Journal of Finance), Section II & Table III.
# No additional regressions. pi0 and the tail counts use the same two-sided
# Student-t (n-5) Newey-West p-values (alpha_p_nw) as alpha_reporting.R, so
# Table pi0, Table 10b and Table I.3 report one number.
cat("=== 7. BSW Gamma-Grid Decomposition ===\n")

# Storey (2002) pi0 from two-sided Student-t NW p-values
p_vals_bsw <- active_fp$alpha_p_nw
n_bsw      <- sum(!is.na(p_vals_bsw))
pi0_bsw    <- min(1.0,
                  sum(p_vals_bsw > LAMBDA_STOREY, na.rm = TRUE) /
                    (n_bsw * (1 - LAMBDA_STOREY)))
cat("pi0 estimate (BSW block):", round(pi0_bsw * 100, 1), "%\n")
cat("Active funds entering BSW decomposition:", n_bsw, "\n")

# Compute four-way decomposition across gamma grid
t_stats_bsw <- active_fp$alpha_t_nw

bsw_df <- do.call(rbind, lapply(GAMMA_GRID, function(g) {
  ok       <- !is.na(p_vals_bsw)
  S_neg    <- mean(p_vals_bsw[ok] < g & t_stats_bsw[ok] < 0)  # significantly negative alpha
  S_pos    <- mean(p_vals_bsw[ok] < g & t_stats_bsw[ok] > 0)  # significantly positive alpha
  F_luck   <- pi0_bsw * g / 2                              # expected false discoveries per tail (BSW Eq. 5)
  data.frame(
    gamma           = round(g * 100),                              # stored as percent (5, 10, ..., 50)
    S_neg_pct       = round(S_neg    * 100, 4),
    S_pos_pct       = round(S_pos    * 100, 4),
    F_luck_pct      = round(F_luck   * 100, 4),
    T_unskilled_pct = round((S_neg - F_luck) * 100, 4),    # T^-_gamma: genuinely unskilled
    T_skilled_pct   = round((S_pos - F_luck) * 100, 4)     # T^+_gamma: genuinely skilled
  )
}))

# Population estimates: T^-_gamma and T^+_gamma at gamma* (BSW 2010, eq. 8)
pi_A_minus <- bsw_df$T_unskilled_pct[bsw_df$gamma == round(GAMMA_STAR * 100)]
pi_A_plus  <- bsw_df$T_skilled_pct[bsw_df$gamma == round(GAMMA_STAR * 100)]
stopifnot(length(pi_A_minus) == 1L)   # GAMMA_STAR must be on GAMMA_GRID
cat(sprintf("Population estimates at gamma* = %.2f:\n  pi^-_A (Unskilled): %.1f%%\n  pi^+_A (Skilled):   %.1f%%\n  pi0 + pi^-_A + pi^+_A = %.1f%%\n",
            GAMMA_STAR, pi_A_minus, pi_A_plus, pi0_bsw * 100 + pi_A_minus + pi_A_plus))

# Metadata row for cross-script reference in alpha_reporting.R
bsw_meta <- data.frame(
  lambda    = LAMBDA_STOREY,
  pi0_pct   = round(pi0_bsw * 100, 4),
  n_active  = n_bsw,
  gamma_star = round(GAMMA_STAR * 100),  # gamma* for population estimates (percent)
  pi_A_minus = pi_A_minus,
  pi_A_plus  = pi_A_plus
)

# --- 8. SAVE OUTPUTS ---
write_xlsx(alpha_full, "alpha_fullperiod.xlsx")
write_xlsx(alpha_roll, "alpha_rolling.xlsx")
write_xlsx(
  list(
    summary           = bootstrap_summary,
    fund_talpha       = active_fp,
    bsw_decomposition = bsw_df,
    bsw_meta          = bsw_meta
  ),
  "bootstrap_results.xlsx"
)
cat("=== ALL Done ===\n")