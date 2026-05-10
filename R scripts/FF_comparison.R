# =============================================================================
# FAMA-FRENCH (2010) SUBPERIOD REPLICATION                                  v1.4
#
# v1.4 changes vs v1.3 (figure inline-text strip):
#   Figure C.1 (fig_luck_vs_skill_combined_FF) inline narrative text stripped
#   per project-wide convention. plot_annotation() removed entirely (overall
#   title and methodology subtitle were both narrative; both moved to the
#   LaTeX \caption{} block). Panel B subtitle ("Dashed line: theoretical
#   N(0,1) null") removed; the dashed-line meaning is now described in the
#   LaTeX caption. Panel A and Panel B identifier titles retained as
#   structural reference handles. Same convention as alpha_reporting.R v8.4
#   and structural_break_test.R v1.2.
#
# v1.3 changes vs v1.2 (Family B audit):
#   (a) filter(!excluded_perf) added to the ap panel-prep stage so the FF
#       subperiod replication uses the same performance-comparison subsample
#       as the main-text analysis (alpha_estimation.R v2.7,
#       alpha_estimation_robust.R v1.3, aggregate_alphas.R v1.2). Without this
#       filter, the FF subperiod tables would describe a strictly larger fund
#       population than the main-text tables, making the parallel comparison
#       incoherent. The filter is applied to the LOCAL ap copy only and does
#       not affect panel_trimmed in the parent environment.
#   (b) Mean-TNA computation in run_full() corrected. v1.2 referenced d$tna
#       which was NULL (panel_trimmed carries TNA as class_assets/total_assets).
#       Same fix as alpha_estimation.R v2.7. mean_tna is now computed from
#       coalesce(class_assets, total_assets) and the resulting alpha_fullperiod_FF.xlsx
#       has valid mean_tna values for any downstream VW weighting.
#   (c) [Family C follow-on] BSW (2010) net-return citation corrected at
#       line 565. Convention attributed to Carhart (1997) and Wermers (2000),
#       the originators; BSW (2010) shares the arithmetic but applies it in
#       the reverse direction.
#
# v1.2 change vs v1.1:
#   Table 7 FF (`table_perf_aggregate_FF.tex`) switched from per-fund alpha -->
#   cross-sectional weighted mean (with static mean-TNA weight) to the FF (2010)
#   portfolio regression methodology, consistent with the main-body Table 7
#   refactor in alpha_reporting.R v8.0. Each of the five groups (Active,
#   Passive, Unknown, Active+Passive, Full) now has a monthly EW/VW portfolio
#   return series regressed on Carhart (1997) factors with NW HAC SEs; table
#   reports point estimates with interleaved t-statistic rows (FF 2010 Table II
#   convention) and significance stars. Section 6 port_agg extended from 2
#   groups (Active, Passive) to all 5 groups; alpha_agg likewise extended.
#   Table 13 FF (Active vs Passive, model comparison) is unchanged because it
#   filters alpha_agg for Active/Passive at point of use.
#
# v1.1 change vs v1.0:
#   BUG FIX: Section 4 bootstrap (one_boot_run) now computes Newey-West HAC
#   standard errors with the same 6-month lag used in Section 3, instead of
#   OLS. Restores symmetry between actual and simulated t-stat distributions
#   in Table C.2 / Figure C.1. Matches the parallel fix applied to
#   alpha_estimation.R v2.5. Compute cost approximately 2.6x v1.0.
#   Tables C.3 (pi_0) and C.4 (BSW) are numerically unchanged because they
#   derive from alpha_p_nw, not from the bootstrap.
#
# Replicates Tables 7, 9, 10, 11, 13 and Figure 3 of the main results document
# on the Jan 1995 - Sep 2006 subperiod, the maximum overlap between this
# project's sample (Dec 1994 onwards) and Fama and French (2010, JF) which
# spans Jan 1984 - Sep 2006.
#
# REQUIRES IN SESSION:
#   panel_trimmed   built by data_import_and_cleaning.R + flow_calculation.R
#                   (must already contain ret_gross, ret_net, tna, tna_lag,
#                   MKT_RF, SMB, HML, MOM, RF, ap_group, Expense_Ratio).
#
# WRITES (all suffixed _FF, no collision with main pipeline outputs):
#   alpha_fullperiod_FF.xlsx        per-fund full-period Carhart alphas
#   bootstrap_results_FF.xlsx       bootstrap summary + BSW decomposition
#   portfolio_alphas_FF.xlsx        aggregate portfolio regression results
#   table_perf_aggregate_FF.tex     Table 7  (FF subperiod)
#   table_bootstrap_tails_FF.tex    Table 9  (FF subperiod)
#   table_pi0_estimate_FF.tex       Table 10 (FF subperiod)
#   table10b_bsw_decomposition_FF.tex   Table 11 (FF subperiod)
#   table_port_agg_alpha_FF.tex     Table 13 (FF subperiod)
#   fig_luck_vs_skill_combined_FF.png   Figure 3 (FF subperiod)
#
# This script does NOT touch panel_trimmed in the parent environment.
# All filtering is local. Safe to run alongside the main pipeline.
# =============================================================================

library(dplyr)
library(tidyr)
library(lubridate)
library(readxl)
library(writexl)
library(parallel)
library(ggplot2)
library(scales)
library(stringr)
library(patchwork)
library(knitr)
library(kableExtra)

# =============================================================================
# 0. CONFIGURATION
# =============================================================================
DATE_MIN_FF  <- as.Date("1995-01-01")   # earliest month with usable data
DATE_MAX_FF  <- as.Date("2006-09-30")   # FF (2010) sample end (Sep 2006)
SAMPLE_LABEL <- "Jan 1995--Sep 2006 (Fama-French 2010 subperiod)"

# Estimation parameters mirror alpha_estimation.R v2.4 exactly so the
# subperiod and full-sample numbers are directly comparable.
MIN_OBS_FULL <- 24L
NW_LAG_FULL  <- 6L
NW_LAG_PORT  <- 6L
B_RUNS       <- 10000L
MIN_OBS_BS   <- 8L
BOOT_SEED    <- 42L
N_CORES      <- max(1L, detectCores() - 1L)
USE_BOOT_CACHE <- TRUE
BOOT_CACHE_DIR <- file.path(".", paste0("cache_", Sys.info()[["nodename"]]))
dir.create(BOOT_CACHE_DIR, showWarnings = FALSE)  # no-op if already exists
PCTS         <- c(1, 2, 3, 4, 5, 10, 20, 30, 40, 50, 60, 70, 80, 90, 95, 96, 97, 98, 99)
GAMMA_GRID   <- c(0.05, 0.10, 0.15, 0.20, 0.25, 0.30, 0.35, 0.40, 0.45, 0.50)
LAMBDA_STOREY <- 0.5

cat("=== FF SUBPERIOD REPLICATION ===\n")
cat("Sample window:", format(DATE_MIN_FF), "to", format(DATE_MAX_FF), "\n")

# =============================================================================
# 1. PANEL RESTRICTION  (LOCAL COPY ONLY)
# =============================================================================
ap <- panel_trimmed %>%
  filter(!excluded_perf) %>%      # v1.3: performance-comparison subsample
  filter(date >= DATE_MIN_FF, date <= DATE_MAX_FF) %>%
  rename(mkt_rf = MKT_RF, smb = SMB, hml = HML, mom = MOM, rf = RF,
         exp_r = Expense_Ratio) %>%
  mutate(excess_ret = ret_gross - rf,
         exp_r      = suppressWarnings(as.numeric(exp_r)),
         ap_group   = gsub("Agtive", "Active", ap_group)) %>%
  filter(!is.na(excess_ret), !is.na(mkt_rf), !is.na(smb),
         !is.na(hml), !is.na(mom)) %>%
  arrange(Ticker, date)

cat("Subperiod fund-months:", nrow(ap), "\n")
cat("Distinct funds in subperiod:", n_distinct(ap$Ticker), "\n")
cat("ap_group counts:\n"); print(table(ap$ap_group))

ap_split <- split(ap, ap$Ticker)

# =============================================================================
# 2. HELPER FUNCTIONS  (mirrors alpha_estimation.R / alpha_reporting.R)
# =============================================================================

# Fast OLS via crossprod (same kernel as alpha_estimation.R)
fast_ols <- function(y, X) {
  tryCatch({
    XtX  <- crossprod(X)
    beta <- solve(XtX, crossprod(X, y))
    e    <- as.vector(y - X %*% beta)
    n    <- length(y); k <- ncol(X)
    s2   <- sum(e^2) / (n - k)
    list(beta = as.vector(beta), e = e,
         se_ols = sqrt(pmax(diag(s2 * solve(XtX)), 0)),
         r2 = 1 - sum(e^2) / sum((y - mean(y))^2),
         sigma = sqrt(s2), n = n, k = k)
  }, error = function(err) NULL)
}

# Newey-West HAC standard errors
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

# Three-digit formatter with -- for missing
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

# Significance stars (matches portfolio_sorts.R add_stars)
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

# kableExtra LaTeX cleaner (mirrors alpha_reporting.R clean_latex)
clean_latex <- function(x, resize = TRUE, small = FALSE) {
  x <- gsub("\\\\end[{]threeparttable[}][}]", "\\\\end{threeparttable}", x)
  x <- gsub("\\\\end[{]ThreePartTable[}][}]", "\\\\end{ThreePartTable}", x)
  x <- gsub("\\\\resizebox[{]\\\\ifdim[^}]*[}][{]![}][{]",
            "\\\\resizebox{\\\\linewidth}{!}{", x)
  x <- gsub("\\begin{table}[!h]", "\\begin{table}[H]", x, fixed = TRUE)
  if (resize && !grepl("resizebox", x, fixed = TRUE)) {
    if (grepl("ThreePartTable", x, fixed = TRUE)) {
      x <- sub("(\\\\begin[{]ThreePartTable[}])",
               "\\\\resizebox{\\\\linewidth}{!}{\n\\1", x)
      x <- sub("(\\\\end[{]ThreePartTable[}])", "\\1\n}", x)
    } else if (grepl("threeparttable", x, fixed = TRUE)) {
      x <- sub("(\\\\begin[{]threeparttable[}])",
               "\\\\resizebox{\\\\linewidth}{!}{\n\\1", x)
      x <- sub("(\\\\end[{]threeparttable[}])", "\\1\n}", x)
    } else {
      x <- sub("(\\\\begin[{]tabular[}])",
               "\\\\resizebox{\\\\linewidth}{!}{\n\\1", x)
      x <- sub("(\\\\end[{]tabular[}])", "\\1\n}", x)
    }
  }
  # SBE caption-width fix (Phase 2b in the SBE_needed_fixes audit):
  # Move \caption inside \begin{threeparttable} so threeparttable constrains
  # the caption width to the tabular's natural width. kableExtra otherwise
  # emits \caption BEFORE threeparttable, leaving caption at full \linewidth
  # and visually overhanging narrower tables. Also strips the redundant
  # second \centering kableExtra inserts between \caption and threeparttable.
  # Brace-balanced caption matching via perl regex (handles \label{...} inside).
  x <- gsub(
    "\\\\caption\\{((?:[^{}]|\\{(?:[^{}]|\\{[^{}]*\\})*\\})*)\\}\\s*\n\\\\centering\\s*\n((?:\\\\resizebox\\{[^{}]*(?:\\{[^{}]*\\}[^{}]*)*\\}\\{[^}]*\\}\\{\\s*\n)?)\\\\begin\\{threeparttable\\}",
    "\\2\\\\begin{threeparttable}\n\\\\caption{\\1}",
    x,
    perl = TRUE
  )
  # Same transform for the ThreePartTable (longtable) variant used by
  # portfolio_sorts.R, persistence_testing.R, and activeness_analysis.R.
  x <- gsub(
    "\\\\caption\\{((?:[^{}]|\\{(?:[^{}]|\\{[^{}]*\\})*\\})*)\\}\\s*\n\\\\centering\\s*\n((?:\\\\resizebox\\{[^{}]*(?:\\{[^{}]*\\}[^{}]*)*\\}\\{[^}]*\\}\\{\\s*\n)?)\\\\begin\\{ThreePartTable\\}",
    "\\2\\\\begin{ThreePartTable}\n\\\\caption{\\1}",
    x,
    perl = TRUE
  )
  if (small) x <- sub("(\\\\begin\\{table\\}[^\n]*\n)", "\\1\\\\small\n", x)
  x
}

# =============================================================================
# 3. FULL-PERIOD CARHART ALPHAS (per fund) -- inputs to Tables 7, 9, 10, 11, Fig 3
# =============================================================================
cat("=== 3. Per-fund Carhart alphas (subperiod) ===\n")

run_full <- function(tk) {
  d <- ap_split[[tk]]; n <- nrow(d)
  if (n < MIN_OBS_FULL) return(NULL)
  y   <- d$excess_ret
  X   <- cbind(1, d$mkt_rf, d$smb, d$hml, d$mom)
  fit <- fast_ols(y, X); if (is.null(fit)) return(NULL)
  se  <- nw_se(X, fit$e, NW_LAG_FULL)
  # v1.3: mean_tna uses coalesce(class_assets, total_assets) directly. v1.2
  # referenced d$tna, which was NULL because the column is named class_assets/
  # total_assets in panel_trimmed. Same fix as alpha_estimation.R v2.7.
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

alpha_full <- bind_rows(lapply(names(ap_split), run_full))
cat("  Funds with >=", MIN_OBS_FULL, "obs:", nrow(alpha_full), "\n")
cat("  Active:",  sum(alpha_full$ap_group == "Active"),  "\n")
cat("  Passive:", sum(alpha_full$ap_group == "Passive"), "\n")

# Scale to percent for reporting
alpha_full <- alpha_full %>%
  mutate(across(c(alpha_ann, alpha_net_ann), ~ .x * 100))

# =============================================================================
# 4. FAMA-FRENCH (2010) BOOTSTRAP ON SUBPERIOD
#    v1.1: NW SE inside bootstrap (was OLS in v1.0). Matches the NW SE used
#    for actual t-stats in Section 3, restoring symmetry between actual and
#    simulated t-stat distributions in Table C.2 / Figure C.1. Same 6-month
#    lag as the main-sample bootstrap. Compute cost approximately 2.6x v1.0.
# =============================================================================
cat("=== 4. Bootstrap (B =", B_RUNS, ", NW SE, lag =", NW_LAG_FULL, ") ===\n")

active_fp <- alpha_full %>% filter(ap_group == "Active", !is.na(alpha_t_nw))
all_dates <- sort(unique(ap$date)); T_total <- length(all_dates)
date_idx  <- setNames(seq_len(T_total), as.character(all_dates))

# Build per-fund residual / factor structures.
# y_tilde = excess_ret - alpha_hat (zero-alpha null), X_fac = factor cols.
# alpha_m here is in monthly decimal (un-percent-scaled), so we re-fetch raw.
bs_data <- lapply(active_fp$Ticker, function(tk) {
  a_hat   <- active_fp$alpha_ann[active_fp$Ticker == tk] / 1200  # back to monthly decimal
  d       <- ap_split[[tk]]
  list(t_idx   = date_idx[as.character(d$date)],
       y_tilde = d$excess_ret - a_hat,
       X_fac   = cbind(d$mkt_rf, d$smb, d$hml, d$mom))
})
names(bs_data) <- active_fp$Ticker

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
  date_lo = DATE_MIN_FF,
  date_hi = DATE_MAX_FF,
  T_total = T_total,
  B_RUNS  = B_RUNS,
  NW_LAG  = NW_LAG_FULL,
  MIN_OBS = MIN_OBS_BS,
  seed    = BOOT_SEED,
  pcts    = PCTS
))
cache_file <- file.path(BOOT_CACHE_DIR, "ff_subperiod_bootstrap_cache.rds")

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
  cl <- makeCluster(N_CORES)
  clusterExport(cl, c("bs_data", "T_total", "one_boot_run", "MIN_OBS_BS", "NW_LAG_FULL"),
                envir = environment())
  clusterSetRNGStream(cl, BOOT_SEED)
  boot_t0 <- Sys.time()
  boot_results <- parLapply(cl, seq_len(B_RUNS), one_boot_run,
                            bs_data, T_total, PCTS / 100, MIN_OBS_BS, NW_LAG_FULL)
  stopCluster(cl)
  cat("  Bootstrap wall time:",
      round(as.numeric(difftime(Sys.time(), boot_t0, units = "secs")), 1), "sec\n")
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
  pct_runs_below   = colMeans(sweep(sim_matrix, 2, actual_pct, "<")) * 100
)

# =============================================================================
# 5. STOREY pi_0 + BSW (2010) DECOMPOSITION
# =============================================================================
cat("=== 5. pi_0 and BSW decomposition ===\n")

active_pi0 <- alpha_full %>% filter(ap_group == "Active", !is.na(alpha_p_nw))
total_n    <- nrow(active_pi0)
num_above  <- sum(active_pi0$alpha_p_nw > LAMBDA_STOREY, na.rm = TRUE)
pi_0_val   <- min(1.0, num_above / (total_n * (1 - LAMBDA_STOREY)))
cat("  pi_0 estimate:", round(pi_0_val * 100, 1), "%\n")

t_stats <- active_pi0$alpha_t_nw
bsw_df <- do.call(rbind, lapply(GAMMA_GRID, function(g) {
  t_thresh <- qnorm(1 - g / 2)
  S_neg <- mean(t_stats < -t_thresh, na.rm = TRUE)
  S_pos <- mean(t_stats >  t_thresh, na.rm = TRUE)
  F_luck <- pi_0_val * g / 2
  data.frame(
    gamma           = g * 100,
    S_neg_pct       = S_neg * 100,
    S_pos_pct       = S_pos * 100,
    F_luck_pct      = F_luck * 100,
    T_unskilled_pct = (S_neg - F_luck) * 100,
    T_skilled_pct   = (S_pos - F_luck) * 100
  )
}))

# =============================================================================
# 6. AGGREGATE ACTIVE/PASSIVE PORTFOLIOS  ->  TABLE 13
# =============================================================================
cat("=== 6. Aggregate portfolio regressions ===\n")

# Build EW/VW portfolio return series for five groups: Active, Passive,
# Unknown, Active+Passive (classified only), and Full Sample. These feed
# both Table 7 FF (5 rows) and Table 13 FF (Active/Passive only, filtered
# downstream).
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

# CAPM / FF3 / Carhart for one return column.
# subtract_rf=FALSE only for self-financing long-short spreads (not used here).
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
  X1 <- cbind(1, d$MKT_RF)
  f1 <- fast_ols(y, X1); if (is.null(f1)) return(na_row)
  s1 <- nw_se(X1, f1$e, NW_LAG_PORT)
  X3 <- cbind(1, d$MKT_RF, d$SMB, d$HML)
  f3 <- fast_ols(y, X3); if (is.null(f3)) return(na_row)
  s3 <- nw_se(X3, f3$e, NW_LAG_PORT)
  X4 <- cbind(1, d$MKT_RF, d$SMB, d$HML, d$MOM)
  f4 <- fast_ols(y, X4); if (is.null(f4)) return(na_row)
  s4 <- nw_se(X4, f4$e, NW_LAG_PORT)
  data.frame(
    n_months   = n,
    alpha_capm = f1$beta[1] * 12, t_capm = f1$beta[1] / s1[1],
    alpha_ff3  = f3$beta[1] * 12, t_ff3  = f3$beta[1] / s3[1],
    alpha_car  = f4$beta[1] * 12, t_car  = f4$beta[1] / s4[1]
  )
}

regress_agg_one <- function(grp, wt) {
  d <- port_agg %>% filter(ap_group == grp)
  g_col <- if (wt == "EW") "ret_ew_gross" else "ret_vw_gross"
  n_col <- if (wt == "EW") "ret_ew_net"   else "ret_vw_net"
  rg <- run_models(g_col, d, factors_ts)
  rn <- run_models(n_col, d, factors_ts)
  rg %>% mutate(alpha_car_net = rn$alpha_car, t_car_net = rn$t_car,
                ap_group = grp, weighting = wt)
}

agg_groups <- c("Active", "Passive", "Unknown", "Active + Passive", "Full Sample")
alpha_agg <- bind_rows(
  lapply(agg_groups,
         function(g) bind_rows(regress_agg_one(g, "EW"),
                               regress_agg_one(g, "VW")))
) %>%
  mutate(across(c(alpha_capm, alpha_ff3, alpha_car, alpha_car_net), ~ .x * 100))

# =============================================================================
# 7. EXCEL EXPORTS
# =============================================================================
write_xlsx(alpha_full,
           "alpha_fullperiod_FF.xlsx")
write_xlsx(list(summary           = boot_summary,
                fund_talpha       = active_fp,
                bsw_decomposition = bsw_df,
                bsw_meta          = data.frame(
                  lambda     = LAMBDA_STOREY,
                  pi0_pct    = round(pi_0_val * 100, 4),
                  n_active   = total_n,
                  gamma_star = 20,
                  pi_A_minus = bsw_df$T_unskilled_pct[bsw_df$gamma == 20],
                  pi_A_plus  = bsw_df$T_skilled_pct[bsw_df$gamma == 20]
                )),
           "bootstrap_results_FF.xlsx")
write_xlsx(alpha_agg, "portfolio_alphas_FF.xlsx")
cat("Written: alpha_fullperiod_FF.xlsx, bootstrap_results_FF.xlsx, portfolio_alphas_FF.xlsx\n")

# =============================================================================
# 8. TABLE 7 (FF):  AGGREGATE PERFORMANCE
# =============================================================================
cat("=== 8. Table 7 (FF) ===\n")

# FF (2010)-style aggregate portfolio alpha. Source: alpha_agg (Section 6),
# which now covers all five groups. Carhart (4-factor) alpha only; gross and
# net separately. Two rows per group following FF (2010) Table II convention.

# Fund counts per group (for the N column).
n_funds_by_group <- list(
  "Active"           = sum(alpha_full$ap_group == "Active",  na.rm = TRUE),
  "Passive"          = sum(alpha_full$ap_group == "Passive", na.rm = TRUE),
  "Unknown"          = sum(alpha_full$ap_group == "Unknown", na.rm = TRUE),
  "Active + Passive" = sum(alpha_full$ap_group %in% c("Active","Passive"), na.rm = TRUE),
  "Full Sample"      = nrow(alpha_full)
)

make_t7_rows_FF <- function(grp) {
  eg <- alpha_agg %>% filter(ap_group == grp, weighting == "EW")
  vg <- alpha_agg %>% filter(ap_group == grp, weighting == "VW")
  if (nrow(eg) == 0L || nrow(vg) == 0L) return(NULL)
  coef_row <- data.frame(
    Group    = grp,
    N        = formatC(n_funds_by_group[[grp]], format = "d", big.mark = ","),
    T        = formatC(eg$n_months, format = "d"),
    EW_Gross = add_stars(fmt(eg$alpha_car),     eg$t_car),
    VW_Gross = add_stars(fmt(vg$alpha_car),     vg$t_car),
    EW_Net   = add_stars(fmt(eg$alpha_car_net), eg$t_car_net),
    VW_Net   = add_stars(fmt(vg$alpha_car_net), vg$t_car_net),
    stringsAsFactors = FALSE
  )
  t_row <- data.frame(
    Group    = "t(coef)",
    N        = "",
    T        = "",
    EW_Gross = paste0("(", fmt(eg$t_car,     2), ")"),
    VW_Gross = paste0("(", fmt(vg$t_car,     2), ")"),
    EW_Net   = paste0("(", fmt(eg$t_car_net, 2), ")"),
    VW_Net   = paste0("(", fmt(vg$t_car_net, 2), ")"),
    stringsAsFactors = FALSE
  )
  rbind(coef_row, t_row)
}

t7_display <- do.call(rbind, lapply(
  c("Active", "Passive", "Unknown", "Active + Passive", "Full Sample"),
  make_t7_rows_FF
))
rownames(t7_display) <- NULL

# Horizontal rule after Unknown's t-stat row (row 6) separates individual
# groups from the combined aggregates.
unknown_tstat_row_FF <- 6L

fn_t7 <- paste(
  "Annualised \\\\textcite{Carhart1997} four-factor alpha (\\\\%) from regressing",
  "the monthly aggregate portfolio return of each group on the market, size,",
  "value, and momentum factors, following \\\\textcite{FamaFrench2010}.",
  "EW: equal-weighted portfolio (each fund alive in month $t$ contributes",
  "$1/N_t$). VW: value-weighted portfolio with lagged TNA weights",
  "$w_{i,t-1} = \\\\text{TNA}_{i,t-1} / \\\\sum_j \\\\text{TNA}_{j,t-1}$.",
  "Net returns are computed as gross returns less one-twelfth of the static",
  "annual expense ratio each month, following \\\\textcite{Carhart1997} and \\\\textcite{Wermers2000}.",
  "Newey-West $t$-statistics (6-month lag) in parentheses below each alpha;",
  "$^{*}$, $^{**}$, $^{***}$: significant at 10\\\\%, 5\\\\%, 1\\\\%.",
  "$N$: unique funds contributing to the portfolio series;",
  "$T$: number of monthly observations in the regression.",
  "The Active + Passive row aggregates only the two classified groups.",
  paste0("Sample: ", SAMPLE_LABEL, ".",
         " The subperiod is the maximum overlap between this study's data window",
         " (Dec 1994 onwards) and the Fama and French (2010, \\\\textit{Journal of",
         " Finance}) sample (Jan 1984--Sep 2006), enabling direct comparison with",
         " their Tables I--III.")
)

latex_t7 <- t7_display %>%
  kbl(format    = "latex",
      booktabs  = TRUE,
      linesep   = "",
      escape    = FALSE,
      caption   = "Aggregate Portfolio Alpha by Group (\\%, Annualised) -- Fama-French (2010) Subperiod",
      label     = "perf_aggregate_FF",
      col.names = c("Group", "$N$", "$T$",
                    "EW", "VW", "EW", "VW"),
      align     = "lrrrrrr") %>%
  kable_styling(latex_options = "hold_position") %>%
  add_header_above(c(" " = 3, "Gross Alpha" = 2, "Net Alpha" = 2), bold = FALSE) %>%
  row_spec(unknown_tstat_row_FF, hline_after = TRUE) %>%
  footnote(general        = fn_t7,
           general_title  = "",
           escape         = FALSE,
           threeparttable = TRUE)

writeLines(clean_latex(latex_t7, resize = TRUE),
           "table_perf_aggregate_FF.tex")
cat("Written: table_perf_aggregate_FF.tex\n")

# =============================================================================
# 9. TABLE 9 (FF):  BOOTSTRAP PERCENTILES
# =============================================================================
cat("=== 9. Table 9 (FF) ===\n")

n_active_bs <- sum(alpha_full$ap_group == "Active" & !is.na(alpha_full$alpha_t_nw))

boot_tab <- boot_summary %>%
  filter(percentile %in% c(1, 5, 10, 50, 90, 95, 99)) %>%
  mutate(
    Actual_t  = sapply(t_alpha_actual,   fmt, digits = 3),
    Sim_Mean  = sapply(t_alpha_sim_mean, fmt, digits = 3),
    Prob_Luck = paste0(formatC(pct_runs_below, format = "f", digits = 1), "\\%"),
    Interpretation = case_when(
      percentile <= 10 & pct_runs_below < 5  ~ "Worse than luck (significant)",
      percentile >= 90 & pct_runs_below > 95 ~ "Evidence of genuine skill",
      percentile == 50                        ~ "Indistinguishable from luck",
      TRUE                                    ~ "Consistent with zero-skill"
    )
  )

fn_t9 <- paste(
  "Bootstrap procedure follows Fama and French (2010,",
  "\\\\textit{Journal of Finance}).",
  paste0("Sample: actively managed funds, ", SAMPLE_LABEL, ","),
  paste0("minimum 24 monthly observations ($N = ", n_active_bs, "$ funds)."),
  "For each fund, estimated monthly alpha is subtracted from the excess return",
  "series to construct a zero-alpha null return.",
  "In each of $B = 10{,}000$ bootstrap iterations, calendar months are resampled",
  "with replacement, preserving cross-sectional factor return dependence.",
  "The Carhart (1997) four-factor model is re-estimated on each resampled series.",
  "\\\\textit{Actual} $t(\\\\hat{\\\\alpha})$: percentile of the empirical $t$-statistic",
  "distribution across active funds.",
  "\\\\textit{Simulated Mean}: average of that percentile across all iterations.",
  "\\\\textit{Prob.\\\\ Luck}: fraction of iterations in which the simulated percentile",
  "falls below the actual value; values below 5\\\\% at lower percentiles indicate",
  "underperformance unlikely to be explained by luck alone.",
  "Newey-West standard errors with a 6-month lag are used throughout.",
  "Compare with Fama and French (2010), Table III."
)

latex_boot <- boot_tab %>%
  select(percentile, Actual_t, Sim_Mean, Prob_Luck, Interpretation) %>%
  kbl(format    = "latex",
      booktabs  = TRUE,
      escape    = FALSE,
      caption   = "Fama--French (2010) Bootstrap: Actual vs.\\ Simulated $t(\\hat{\\alpha})$ Percentiles -- FF Subperiod",
      label     = "bootstrap_tails_FF",
      col.names = c("Percentile", "Actual $t(\\hat{\\alpha})$",
                    "Simulated Mean", "Prob.\\ Luck", "Interpretation"),
      align     = c("r", "r", "r", "r", "l")) %>%
  kable_styling(latex_options = "hold_position") %>%
  column_spec(5, width = "15em") %>%
  footnote(general        = fn_t9,
           general_title  = "",
           escape         = FALSE,
           threeparttable = TRUE)

# Replace with EMPTY string, not "\n": replacing with "\n" leaves a blank line
# inside tabular, which triggers "Misplaced \noalign" in LaTeX. Same fix needed
# for table10b below; latent bug in alpha_reporting.R lines 370 and 575.
boot_str <- gsub("\\\\addlinespace[^\n]*\n", "", as.character(latex_boot))
# resize=FALSE: wrapping threeparttable in \resizebox is a fragile combo on
# TeX Live 2024+ (Overleaf default) -- the hbox-restricted mode breaks the
# \noalign expansion in booktabs' \bottomrule. Table 9 has 5 narrow columns
# and fits within \linewidth without resizing. Latent bug in alpha_reporting.R
# v7.5/7.6 line 371 -- same fix applied there.
writeLines(clean_latex(boot_str, resize = FALSE, small = TRUE),
           "table_bootstrap_tails_FF.tex")
cat("Written: table_bootstrap_tails_FF.tex\n")

# =============================================================================
# 10. TABLE 10 (FF):  pi_0 ESTIMATE
# =============================================================================
cat("=== 10. Table 10 (FF) ===\n")

interp <- case_when(
  pi_0_val > 0.90 ~ "Industry dominated by luck",
  pi_0_val > 0.75 ~ "Heterogeneous skill; majority zero-alpha",
  pi_0_val > 0.50 ~ "Moderate skill heterogeneity",
  TRUE            ~ "Substantial skilled-fund presence"
)

pi0_pct <- paste0(formatC(pi_0_val * 100, format = "f", digits = 1), "\\%")
pi0_str <- paste0(formatC(pi_0_val * 100, format = "f", digits = 1), "\\\\%")

pi0_table <- data.frame(
  Metric         = "$\\hat{\\pi}_0$: Proportion of True Zero-Alpha Active Funds",
  Estimate       = pi0_pct,
  N_Funds        = as.character(total_n),
  Lambda         = formatC(LAMBDA_STOREY, format = "f", digits = 1),
  Interpretation = interp
)

fn_t10 <- paste(
  "The proportion of true zero-alpha funds ($\\\\hat{\\\\pi}_0$) is estimated",
  "following Storey (2002) and Barras, Scaillet and Wermers (2010,",
  "\\\\textit{Journal of Finance}), using p-values from Newey-West $t$-tests on",
  "full-period Carhart (1997) four-factor alphas.",
  "The estimator is $\\\\hat{\\\\pi}_0 = |\\\\{p_i > \\\\lambda\\\\}| \\\\,/\\\\, [N(1-\\\\lambda)]$,",
  "where $\\\\lambda = 0.5$ is the standard tuning parameter following Storey (2002).",
  "Funds with p-values exceeding $\\\\lambda$ are unlikely to have non-zero true alpha;",
  "the density of p-values in $(\\\\lambda, 1]$ provides a conservative estimate",
  "of the zero-alpha proportion, bounded above at 1.",
  paste0("Sample: actively managed funds ($N = ", total_n, "$), ", SAMPLE_LABEL, "."),
  "Passive and Unknown-classified funds are excluded.",
  "Compare with Fama and French (2010, \\\\textit{Journal of Finance}),",
  "Table III, which reports an analogous bootstrap-based assessment over",
  "Jan 1984--Sep 2006."
)

latex_pi0 <- pi0_table %>%
  kbl(format    = "latex",
      booktabs  = TRUE,
      escape    = FALSE,
      caption   = "Aggregate Skill Estimate: Proportion of True Zero-Alpha Active Funds -- FF Subperiod",
      label     = "pi0_estimate_FF",
      col.names = c("Metric", "Estimate", "$N$", "$\\lambda$", "Interpretation"),
      align     = c("l", "r", "r", "r", "l")) %>%
  kable_styling(latex_options = "hold_position") %>%
  column_spec(1, width = "16em") %>%
  column_spec(5, width = "14em") %>%
  footnote(general        = fn_t10,
           general_title  = "",
           escape         = FALSE,
           threeparttable = TRUE)

writeLines(clean_latex(latex_pi0, resize = TRUE),
           "table_pi0_estimate_FF.tex")
cat("Written: table_pi0_estimate_FF.tex\n")

# =============================================================================
# 11. TABLE 11 (FF):  BSW FOUR-WAY DECOMPOSITION
# =============================================================================
cat("=== 11. Table 11 (FF) ===\n")

bsw_display <- bsw_df %>%
  mutate(
    gamma_fmt       = paste0(formatC(gamma, format = "f", digits = 0), "\\%"),
    S_neg_fmt       = sapply(S_neg_pct,       fmt1),
    S_pos_fmt       = sapply(S_pos_pct,       fmt1),
    F_luck_fmt      = sapply(F_luck_pct,      fmt1),
    T_unskilled_fmt = sapply(T_unskilled_pct, fmt1),
    T_skilled_fmt   = sapply(T_skilled_pct,   fmt1)
  ) %>%
  select(gamma_fmt, S_neg_fmt, S_pos_fmt, F_luck_fmt, T_unskilled_fmt, T_skilled_fmt)

fn_t10b <- paste(
  "Decomposition follows Barras, Scaillet and Wermers (2010,",
  "\\\\textit{Journal of Finance}), Section~II.B and Table~III.",
  "$S^-_\\\\gamma$ ($S^+_\\\\gamma$): observed fraction of active funds with",
  "significantly negative (positive) Newey-West $t(\\\\hat{\\\\alpha})$ at",
  "two-sided significance level $\\\\gamma$, using full-period Carhart (1997)",
  "four-factor alphas. Critical values are from the standard normal distribution,",
  "consistent with the large-sample approximation in BSW (2010).",
  "$F_\\\\gamma = \\\\hat{\\\\pi}_0 \\\\cdot \\\\gamma/2$: expected proportion of false",
  "discoveries per tail arising from zero-alpha funds,",
  paste0("where $\\\\hat{\\\\pi}_0 = ", pi0_str, "$ is the Storey (2002) estimate"),
  "at $\\\\lambda = 0.5$ (see Table~\\\\ref{tab:pi0_estimate_FF}).",
  "$T^-_\\\\gamma = S^-_\\\\gamma - F_\\\\gamma$: genuinely unskilled funds",
  "(significant negative alpha net of false discoveries).",
  "$T^+_\\\\gamma = S^+_\\\\gamma - F_\\\\gamma$: genuinely skilled funds",
  "(significant positive alpha net of false discoveries).",
  "The reference row ($\\\\gamma = 0.20$) provides the population-level estimates",
  "$\\\\hat{\\\\pi}^-_A$ and $\\\\hat{\\\\pi}^+_A$ following BSW (2010).",
  "Negative $T^+_\\\\gamma$ entries indicate right-tail significance does not",
  "exceed the false-discovery rate at that threshold.",
  paste0("Sample: $N = ", total_n, "$ actively managed funds, ", SAMPLE_LABEL, ";"),
  "Passive and Unknown funds excluded."
)



latex_t10b <- bsw_display %>%
  kbl(format    = "latex",
      booktabs  = TRUE,
      escape    = FALSE,
      caption   = paste0("BSW (2010) Four-Way Decomposition: ",
                         "Proportions of Skilled, Unskilled, and Lucky Funds (\\%) -- FF Subperiod"),
      label     = "bsw_decomposition_FF",
      col.names = c("$\\gamma$", "$S^-_\\gamma$", "$S^+_\\gamma$",
                    "$F_\\gamma$", "$T^-_\\gamma$", "$T^+_\\gamma$"),
      align = c("r", "r", "r", "r", "r", "r")) %>%
  kable_styling(latex_options = "hold_position") %>%
  add_header_above(c(" " = 1,
                     "Observed Tails"  = 2,
                     "False Disc."     = 1,
                     "True Proportions" = 2),
                   escape = FALSE, bold = FALSE) %>%
  footnote(general        = fn_t10b,
           general_title  = "",
           escape         = FALSE,
           threeparttable = TRUE)

t10b_str <- gsub("\\\\addlinespace[^\n]*\n", "", as.character(latex_t10b))
writeLines(clean_latex(t10b_str, resize = FALSE, small = TRUE),
           "table10b_bsw_decomposition_FF.tex")
cat("Written: table10b_bsw_decomposition_FF.tex\n")

# =============================================================================
# 12. FIGURE 3 (FF):  CDF / PDF DUAL PANEL
# =============================================================================
cat("=== 12. Figure 3 (FF) ===\n")

actual_t    <- alpha_full %>%
  filter(ap_group == "Active", !is.na(alpha_t_nw)) %>%
  pull(alpha_t_nw)
actual_data <- data.frame(t_stat = actual_t, Type = "Actual Distribution")
sim_data    <- data.frame(
  t_stat = boot_summary$t_alpha_sim_mean,
  prob   = boot_summary$percentile / 100,
  Type   = "Simulated (Zero-Skill)"
)

p_cdf <- ggplot() +
  stat_ecdf(data = actual_data, aes(x = t_stat, color = Type), linewidth = 1) +
  geom_line(data = sim_data,
            aes(x = t_stat, y = prob, color = Type, linetype = Type), linewidth = 1) +
  scale_color_manual(values = c("Actual Distribution" = "#2166AC",
                                "Simulated (Zero-Skill)" = "black")) +
  theme_classic(base_size = 10) +
  labs(title = "Panel A: Cumulative Distribution (CDF)",
       y = "Cumulative Probability", x = NULL) +
  theme(legend.position = "none") +
  coord_cartesian(xlim = c(-4, 4))

p_pdf <- ggplot() +
  geom_density(data = actual_data, aes(x = t_stat, fill = Type),
               alpha = 0.2, color = "#2166AC") +
  stat_function(fun = dnorm, args = list(mean = 0, sd = 1),
                color = "black", linetype = "dashed", linewidth = 1) +
  scale_fill_manual(values = c("Actual Distribution" = "#2166AC")) +
  theme_classic(base_size = 10) +
  labs(title    = "Panel B: Probability Density (PDF)",
       y        = "Density",
       x        = expression(italic(t)*"-Statistic of "*hat(alpha))) +
  theme(legend.position = "bottom", legend.title = element_blank()) +
  coord_cartesian(xlim = c(-4, 4))

combined_plot <- (p_cdf / p_pdf)

ggsave("fig_luck_vs_skill_combined_FF.png", plot = combined_plot,
       width = 7.5, height = 8, dpi = 300)
cat("Written: fig_luck_vs_skill_combined_FF.png\n")

# =============================================================================
# 13. TABLE 13 (FF):  AGGREGATE ACTIVE/PASSIVE PORTFOLIO ALPHAS
# =============================================================================
cat("=== 13. Table 13 (FF) ===\n")

make_d2_rows <- function(grp) {
  out <- list()
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
  df <- do.call(rbind, out)
  rownames(df) <- NULL
  df
}

d2_table <- bind_rows(make_d2_rows("Active"), make_d2_rows("Passive"))

fn_d2 <- paste(
  "Monthly EW and VW aggregate portfolio returns regressed on CAPM,",
  "Fama-French three-factor, and Carhart (1997) four-factor models.",
  "Alphas annualised ($\\\\times 12$) and expressed as \\\\%.",
  "Newey-West $t$-statistics (6-month lag) in parentheses.",
  "$^{*}$, $^{**}$, $^{***}$: significant at 10\\\\%, 5\\\\%, 1\\\\% respectively.",
  paste0("Sample: ", SAMPLE_LABEL, "."),
  "Compare with Fama and French (2010), Table I."
)

latex_d2 <- d2_table %>%
  kbl(format    = "latex",
      booktabs  = TRUE,
      linesep   = "",
      escape    = FALSE,
      caption   = "Active vs.\\ Passive Aggregate Portfolio Alpha (\\%, Annualised) -- FF Subperiod",
      label     = "port_agg_alpha_FF",
      col.names = c("Portfolio",
                    "$\\alpha_{\\text{CAPM}}$",
                    "$\\alpha_{\\text{FF3}}$",
                    "$\\alpha_{\\text{Car}}$"),
      align     = c("l", "r", "r", "r")) %>%
  kable_styling(latex_options = "hold_position") %>%
  footnote(general        = fn_d2,
           general_title  = "",
           escape         = FALSE,
           threeparttable = TRUE)

writeLines(clean_latex(as.character(latex_d2), resize = FALSE, small = FALSE),
           "table_port_agg_alpha_FF.tex")
cat("Written: table_port_agg_alpha_FF.tex\n")

# =============================================================================
# 14. SUMMARY
# =============================================================================
cat("\n=== FF SUBPERIOD REPLICATION COMPLETE ===\n")
cat("Subperiod              :", format(DATE_MIN_FF), "to", format(DATE_MAX_FF), "\n")
cat("Funds (full-period)    :", nrow(alpha_full), "\n")
cat("Active funds (Carhart) :", n_active_bs, "\n")
cat("pi_0 (Storey, lambda=0.5):", round(pi_0_val * 100, 1), "%\n")
cat("BSW pi^-_A (gamma=0.20):", round(bsw_df$T_unskilled_pct[bsw_df$gamma == 20], 1), "%\n")
cat("BSW pi^+_A (gamma=0.20):", round(bsw_df$T_skilled_pct[bsw_df$gamma == 20], 1), "%\n")
cat("\nLaTeX outputs (move into your tables/ subdirectory):\n")
cat("  table_perf_aggregate_FF.tex        Table 7  (FF)\n")
cat("  table_bootstrap_tails_FF.tex       Table 9  (FF)\n")
cat("  table_pi0_estimate_FF.tex          Table 10 (FF)\n")
cat("  table10b_bsw_decomposition_FF.tex  Table 11 (FF)\n")
cat("  table_port_agg_alpha_FF.tex        Table 13 (FF)\n")
cat("  fig_luck_vs_skill_combined_FF.png  Figure 3 (FF)\n")