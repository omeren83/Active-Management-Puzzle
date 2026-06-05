# =============================================================================
# ROBUSTNESS ALPHA ESTIMATION: FF6 AND CARHART + PS LIQUIDITY (v1.4)
#
# v1.4 changes vs v1.3
# --------------------
#   Aggregate portfolio regressions added (Table I.1 inputs).
#   (a) New portfolio regression helpers added in Section 1: fast_ols_port(),
#       nw_se_port(), wm_monthly(), build_port_returns_robust(), run_port_reg().
#       These are generalised to arbitrary factor sets and mirror aggregate_alphas.R
#       exactly. Adjusted R² (not plain R²) is computed and stored.
#   (b) New step 5e inside the per-spec loop: runs aggregate portfolio regressions
#       for Active and Passive groups (EW/VW × Gross/Net), producing a
#       port_alpha data frame with columns: Group, n_funds, t_months,
#       alpha_ew_gross, t_ew_gross, alpha_vw_gross, t_vw_gross,
#       alpha_ew_net, t_ew_net, alpha_vw_net, t_vw_net, r2_adj_ew, r2_adj_vw.
#       All alpha values stored in annualised decimal (×12) form, consistent
#       with the existing alpha_ann convention.
#   (c) Carhart added to SPECS so the same loop produces the Carhart
#       bootstrap_results.xlsx port_alpha sheet, giving build_robust_tables.R
#       a single uniform reading pattern across all three specifications.
#       The Carhart bootstrap and BSW blocks are skipped (those results already
#       exist in bootstrap_results.xlsx from alpha_estimation.R); only the
#       portfolio regressions and per-fund alpha xlsx are produced.
#   (d) port_alpha written as a new sheet in bootstrap_results_{spec}.xlsx
#       alongside the existing summary / bsw_decomposition / bsw_meta sheets.
#       build_robust_tables.R reads this sheet for Table I.1.
#
# v1.3 changes vs v1.2 (Family B audit)
# --------------------------------------
#   filter(!excluded_perf) added to the ap_robust panel-prep stage.
#
# v1.1 / v1.2 changes — see prior version headers.
#
# Scope
# -----
# Reproduces the components needed for Appendix I: full-period per-fund alphas,
# bootstrap t-stat distributions, BSW decomposition, and aggregate portfolio
# alphas — all under three factor specifications (Carhart, FF6, C+PSL).
# Rolling alphas and flow regressions use Carhart factors by design and are
# not re-estimated here.
#
# Outputs
# -------
#   alpha_fullperiod_FF6.xlsx / alpha_fullperiod_C5.xlsx
#   alpha_fullperiod_Carhart.xlsx   (Carhart per-fund alphas, for cross-check)
#   bootstrap_results_FF6.xlsx      sheets: summary, fund_talpha,
#   bootstrap_results_C5.xlsx              bsw_decomposition, bsw_meta,
#   bootstrap_results_Carhart.xlsx         port_alpha  (NEW in v1.4)
#   robust_alpha_summary.xlsx       side-by-side pooled alphas (diagnostic)
#
# Prerequisites
# -------------
#   - data_import_and_cleaning.R sourced; panel_incubation in session with
#     columns RMW, CMA, PSL populated from the factors sheet.
#   - alpha_estimation.R run; bootstrap_results.xlsx exists for BSW meta
#     (Carhart bootstrap/BSW not re-estimated here — too expensive to duplicate).
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

# Factor specifications. Carhart included so portfolio regressions for Table
# I.1 are produced here for all three specs uniformly. The Carhart bootstrap
# and BSW are NOT re-run (already done in alpha_estimation.R); a flag below
# gates those expensive blocks to FF6 and C5 only.
SPECS <- list(
  Carhart = c("MKT_RF", "SMB", "HML", "MOM"),
  FF6     = c("MKT_RF", "SMB", "HML", "RMW", "CMA", "MOM"),
  C5      = c("MKT_RF", "SMB", "HML", "MOM", "PSL")
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

# --- 1b. PORTFOLIO REGRESSION HELPERS ----------------------------------------
# Used by step 5e (aggregate portfolio alphas for Table I.1).
# Mirrors aggregate_alphas.R but generalised to arbitrary factor sets.

# Monthly weighted mean for VW portfolio returns (lagged TNA weights).
wm_monthly <- function(x, w) {
  v <- !is.na(x) & !is.na(w) & w > 0
  if (sum(v) == 0L) return(NA_real_)
  sum(x[v] * w[v]) / sum(w[v])
}

# Build monthly EW/VW portfolio return series from a fund-month panel.
build_port_returns_robust <- function(data) {
  data %>%
    group_by(date) %>%
    summarise(
      ret_ew_gross = mean(ret_gross, na.rm = TRUE),
      ret_ew_net   = mean(ret_net,   na.rm = TRUE),
      ret_vw_gross = wm_monthly(ret_gross, tna_lag),
      ret_vw_net   = wm_monthly(ret_net,   tna_lag),
      .groups = "drop"
    ) %>%
    arrange(date)
}

# Regress one portfolio return series on a given factor set.
# factors_ts: distinct date × rf × factor_cols data frame.
# ret_col:    name of the return column in port_df.
# Returns list: alpha_ann (annualised decimal), t_nw, r2_adj, n_months.
run_port_reg <- function(port_df, factors_ts, ret_col, factor_cols) {
  d <- port_df %>%
    inner_join(factors_ts, by = "date") %>%
    filter(!is.na(.data[[ret_col]]),
           if_all(all_of(factor_cols), ~ !is.na(.)),
           !is.na(rf))
  n <- nrow(d)
  if (n < MIN_OBS_FULL)
    return(list(alpha_ann = NA_real_, t_nw = NA_real_,
                r2_adj = NA_real_, n_months = n))
  y   <- d[[ret_col]] - d$rf
  X   <- cbind(1, as.matrix(d[, factor_cols, drop = FALSE]))
  fit <- fast_ols(y, X)
  if (is.null(fit))
    return(list(alpha_ann = NA_real_, t_nw = NA_real_,
                r2_adj = NA_real_, n_months = n))
  k   <- ncol(X)
  tss <- sum((y - mean(y))^2)
  r2_adj <- if (tss == 0) NA_real_
             else 1 - (sum(fit$e^2) / (n - k)) / (tss / (n - 1))
  se  <- nw_se(X, fit$e, NW_LAG_FULL)
  list(alpha_ann = fit$beta[1] * 12,
       t_nw      = fit$beta[1] / se[1],
       r2_adj    = r2_adj,
       n_months  = n)
}

# Run all four regressions (EW/VW × Gross/Net) for one group, return one row.
port_alpha_row <- function(group_name, panel_sub, factors_ts, factor_cols) {
  n_funds <- n_distinct(panel_sub$Ticker)
  if (n_funds == 0L) {
    return(data.frame(
      Group = group_name, n_funds = 0L, t_months = NA_integer_,
      alpha_ew_gross = NA_real_, t_ew_gross = NA_real_,
      alpha_vw_gross = NA_real_, t_vw_gross = NA_real_,
      alpha_ew_net   = NA_real_, t_ew_net   = NA_real_,
      alpha_vw_net   = NA_real_, t_vw_net   = NA_real_,
      r2_adj_ew = NA_real_,     r2_adj_vw  = NA_real_
    ))
  }
  pr  <- build_port_returns_robust(panel_sub)
  rEG <- run_port_reg(pr, factors_ts, "ret_ew_gross", factor_cols)
  rVG <- run_port_reg(pr, factors_ts, "ret_vw_gross", factor_cols)
  rEN <- run_port_reg(pr, factors_ts, "ret_ew_net",   factor_cols)
  rVN <- run_port_reg(pr, factors_ts, "ret_vw_net",   factor_cols)
  data.frame(
    Group          = group_name,
    n_funds        = n_funds,
    t_months       = rEG$n_months,
    alpha_ew_gross = rEG$alpha_ann, t_ew_gross = rEG$t_nw,
    alpha_vw_gross = rVG$alpha_ann, t_vw_gross = rVG$t_nw,
    alpha_ew_net   = rEN$alpha_ann, t_ew_net   = rEN$t_nw,
    alpha_vw_net   = rVN$alpha_ann, t_vw_net   = rVN$t_nw,
    r2_adj_ew      = rEG$r2_adj,   r2_adj_vw  = rVG$r2_adj
  )
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
# ret_gross, ret_net, tna_lag are passed through from panel_incubation and are
# available in ap_robust for the portfolio regression helpers (step 5e).

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

  # 5b–5d. Bootstrap + BSW: run for FF6 and C5 only.
  # Carhart bootstrap/BSW already produced by alpha_estimation.R; re-running
  # them here would be redundant and expensive (~30 min per run).
  if (spec_name != "Carhart") {

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

  } else {
    # Carhart: read bootstrap/BSW results from alpha_estimation.R outputs.
    cat("  [b-d] Carhart: reading bootstrap/BSW from bootstrap_results.xlsx...\n")
    bootstrap_summary <- tryCatch(
      read_excel("bootstrap_results.xlsx", sheet = "summary"),
      error = function(e) { cat("      WARNING: bootstrap_results.xlsx not found.\n"); NULL }
    )
    bsw_df   <- tryCatch(read_excel("bootstrap_results.xlsx", sheet = "bsw_decomposition"),
                         error = function(e) NULL)
    bsw_meta <- tryCatch(read_excel("bootstrap_results.xlsx", sheet = "bsw_meta"),
                         error = function(e) NULL)
    active_fp <- alpha_full %>% filter(ap_group == "Active", !is.na(alpha_t_nw))
    n_bsw     <- if (!is.null(bsw_meta)) as.integer(bsw_meta$n_active[1]) else nrow(active_fp)
    pi_A_minus <- if (!is.null(bsw_meta)) bsw_meta$pi_A_minus[1] else NA_real_
    pi_A_plus  <- if (!is.null(bsw_meta)) bsw_meta$pi_A_plus[1]  else NA_real_
    pi0_bsw    <- if (!is.null(bsw_meta)) bsw_meta$pi0_pct[1] / 100 else NA_real_
  }

  # 5e. Aggregate portfolio regressions (Table I.1 inputs).
  # Runs for ALL three specs. Produces Active and Passive rows.
  cat("  [d2] Aggregate portfolio regressions...\n")
  factors_ts <- ap_spec %>%
    distinct(date, rf, !!!syms(factor_cols)) %>%
    arrange(date)
  port_alpha <- bind_rows(
    port_alpha_row("Active",  ap_spec %>% filter(ap_group == "Active"),  factors_ts, factor_cols),
    port_alpha_row("Passive", ap_spec %>% filter(ap_group == "Passive"), factors_ts, factor_cols)
  )
  cat(sprintf("      Active  N=%d T=%d | Passive N=%d T=%d\n",
              port_alpha$n_funds[1], port_alpha$t_months[1],
              port_alpha$n_funds[2], port_alpha$t_months[2]))

  # 5f. Save spec-specific outputs
  write_xlsx(alpha_full,
             sprintf("alpha_fullperiod_%s.xlsx", spec_name))
  write_xlsx(
    list(
      summary           = if (!is.null(bootstrap_summary)) bootstrap_summary else data.frame(),
      fund_talpha       = active_fp,
      bsw_decomposition = if (!is.null(bsw_df))   bsw_df   else data.frame(),
      bsw_meta          = if (!is.null(bsw_meta))  bsw_meta else data.frame(),
      port_alpha        = port_alpha        # NEW v1.4: aggregate portfolio results
    ),
    sprintf("bootstrap_results_%s.xlsx", spec_name)
  )
  
  # 5g. Accumulate diagnostic summary row (pooled cross-sectional means,
  # for robust_alpha_summary.xlsx only — no longer drives any table).
  if (spec_name != "Carhart") {
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
  } # end if (spec_name != "Carhart")
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
cat("  alpha_fullperiod_Carhart.xlsx / alpha_fullperiod_FF6.xlsx / alpha_fullperiod_C5.xlsx\n")
cat("  bootstrap_results_Carhart.xlsx / bootstrap_results_FF6.xlsx / bootstrap_results_C5.xlsx\n")
cat("    Each bootstrap_results_*.xlsx now contains a 'port_alpha' sheet (NEW v1.4)\n")
cat("    with aggregate portfolio alphas for Active and Passive groups (Table I.1 inputs).\n")
cat("  robust_alpha_summary.xlsx (diagnostic pooled cross-sectional means, FF6 and C5 only)\n")