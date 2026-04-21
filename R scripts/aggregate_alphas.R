# =============================================================================
# AGGREGATE PORTFOLIO ALPHAS - FF (2010) METHODOLOGY                     v1.0
#
# Builds FF (2010)-style aggregate portfolio alphas for Tables 7, 8, and
# Figure 2. Runs AFTER alpha_estimation.R and BEFORE alpha_reporting.R.
#
# Methodology (per Fama & French 2010, Journal of Finance):
#   For each group (ap_group, Lipper class, etc.), construct a monthly
#   portfolio return time series:
#     R_EW_t = (1/N_t) * sum_i R_{i,t}                    (equal-weighted)
#     R_VW_t = sum_i w_{i,t-1} * R_{i,t}                  (VW, lagged TNA)
#       where w_{i,t-1} = TNA_{i,t-1} / sum_j TNA_{j,t-1}
#   Then regress each portfolio return series on the Carhart (1997) four-
#   factor model with Newey-West HAC standard errors (6-month lag).
#
# This replaces the previous per-fund-alpha --> cross-sectional weighted mean
# approach (with static mean-TNA weight) in Tables 7 and 8. The old approach
# produced a weighted average of per-fund point estimates with a time-invariant
# weight; the FF (2010) approach produces a single portfolio-level regression
# with contemporaneous dollar weighting. These are fundamentally different
# estimators; the FF approach is the field standard for aggregate claims.
#
# Produces:
#   aggregate_alphas.xlsx  with sheets:
#     t7_agg          - Table 7 input: 5 group rows (Active, Passive, Unknown,
#                       Active+Passive, Full) x 4 alphas each (EW/VW Gross/Net)
#     t8_lipper       - Table 8 input: (Lipper class x ap_group) rows x 4 alphas
#     fig2_rolling    - Figure 2 input: monthly 36-month rolling portfolio
#                       alphas for Active and Passive EW aggregate portfolios
#
# Requires (in session): panel_trimmed with columns
#   Ticker, date, ap_group, Lipper_Category, ret_gross, ret_net, tna_lag,
#   MKT_RF, SMB, HML, MOM, RF
#
# Dependencies: dplyr, lubridate, writexl
# =============================================================================

library(dplyr)
library(lubridate)
library(writexl)

# -----------------------------------------------------------------------------
# 0. CONFIG
# -----------------------------------------------------------------------------
NW_LAG_FULL   <- 6L    # Newey-West lag for full-period regressions (matches alpha_estimation.R)
MIN_OBS_FULL  <- 24L   # Minimum months for a portfolio regression
ROLL_WINDOW   <- 36L   # Rolling window length for Figure 2
MIN_FUNDS_LIP <- 3L    # Skip Lipper x group cells with fewer than this many funds

# -----------------------------------------------------------------------------
# 1. DATA PREP: rename columns, extract factor time series
# -----------------------------------------------------------------------------
cat("=== 1. Data preparation ===\n")

ap_agg <- panel_trimmed %>%
  rename(mkt_rf = MKT_RF, smb = SMB, hml = HML, mom = MOM, rf = RF,
         lipper = Lipper_Category) %>%
  filter(!is.na(ret_gross), !is.na(mkt_rf), !is.na(rf)) %>%
  mutate(ap_group = gsub("Agtive", "Active", ap_group)) %>%
  arrange(Ticker, date)

factors_ts <- ap_agg %>%
  distinct(date, mkt_rf, smb, hml, mom, rf) %>%
  arrange(date)

cat("  Fund-months:", nrow(ap_agg),
    " |  Unique dates:", nrow(factors_ts), "\n")

# -----------------------------------------------------------------------------
# 2. HELPERS: OLS, NW SE, monthly weighted mean
# -----------------------------------------------------------------------------

# OLS via normal equations - matches alpha_estimation.R fast_ols
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

# Newey-West HAC SE with Bartlett kernel
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

# Monthly contemporaneous weighted mean (used for VW portfolio returns)
wm_panel <- function(x, w) {
  v <- !is.na(x) & !is.na(w) & w > 0
  if (sum(v) == 0L) return(NA_real_)
  sum(x[v] * w[v]) / sum(w[v])
}

# -----------------------------------------------------------------------------
# 3. CARHART 4-FACTOR REGRESSION ON A PORTFOLIO RETURN SERIES
# -----------------------------------------------------------------------------
# Returns annualised alpha and NW t-stat. Expects a data frame with columns
# date and ret (total portfolio return, not excess). Subtracts RF internally.
run_port_carhart <- function(port_df) {
  d <- port_df %>%
    inner_join(factors_ts, by = "date") %>%
    filter(!is.na(ret), !is.na(mkt_rf), !is.na(rf))
  n <- nrow(d)
  if (n < MIN_OBS_FULL) {
    return(list(alpha_ann = NA_real_, t_nw = NA_real_, n_months = n))
  }
  y <- d$ret - d$rf
  X <- cbind(1, d$mkt_rf, d$smb, d$hml, d$mom)
  fit <- fast_ols(y, X)
  if (is.null(fit)) {
    return(list(alpha_ann = NA_real_, t_nw = NA_real_, n_months = n))
  }
  se <- nw_se(X, fit$e, NW_LAG_FULL)
  list(alpha_ann = fit$beta[1] * 12,
       t_nw      = fit$beta[1] / se[1],
       n_months  = n)
}

# -----------------------------------------------------------------------------
# 4. PORTFOLIO RETURN CONSTRUCTION
# -----------------------------------------------------------------------------
# Given a subset of fund-month rows, build the monthly EW/VW portfolio return
# time series. Each month: EW = simple mean across funds alive that month;
# VW = lagged-TNA-weighted mean. Funds missing tna_lag are dropped from VW
# but retained in EW (standard treatment).
build_port_returns <- function(data) {
  data %>%
    group_by(date) %>%
    summarise(
      ret_ew_gross = mean(ret_gross,          na.rm = TRUE),
      ret_ew_net   = mean(ret_net,            na.rm = TRUE),
      ret_vw_gross = wm_panel(ret_gross, tna_lag),
      ret_vw_net   = wm_panel(ret_net,   tna_lag),
      n_funds      = sum(!is.na(ret_gross)),
      .groups      = "drop"
    ) %>%
    arrange(date)
}

# -----------------------------------------------------------------------------
# 5. TABLE 7: AGGREGATE ALPHA BY GROUP
# -----------------------------------------------------------------------------
cat("=== 5. Table 7: aggregate alpha by ap_group ===\n")

# Five groups (same row structure as current Table 7)
t7_groups <- list(
  "Active"           = ap_agg %>% filter(ap_group == "Active"),
  "Passive"          = ap_agg %>% filter(ap_group == "Passive"),
  "Unknown"          = ap_agg %>% filter(ap_group == "Unknown"),
  "Active + Passive" = ap_agg %>% filter(ap_group %in% c("Active", "Passive")),
  "Full Sample"      = ap_agg
)

t7_rows <- lapply(names(t7_groups), function(g) {
  sub <- t7_groups[[g]]
  if (nrow(sub) == 0L) return(NULL)
  pr  <- build_port_returns(sub)
  # Four regressions per group (Table 7's four alpha cells)
  r_eg <- run_port_carhart(pr %>% select(date, ret = ret_ew_gross))
  r_en <- run_port_carhart(pr %>% select(date, ret = ret_ew_net))
  r_vg <- run_port_carhart(pr %>% select(date, ret = ret_vw_gross))
  r_vn <- run_port_carhart(pr %>% select(date, ret = ret_vw_net))
  data.frame(
    Group          = g,
    n_funds        = n_distinct(sub$Ticker),
    t_months       = r_eg$n_months,
    alpha_ew_gross = r_eg$alpha_ann, t_ew_gross = r_eg$t_nw,
    alpha_ew_net   = r_en$alpha_ann, t_ew_net   = r_en$t_nw,
    alpha_vw_gross = r_vg$alpha_ann, t_vw_gross = r_vg$t_nw,
    alpha_vw_net   = r_vn$alpha_ann, t_vw_net   = r_vn$t_nw
  )
})

t7_agg <- bind_rows(t7_rows)
cat("  Rows produced:", nrow(t7_agg), "\n")
print(t7_agg)

# -----------------------------------------------------------------------------
# 6. TABLE 8: AGGREGATE ALPHA BY LIPPER CLASS x AP_GROUP
# -----------------------------------------------------------------------------
cat("\n=== 6. Table 8: aggregate alpha by Lipper class ===\n")

# Only Active and Passive, with valid Lipper classification
lipper_base <- ap_agg %>%
  filter(ap_group %in% c("Active", "Passive"),
         !is.na(lipper), lipper != "",
         lipper != "#N/A", lipper != "#N/A N/A")

t8_combos <- lipper_base %>%
  distinct(lipper, ap_group) %>%
  arrange(lipper, ap_group)

t8_rows <- lapply(seq_len(nrow(t8_combos)), function(i) {
  lp <- t8_combos$lipper[i]
  gp <- t8_combos$ap_group[i]
  sub <- lipper_base %>% filter(lipper == lp, ap_group == gp)
  n_funds <- n_distinct(sub$Ticker)
  if (n_funds < MIN_FUNDS_LIP) return(NULL)
  pr <- build_port_returns(sub)
  # Compute both gross and net to give the downstream script a choice.
  # Table B.1 (alpha_reporting.R v8.1+) uses gross; keep net for robustness.
  r_eg <- run_port_carhart(pr %>% select(date, ret = ret_ew_gross))
  r_vg <- run_port_carhart(pr %>% select(date, ret = ret_vw_gross))
  r_en <- run_port_carhart(pr %>% select(date, ret = ret_ew_net))
  r_vn <- run_port_carhart(pr %>% select(date, ret = ret_vw_net))
  data.frame(
    lipper         = lp,
    Group          = gp,
    n_funds        = n_funds,
    t_months       = r_eg$n_months,
    alpha_ew_gross = r_eg$alpha_ann, t_ew_gross = r_eg$t_nw,
    alpha_vw_gross = r_vg$alpha_ann, t_vw_gross = r_vg$t_nw,
    alpha_ew_net   = r_en$alpha_ann, t_ew_net   = r_en$t_nw,
    alpha_vw_net   = r_vn$alpha_ann, t_vw_net   = r_vn$t_nw
  )
})

t8_lipper <- bind_rows(t8_rows)
cat("  Rows produced:", nrow(t8_lipper),
    " |  Skipped (n_funds <", MIN_FUNDS_LIP, "):",
    nrow(t8_combos) - nrow(t8_lipper), "\n")

# -----------------------------------------------------------------------------
# 7. FIGURE 2: ROLLING 36-MONTH PORTFOLIO ALPHA
# -----------------------------------------------------------------------------
# Rolling Carhart regression on the monthly EW portfolio return for Active and
# Passive separately. One alpha per month-end with a 36-month backward window.
cat("\n=== 7. Figure 2: rolling aggregate alpha ===\n")

build_rolling_agg <- function(group_name) {
  sub <- ap_agg %>% filter(ap_group == group_name)
  if (nrow(sub) == 0L) return(data.frame())
  pr <- build_port_returns(sub) %>%
    inner_join(factors_ts, by = "date") %>%
    filter(!is.na(ret_ew_gross), !is.na(mkt_rf), !is.na(rf)) %>%
    arrange(date)
  n <- nrow(pr)
  if (n < ROLL_WINDOW) return(data.frame())
  
  y_full <- pr$ret_ew_gross - pr$rf
  X_full <- cbind(1, pr$mkt_rf, pr$smb, pr$hml, pr$mom)
  dates  <- pr$date
  
  out <- lapply(ROLL_WINDOW:n, function(i) {
    win <- (i - ROLL_WINDOW + 1L):i
    fit <- fast_ols(y_full[win], X_full[win, , drop = FALSE])
    if (is.null(fit)) return(NULL)
    data.frame(date = dates[i], ap_group = group_name,
               alpha_ann = fit$beta[1] * 12)
  })
  bind_rows(out)
}

fig2_rolling <- bind_rows(
  build_rolling_agg("Active"),
  build_rolling_agg("Passive")
)
cat("  Monthly observations:", nrow(fig2_rolling), "\n")

# -----------------------------------------------------------------------------
# 8. SAVE OUTPUTS
# -----------------------------------------------------------------------------
write_xlsx(
  list(
    t7_agg       = t7_agg,
    t8_lipper    = t8_lipper,
    fig2_rolling = fig2_rolling
  ),
  "aggregate_alphas.xlsx"
)
cat("\n[SUCCESS] aggregate_alphas.xlsx written. ")
cat("aggregate_alphas.R v1.0 complete.\n")# =============================================================================
# AGGREGATE PORTFOLIO ALPHAS - FF (2010) METHODOLOGY                     v1.0
#
# Builds FF (2010)-style aggregate portfolio alphas for Tables 7, 8, and
# Figure 2. Runs AFTER alpha_estimation.R and BEFORE alpha_reporting.R.
#
# Methodology (per Fama & French 2010, Journal of Finance):
#   For each group (ap_group, Lipper class, etc.), construct a monthly
#   portfolio return time series:
#     R_EW_t = (1/N_t) * sum_i R_{i,t}                    (equal-weighted)
#     R_VW_t = sum_i w_{i,t-1} * R_{i,t}                  (VW, lagged TNA)
#       where w_{i,t-1} = TNA_{i,t-1} / sum_j TNA_{j,t-1}
#   Then regress each portfolio return series on the Carhart (1997) four-
#   factor model with Newey-West HAC standard errors (6-month lag).
#
# This replaces the previous per-fund-alpha --> cross-sectional weighted mean
# approach (with static mean-TNA weight) in Tables 7 and 8. The old approach
# produced a weighted average of per-fund point estimates with a time-invariant
# weight; the FF (2010) approach produces a single portfolio-level regression
# with contemporaneous dollar weighting. These are fundamentally different
# estimators; the FF approach is the field standard for aggregate claims.
#
# Produces:
#   aggregate_alphas.xlsx  with sheets:
#     t7_agg          - Table 7 input: 5 group rows (Active, Passive, Unknown,
#                       Active+Passive, Full) x 4 alphas each (EW/VW Gross/Net)
#     t8_lipper       - Table 8 input: (Lipper class x ap_group) rows x 4 alphas
#     fig2_rolling    - Figure 2 input: monthly 36-month rolling portfolio
#                       alphas for Active and Passive EW aggregate portfolios
#
# Requires (in session): panel_trimmed with columns
#   Ticker, date, ap_group, Lipper_Category, ret_gross, ret_net, tna_lag,
#   MKT_RF, SMB, HML, MOM, RF
#
# Dependencies: dplyr, lubridate, writexl
# =============================================================================

library(dplyr)
library(lubridate)
library(writexl)

# -----------------------------------------------------------------------------
# 0. CONFIG
# -----------------------------------------------------------------------------
NW_LAG_FULL   <- 6L    # Newey-West lag for full-period regressions (matches alpha_estimation.R)
MIN_OBS_FULL  <- 24L   # Minimum months for a portfolio regression
ROLL_WINDOW   <- 36L   # Rolling window length for Figure 2
MIN_FUNDS_LIP <- 3L    # Skip Lipper x group cells with fewer than this many funds

# -----------------------------------------------------------------------------
# 1. DATA PREP: rename columns, extract factor time series
# -----------------------------------------------------------------------------
cat("=== 1. Data preparation ===\n")

ap_agg <- panel_trimmed %>%
  rename(mkt_rf = MKT_RF, smb = SMB, hml = HML, mom = MOM, rf = RF,
         lipper = Lipper_Category) %>%
  filter(!is.na(ret_gross), !is.na(mkt_rf), !is.na(rf)) %>%
  mutate(ap_group = gsub("Agtive", "Active", ap_group)) %>%
  arrange(Ticker, date)

factors_ts <- ap_agg %>%
  distinct(date, mkt_rf, smb, hml, mom, rf) %>%
  arrange(date)

cat("  Fund-months:", nrow(ap_agg),
    " |  Unique dates:", nrow(factors_ts), "\n")

# -----------------------------------------------------------------------------
# 2. HELPERS: OLS, NW SE, monthly weighted mean
# -----------------------------------------------------------------------------

# OLS via normal equations - matches alpha_estimation.R fast_ols
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

# Newey-West HAC SE with Bartlett kernel
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

# Monthly contemporaneous weighted mean (used for VW portfolio returns)
wm_panel <- function(x, w) {
  v <- !is.na(x) & !is.na(w) & w > 0
  if (sum(v) == 0L) return(NA_real_)
  sum(x[v] * w[v]) / sum(w[v])
}

# -----------------------------------------------------------------------------
# 3. CARHART 4-FACTOR REGRESSION ON A PORTFOLIO RETURN SERIES
# -----------------------------------------------------------------------------
# Returns annualised alpha and NW t-stat. Expects a data frame with columns
# date and ret (total portfolio return, not excess). Subtracts RF internally.
run_port_carhart <- function(port_df) {
  d <- port_df %>%
    inner_join(factors_ts, by = "date") %>%
    filter(!is.na(ret), !is.na(mkt_rf), !is.na(rf))
  n <- nrow(d)
  if (n < MIN_OBS_FULL) {
    return(list(alpha_ann = NA_real_, t_nw = NA_real_, n_months = n))
  }
  y <- d$ret - d$rf
  X <- cbind(1, d$mkt_rf, d$smb, d$hml, d$mom)
  fit <- fast_ols(y, X)
  if (is.null(fit)) {
    return(list(alpha_ann = NA_real_, t_nw = NA_real_, n_months = n))
  }
  se <- nw_se(X, fit$e, NW_LAG_FULL)
  list(alpha_ann = fit$beta[1] * 12,
       t_nw      = fit$beta[1] / se[1],
       n_months  = n)
}

# -----------------------------------------------------------------------------
# 4. PORTFOLIO RETURN CONSTRUCTION
# -----------------------------------------------------------------------------
# Given a subset of fund-month rows, build the monthly EW/VW portfolio return
# time series. Each month: EW = simple mean across funds alive that month;
# VW = lagged-TNA-weighted mean. Funds missing tna_lag are dropped from VW
# but retained in EW (standard treatment).
build_port_returns <- function(data) {
  data %>%
    group_by(date) %>%
    summarise(
      ret_ew_gross = mean(ret_gross,          na.rm = TRUE),
      ret_ew_net   = mean(ret_net,            na.rm = TRUE),
      ret_vw_gross = wm_panel(ret_gross, tna_lag),
      ret_vw_net   = wm_panel(ret_net,   tna_lag),
      n_funds      = sum(!is.na(ret_gross)),
      .groups      = "drop"
    ) %>%
    arrange(date)
}

# -----------------------------------------------------------------------------
# 5. TABLE 7: AGGREGATE ALPHA BY GROUP
# -----------------------------------------------------------------------------
cat("=== 5. Table 7: aggregate alpha by ap_group ===\n")

# Five groups (same row structure as current Table 7)
t7_groups <- list(
  "Active"           = ap_agg %>% filter(ap_group == "Active"),
  "Passive"          = ap_agg %>% filter(ap_group == "Passive"),
  "Unknown"          = ap_agg %>% filter(ap_group == "Unknown"),
  "Active + Passive" = ap_agg %>% filter(ap_group %in% c("Active", "Passive")),
  "Full Sample"      = ap_agg
)

t7_rows <- lapply(names(t7_groups), function(g) {
  sub <- t7_groups[[g]]
  if (nrow(sub) == 0L) return(NULL)
  pr  <- build_port_returns(sub)
  # Four regressions per group (Table 7's four alpha cells)
  r_eg <- run_port_carhart(pr %>% select(date, ret = ret_ew_gross))
  r_en <- run_port_carhart(pr %>% select(date, ret = ret_ew_net))
  r_vg <- run_port_carhart(pr %>% select(date, ret = ret_vw_gross))
  r_vn <- run_port_carhart(pr %>% select(date, ret = ret_vw_net))
  data.frame(
    Group          = g,
    n_funds        = n_distinct(sub$Ticker),
    t_months       = r_eg$n_months,
    alpha_ew_gross = r_eg$alpha_ann, t_ew_gross = r_eg$t_nw,
    alpha_ew_net   = r_en$alpha_ann, t_ew_net   = r_en$t_nw,
    alpha_vw_gross = r_vg$alpha_ann, t_vw_gross = r_vg$t_nw,
    alpha_vw_net   = r_vn$alpha_ann, t_vw_net   = r_vn$t_nw
  )
})

t7_agg <- bind_rows(t7_rows)
cat("  Rows produced:", nrow(t7_agg), "\n")
print(t7_agg)

# -----------------------------------------------------------------------------
# 6. TABLE 8: AGGREGATE ALPHA BY LIPPER CLASS x AP_GROUP
# -----------------------------------------------------------------------------
cat("\n=== 6. Table 8: aggregate alpha by Lipper class ===\n")

# Only Active and Passive, with valid Lipper classification
lipper_base <- ap_agg %>%
  filter(ap_group %in% c("Active", "Passive"),
         !is.na(lipper), lipper != "",
         lipper != "#N/A", lipper != "#N/A N/A")

t8_combos <- lipper_base %>%
  distinct(lipper, ap_group) %>%
  arrange(lipper, ap_group)

t8_rows <- lapply(seq_len(nrow(t8_combos)), function(i) {
  lp <- t8_combos$lipper[i]
  gp <- t8_combos$ap_group[i]
  sub <- lipper_base %>% filter(lipper == lp, ap_group == gp)
  n_funds <- n_distinct(sub$Ticker)
  if (n_funds < MIN_FUNDS_LIP) return(NULL)
  pr <- build_port_returns(sub)
  # Compute both gross and net to give the downstream script a choice.
  # Table B.1 (alpha_reporting.R v8.1+) uses gross; keep net for robustness.
  r_eg <- run_port_carhart(pr %>% select(date, ret = ret_ew_gross))
  r_vg <- run_port_carhart(pr %>% select(date, ret = ret_vw_gross))
  r_en <- run_port_carhart(pr %>% select(date, ret = ret_ew_net))
  r_vn <- run_port_carhart(pr %>% select(date, ret = ret_vw_net))
  data.frame(
    lipper         = lp,
    Group          = gp,
    n_funds        = n_funds,
    t_months       = r_eg$n_months,
    alpha_ew_gross = r_eg$alpha_ann, t_ew_gross = r_eg$t_nw,
    alpha_vw_gross = r_vg$alpha_ann, t_vw_gross = r_vg$t_nw,
    alpha_ew_net   = r_en$alpha_ann, t_ew_net   = r_en$t_nw,
    alpha_vw_net   = r_vn$alpha_ann, t_vw_net   = r_vn$t_nw
  )
})

t8_lipper <- bind_rows(t8_rows)
cat("  Rows produced:", nrow(t8_lipper),
    " |  Skipped (n_funds <", MIN_FUNDS_LIP, "):",
    nrow(t8_combos) - nrow(t8_lipper), "\n")

# -----------------------------------------------------------------------------
# 7. FIGURE 2: ROLLING 36-MONTH PORTFOLIO ALPHA
# -----------------------------------------------------------------------------
# Rolling Carhart regression on the monthly EW portfolio return for Active and
# Passive separately. One alpha per month-end with a 36-month backward window.
cat("\n=== 7. Figure 2: rolling aggregate alpha ===\n")

build_rolling_agg <- function(group_name) {
  sub <- ap_agg %>% filter(ap_group == group_name)
  if (nrow(sub) == 0L) return(data.frame())
  pr <- build_port_returns(sub) %>%
    inner_join(factors_ts, by = "date") %>%
    filter(!is.na(ret_ew_gross), !is.na(mkt_rf), !is.na(rf)) %>%
    arrange(date)
  n <- nrow(pr)
  if (n < ROLL_WINDOW) return(data.frame())
  
  y_full <- pr$ret_ew_gross - pr$rf
  X_full <- cbind(1, pr$mkt_rf, pr$smb, pr$hml, pr$mom)
  dates  <- pr$date
  
  out <- lapply(ROLL_WINDOW:n, function(i) {
    win <- (i - ROLL_WINDOW + 1L):i
    fit <- fast_ols(y_full[win], X_full[win, , drop = FALSE])
    if (is.null(fit)) return(NULL)
    data.frame(date = dates[i], ap_group = group_name,
               alpha_ann = fit$beta[1] * 12)
  })
  bind_rows(out)
}

fig2_rolling <- bind_rows(
  build_rolling_agg("Active"),
  build_rolling_agg("Passive")
)
cat("  Monthly observations:", nrow(fig2_rolling), "\n")

# -----------------------------------------------------------------------------
# 8. SAVE OUTPUTS
# -----------------------------------------------------------------------------
write_xlsx(
  list(
    t7_agg       = t7_agg,
    t8_lipper    = t8_lipper,
    fig2_rolling = fig2_rolling
  ),
  "aggregate_alphas.xlsx"
)
cat("\n[SUCCESS] aggregate_alphas.xlsx written. ")
cat("aggregate_alphas.R v1.0 complete.\n")