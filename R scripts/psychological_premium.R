# psychological_premium.R                                              v1.2
# =============================================================================
# v1.2 changes (Family E pre-defense audit):
#   - filter(!excluded_h3) added to the panel-prep stage. The Shadow Price /
#     Psychological Premium is identified jointly with H1 + H3 channels and
#     uses the same lottery proxies (ActR2, ActSkew, MAX12) as H3, so it
#     shares H3's identification scope: the !excluded_h3 subsample of
#     flagged_funds.xlsx Step 8c. Without this restriction, sector funds,
#     covered-call overlays, and Lipper categories without unambiguous
#     size-style benchmarks would contaminate the activeness measures and
#     therefore the Shadow Price estimates. Mirrors the v2.1 fix in
#     H3_lottery_demand.R. Requires panel_regressions_setup.R v1.3+, which
#     preserves the excluded_h3 flag column in panel_reg.
#   - Sample-source phrasing added to the table footnote.
#
# v1.1 changes (May 2026):
#   - Footnote expanded with alpha-basis-points conversion paragraph for
#     Table 23. Calibration: EW Carhart alpha Q5-Q1 spread of 2.23% p.a.
#     across midpoint-to-midpoint rank distance 0.8 = 279 bps per rank-point.
#     Worked example: MAX12 R_MID, low/high sentiment = 14 / 58 bps per SD.
#     Sourced from Table 12 (Active EW, Momentum); see fn variable below.
# =============================================================================
# =============================================================================
# Computes the Shadow Price / Psychological Premium derived from the joint
# H1+H3 estimation, following Equation 12 of the revised proposal.
#
# Theoretical object (proposal eq. 12):
#     SP_t  =  dR/dAS |_{dFlow=0}  =  -(eta + phi * D^HIGH_t) /
#                                        (beta^rank + delta^rank * D^HIGH_t)
# where eta   = main effect of the lottery proxy on flow
#       phi   = lottery x sentiment interaction (H3 channel)
#       beta  = main effect of return rank on flow (H1 channel)
#       delta = rank x sentiment interaction (H1 channel)
#
# For sample-aligned identification, all four parameters are estimated in
# a single joint regression (one per lottery measure):
#
#     flow ~ R_LOW + R_MID + R_HIGH + Lottery_z
#          + D_SENT
#          + R_LOW:D_SENT + R_MID:D_SENT + R_HIGH:D_SENT
#          + Lottery_z:D_SENT
#          + controls | Ticker FE
#
# The lottery measure is standardised within sample, so SP is denominated
# in "rank points per standard deviation of lottery measure". This unit
# is directly comparable across the three measures (ActR2, ActSkew, MAX12).
# The dissertation prose can map rank points to alpha basis points using
# the in-sample rank--alpha correlation if a dimensioned figure is needed.
#
# At each of the three rank segments q in {LOW, MID, HIGH}:
#     SP_low(q)  = -eta / beta_q                          (low-sentiment regime)
#     SP_high(q) = -(eta + phi) / (beta_q + delta_q)      (high-sentiment regime)
#     Diff(q)    = SP_high(q) - SP_low(q)                 (regime contrast)
#
# Standard errors via delta method using the joint vcov from the regression.
# Optional parametric MVN bootstrap (toggle DO_BOOTSTRAP) provides a
# finite-sample robustness check.
#
# Output: table_psychological_premium.tex (rows = lottery x segment,
#         columns = SP_low, SP_high, Diff, 1-sided p)
# =============================================================================

suppressPackageStartupMessages({
  library(dplyr)
  library(fixest)
  library(kableExtra)
})
# MASS is needed only for mvrnorm() in the parametric bootstrap.
# Do NOT call library(MASS) — MASS exports select() which masks
# dplyr::select() and breaks panel_regressions_setup.R if both
# run in the same session. Use MASS::mvrnorm() directly instead.
if (!requireNamespace("MASS", quietly = TRUE))
  stop("Package MASS needed for bootstrap. Install with install.packages('MASS').")

# --- 0. Config --------------------------------------------------------------
if (!exists("WORKING_DIR")) WORKING_DIR <- getwd()
OUTPUT_TEX <- file.path(WORKING_DIR, "table_psychological_premium.tex")

# PHASE B helper: move tablenotes outside the table float (fixes Overfull \vbox).
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
  s  <- gsub(note_rx, "", s, perl = TRUE)
  s  <- gsub("\\\\begin\\{threeparttable\\}\\s*\n?", "", s)
  s  <- gsub("\\\\end\\{threeparttable\\}\\}?\\s*\n?", "", s)
  sub("\\end{table}", paste0("\\end{table}\n\\begin{minipage}{0.92\\linewidth}\n",
    "\\footnotesize\\textit{Note:} ", ni, "\n\\end{minipage}\n"),
    s, fixed = TRUE)
}

DO_BOOTSTRAP <- TRUE
B_BOOT       <- 1000L
SET_SEED     <- 20260502L
STAR_THR     <- c(`***` = 2.576, `**` = 1.960, `*` = 1.645)

# --- 1. Pre-flight ----------------------------------------------------------
if (!exists("panel_reg")) {
  rds_path <- file.path(WORKING_DIR, "panel_reg.rds")
  if (file.exists(rds_path)) {
    panel_reg <- readRDS(rds_path)
    cat("Loaded panel_reg from", rds_path, "\n")
  } else {
    stop("panel_reg not in session and panel_reg.rds not found.")
  }
}
required <- c("ActR2", "ActSkew", "MAX12", "D_SENT", "excluded_h3")
missing_cols <- setdiff(required, names(panel_reg))
if (length(missing_cols)) {
  stop("panel_reg missing required column(s): ",
       paste(missing_cols, collapse = ", "),
       ". Re-run panel_regressions_setup.R (v1.3 or later: requires ",
       "MAX12 and the excluded_h3 flag).")
}

# --- 2. Aligned estimation sample -------------------------------------------
core_rhs <- c("flow", "R_LOW", "R_MID", "R_HIGH",
              "log_TNA", "log_Age", "ExpRatio", "LoadDummy",
              "ret_vol", "Turnover", "style_flow_lag")

# v1.2: filter(!excluded_h3) restricts to the H3 / activeness subsample
# per flagged_funds.xlsx Step 8c. The Shadow Price uses lottery proxies
# (ActR2, ActSkew, MAX12) and therefore shares H3's identification scope.
samp <- panel_reg %>%
  filter(!excluded_h3) %>%
  filter(!is_december) %>%
  filter(if_all(all_of(c(core_rhs, "ActR2", "ActSkew", "MAX12", "D_SENT")),
                ~ !is.na(.))) %>%
  mutate(Ticker = as.factor(Ticker), yearmo = as.factor(yearmo))

# Winsorise ActSkew and MAX12 (consistent with H3 specification)
for (v in c("ActSkew", "MAX12")) {
  lo <- quantile(samp[[v]], 0.01, na.rm = TRUE)
  hi <- quantile(samp[[v]], 0.99, na.rm = TRUE)
  samp[[v]] <- pmin(pmax(samp[[v]], lo), hi)
}

# Standardise lottery measures so SP is comparable across measures
for (v in c("ActR2", "ActSkew", "MAX12")) {
  samp[[paste0(v, "_z")]] <- as.numeric(scale(samp[[v]]))
}

cat(sprintf("\nPPF estimation sample: %d fund-months | %d funds | %d months\n",
            nrow(samp), nlevels(droplevels(samp$Ticker)),
            nlevels(droplevels(samp$yearmo))))

# --- 3. Helpers -------------------------------------------------------------
# Index lookup robust to fixest's permutation-dependent interaction naming.
find_idx <- function(b_names, target) {
  i <- which(b_names == target)
  if (length(i) > 0L) return(i[1])
  if (grepl(":", target, fixed = TRUE)) {
    parts <- strsplit(target, ":", fixed = TRUE)[[1]]
    rev_target <- paste(rev(parts), collapse = ":")
    i <- which(b_names == rev_target)
    if (length(i) > 0L) return(i[1])
  }
  NA_integer_
}

# Generic delta method: var(f) = g' V g where g is the gradient vector.
delta_var <- function(grad_vec, V) {
  as.numeric(t(grad_vec) %*% V %*% grad_vec)
}

# SP_low and its gradient w.r.t. (eta, beta).
sp_low_val  <- function(eta, beta) -eta / beta
sp_low_grad <- function(eta, beta) c(-1/beta, eta / beta^2)

# SP_high and its gradient w.r.t. (eta, phi, beta, delta).
sp_high_val  <- function(eta, phi, beta, delta) {
  -(eta + phi) / (beta + delta)
}
sp_high_grad <- function(eta, phi, beta, delta) {
  M <- beta + delta
  c(-1/M, -1/M, (eta + phi) / M^2, (eta + phi) / M^2)
}

# Diff = SP_high - SP_low; gradient w.r.t. (eta, phi, beta, delta).
diff_val  <- function(eta, phi, beta, delta) {
  sp_high_val(eta, phi, beta, delta) - sp_low_val(eta, beta)
}
diff_grad <- function(eta, phi, beta, delta) {
  M <- beta + delta
  c(-1/M + 1/beta,                       # d/d eta
    -1/M,                                # d/d phi
    (eta + phi) / M^2 - eta / beta^2,    # d/d beta
    (eta + phi) / M^2)                   # d/d delta
}

fmt_num   <- function(x, n = 3) {
  if (is.null(x) || length(x) == 0L || !is.finite(x)) return("--")
  formatC(x, format = "f", digits = n)
}
fmt_pse <- function(est, se, n = 3) {
  if (!is.finite(est) || !is.finite(se)) return("--")
  sprintf("%s [%s]", fmt_num(est, n), fmt_num(se, n))
}
fmt_p     <- function(p) {
  if (!is.finite(p)) return("--")
  if (p < 0.001) "$<\\!0.001$" else formatC(p, format = "f", digits = 3)
}
star_for  <- function(t_stat) {
  if (!is.finite(t_stat)) return("")
  for (s in names(STAR_THR)) if (abs(t_stat) >= STAR_THR[[s]]) return(s)
  ""
}

# --- 4. Estimate one joint regression per lottery measure -------------------
controls <- c("log_TNA", "log_Age", "ExpRatio", "LoadDummy",
              "ret_vol", "Turnover", "style_flow_lag")
ctrl_str <- paste(controls, collapse = " + ")

run_joint <- function(lottery_z) {
  rhs <- paste(
    "R_LOW + R_MID + R_HIGH +", lottery_z, "+ D_SENT",
    "+ R_LOW:D_SENT + R_MID:D_SENT + R_HIGH:D_SENT",
    "+", lottery_z, ":D_SENT",
    "+", ctrl_str
  )
  feols(as.formula(paste("flow ~", rhs, "| Ticker")),
        data = samp, cluster = ~ Ticker + yearmo)
}

cat("\nEstimating joint H1+H3 regressions (one per lottery measure)...\n")
mod_actr2   <- run_joint("ActR2_z")
cat(sprintf("  ActR2:   N=%s\n", formatC(nobs(mod_actr2), big.mark=",")))
mod_actskew <- run_joint("ActSkew_z")
cat(sprintf("  ActSkew: N=%s\n", formatC(nobs(mod_actskew), big.mark=",")))
mod_max12   <- run_joint("MAX12_z")
cat(sprintf("  MAX12:   N=%s\n", formatC(nobs(mod_max12), big.mark=",")))

# --- 5. Compute SPs from each joint regression ------------------------------
compute_sps <- function(mod, lottery_z_name) {
  b <- coef(mod); V <- vcov(mod); bn <- names(b)
  results <- list()
  for (q in c("LOW", "MID", "HIGH")) {
    R_q     <- paste0("R_", q)
    R_q_int <- paste0(R_q, ":D_SENT")
    eta_idx   <- find_idx(bn, lottery_z_name)
    phi_idx   <- find_idx(bn, paste0(lottery_z_name, ":D_SENT"))
    beta_idx  <- find_idx(bn, R_q)
    delta_idx <- find_idx(bn, R_q_int)
    if (any(is.na(c(eta_idx, phi_idx, beta_idx, delta_idx)))) {
      results[[q]] <- list(segment = q, SP_low = NA, SE_low = NA,
                           SP_high = NA, SE_high = NA,
                           Diff = NA, SE_diff = NA, p_diff = NA,
                           eta = NA, phi = NA, beta = NA, delta = NA)
      next
    }
    eta   <- as.numeric(b[eta_idx])
    phi   <- as.numeric(b[phi_idx])
    beta  <- as.numeric(b[beta_idx])
    delta <- as.numeric(b[delta_idx])

    SP_low_est  <- sp_low_val(eta, beta)
    SP_high_est <- sp_high_val(eta, phi, beta, delta)
    Diff_est    <- diff_val(eta, phi, beta, delta)

    # SE via delta method
    V_low <- V[c(eta_idx, beta_idx), c(eta_idx, beta_idx)]
    SE_low  <- sqrt(delta_var(sp_low_grad(eta, beta), V_low))

    V_full_idx <- c(eta_idx, phi_idx, beta_idx, delta_idx)
    V_full <- V[V_full_idx, V_full_idx]
    SE_high <- sqrt(delta_var(sp_high_grad(eta, phi, beta, delta), V_full))
    SE_diff <- sqrt(delta_var(diff_grad(eta, phi, beta, delta), V_full))

    # 1-sided test: prediction is Diff < 0 (more negative SP under high sent.
    # = larger psychological premium, given both SPs are negative when
    # investors chase both rank and lottery).
    z_diff <- if (is.finite(SE_diff) && SE_diff > 0) Diff_est / SE_diff else NA
    p_diff <- if (is.finite(z_diff)) pnorm(z_diff, lower.tail = TRUE) else NA

    results[[q]] <- list(
      segment = q,
      SP_low  = SP_low_est,  SE_low  = SE_low,
      SP_high = SP_high_est, SE_high = SE_high,
      Diff    = Diff_est,    SE_diff = SE_diff,
      z_diff  = z_diff,      p_diff  = p_diff,
      eta = eta, phi = phi, beta = beta, delta = delta
    )
  }
  results
}

cat("\nComputing shadow prices via delta method...\n")
sp_actr2   <- compute_sps(mod_actr2,   "ActR2_z")
sp_actskew <- compute_sps(mod_actskew, "ActSkew_z")
sp_max12   <- compute_sps(mod_max12,   "MAX12_z")

# --- 6. Optional parametric bootstrap ---------------------------------------
boot_results <- NULL
if (DO_BOOTSTRAP) {
  cat(sprintf("\nParametric MVN bootstrap (B=%d)...\n", B_BOOT))
  set.seed(SET_SEED)
  mods <- list(ActR2 = mod_actr2, ActSkew = mod_actskew, MAX12 = mod_max12)
  lots <- c(ActR2 = "ActR2_z", ActSkew = "ActSkew_z", MAX12 = "MAX12_z")
  boot_results <- list()
  for (lname in names(mods)) {
    m <- mods[[lname]]
    b <- coef(m); V <- vcov(m); bn <- names(b)
    draws <- MASS::mvrnorm(n = B_BOOT, mu = b, Sigma = V)
    # Indices once
    eta_i <- find_idx(bn, lots[[lname]])
    phi_i <- find_idx(bn, paste0(lots[[lname]], ":D_SENT"))
    seg_results <- list()
    for (q in c("LOW", "MID", "HIGH")) {
      beta_i  <- find_idx(bn, paste0("R_", q))
      delta_i <- find_idx(bn, paste0("R_", q, ":D_SENT"))
      if (any(is.na(c(eta_i, phi_i, beta_i, delta_i)))) next
      sp_low_draws  <- -draws[, eta_i] / draws[, beta_i]
      sp_high_draws <- -(draws[, eta_i] + draws[, phi_i]) /
                       (draws[, beta_i] + draws[, delta_i])
      diff_draws    <- sp_high_draws - sp_low_draws
      seg_results[[q]] <- list(
        SP_low_ci  = quantile(sp_low_draws,  c(0.025, 0.975), na.rm = TRUE),
        SP_high_ci = quantile(sp_high_draws, c(0.025, 0.975), na.rm = TRUE),
        Diff_ci    = quantile(diff_draws,    c(0.025, 0.975), na.rm = TRUE),
        Diff_p     = mean(diff_draws >= 0, na.rm = TRUE)  # 1-sided p for Diff<0
      )
    }
    boot_results[[lname]] <- seg_results
  }
  cat("  Bootstrap complete.\n")
}

# --- 7. Build summary table -------------------------------------------------
seg_label <- c(LOW = "$R^{LOW}$", MID = "$R^{MID}$", HIGH = "$R^{HIGH}$")
all_sps <- list(ActR2 = sp_actr2, ActSkew = sp_actskew, MAX12 = sp_max12)

rows <- list()
for (lname in names(all_sps)) {
  for (q in c("LOW", "MID", "HIGH")) {
    r <- all_sps[[lname]][[q]]
    rows[[length(rows) + 1L]] <- c(
      lname,
      seg_label[[q]],
      fmt_pse(r$SP_low,  r$SE_low),
      fmt_pse(r$SP_high, r$SE_high),
      fmt_pse(r$Diff,    r$SE_diff),
      paste0(fmt_p(r$p_diff), star_for(r$z_diff))
    )
  }
}
ppf_df <- as.data.frame(do.call(rbind, rows), stringsAsFactors = FALSE)
colnames(ppf_df) <- c("Lottery", "Segment",
                      "SP$_{\\text{low}}$ [SE]",
                      "SP$_{\\text{high}}$ [SE]",
                      "Diff [SE]",
                      "$p$ (1-sided)")
rownames(ppf_df) <- NULL

caption <- paste0(
  "Psychological Premium (Shadow Price) Estimates. Marginal rate of ",
  "substitution between performance rank ($R$) and the lottery proxy ",
  "($\\text{AS}$) implied by the flow indifference condition ",
  "$dF=0$, computed at three points along the Sirri--Tufano piecewise ",
  "rank distribution and under both sentiment regimes (low: $D^{SENT}=0$; ",
  "high: $D^{SENT}=1$). Negative entries indicate that investors trade ",
  "off rank for lottery exposure; larger absolute values correspond to a ",
  "larger psychological premium. The proposal's prediction is that ",
  "$|\\text{SP}_{\\text{high}}|>|\\text{SP}_{\\text{low}}|$ for the ",
  "behavioural-channel proxies (ActSkew, MAX12), with no such gap for ",
  "the rational-channel proxy (ActR2). The 1-sided $p$ tests ",
  "$\\text{Diff}=\\text{SP}_{\\text{high}}-\\text{SP}_{\\text{low}}<0$."
)
fn <- paste0(
  "Each row is computed from a separate joint regression of the form ",
  "$\\\\text{flow}=\\\\beta_q R^q + \\\\delta_q R^q D^{SENT} + ",
  "\\\\eta\\\\,\\\\text{AS}^z + \\\\phi\\\\,\\\\text{AS}^z D^{SENT} + ",
  "\\\\text{controls} + \\\\mu_i$, with $\\\\text{AS}^z$ standardised within ",
  "sample. Shadow Price units are rank points per standard deviation of ",
  "the lottery measure. Standard errors via the delta method using the ",
  "joint coefficient covariance from the underlying regression; clustered ",
  "two-way on Ticker and yearmo. ",
  "Stars: $^{*}\\\\,p<0.10$, $^{**}\\\\,p<0.05$, $^{***}\\\\,p<0.01$. ",
  "\\\\smallskip ",
  "\\\\textit{Conversion to alpha basis points.} To express the premium ",
  "in welfare-relevant units, the rank-to-alpha mapping is calibrated ",
  "using the equal-weighted Carhart four-factor alpha spread between the ",
  "top and bottom momentum quintiles of the Active panel (Table~12, ",
  "$Q5-Q1=2.23\\\\%$ p.a.). With a midpoint-to-midpoint rank distance of ",
  "$0.8$, this implies approximately 279 basis points of annual Carhart ",
  "alpha per rank-point. Applying this conversion at the middle rank ",
  "segment, the MAX12 premium is approximately 14 basis points of annual ",
  "alpha per standard deviation at low-sentiment regimes ",
  "($|\\\\text{SP}_{\\\\text{low}}|\\\\times 279\\\\approx 0.049\\\\times 279$) ",
  "and approximately 58 basis points per standard deviation at ",
  "high-sentiment regimes ",
  "($|\\\\text{SP}_{\\\\text{high}}|\\\\times 279\\\\approx 0.208\\\\times 279$). ",
  "The conversion uses Jegadeesh--Titman momentum sorts as the closest ",
  "available rank-alpha mapping in the dissertation; the ",
  "within-Lipper-category mapping that drives the H1--H4 panel ",
  "regressions is conceptually similar but slightly attenuated, so the ",
  "quoted basis-point figures should be read as orders of magnitude ",
  "rather than precise estimates. ",
  "\\\\smallskip ",
  "Sample: actively managed funds, \\\\textcite{Evans2010}-corrected panel, ",
  "no date cap; H3 / activeness subsample per flagged\\\\_funds.xlsx."
)

ktab <- kbl(
  ppf_df, format = "latex", booktabs = TRUE,
  caption = caption, label = "psychological_premium",
  align = c("l", "l", "r", "r", "r", "r"),
  escape = FALSE, linesep = ""
) %>%
  kable_styling(latex_options = c("hold_position", "scale_down")) %>%
  footnote(general = fn, general_title = "",
           threeparttable = TRUE, escape = FALSE) %>%
  row_spec(c(3, 6), hline_after = TRUE)

tex <- as.character(ktab)
tex <- gsub("\\begin{table}[!h]", "\\begin{table}[H]", tex, fixed = TRUE)
tex <- threeparttable_note_after(tex)  # PHASE B: move note outside float
writeLines(tex, OUTPUT_TEX)
cat(sprintf("Wrote %s\n", OUTPUT_TEX))

# --- 8. Console echo --------------------------------------------------------
cat("\n========================== Shadow Price Summary =========================\n")
print(ppf_df, row.names = FALSE)
cat("==========================================================================\n")

if (DO_BOOTSTRAP && !is.null(boot_results)) {
  cat("\nBootstrap 95% confidence intervals on Diff (SP_high - SP_low):\n")
  for (lname in names(boot_results)) {
    cat(sprintf("  %s:\n", lname))
    for (q in c("LOW", "MID", "HIGH")) {
      bb <- boot_results[[lname]][[q]]
      if (is.null(bb)) next
      cat(sprintf("    %-8s Diff in [%6.3f, %6.3f]  bootstrap p(Diff>=0)=%.3f\n",
                  q, bb$Diff_ci[1], bb$Diff_ci[2], bb$Diff_p))
    }
  }
}

PPF_results <- list(
  ActR2 = sp_actr2, ActSkew = sp_actskew, MAX12 = sp_max12,
  bootstrap = boot_results, table = ppf_df
)
assign("PPF_results", PPF_results, envir = .GlobalEnv)
saveRDS(PPF_results, file.path(WORKING_DIR, "PPF_results.rds"))
cat("Saved PPF_results.rds\n")
