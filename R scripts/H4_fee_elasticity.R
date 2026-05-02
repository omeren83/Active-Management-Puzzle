# H4_fee_elasticity.R                                                  v1.0
# =============================================================================
# Tests H4 (Fee Elasticity / Berk-Green) under two specifications and writes
# two .tex tables. Aligned-sample three-spec pattern of H1-H3 is RELAXED for
# H4 because the focal predictor (ExpRatio) is time-invariant within fund:
# fund FE perfectly absorbs ExpRatio main effects, leaving only the
# interactions identified. The PRIMARY spec therefore uses Lipper x yearmo
# FE (which absorbs only style-month aggregates and leaves cross-fund
# expense-ratio variation intact). Lagged-sentiment timing robustness is
# omitted by default: lagging ExpRatio is meaningless and the sentiment-
# timing question is already answered by H1's lagged spec. To re-enable,
# set PRODUCE_LAGGED <- TRUE below.
#
# H4 hypothesis (Berk-Green 2004 vs behavioral alternative): under the
# rational Berk-Green equilibrium investors should chase performance
# regardless of fee, so the interaction ExpRatio x SENT should be zero.
# Behavioral alternative: high-fee funds attract relatively more sentiment-
# driven flows than low-fee funds, in particular at the loser tail
# (psychological premium / shadow-price story).
#
# Model:
#   flow_{i,t} = alpha + sum_q beta_q * R^q_{i,t}
#                      + theta * ExpRatio_i
#                      + gamma * STATE_t
#                      + sum_q kappa_q * R^q * STATE          [H1 channel]
#                      + sum_q phi_q   * R^q * ExpRatio       [fee-modulated convexity]
#                      + delta_F * ExpRatio * STATE           [HEADLINE]
#                      + sum_q delta^F_q * R^q * ExpRatio * STATE  [HEADLINE]
#                      + controls + FE + e
#
# Joint H4 test: chi^2(4) on (delta_F, delta^F_LOW, delta^F_MID, delta^F_HIGH)
# -- "fee elasticity is amplified by sentiment, somewhere along the rank
# distribution."
#
# Tables produced:
#   (1) PRIMARY                  -> table_H4_regression.tex
#       Contemporaneous + Lipper x yearmo FE.   Main body Section 5.5.
#   (2) FE ROBUSTNESS            -> table_H4_robustness.tex
#       Contemporaneous + yearmo-only FE (between-style identification).
#       Appendix F.2.
# =============================================================================

suppressPackageStartupMessages({
  library(dplyr)
  library(fixest)
  library(kableExtra)
})

# --- 0. Config ---------------------------------------------------------------
if (!exists("WORKING_DIR")) WORKING_DIR <- getwd()
STAR_THR <- c(`***` = 2.576, `**` = 1.960, `*` = 1.645)

PRODUCE_LAGGED <- FALSE   # set TRUE to also produce table_H4_lagged.tex

OUTPUT_PRIMARY <- file.path(WORKING_DIR, "table_H4_regression.tex")
OUTPUT_LAGGED  <- file.path(WORKING_DIR, "table_H4_lagged.tex")
OUTPUT_ROBUST  <- file.path(WORKING_DIR, "table_H4_robustness.tex")

# --- 1. Pre-flight -----------------------------------------------------------
if (!exists("panel_reg")) {
  rds_path <- file.path(WORKING_DIR, "panel_reg.rds")
  if (file.exists(rds_path)) {
    panel_reg <- readRDS(rds_path)
    cat("Loaded panel_reg from", rds_path, "\n")
  } else {
    stop("panel_reg not in session and panel_reg.rds not found in ",
         WORKING_DIR, ". Run Phase J setup first.")
  }
}
required <- c("ExpRatio", "D_SENT_lag", "D_AAII_lag", "SENT_ORTH_lag")
missing_cols <- setdiff(required, names(panel_reg))
if (length(missing_cols)) {
  stop("panel_reg missing required column(s): ",
       paste(missing_cols, collapse = ", "),
       ". Re-run panel_regressions_setup.R.")
}

# --- 2. Aligned estimation sample --------------------------------------------
core_rhs <- c("flow", "R_LOW", "R_MID", "R_HIGH",
              "log_TNA", "log_Age", "ExpRatio", "LoadDummy",
              "ret_vol", "Turnover", "style_flow_lag")

# Both contemporaneous and lagged state variables required non-NA so that
# the lagged spec (if produced) uses the same sample.
samp <- panel_reg %>%
  filter(!is_december) %>%
  filter(if_all(all_of(c(core_rhs,
                         "D_SENT", "D_AAII", "SENT_ORTH",
                         "D_SENT_lag", "D_AAII_lag", "SENT_ORTH_lag")),
                ~ !is.na(.))) %>%
  mutate(
    SENT_ORTH_z     = as.numeric(scale(SENT_ORTH)),
    SENT_ORTH_lag_z = as.numeric(scale(SENT_ORTH_lag)),
    Ticker          = as.factor(Ticker),
    yearmo          = as.factor(yearmo),
    Lipper_Category = as.factor(Lipper_Category)
  )

# Defensive: zero-fee or negative-fee funds (data errors / passive index
# share classes that slipped through the active filter) would invalidate
# the linearity assumption. Drop them with a small warning.
zero_fee <- sum(samp$ExpRatio <= 0, na.rm = TRUE)
if (zero_fee > 0) {
  cat(sprintf("Note: dropping %d obs with ExpRatio <= 0.\n", zero_fee))
  samp <- samp %>% filter(ExpRatio > 0)
}

cat(sprintf(
  "\nAligned H4 estimation sample: %d fund-months | %d funds | %d months | %d styles\n",
  nrow(samp), nlevels(droplevels(samp$Ticker)),
  nlevels(droplevels(samp$yearmo)),
  nlevels(droplevels(samp$Lipper_Category))
))

# --- 3. Helpers --------------------------------------------------------------
fmt_num <- function(x, n = 4) {
  if (is.na(x)) return("")
  formatC(x, format = "f", digits = n, big.mark = "")
}
add_stars <- function(coef_str, t) {
  if (is.na(t) || coef_str == "") return(coef_str)
  for (s in names(STAR_THR)) {
    if (abs(t) >= STAR_THR[[s]]) return(paste0(coef_str, s))
  }
  coef_str
}
fmt_p <- function(p) {
  if (is.na(p)) return("")
  if (p < 0.001) "$<\\!0.001$" else formatC(p, format = "f", digits = 3)
}

# Locate a coef name accounting for fixest's deterministic but order-
# dependent storage of multi-way interactions. We try every permutation
# of the colon-separated parts.
find_coef <- function(b_names, parts) {
  perms <- function(x) {
    if (length(x) <= 1L) return(list(x))
    out <- list()
    for (i in seq_along(x)) {
      rest <- perms(x[-i])
      for (r in rest) out[[length(out) + 1L]] <- c(x[i], r)
    }
    out
  }
  candidates <- vapply(perms(parts), paste, character(1), collapse = ":")
  idx <- which(b_names %in% candidates)
  if (length(idx) == 0L) return(NA_integer_)
  idx[1]
}

# Joint test on (delta_F, delta^F_LOW, delta^F_MID, delta^F_HIGH) -- chi^2(4)
# plus a 1-sided z on the headline delta_F (ExpRatio x STATE).
test_h4 <- function(m, state_label) {
  b <- coef(m); V <- vcov(m); bn <- names(b)

  i_es     <- find_coef(bn, c("ExpRatio", state_label))
  i_low_t  <- find_coef(bn, c("R_LOW",  "ExpRatio", state_label))
  i_mid_t  <- find_coef(bn, c("R_MID",  "ExpRatio", state_label))
  i_high_t <- find_coef(bn, c("R_HIGH", "ExpRatio", state_label))

  pos <- c(i_es, i_low_t, i_mid_t, i_high_t)
  pos_clean <- pos[!is.na(pos)]
  if (length(pos_clean) < 1L) {
    return(list(joint_chi2 = NA, joint_p = NA, df = 0,
                delta_b = NA, delta_z = NA, delta_p_1sd = NA))
  }

  bs <- b[pos_clean]; Vs <- V[pos_clean, pos_clean, drop = FALSE]
  chi2 <- as.numeric(t(bs) %*% solve(Vs) %*% bs)
  p_joint <- pchisq(chi2, df = length(pos_clean), lower.tail = FALSE)

  if (!is.na(i_es)) {
    delta_b   <- as.numeric(b[i_es])
    delta_se  <- sqrt(as.numeric(V[i_es, i_es]))
    z_delta   <- delta_b / delta_se
    p_delta_1sd <- pnorm(z_delta, lower.tail = FALSE)
  } else {
    delta_b <- NA; z_delta <- NA; p_delta_1sd <- NA
  }

  list(joint_chi2 = chi2, joint_p = p_joint, df = length(pos_clean),
       delta_b = delta_b, delta_z = z_delta, delta_p_1sd = p_delta_1sd)
}

get_coef <- function(mod, parts) {
  if (length(parts) == 0L || any(is.na(parts))) {
    return(c(coef = "", t = ""))
  }
  ct <- tryCatch(fixest::coeftable(mod), error = function(e) NULL)
  if (is.null(ct)) return(c(coef = "", t = ""))
  rn <- rownames(ct)
  i <- find_coef(rn, parts)
  if (is.na(i)) return(c(coef = "", t = ""))
  est    <- as.numeric(ct[i, "Estimate"])
  se_val <- as.numeric(ct[i, "Std. Error"])
  if (is.na(est) || is.na(se_val) || se_val == 0) return(c(coef = "", t = ""))
  tstat <- est / se_val
  c(coef = add_stars(fmt_num(est, 4), tstat),
    t    = paste0("(", formatC(tstat, format = "f", digits = 2), ")"))
}

# --- 4. Master table builder -------------------------------------------------
# Always 4 columns (Baseline + D_SENT + SENT_ORTH cont + D_AAII). The full
# triple-interaction is included in cols 2-4; only the focal coefficients
# (R_q main, ExpRatio main, ExpRatio x STATE, R_q x ExpRatio x STATE) are
# displayed. Lower-order R_q x STATE and R_q x ExpRatio are estimated for
# valid triple-interaction inference and suppressed for table clarity.
build_h4_table <- function(samp, fe_string,
                           d_sent_var, sent_cont_var, d_aaii_var,
                           output_path, table_label, caption, footnote_text,
                           col_headers, spec_id) {
  controls <- c("log_TNA", "log_Age", "LoadDummy",
                "ret_vol", "Turnover", "style_flow_lag")
  ctrl_str <- paste(controls, collapse = " + ")

  mk_formula <- function(state_var = NULL) {
    base <- "R_LOW + R_MID + R_HIGH + ExpRatio"
    if (is.null(state_var)) {
      return(as.formula(paste("flow ~", base, "+", ctrl_str, "|", fe_string)))
    }
    s <- state_var
    rq_state     <- paste0("R_LOW:", s, " + R_MID:", s, " + R_HIGH:", s)
    rq_exp       <- "R_LOW:ExpRatio + R_MID:ExpRatio + R_HIGH:ExpRatio"
    exp_state    <- paste0("ExpRatio:", s)
    rq_exp_state <- paste0("R_LOW:ExpRatio:", s,
                           " + R_MID:ExpRatio:", s,
                           " + R_HIGH:ExpRatio:", s)
    rhs <- paste(base, "+", s, "+", rq_state, "+", rq_exp, "+",
                 exp_state, "+", rq_exp_state, "+", ctrl_str)
    as.formula(paste("flow ~", rhs, "|", fe_string))
  }

  cat(sprintf("\n[%s] Estimating 4 specifications...\n", spec_id))
  m1 <- feols(mk_formula(NULL),          data = samp, cluster = ~ Ticker + yearmo)
  m2 <- feols(mk_formula(d_sent_var),    data = samp, cluster = ~ Ticker + yearmo)
  m3 <- feols(mk_formula(sent_cont_var), data = samp, cluster = ~ Ticker + yearmo)
  m4 <- feols(mk_formula(d_aaii_var),    data = samp, cluster = ~ Ticker + yearmo)

  t2 <- test_h4(m2, d_sent_var)
  t3 <- test_h4(m3, sent_cont_var)
  t4 <- test_h4(m4, d_aaii_var)

  cat(sprintf(
    "  D_SENT     joint chi2(%d)=%5.2f (p=%.4f) | delta_F z=%+.2f (p=%.4f)\n",
    t2$df, t2$joint_chi2, t2$joint_p, t2$delta_z, t2$delta_p_1sd))
  cat(sprintf(
    "  SENT cont. joint chi2(%d)=%5.2f (p=%.4f) | delta_F z=%+.2f (p=%.4f)\n",
    t3$df, t3$joint_chi2, t3$joint_p, t3$delta_z, t3$delta_p_1sd))
  cat(sprintf(
    "  D_AAII     joint chi2(%d)=%5.2f (p=%.4f) | delta_F z=%+.2f (p=%.4f)\n",
    t4$df, t4$joint_chi2, t4$joint_p, t4$delta_z, t4$delta_p_1sd))

  # Display rows. "STATE" placeholder is resolved per column via state_var.
  # Lower-order R_q x STATE and R_q x ExpRatio are included in the
  # regression (for valid triple-interaction inference) but suppressed
  # from the table to keep height manageable.
  row_specs <- list(
    list(parts = "R_LOW",   label = "$R^{LOW}$"),
    list(parts = "R_MID",   label = "$R^{MID}$"),
    list(parts = "R_HIGH",  label = "$R^{HIGH}$"),
    list(parts = "ExpRatio", label = "Expense ratio"),
    list(parts = "EXP_STATE",
         label = "Exp.\\ ratio $\\times$ Sent."),
    list(parts = "R_LOW_EXP_STATE",
         label = "$R^{LOW}\\times$ Exp.\\ $\\times$ Sent."),
    list(parts = "R_MID_EXP_STATE",
         label = "$R^{MID}\\times$ Exp.\\ $\\times$ Sent."),
    list(parts = "R_HIGH_EXP_STATE",
         label = "$R^{HIGH}\\times$ Exp.\\ $\\times$ Sent."),
    list(parts = "log_TNA",  label = "$\\log(\\text{TNA})$"),
    list(parts = "log_Age",  label = "$\\log(\\text{Age})$"),
    list(parts = "LoadDummy", label = "Load dummy"),
    list(parts = "ret_vol",  label = "Return volatility"),
    list(parts = "Turnover", label = "Turnover"),
    list(parts = "style_flow_lag", label = "Style flow")
  )

  state_var <- list(m1 = NULL, m2 = d_sent_var,
                    m3 = sent_cont_var, m4 = d_aaii_var)
  resolve_parts <- function(token, sv) {
    if (token == "EXP_STATE") {
      if (is.null(sv)) return(NA_character_)
      return(c("ExpRatio", sv))
    }
    if (token == "R_LOW_EXP_STATE") {
      if (is.null(sv)) return(NA_character_)
      return(c("R_LOW", "ExpRatio", sv))
    }
    if (token == "R_MID_EXP_STATE") {
      if (is.null(sv)) return(NA_character_)
      return(c("R_MID", "ExpRatio", sv))
    }
    if (token == "R_HIGH_EXP_STATE") {
      if (is.null(sv)) return(NA_character_)
      return(c("R_HIGH", "ExpRatio", sv))
    }
    token
  }

  mods <- list(m1 = m1, m2 = m2, m3 = m3, m4 = m4)
  mod_keys <- c("m1", "m2", "m3", "m4")
  body_rows <- list()
  for (spec in row_specs) {
    vals <- vector("list", length(mod_keys))
    for (j in seq_along(mod_keys)) {
      sv <- state_var[[mod_keys[j]]]
      parts <- resolve_parts(spec$parts, sv)
      vals[[j]] <- get_coef(mods[[j]], parts)
    }
    coef_row <- c(spec$label, sapply(vals, function(v) v["coef"]))
    t_row    <- c("",         sapply(vals, function(v) v["t"]))
    body_rows[[length(body_rows) + 1]] <- coef_row
    body_rows[[length(body_rows) + 1]] <- t_row
  }
  body_df <- do.call(rbind, body_rows)
  colnames(body_df) <- c("Variable", "(1)", "(2)", "(3)", "(4)")
  rownames(body_df) <- NULL

  # FE label depends on fe_string. Lipper^yearmo for primary,
  # yearmo-only for FE-robust.
  fe_label <- if (grepl("Lipper", fe_string, fixed = TRUE)) {
    "FE: Lipper $\\times$ yearmo"
  } else {
    "FE: yearmo"
  }
  fe_row    <- c(fe_label, rep("Yes", 4))
  clust_row <- c("Cluster (Ticker, yearmo)", rep("Yes", 4))
  n_row     <- c("$N$",
                 formatC(nobs(m1), format="d", big.mark=","),
                 formatC(nobs(m2), format="d", big.mark=","),
                 formatC(nobs(m3), format="d", big.mark=","),
                 formatC(nobs(m4), format="d", big.mark=","))
  r2_row    <- c("$R^2$ (within)",
                 fmt_num(r2(m1, "wr2"), 3), fmt_num(r2(m2, "wr2"), 3),
                 fmt_num(r2(m3, "wr2"), 3), fmt_num(r2(m4, "wr2"), 3))
  joint_row <- c("Joint test ($\\chi^2_4$, $p$)",
                 "--",
                 sprintf("%.2f (%s)", t2$joint_chi2, fmt_p(t2$joint_p)),
                 sprintf("%.2f (%s)", t3$joint_chi2, fmt_p(t3$joint_p)),
                 sprintf("%.2f (%s)", t4$joint_chi2, fmt_p(t4$joint_p)))
  delta_row <- c("Headline ($z$, 1-sided $p$)",
                 "--",
                 sprintf("z=%+.2f (p=%s)", t2$delta_z, fmt_p(t2$delta_p_1sd)),
                 sprintf("z=%+.2f (p=%s)", t3$delta_z, fmt_p(t3$delta_p_1sd)),
                 sprintf("z=%+.2f (p=%s)", t4$delta_z, fmt_p(t4$delta_p_1sd)))
  footer_df <- rbind(fe_row, clust_row, n_row, r2_row, joint_row, delta_row)
  colnames(footer_df) <- c("Variable","(1)","(2)","(3)","(4)")
  rownames(footer_df) <- NULL

  full_df <- rbind(body_df, footer_df)
  rownames(full_df) <- NULL

  ktab <- kbl(
    full_df, format = "latex", booktabs = TRUE,
    caption = caption, label = table_label,
    align = c("l", rep("r", 4)), escape = FALSE, linesep = ""
  ) %>%
    kable_styling(latex_options = c("hold_position", "scale_down")) %>%
    add_header_above(col_headers, escape = FALSE) %>%
    row_spec(nrow(body_df), hline_after = TRUE) %>%
    footnote(general = footnote_text, general_title = "",
             threeparttable = TRUE, escape = FALSE)

  tex <- as.character(ktab)
  tex <- gsub("\\begin{table}[!h]", "\\begin{table}[H]", tex, fixed = TRUE)
  writeLines(tex, output_path)
  cat(sprintf("Wrote %s\n", output_path))

  list(m1 = m1, m2 = m2, m3 = m3, m4 = m4,
       tests = list(t2 = t2, t3 = t3, t4 = t4))
}

# --- 5. Captions and footnotes -----------------------------------------------
cap_primary <- paste0(
  "H4: Fee Elasticity Hypothesis. Panel regression of monthly proportional ",
  "fund flows on within-Lipper-category lagged 12-month performance-rank ",
  "segments and cross-fund expense ratios, with full triple interactions on ",
  "contemporaneous sentiment regime indicators (Baker-Wurgler 2007 timing). ",
  "Under the Berk \\& Green (2004) rational equilibrium, sentiment-driven ",
  "flow should not depend on fee level: $\\delta_F = \\delta^F_q = 0$. The ",
  "behavioral alternative predicts $\\delta_F > 0$ (sentiment amplifies ",
  "fee tolerance) or $\\delta^F_1 > 0$ (the loser tail is most fee-elastic ",
  "under high sentiment)."
)
fn_primary <- paste0(
  "The dependent variable is the Sirri-Tufano (1998) winsorised proportional ",
  "fund flow (decimal). $t$-statistics in parentheses below each coefficient. ",
  "Lower-order interactions $R^q\\\\times$ Sent.\\\\ and $R^q\\\\times$ Exp.\\\\ ",
  "ratio are included in the regression for valid triple-interaction ",
  "inference but suppressed from the table; $R^q\\\\times$ Sent.\\\\ ",
  "estimates are reported in Table~\\\\ref{tab:H1_regression}. Sentiment ",
  "regimes follow Baker-Wurgler (2007) construction: $D^{SENT}_t$ is the ",
  "top-34\\\\% indicator for orthogonalised sentiment; column (3) substitutes ",
  "the standardised continuous index; column (4) substitutes the AAII bull-",
  "bear regime dummy. ExpRatio is the time-invariant fund expense ratio ",
  "(decimal). Lipper-category $\\\\times$ yearmo fixed effects absorb both ",
  "sentiment main effects and any style-month aggregate flow shocks; cross-",
  "fund expense-ratio variation within each (style, month) cell identifies ",
  "$\\\\theta$ and the fee interactions. Standard errors two-way clustered on ",
  "Ticker and calendar month (Petersen 2009). The joint $\\\\chi^2_4$ test ",
  "covers all four fee-amplified-sentiment terms ($\\\\delta_F$ and the three ",
  "$\\\\delta^F_q$). ",
  "Stars: $^{*}\\\\,p<0.10$, $^{**}\\\\,p<0.05$, $^{***}\\\\,p<0.01$."
)

cap_lagged <- paste0(
  "H4 Robustness --- Lagged Sentiment. Same specification as ",
  "Table~\\ref{tab:H4_regression}, except sentiment is measured at $t-1$ ",
  "rather than $t$ (Huang et al.\\ 2015 timing convention). ExpRatio is ",
  "left in levels (it is time-invariant within fund, so lagging is ",
  "meaningless)."
)
fn_lagged <- paste0(
  "Same sample, dependent variable, controls, identification strategy, ",
  "and FE structure as Table~\\\\ref{tab:H4_regression}. State variables in ",
  "columns (2)--(4) are lagged one period. ExpRatio enters at $t$ (= $t-1$ ",
  "= constant). Standard errors two-way clustered on Ticker and calendar ",
  "month (Petersen 2009). ",
  "Stars: $^{*}\\\\,p<0.10$, $^{**}\\\\,p<0.05$, $^{***}\\\\,p<0.01$."
)

cap_robust <- paste0(
  "H4 Robustness --- Yearmo-only Fixed Effects. Same contemporaneous-",
  "sentiment specification as Table~\\ref{tab:H4_regression}, but with ",
  "yearmo fixed effects in place of Lipper-category $\\times$ yearmo. ",
  "Identification of expense-ratio slopes now includes between-style ",
  "variation, providing a cross-check against unobserved style-level ",
  "confounders (e.g.\\ growth funds systematically charging higher fees and ",
  "attracting different flows)."
)
fn_robust <- paste0(
  "Same sample, dependent variable, and rank construction as ",
  "Table~\\\\ref{tab:H4_regression}. Yearmo fixed effects absorb all ",
  "time-aggregate variation (including sentiment main effects, market-wide ",
  "flow waves, and risk-on/risk-off cycles) but leave both within- and ",
  "between-style cross-fund variation in expense ratios available for ",
  "identification. Standard errors two-way clustered on Ticker and ",
  "calendar month (Petersen 2009). The joint $\\\\chi^2_4$ test covers all ",
  "four fee-amplified-sentiment terms. ",
  "Stars: $^{*}\\\\,p<0.10$, $^{**}\\\\,p<0.05$, $^{***}\\\\,p<0.01$."
)

hdr_lagged <- c(" " = 1, "Baseline" = 1, "$D^{SENT}_{t-1}$" = 1,
                "$\\\\text{SENT}^\\\\perp_{t-1}$" = 1, "$D^{AAII}_{t-1}$" = 1)
hdr_cont   <- c(" " = 1, "Baseline" = 1, "$D^{SENT}$" = 1,
                "$\\\\text{SENT}^\\\\perp$" = 1, "$D^{AAII}$" = 1)

# --- 6. Estimate specifications ---------------------------------------------
H4_primary <- build_h4_table(
  samp, "Lipper_Category^yearmo",
  "D_SENT", "SENT_ORTH_z", "D_AAII",
  OUTPUT_PRIMARY, "H4_regression", cap_primary, fn_primary,
  col_headers = hdr_cont, spec_id = "PRIMARY"
)
H4_robust <- build_h4_table(
  samp, "yearmo",
  "D_SENT", "SENT_ORTH_z", "D_AAII",
  OUTPUT_ROBUST, "H4_robustness", cap_robust, fn_robust,
  col_headers = hdr_cont, spec_id = "FE ROBUSTNESS"
)
H4_lagged <- NULL
if (PRODUCE_LAGGED) {
  H4_lagged <- build_h4_table(
    samp, "Lipper_Category^yearmo",
    "D_SENT_lag", "SENT_ORTH_lag_z", "D_AAII_lag",
    OUTPUT_LAGGED, "H4_lagged", cap_lagged, fn_lagged,
    col_headers = hdr_lagged, spec_id = "TIMING ROBUSTNESS"
  )
}

H4_models <- list(primary = H4_primary, lagged = H4_lagged, robust = H4_robust)
assign("H4_models", H4_models, envir = .GlobalEnv)

# Persist for the reporting script.
saveRDS(H4_models, file.path(WORKING_DIR, "H4_models.rds"))
cat("Saved H4_models.rds\n")
