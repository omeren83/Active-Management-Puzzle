# H3_lottery_demand.R                                                  v2.1
# =============================================================================
# v2.1 changes vs v2.0 (Family E pre-defense audit):
#   - filter(!excluded_h3) added to the panel-prep stage. Per
#     data_import_and_cleaning.R Step 8c (flagged_funds.xlsx ledger), H3
#     lottery-demand identification requires the !excluded_h3 subsample so
#     that Equity Income / Specialty Diversified / Specialty-Miscellaneous
#     funds (Lipper categories without unambiguous size-style benchmarks),
#     sector funds, and covered-call overlays do not contaminate the
#     activeness/lottery proxies. Mirrors the convention already applied in
#     activeness_analysis.R v1.2 and activeness_persistence.R v1.1.
#     Requires panel_regressions_setup.R v1.3+, which preserves the
#     excluded_h3 flag column in panel_reg.
#   - Sample-source phrasing added to fn_primary, fn_lagged, fn_robust:
#     "H3 / activeness subsample per flagged\_funds.xlsx".
#
# v2.0 (original):
# =============================================================================
# Tests H3 (Lottery Demand) under three specifications and writes three .tex
# tables. The 4-column architecture follows the activeness-proxy diagnostic
# in Appendix G:
#   Col 1  Baseline    no lottery measure, no sentiment interaction
#   Col 2  ActR2       1 - rolling 36m Carhart R^2 (rational-channel control)
#   Col 3  ActSkew     full-sample 36m rolling skewness (lottery-channel)
#   Col 4  MAX12       max monthly return over trailing 12m (right-tail)
#
# All cols 2-4 use the same sentiment proxy, D^SENT (Baker-Wurgler 2007).
# Joint test per column: chi^2(2) on (beta, delta), where beta is the main
# effect of the lottery measure and delta is its interaction with D^SENT.
# Headline z per column: 1-sided positive on delta. ActR2's delta is the
# rational-null test (predicted insignificant); ActSkew/MAX12 deltas are
# the behavioural-channel tests (predicted positive).
#
# (1) PRIMARY                          -> table_H3_regression.tex
#     Contemporaneous D^SENT + Ticker FE.   Main body Section 5.4.
# (2) TIMING ROBUSTNESS                -> table_H3_lagged.tex
#     D^SENT_{t-1} + Ticker FE.             Appendix F.1.
# (3) IDENTIFICATION ROBUSTNESS        -> table_H3_robustness.tex
#     Contemporaneous D^SENT + Lipper x yearmo FE.  Appendix F.2.
# =============================================================================

suppressPackageStartupMessages({
  library(dplyr)
  library(fixest)
  library(kableExtra)
})

# --- 0. Config ---------------------------------------------------------------
if (!exists("WORKING_DIR")) WORKING_DIR <- getwd()
STAR_THR <- c(`***` = 2.576, `**` = 1.960, `*` = 1.645)

OUTPUT_PRIMARY <- file.path(WORKING_DIR, "table_H3_regression.tex")
OUTPUT_LAGGED  <- file.path(WORKING_DIR, "table_H3_lagged.tex")
OUTPUT_ROBUST  <- file.path(WORKING_DIR, "table_H3_robustness.tex")

# --- 1. Pre-flight -----------------------------------------------------------
if (!exists("panel_reg")) {
  rds_path <- file.path(WORKING_DIR, "panel_reg.rds")
  if (file.exists(rds_path)) {
    panel_reg <- readRDS(rds_path)
    cat("Loaded panel_reg from", rds_path, "\n")
  } else {
    stop("panel_reg not in session and panel_reg.rds not found in ",
         WORKING_DIR, ". Run panel_regressions_setup.R first.")
  }
}
required <- c("ActR2", "ActSkew", "MAX12", "D_SENT", "D_SENT_lag",
              "excluded_h3")
missing_cols <- setdiff(required, names(panel_reg))
if (length(missing_cols)) {
  stop("panel_reg missing required column(s): ",
       paste(missing_cols, collapse = ", "),
       ". Re-run panel_regressions_setup.R (v1.3 or later: requires ",
       "MAX12 and the excluded_h3 flag).")
}

# --- 2. Aligned estimation sample --------------------------------------------
core_rhs <- c("flow", "R_LOW", "R_MID", "R_HIGH",
              "log_TNA", "log_Age", "ExpRatio", "LoadDummy",
              "ret_vol", "Turnover", "style_flow_lag")

# v2.1: filter(!excluded_h3) restricts to the H3 / activeness subsample
# per flagged_funds.xlsx Step 8c.
samp <- panel_reg %>%
  filter(!excluded_h3) %>%
  filter(!is_december) %>%
  filter(if_all(all_of(c(core_rhs,
                         "ActR2", "ActSkew", "MAX12",
                         "D_SENT", "D_SENT_lag")),
                ~ !is.na(.))) %>%
  mutate(
    Ticker          = as.factor(Ticker),
    yearmo          = as.factor(yearmo),
    Lipper_Category = as.factor(Lipper_Category)
  )

# Winsorise ActSkew and MAX12 at 1st/99th of pooled distribution. ActR2 is
# naturally bounded in (0,1) and does not need winsorisation.
for (v in c("ActSkew", "MAX12")) {
  lo <- quantile(samp[[v]], 0.01, na.rm = TRUE)
  hi <- quantile(samp[[v]], 0.99, na.rm = TRUE)
  samp[[v]] <- pmin(pmax(samp[[v]], lo), hi)
  cat(sprintf("%s winsorised at [%.4f, %.4f] (1st/99th pctile pooled).\n",
              v, lo, hi))
}

cat(sprintf(
  "\nAligned H3 sample: %d fund-months | %d funds | %d months | %d styles\n",
  nrow(samp), nlevels(droplevels(samp$Ticker)),
  nlevels(droplevels(samp$yearmo)),
  nlevels(droplevels(samp$Lipper_Category))
))

# --- 3. Helpers --------------------------------------------------------------
fmt_num <- function(x, n = 4) {
  if (is.null(x) || length(x) == 0L || is.na(x)) return("")
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

test_h3 <- function(m, lottery_var, state_var) {
  b <- coef(m); V <- vcov(m)
  i_main <- which(names(b) == lottery_var)
  int_name1 <- paste0(lottery_var, ":", state_var)
  int_name2 <- paste0(state_var,    ":", lottery_var)
  i_int <- which(names(b) == int_name1)
  if (length(i_int) == 0L) i_int <- which(names(b) == int_name2)
  if (length(i_main) == 0L || length(i_int) == 0L) {
    return(list(joint_chi2 = NA, joint_p = NA,
                beta_b = NA, beta_z = NA,
                delta_b = NA, delta_z = NA, delta_p_1sd = NA))
  }
  pos <- c(i_main, i_int)
  bs <- b[pos]; Vs <- V[pos, pos]
  chi2 <- as.numeric(t(bs) %*% solve(Vs) %*% bs)
  p_joint <- pchisq(chi2, df = 2L, lower.tail = FALSE)

  beta_b   <- as.numeric(b[i_main])
  beta_z   <- beta_b / sqrt(as.numeric(V[i_main, i_main]))
  delta_b  <- as.numeric(b[i_int])
  delta_se <- sqrt(as.numeric(V[i_int, i_int]))
  delta_z  <- delta_b / delta_se
  p_delta  <- pnorm(delta_z, lower.tail = FALSE)

  list(joint_chi2 = chi2,    joint_p = p_joint,
       beta_b = beta_b,      beta_z = beta_z,
       delta_b = delta_b,    delta_z = delta_z, delta_p_1sd = p_delta)
}

get_coef <- function(mod, name) {
  if (is.null(name) || is.na(name) || name == "") {
    return(c(coef = "", t = ""))
  }
  ct <- tryCatch(fixest::coeftable(mod), error = function(e) NULL)
  if (is.null(ct)) return(c(coef = "", t = ""))
  rn <- rownames(ct); i <- which(rn == name)
  if (length(i) == 0L && grepl(":", name, fixed = TRUE)) {
    parts <- strsplit(name, ":", fixed = TRUE)[[1]]
    rev_name <- paste(rev(parts), collapse = ":")
    i <- which(rn == rev_name)
  }
  if (length(i) == 0L) return(c(coef = "", t = ""))
  est    <- as.numeric(ct[i, "Estimate"])
  se_val <- as.numeric(ct[i, "Std. Error"])
  if (is.na(est) || is.na(se_val) || se_val == 0) return(c(coef = "", t = ""))
  tstat <- est / se_val
  c(coef = add_stars(fmt_num(est, 4), tstat),
    t    = paste0("(", formatC(tstat, format = "f", digits = 2), ")"))
}

# Phase B helper (compact form): extract tablenotes from a threeparttable
# float and re-emit them AFTER \end{table} as a flowing paragraph (NOT a
# minipage --- minipage is unbreakable and overflowed page bottoms).
threeparttable_note_after_compact <- function(s) {
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
  sub("\\end{table}",
      paste0("\\end{table}\n{\\footnotesize\\noindent\\textit{Note:} ", ni, "\\par}\n"),
      s, fixed = TRUE)
}

# --- 4. Master table builder -------------------------------------------------
build_h3_table <- function(samp, fe_string, state_var,
                           output_path, table_label, caption, footnote_text,
                           show_state_main, col_headers, spec_id) {
  controls <- c("log_TNA", "log_Age", "ExpRatio", "LoadDummy",
                "ret_vol", "Turnover", "style_flow_lag")
  ctrl_str <- paste(controls, collapse = " + ")

  mk_baseline <- function() {
    as.formula(paste("flow ~ R_LOW + R_MID + R_HIGH +",
                     ctrl_str, "|", fe_string))
  }
  mk_with <- function(lottery_var) {
    rhs <- paste("R_LOW + R_MID + R_HIGH +", lottery_var, "+", state_var,
                 "+", lottery_var, ":", state_var, "+", ctrl_str)
    as.formula(paste("flow ~", rhs, "|", fe_string))
  }

  cat(sprintf("\n[%s] Estimating 4 specifications...\n", spec_id))
  m1 <- feols(mk_baseline(),         data = samp, cluster = ~ Ticker + yearmo)
  m2 <- feols(mk_with("ActR2"),      data = samp, cluster = ~ Ticker + yearmo)
  m3 <- feols(mk_with("ActSkew"),    data = samp, cluster = ~ Ticker + yearmo)
  m4 <- feols(mk_with("MAX12"),      data = samp, cluster = ~ Ticker + yearmo)

  t2 <- test_h3(m2, "ActR2",   state_var)
  t3 <- test_h3(m3, "ActSkew", state_var)
  t4 <- test_h3(m4, "MAX12",   state_var)

  cat(sprintf("  ActR2   joint chi2(2)=%5.2f (p=%.4f) | delta z=%+.2f (p=%.4f)\n",
              t2$joint_chi2, t2$joint_p, t2$delta_z, t2$delta_p_1sd))
  cat(sprintf("  ActSkew joint chi2(2)=%5.2f (p=%.4f) | delta z=%+.2f (p=%.4f)\n",
              t3$joint_chi2, t3$joint_p, t3$delta_z, t3$delta_p_1sd))
  cat(sprintf("  MAX12   joint chi2(2)=%5.2f (p=%.4f) | delta z=%+.2f (p=%.4f)\n",
              t4$joint_chi2, t4$joint_p, t4$delta_z, t4$delta_p_1sd))

  blank_acc <- function(mod) c(coef = "", t = "")
  acc <- function(name) { force(name); function(mod) get_coef(mod, name) }

  body_specs <- list(
    list(label = "$R^{LOW}$",  accs = list(acc("R_LOW"),  acc("R_LOW"),  acc("R_LOW"),  acc("R_LOW"))),
    list(label = "$R^{MID}$",  accs = list(acc("R_MID"),  acc("R_MID"),  acc("R_MID"),  acc("R_MID"))),
    list(label = "$R^{HIGH}$", accs = list(acc("R_HIGH"), acc("R_HIGH"), acc("R_HIGH"), acc("R_HIGH"))),
    list(label = "Lottery measure",
         accs = list(blank_acc, acc("ActR2"), acc("ActSkew"), acc("MAX12")))
  )
  if (show_state_main) {
    body_specs[[length(body_specs) + 1L]] <- list(
      label = "Sentiment",
      accs = list(blank_acc, acc(state_var), acc(state_var), acc(state_var))
    )
  }
  body_specs[[length(body_specs) + 1L]] <- list(
    label = "Lottery $\\times$ Sent.",
    accs = list(blank_acc,
                acc(paste0("ActR2:",   state_var)),
                acc(paste0("ActSkew:", state_var)),
                acc(paste0("MAX12:",   state_var)))
  )
  ctrl_rows <- list(
    list(label = "$\\log(\\text{TNA})$", var = "log_TNA"),
    list(label = "$\\log(\\text{Age})$", var = "log_Age")
  )
  # Time-invariant controls (ExpRatio, LoadDummy, Turnover) are absorbed
  # by fund FE in the primary and timing-robust specs, so they're omitted
  # there. Under Lipper x yearmo FE (show_state_main = FALSE) they are
  # identified and should be displayed.
  if (!show_state_main) {
    ctrl_rows <- c(ctrl_rows, list(
      list(label = "Expense ratio", var = "ExpRatio"),
      list(label = "Load dummy",    var = "LoadDummy")
    ))
  }
  ctrl_rows <- c(ctrl_rows, list(
    list(label = "Return volatility", var = "ret_vol")
  ))
  if (!show_state_main) {
    ctrl_rows <- c(ctrl_rows, list(
      list(label = "Turnover", var = "Turnover")
    ))
  }
  ctrl_rows <- c(ctrl_rows, list(
    list(label = "Style flow", var = "style_flow_lag")
  ))
  for (cr in ctrl_rows) {
    body_specs[[length(body_specs) + 1L]] <- list(
      label = cr$label,
      accs = list(acc(cr$var), acc(cr$var), acc(cr$var), acc(cr$var))
    )
  }

  mods <- list(m1, m2, m3, m4)
  body_rows <- list()
  for (spec in body_specs) {
    vals <- mapply(function(f, m) f(m), spec$accs, mods, SIMPLIFY = FALSE)
    coef_row <- c(spec$label, sapply(vals, function(v) v["coef"]))
    t_row    <- c("",         sapply(vals, function(v) v["t"]))
    body_rows[[length(body_rows) + 1L]] <- coef_row
    body_rows[[length(body_rows) + 1L]] <- t_row
  }
  body_df <- do.call(rbind, body_rows)
  colnames(body_df) <- c("Variable", "(1)", "(2)", "(3)", "(4)")
  rownames(body_df) <- NULL

  fe_label <- if (show_state_main) "Fund FE" else "FE: Lipper $\\times$ yearmo"
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
  joint_row <- c("Joint test ($\\chi^2_2$, $p$)",
                 "--",
                 sprintf("%.2f (%s)", t2$joint_chi2, fmt_p(t2$joint_p)),
                 sprintf("%.2f (%s)", t3$joint_chi2, fmt_p(t3$joint_p)),
                 sprintf("%.2f (%s)", t4$joint_chi2, fmt_p(t4$joint_p)))
  delta_row <- c("Sent.\\ amp.\\ ($z$, 1-sided $p$)",
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
    add_header_above(col_headers, escape = FALSE, bold = FALSE) %>%
    row_spec(nrow(body_df), hline_after = TRUE) %>%
    footnote(general = footnote_text, general_title = "",
             threeparttable = TRUE, escape = FALSE)

  tex <- as.character(ktab)
  tex <- gsub("\\begin{table}[!h]", "\\begin{table}[H]", tex, fixed = TRUE)
  tex <- threeparttable_note_after_compact(tex)  # PHASE B: move note outside float
  writeLines(tex, output_path)
  cat(sprintf("Wrote %s\n", output_path))

  list(m1 = m1, m2 = m2, m3 = m3, m4 = m4,
       tests = list(t2 = t2, t3 = t3, t4 = t4))
}

# --- 5. Captions and footnotes -----------------------------------------------
cap_primary <- "H3: Lottery Demand Hypothesis"
fn_primary <- paste0(
  "Dependent variable is the Sirri--Tufano (1998) winsorised proportional ",
  "fund flow (decimal). $t$-statistics in parentheses below each coefficient. ",
  "All lottery proxies lagged one period and winsorised at the 1st/99th ",
  "percentiles of the pooled distribution (ActR2 naturally bounded). ",
  "Performance segments $R^{LOW}$, $R^{MID}$, $R^{HIGH}$ retained as ",
  "nuisance controls so H3 conditions on the H1 channel. Time-invariant ",
  "fund characteristics (expense ratio, load dummy, turnover) absorbed by ",
  "fund fixed effects. Standard errors two-way clustered on Ticker and ",
  "calendar month (\\\\textcite{Petersen2009}). The joint $\\\\chi^2_2$ test ",
  "is $\\\\beta=\\\\delta=0$ for the focal lottery measure in each column. ",
  "The headline $z$ tests $\\\\delta>0$ (1-sided): sentiment amplifies ",
  "investor response to the lottery proxy. ",
  "Stars: $^{*}\\\\,p<0.10$, $^{**}\\\\,p<0.05$, $^{***}\\\\,p<0.01$. ",
  "Sample: actively managed funds, ",
  "\\\\textcite{Evans2010}-corrected panel, no date cap; H3 / activeness ",
  "subsample per flagged\\\\_funds.xlsx."
)

cap_lagged <- "H3 Robustness --- Lagged Sentiment"
fn_lagged <- paste0(
  "Same sample, dependent variable, lottery measures, and identification ",
  "strategy as Table~\\\\ref{tab:H3_regression}. $D^{SENT}_{t-1}$ replaces ",
  "$D^{SENT}_{t}$ in columns (2)--(4). Standard errors two-way clustered on ",
  "Ticker and calendar month. ",
  "Stars: $^{*}\\\\,p<0.10$, $^{**}\\\\,p<0.05$, $^{***}\\\\,p<0.01$. ",
  "Sample: actively managed funds, \\\\textcite{Evans2010}-corrected panel, ",
  "no date cap; H3 / activeness subsample per flagged\\\\_funds.xlsx."
)

cap_robust <- "H3 Robustness --- Style $\\times$ Time Fixed Effects"
fn_robust <- paste0(
  "Same sample, dependent variable, and lottery measure construction as ",
  "Table~\\\\ref{tab:H3_regression}. Sentiment regime $D^{SENT}$ remains ",
  "contemporaneous. The state main effect ($\\\\gamma$) is absorbed by the ",
  "Lipper $\\\\times$ yearmo fixed effects and so does not appear in the ",
  "table. Time-invariant controls (expense ratio, load dummy, turnover) ",
  "are now identified because there is no fund FE. Standard errors two-way ",
  "clustered on Ticker and calendar month. ",
  "Stars: $^{*}\\\\,p<0.10$, $^{**}\\\\,p<0.05$, $^{***}\\\\,p<0.01$. ",
  "Sample: actively managed funds, \\\\textcite{Evans2010}-corrected panel, ",
  "no date cap; H3 / activeness subsample per flagged\\\\_funds.xlsx."
)

hdr <- c(" " = 1, "Baseline" = 1, "ActR2" = 1, "ActSkew" = 1, "MAX12" = 1)

# --- 6. Estimate three specifications ----------------------------------------
H3_primary <- build_h3_table(
  samp, "Ticker", "D_SENT",
  OUTPUT_PRIMARY, "H3_regression", cap_primary, fn_primary,
  show_state_main = TRUE, col_headers = hdr, spec_id = "PRIMARY"
)
H3_lagged <- build_h3_table(
  samp, "Ticker", "D_SENT_lag",
  OUTPUT_LAGGED, "H3_lagged", cap_lagged, fn_lagged,
  show_state_main = TRUE, col_headers = hdr, spec_id = "TIMING ROBUSTNESS"
)
H3_robust <- build_h3_table(
  samp, "Lipper_Category^yearmo", "D_SENT",
  OUTPUT_ROBUST, "H3_robustness", cap_robust, fn_robust,
  show_state_main = FALSE, col_headers = hdr, spec_id = "FE ROBUSTNESS"
)

H3_models <- list(primary = H3_primary, lagged = H3_lagged, robust = H3_robust)
assign("H3_models", H3_models, envir = .GlobalEnv)
saveRDS(H3_models, file.path(WORKING_DIR, "H3_models.rds"))
cat("Saved H3_models.rds\n")
