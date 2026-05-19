# H1_sentiment_convexity.R                                              v3.1
# =============================================================================
# v3.1 changes vs v3.0 (Family E pre-defense audit):
#   - No filter change. Per data_import_and_cleaning.R Step 8c convention,
#     the Entire-Analysis exclusions in flagged_funds.xlsx are dropped from
#     the source panel; H1 sentiment-convexity identification operates on
#     the resulting full universe and does not need filter(!excluded_perf)
#     or filter(!excluded_h3).
#   - Sample-source phrasing added to fn_primary, fn_lagged, fn_robust
#     (documentation parity with H3, activeness, performance-comparison
#     scripts that DO apply additional subsample filters).
#
# v3.0 (original):
# =============================================================================
# Tests H1 (Sentiment-Convexity) under three specifications and writes three
# .tex tables. All specifications use the SAME aligned estimation sample so
# coefficient differences are attributable to spec changes, not sample
# composition.
#
# (1) PRIMARY                          -> table_H1_regression.tex
#     Lagged sentiment + fund FE       (Huang et al. 2015 timing)
#     Goes in the main body of the dissertation.
#
# (2) TIMING ROBUSTNESS                -> table_H1_contemporaneous.tex
#     Contemporaneous sentiment + fund FE   (Baker-Wurgler 2007 timing)
#     Goes in Appendix F.1.
#
# (3) IDENTIFICATION ROBUSTNESS        -> table_H1_robustness.tex
#     Lagged sentiment + Lipper x yearmo FE  (Cheng et al. 2025)
#     Goes in Appendix F.2.
# =============================================================================

suppressPackageStartupMessages({
  library(dplyr)
  library(fixest)
  library(kableExtra)
})

# --- 0. Config ---------------------------------------------------------------
if (!exists("WORKING_DIR")) WORKING_DIR <- getwd()
STAR_THR <- c(`***` = 2.576, `**` = 1.960, `*` = 1.645)

OUTPUT_PRIMARY <- file.path(WORKING_DIR, "table_H1_regression.tex")
OUTPUT_LAGGED  <- file.path(WORKING_DIR, "table_H1_lagged.tex")
OUTPUT_ROBUST  <- file.path(WORKING_DIR, "table_H1_robustness.tex")

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
required_lag <- c("D_SENT_lag", "D_AAII_lag", "SENT_ORTH_lag")
missing_lag  <- setdiff(required_lag, names(panel_reg))
if (length(missing_lag)) {
  stop("panel_reg is missing lagged columns: ",
       paste(missing_lag, collapse = ", "),
       ". Re-run panel_regressions_setup.R (v1.2+) with lag patch applied.")
}

# --- 2. Aligned estimation sample --------------------------------------------
core_rhs <- c("flow", "R_LOW", "R_MID", "R_HIGH",
              "log_TNA", "log_Age", "ExpRatio", "LoadDummy",
              "ret_vol", "Turnover", "style_flow_lag")

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

cat(sprintf(
  "\nAligned H1 estimation sample: %d fund-months | %d funds | %d months | %d styles\n",
  nrow(samp), nlevels(samp$Ticker), nlevels(samp$yearmo),
  nlevels(samp$Lipper_Category)
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

test_h1 <- function(m, state_label) {
  b <- coef(m); V <- vcov(m)
  i_low  <- which(names(b) == paste0("R_LOW:",  state_label))
  i_mid  <- which(names(b) == paste0("R_MID:",  state_label))
  i_high <- which(names(b) == paste0("R_HIGH:", state_label))
  if (length(i_low) == 0) {
    i_low  <- which(names(b) == paste0(state_label, ":R_LOW"))
    i_mid  <- which(names(b) == paste0(state_label, ":R_MID"))
    i_high <- which(names(b) == paste0(state_label, ":R_HIGH"))
  }
  pos <- c(i_low, i_mid, i_high)
  bs <- b[pos]; Vs <- V[pos, pos]
  chi2 <- as.numeric(t(bs) %*% solve(Vs) %*% bs)
  p_joint <- pchisq(chi2, df = length(pos), lower.tail = FALSE)

  diff_b  <- as.numeric(b[i_high]) - as.numeric(b[i_low])
  diff_se <- sqrt(as.numeric(V[i_high, i_high]) + as.numeric(V[i_low, i_low])
                  - 2 * as.numeric(V[i_high, i_low]))
  z_asym  <- diff_b / diff_se
  p_asym_1sd <- pnorm(z_asym, lower.tail = FALSE)

  list(joint_chi2 = chi2, joint_p = p_joint,
       asym_diff = diff_b, asym_z = z_asym, asym_p_1sd = p_asym_1sd)
}

get_coef <- function(mod, name) {
  if (is.null(name) || (length(name) == 1 && is.na(name))) {
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
  # FIX: (?s) modifier so '.' matches newlines; tablenotes block always spans
  # multiple lines so the previous regex silently failed and left the note
  # trapped inside the float environment, causing page-bottom overflow.
  note_rx <- "(?s)\\\\begin\\{tablenotes\\}.*?\\\\end\\{tablenotes\\}"
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
  s <- gsub("\\\\end\\{threeparttable\\}\\s*\n?", "", s)
  # New caption format: matches Tables 4.9-4.18 style — drop italic Note:,
  # wrap in singlespace so the note renders single-spaced despite document's
  # \doublespacing default.
  sub("\\end{table}",
      paste0("\\end{table}\n",
             "\\begin{singlespace}\\footnotesize\\noindent\n", ni, "\n",
             "\\end{singlespace}\n"),
      s, fixed = TRUE)
}

# --- 4. Master table builder -------------------------------------------------
build_h1_table <- function(samp, fe_string,
                           d_sent_var, sent_cont_var, d_aaii_var,
                           output_path, table_label, caption, footnote_text,
                           show_lambda, col_headers, spec_id) {
  controls <- c("log_TNA", "log_Age", "ExpRatio", "LoadDummy",
                "ret_vol", "Turnover", "style_flow_lag")
  ctrl_str <- paste(controls, collapse = " + ")

  mk_formula <- function(state_var = NULL) {
    base_rhs <- paste("R_LOW + R_MID + R_HIGH +", ctrl_str)
    if (is.null(state_var)) {
      return(as.formula(paste("flow ~", base_rhs, "|", fe_string)))
    }
    if (show_lambda) {
      rhs <- paste(base_rhs, "+", state_var,
                   "+ R_LOW:", state_var, " + R_MID:", state_var,
                   " + R_HIGH:", state_var)
    } else {
      rhs <- paste(base_rhs,
                   "+ R_LOW:", state_var, " + R_MID:", state_var,
                   " + R_HIGH:", state_var)
    }
    as.formula(paste("flow ~", rhs, "|", fe_string))
  }

  cat(sprintf("\n[%s] Estimating 4 specifications...\n", spec_id))
  m1 <- feols(mk_formula(NULL),          data = samp, cluster = ~ Ticker + yearmo)
  m2 <- feols(mk_formula(d_sent_var),    data = samp, cluster = ~ Ticker + yearmo)
  m3 <- feols(mk_formula(sent_cont_var), data = samp, cluster = ~ Ticker + yearmo)
  m4 <- feols(mk_formula(d_aaii_var),    data = samp, cluster = ~ Ticker + yearmo)

  t2 <- test_h1(m2, d_sent_var)
  t3 <- test_h1(m3, sent_cont_var)
  t4 <- test_h1(m4, d_aaii_var)

  cat(sprintf("  D_SENT     joint chi2(3)=%5.2f (p=%.4f) | asym z=%+.2f (p=%.4f)\n",
              t2$joint_chi2, t2$joint_p, t2$asym_z, t2$asym_p_1sd))
  cat(sprintf("  SENT cont. joint chi2(3)=%5.2f (p=%.4f) | asym z=%+.2f (p=%.4f)\n",
              t3$joint_chi2, t3$joint_p, t3$asym_z, t3$asym_p_1sd))
  cat(sprintf("  D_AAII     joint chi2(3)=%5.2f (p=%.4f) | asym z=%+.2f (p=%.4f)\n",
              t4$joint_chi2, t4$joint_p, t4$asym_z, t4$asym_p_1sd))

  row_specs <- list(
    list(coef = "R_LOW",  label = "$R^{\\text{LOW}}$"),
    list(coef = "R_MID",  label = "$R^{\\text{MID}}$"),
    list(coef = "R_HIGH", label = "$R^{\\text{HIGH}}$")
  )
  if (show_lambda) {
    row_specs <- c(row_specs, list(
      list(coef = "STATE", label = "Sentiment")
    ))
  }
  row_specs <- c(row_specs, list(
    list(coef = "R_LOW:STATE",    label = "$R^{\\text{LOW}}\\times$ Sent."),
    list(coef = "R_MID:STATE",    label = "$R^{\\text{MID}}\\times$ Sent."),
    list(coef = "R_HIGH:STATE",   label = "$R^{\\text{HIGH}}\\times$ Sent."),
    list(coef = "log_TNA",        label = "$\\log(\\text{TNA})$"),
    list(coef = "log_Age",        label = "$\\log(\\text{Age})$")
  ))
  if (!show_lambda) {
    row_specs <- c(row_specs, list(
      list(coef = "ExpRatio",  label = "Expense ratio"),
      list(coef = "LoadDummy", label = "Load dummy")
    ))
  }
  row_specs <- c(row_specs, list(
    list(coef = "ret_vol", label = "Return volatility")
  ))
  if (!show_lambda) {
    row_specs <- c(row_specs, list(
      list(coef = "Turnover", label = "Turnover")
    ))
  }
  row_specs <- c(row_specs, list(
    list(coef = "style_flow_lag", label = "Style flow")
  ))

  state_var <- list(m1 = NULL, m2 = d_sent_var,
                    m3 = sent_cont_var, m4 = d_aaii_var)
  resolve_coef_name <- function(coef_pat, sv) {
    if (coef_pat == "STATE") {
      if (!show_lambda || is.null(sv)) return(NA_character_)
      return(sv)
    }
    if (startsWith(coef_pat, "R_") && grepl(":STATE$", coef_pat)) {
      if (is.null(sv)) return(NA_character_)
      return(paste0(sub(":STATE$", "", coef_pat), ":", sv))
    }
    coef_pat
  }

  mods <- list(m1 = m1, m2 = m2, m3 = m3, m4 = m4)
  mod_keys <- c("m1", "m2", "m3", "m4")
  body_rows <- list()
  for (spec in row_specs) {
    vals <- vector("list", length(mod_keys))
    for (j in seq_along(mod_keys)) {
      sv <- state_var[[mod_keys[j]]]
      cn <- resolve_coef_name(spec$coef, sv)
      vals[[j]] <- get_coef(mods[[j]], cn)
    }
    coef_row <- c(spec$label, sapply(vals, function(v) v["coef"]))
    t_row    <- c("",         sapply(vals, function(v) v["t"]))
    body_rows[[length(body_rows) + 1]] <- coef_row
    body_rows[[length(body_rows) + 1]] <- t_row
  }
  body_df <- do.call(rbind, body_rows)
  colnames(body_df) <- c("Variable", "(1)", "(2)", "(3)", "(4)")
  rownames(body_df) <- NULL

  fe_label <- if (show_lambda) "Fund FE" else "FE: Lipper $\\times$ yearmo"
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
  joint_row <- c("Joint test ($\\chi^2_3$, $p$)",
                 "--",
                 sprintf("%.2f (%s)", t2$joint_chi2, fmt_p(t2$joint_p)),
                 sprintf("%.2f (%s)", t3$joint_chi2, fmt_p(t3$joint_p)),
                 sprintf("%.2f (%s)", t4$joint_chi2, fmt_p(t4$joint_p)))
  asym_row  <- c("Asymmetry ($z$, 1-sided $p$)",
                 "--",
                 sprintf("z=%+.2f (p=%s)", t2$asym_z, fmt_p(t2$asym_p_1sd)),
                 sprintf("z=%+.2f (p=%s)", t3$asym_z, fmt_p(t3$asym_p_1sd)),
                 sprintf("z=%+.2f (p=%s)", t4$asym_z, fmt_p(t4$asym_p_1sd)))
  footer_df <- rbind(fe_row, clust_row, n_row, r2_row, joint_row, asym_row)
  colnames(footer_df) <- c("Variable","(1)","(2)","(3)","(4)")
  rownames(footer_df) <- NULL
  full_df <- rbind(body_df, footer_df)
  rownames(full_df) <- NULL

  # Phase 2.7 (19 May 2026): Convert from float+scale_down to longtable+
  # ThreePartTable. The float pattern with \begin{table}[H] cannot break
  # across pages, so tall tables overflowed and threeparttable_note_after_
  # compact() pulled the notes outside the float --- where they could drift
  # to the next page (see images 1, 2, 9 of pre-defense PDF review).
  # Longtable+ThreePartTable embeds notes inside \endlastfoot via
  # \insertTableNotes, gluing them to the last row regardless of page
  # break. Matches the H4 builder (H4_fee_elasticity.R) exactly.
  ktab <- kbl(
    full_df, format = "latex", booktabs = TRUE, longtable = TRUE,
    caption = caption, label = table_label,
    align = c("l", rep("r", 4)), escape = FALSE, linesep = ""
  ) %>%
    kable_styling(latex_options = c("repeat_header"),
                  font_size = 9) %>%
    add_header_above(col_headers, escape = FALSE, bold = FALSE) %>%
    row_spec(nrow(body_df), hline_after = TRUE) %>%
    footnote(general = footnote_text, general_title = "",
             threeparttable = TRUE, escape = FALSE)

  tex <- as.character(ktab)
  # Defensive: longtable does not emit \begin{table}[!h]; gsub is a no-op
  # but retained against future kableExtra changes.
  tex <- gsub("\\begin{table}[!h]", "\\begin{table}[H]", tex, fixed = TRUE)
  # No-op for longtable output (kableExtra emits \begin{TableNotes} in
  # CamelCase; regex targets lowercase \begin{tablenotes}). Retained for
  # safety against any residual float emission.
  tex <- threeparttable_note_after_compact(tex)
  # Phase 2.8 (19 May 2026): Two-step injection before \begin{longtable}:
  #   1. \setlength{\tabcolsep}{3pt}  --- tightens column spacing so the
  #      natural table width fits within \linewidth (prevents the right-
  #      margin overflow observed on J.1, J.2 lagged tables).
  #   2. @{\extracolsep{\fill}}       --- stretches columns so the table
  #      spans exactly \linewidth, putting its left and right edges at the
  #      page margins. Without this, columns are at natural width which is
  #      narrower than \linewidth, causing the caption (at \linewidth) to
  #      appear wider than the table --- the perceived caption-vs-table
  #      misalignment that the user flagged.
  tex <- sub("\\begin{longtable}[t]{",
             "\\setlength{\\tabcolsep}{3pt}\\begin{longtable}[t]{@{\\extracolsep{\\fill}}",
             tex, fixed = TRUE)
  writeLines(tex, output_path)
  cat(sprintf("Wrote %s\n", output_path))

  list(m1 = m1, m2 = m2, m3 = m3, m4 = m4,
       tests = list(t2 = t2, t3 = t3, t4 = t4))
}

# --- 5. Captions and footnotes -----------------------------------------------
cap_primary <- "H1: Sentiment-Convexity Hypothesis"
fn_primary <- paste0(
  "The dependent variable is the Sirri-Tufano (1998) winsorised proportional ",
  "fund flow (decimal). $t$-statistics in parentheses below each coefficient. ",
  "Performance segments $R^{\\\\text{LOW}}$, $R^{\\\\text{MID}}$, $R^{\\\\text{HIGH}}$ are constructed from ",
  "the lagged within-Lipper-category fractional rank of cumulative 12-month ",
  "gross returns (Equations 6--8 of the proposal). Sentiment in column (2) is ",
  "the regime dummy $D^{\\\\text{SENT}}_t$ (= 1 if Baker-Wurgler orthogonalised ",
  "sentiment exceeds its 66th in-sample percentile, following Baker-Wurgler ",
  "2007); column (3) is the standardised Baker-Wurgler orthogonalised ",
  "sentiment index; column (4) is the AAII bull-bear regime dummy. All ",
  "controls lagged one period; time-invariant fund characteristics (expense ",
  "ratio, load dummy, turnover ratio) are included in the specification but ",
  "absorbed by the fund fixed effects. Standard errors two-way clustered on ",
  "Ticker and calendar month (Petersen 2009). ",
  "Stars: $^{*}\\\\,p<0.10$, $^{**}\\\\,p<0.05$, $^{***}\\\\,p<0.01$. ",
  "Sample: actively managed funds, \\\\textcite{Evans2010}-corrected panel, ",
  "no date cap; Entire-Analysis exclusions per flagged\\\\_funds.xlsx applied ",
  "at source."
)

cap_lagged <- "H1 Robustness --- Lagged Sentiment"
fn_lagged <- paste0(
  "Same sample, dependent variable, controls, and identification strategy as ",
  "Table~\\\\ref{tab:H1_regression}. Sentiment proxies are lagged one period: ",
  "column (2) uses $D^{\\\\text{SENT}}_{t-1}$, column (3) uses the standardised ",
  "Baker-Wurgler orthogonalised sentiment index at $t-1$, column (4) uses ",
  "$D^{\\\\text{AAII}}_{t-1}$. Standard errors two-way clustered on Ticker and ",
  "calendar month (Petersen 2009). ",
  "Stars: $^{*}\\\\,p<0.10$, $^{**}\\\\,p<0.05$, $^{***}\\\\,p<0.01$. ",
  "Sample: actively managed funds, \\\\textcite{Evans2010}-corrected panel, ",
  "no date cap; Entire-Analysis exclusions per flagged\\\\_funds.xlsx applied ",
  "at source."
)

cap_robust <- "H1 Robustness --- Style $\\times$ Time Fixed Effects"
fn_robust <- paste0(
  "Same sample, dependent variable, and rank construction as ",
  "Table~\\\\ref{tab:H1_regression}. Sentiment variables remain ",
  "contemporaneous. State main effects ($\\\\lambda$) are absorbed by the ",
  "Lipper $\\\\times$ yearmo fixed effects and so do not appear in the table. ",
  "Time-invariant controls (expense ratio, load dummy, turnover) are now ",
  "identified because there is no fund FE. Standard errors two-way clustered ",
  "on Ticker and calendar month (Petersen 2009). ",
  "Stars: $^{*}\\\\,p<0.10$, $^{**}\\\\,p<0.05$, $^{***}\\\\,p<0.01$. ",
  "Sample: actively managed funds, \\\\textcite{Evans2010}-corrected panel, ",
  "no date cap; Entire-Analysis exclusions per flagged\\\\_funds.xlsx applied ",
  "at source."
)

hdr_lagged <- c(" " = 1, "Baseline" = 1, "$D^{\\\\text{SENT}}_{t-1}$" = 1,
                "$\\\\text{SENT}^\\\\perp_{t-1}$" = 1, "$D^{\\\\text{AAII}}_{t-1}$" = 1)
hdr_cont   <- c(" " = 1, "Baseline" = 1, "$D^{\\\\text{SENT}}$" = 1,
                "$\\\\text{SENT}^\\\\perp$" = 1, "$D^{\\\\text{AAII}}$" = 1)

# --- 6. Estimate three specifications ----------------------------------------
H1_primary <- build_h1_table(
  samp, "Ticker",
  "D_SENT", "SENT_ORTH_z", "D_AAII",
  OUTPUT_PRIMARY, "H1_regression", cap_primary, fn_primary,
  show_lambda = TRUE, col_headers = hdr_cont, spec_id = "PRIMARY"
)
H1_lagged <- build_h1_table(
  samp, "Ticker",
  "D_SENT_lag", "SENT_ORTH_lag_z", "D_AAII_lag",
  OUTPUT_LAGGED, "H1_lagged", cap_lagged, fn_lagged,
  show_lambda = TRUE, col_headers = hdr_lagged, spec_id = "TIMING ROBUSTNESS"
)
H1_robust <- build_h1_table(
  samp, "Lipper_Category^yearmo",
  "D_SENT", "SENT_ORTH_z", "D_AAII",
  OUTPUT_ROBUST, "H1_robustness", cap_robust, fn_robust,
  show_lambda = FALSE, col_headers = hdr_cont, spec_id = "FE ROBUSTNESS"
)

H1_models <- list(primary = H1_primary, lagged = H1_lagged, robust = H1_robust)
assign("H1_models", H1_models, envir = .GlobalEnv)
