# H2_disposition_control.R                                              v3.1
# =============================================================================
# v3.1 changes vs v3.0 (Family E pre-defense audit):
#   - No filter change. Per data_import_and_cleaning.R Step 8c convention,
#     Entire-Analysis exclusions in flagged_funds.xlsx are dropped from the
#     source panel; H2 disposition / illusion-of-control identification
#     operates on the resulting full universe and does not need
#     filter(!excluded_perf) or filter(!excluded_h3).
#   - Sample-source phrasing added to fn_primary, fn_lagged, fn_robust.
#
# v3.0 (original):
# =============================================================================
# Tests H2 (Disposition / Illusion-of-Control) under three specifications and
# writes three .tex tables. All specifications share aligned samples so
# coefficient differences reflect spec changes, not sample composition.
#
# (1) PRIMARY                          -> table_H2_regression.tex
#     Lagged state variables + fund FE   (Huang et al. 2015 timing)
#     Goes in the main body of the dissertation.
#
# (2) TIMING ROBUSTNESS                -> table_H2_contemporaneous.tex
#     Contemporaneous state + fund FE    (Baker-Wurgler 2007 timing)
#     Goes in Appendix F.1.
#
# (3) IDENTIFICATION ROBUSTNESS        -> table_H2_robustness.tex
#     Lagged state + Lipper x yearmo FE  (Cheng et al. 2025)
#     Goes in Appendix F.2.
#
# Four columns per table:
#   (1) Baseline
#   (2) D_MD_DETREND (or _lag)         primary margin-debt test
#   (3) D_INV_PCR    (or _lag)         illusion of control via PCR
#   (4) Discriminant: D_MD_DETREND + D_SENT (or both lagged)
#
# H2 prediction: delta_1 < 0 (disposition flattens loser-end slope).
# Brunnermeier-Pedersen (2009) margin-call alternative: delta_1 > 0.
# =============================================================================

suppressPackageStartupMessages({
  library(dplyr)
  library(fixest)
  library(kableExtra)
})

# --- 0. Config ---------------------------------------------------------------
if (!exists("WORKING_DIR")) WORKING_DIR <- getwd()
STAR_THR <- c(`***` = 2.576, `**` = 1.960, `*` = 1.645)

OUTPUT_PRIMARY <- file.path(WORKING_DIR, "table_H2_regression.tex")
OUTPUT_LAGGED  <- file.path(WORKING_DIR, "table_H2_lagged.tex")
OUTPUT_ROBUST  <- file.path(WORKING_DIR, "table_H2_robustness.tex")

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
required_lag <- c("D_MD_DETREND_lag", "D_SENT_lag", "PUT_CALL_RATIO_lag")
missing_lag  <- setdiff(required_lag, names(panel_reg))
if (length(missing_lag)) {
  stop("panel_reg is missing lagged columns: ",
       paste(missing_lag, collapse = ", "),
       ". Re-run panel_regressions_setup.R (v1.2+) with lag patch applied.")
}

# --- 2. Build aligned samples ------------------------------------------------
core_rhs <- c("flow", "R_LOW", "R_MID", "R_HIGH",
              "log_TNA", "log_Age", "ExpRatio", "LoadDummy",
              "ret_vol", "Turnover", "style_flow_lag")

# Margin-debt sample: requires both lagged AND contemporaneous MD/sentiment
# non-NA so all three specs use identical observations.
samp_md <- panel_reg %>%
  filter(!is_december) %>%
  filter(if_all(all_of(c(core_rhs,
                         "D_MD_DETREND", "D_SENT",
                         "D_MD_DETREND_lag", "D_SENT_lag")),
                ~ !is.na(.))) %>%
  mutate(
    D_MD_DETREND     = as.numeric(D_MD_DETREND),
    D_MD_DETREND_lag = as.numeric(D_MD_DETREND_lag),
    D_SENT           = as.numeric(D_SENT),
    D_SENT_lag       = as.numeric(D_SENT_lag),
    Ticker           = as.factor(Ticker),
    yearmo           = as.factor(yearmo),
    Lipper_Category  = as.factor(Lipper_Category)
  )

# PCR sample: own shorter window. D_INV_PCR (cont) and D_INV_PCR_lag built
# from PUT_CALL_RATIO and its lag respectively, using the same Q34 threshold.
samp_pcr <- panel_reg %>%
  filter(!is_december) %>%
  filter(if_all(all_of(c(core_rhs, "PUT_CALL_RATIO", "PUT_CALL_RATIO_lag")),
                ~ !is.na(.))) %>%
  mutate(
    pcr_thr_cont = quantile(PUT_CALL_RATIO,     1 - 0.66, na.rm = TRUE),
    pcr_thr_lag  = quantile(PUT_CALL_RATIO_lag, 1 - 0.66, na.rm = TRUE),
    D_INV_PCR     = as.numeric(PUT_CALL_RATIO     <= pcr_thr_cont),
    D_INV_PCR_lag = as.numeric(PUT_CALL_RATIO_lag <= pcr_thr_lag),
    Ticker          = as.factor(Ticker),
    yearmo          = as.factor(yearmo),
    Lipper_Category = as.factor(Lipper_Category)
  ) %>%
  select(-pcr_thr_cont, -pcr_thr_lag)

cat(sprintf(
  "\nAligned MD sample (cols 1, 2, 4): %d fund-months | %d funds | %d months | %d styles\n",
  nrow(samp_md), nlevels(samp_md$Ticker), nlevels(samp_md$yearmo),
  nlevels(samp_md$Lipper_Category)
))
cat(sprintf(
  "Aligned PCR sample (col 3): %d fund-months | %d funds | %d months\n",
  nrow(samp_pcr), nlevels(samp_pcr$Ticker), nlevels(samp_pcr$yearmo)
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

test_h2 <- function(m, state_label) {
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

  d1_b   <- as.numeric(b[i_low])
  d1_se  <- sqrt(as.numeric(V[i_low, i_low]))
  z_main <- d1_b / d1_se
  p_main_1sd <- pnorm(z_main, lower.tail = TRUE)

  diff_b  <- as.numeric(b[i_high]) - as.numeric(b[i_low])
  diff_se <- sqrt(as.numeric(V[i_high, i_high]) + as.numeric(V[i_low, i_low])
                  - 2 * as.numeric(V[i_high, i_low]))
  z_asym  <- diff_b / diff_se
  p_asym_1sd <- pnorm(z_asym, lower.tail = FALSE)

  list(joint_chi2 = chi2,    joint_p    = p_joint,
       d1_b       = d1_b,    d1_z       = z_main,    main_p_1sd = p_main_1sd,
       asym_diff  = diff_b,  asym_z     = z_asym,    asym_p_1sd = p_asym_1sd)
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
build_h2_table <- function(samp_md, samp_pcr, fe_string,
                           d_md_var, d_pcr_var, d_sent_var,
                           output_path, table_label, caption, footnote_text,
                           show_lambda, col_headers, spec_id) {
  controls <- c("log_TNA", "log_Age", "ExpRatio", "LoadDummy",
                "ret_vol", "Turnover", "style_flow_lag")
  ctrl_str <- paste(controls, collapse = " + ")

  mk_state_formula <- function(state_var) {
    base_rhs <- paste("R_LOW + R_MID + R_HIGH +", ctrl_str)
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

  f1 <- as.formula(paste("flow ~ R_LOW + R_MID + R_HIGH +", ctrl_str,
                         "|", fe_string))
  f2 <- mk_state_formula(d_md_var)
  f3 <- mk_state_formula(d_pcr_var)
  # Discriminant: include d_md_var and d_sent_var with full rank interactions
  # State main effects only if show_lambda.
  if (show_lambda) {
    f4 <- as.formula(paste(
      "flow ~ R_LOW + R_MID + R_HIGH +", ctrl_str,
      "+", d_md_var, "+", d_sent_var,
      "+ R_LOW:", d_md_var, " + R_MID:", d_md_var, " + R_HIGH:", d_md_var,
      "+ R_LOW:", d_sent_var, " + R_MID:", d_sent_var, " + R_HIGH:", d_sent_var,
      "|", fe_string))
  } else {
    f4 <- as.formula(paste(
      "flow ~ R_LOW + R_MID + R_HIGH +", ctrl_str,
      "+ R_LOW:", d_md_var, " + R_MID:", d_md_var, " + R_HIGH:", d_md_var,
      "+ R_LOW:", d_sent_var, " + R_MID:", d_sent_var, " + R_HIGH:", d_sent_var,
      "|", fe_string))
  }

  cat(sprintf("\n[%s] Estimating 4 specifications...\n", spec_id))
  m1 <- feols(f1, data = samp_md,  cluster = ~ Ticker + yearmo)
  m2 <- feols(f2, data = samp_md,  cluster = ~ Ticker + yearmo)
  m3 <- feols(f3, data = samp_pcr, cluster = ~ Ticker + yearmo)
  m4 <- feols(f4, data = samp_md,  cluster = ~ Ticker + yearmo)

  t2 <- test_h2(m2, d_md_var)
  t3 <- test_h2(m3, d_pcr_var)
  t4 <- test_h2(m4, d_md_var)

  cat(sprintf("  D_MD_DETREND  joint=%5.2f p=%.4f | d1=%+.4f z=%+.2f | d3-d1 z=%+.2f\n",
              t2$joint_chi2, t2$joint_p, t2$d1_b, t2$d1_z, t2$asym_z))
  cat(sprintf("  D_INV_PCR     joint=%5.2f p=%.4f | d1=%+.4f z=%+.2f | d3-d1 z=%+.2f\n",
              t3$joint_chi2, t3$joint_p, t3$d1_b, t3$d1_z, t3$asym_z))
  cat(sprintf("  Discriminant  joint=%5.2f p=%.4f | d1=%+.4f z=%+.2f | d3-d1 z=%+.2f\n",
              t4$joint_chi2, t4$joint_p, t4$d1_b, t4$d1_z, t4$asym_z))

  # Row specs for body
  row_specs <- list(
    list(coef = "R_LOW",  label = "$R^{\\text{LOW}}$"),
    list(coef = "R_MID",  label = "$R^{\\text{MID}}$"),
    list(coef = "R_HIGH", label = "$R^{\\text{HIGH}}$")
  )
  if (show_lambda) {
    row_specs <- c(row_specs, list(
      list(coef = "STATE", label = "State")
    ))
  }
  row_specs <- c(row_specs, list(
    list(coef = "R_LOW:STATE",    label = "$R^{\\text{LOW}}\\times$ State"),
    list(coef = "R_MID:STATE",    label = "$R^{\\text{MID}}\\times$ State"),
    list(coef = "R_HIGH:STATE",   label = "$R^{\\text{HIGH}}\\times$ State")
  ))
  if (show_lambda) {
    row_specs <- c(row_specs, list(
      list(coef = "D_SENT_VAR",     label = "$D^{\\text{SENT}}$")
    ))
  }
  row_specs <- c(row_specs, list(
    list(coef = "R_LOW:D_SENT_VAR",  label = "$R^{\\text{LOW}}\\times D^{\\text{SENT}}$"),
    list(coef = "R_MID:D_SENT_VAR",  label = "$R^{\\text{MID}}\\times D^{\\text{SENT}}$"),
    list(coef = "R_HIGH:D_SENT_VAR", label = "$R^{\\text{HIGH}}\\times D^{\\text{SENT}}$"),
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
    list(coef = "ret_vol",        label = "Return volatility")
  ))
  if (!show_lambda) {
    row_specs <- c(row_specs, list(
      list(coef = "Turnover",     label = "Turnover")
    ))
  }
  row_specs <- c(row_specs, list(
    list(coef = "style_flow_lag", label = "Style flow")
  ))

  # Per-column state-variable name (column 3 has its own state, col 4 has 2)
  state_var <- list(m1 = NULL, m2 = d_md_var, m3 = d_pcr_var, m4 = d_md_var)
  has_dsent <- list(m1 = FALSE, m2 = FALSE, m3 = FALSE, m4 = TRUE)

  resolve_coef_name <- function(coef_pat, sv, has_sent) {
    if (coef_pat == "STATE") {
      if (!show_lambda || is.null(sv)) return(NA_character_)
      return(sv)
    }
    if (startsWith(coef_pat, "R_") && grepl(":STATE$", coef_pat)) {
      if (is.null(sv)) return(NA_character_)
      return(paste0(sub(":STATE$", "", coef_pat), ":", sv))
    }
    if (coef_pat == "D_SENT_VAR") {
      if (!has_sent || !show_lambda) return(NA_character_)
      return(d_sent_var)
    }
    if (startsWith(coef_pat, "R_") && grepl(":D_SENT_VAR$", coef_pat)) {
      if (!has_sent) return(NA_character_)
      return(paste0(sub(":D_SENT_VAR$", "", coef_pat), ":", d_sent_var))
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
      hs <- has_dsent[[mod_keys[j]]]
      cn <- resolve_coef_name(spec$coef, sv, hs)
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
  main_row  <- c("Loser-end test ($z$, 1-sided $p$)",
                 "--",
                 sprintf("z=%+.2f (p=%s)", t2$d1_z, fmt_p(t2$main_p_1sd)),
                 sprintf("z=%+.2f (p=%s)", t3$d1_z, fmt_p(t3$main_p_1sd)),
                 sprintf("z=%+.2f (p=%s)", t4$d1_z, fmt_p(t4$main_p_1sd)))
  asym_row  <- c("Asymmetry ($z$, 1-sided $p$)",
                 "--",
                 sprintf("z=%+.2f (p=%s)", t2$asym_z, fmt_p(t2$asym_p_1sd)),
                 sprintf("z=%+.2f (p=%s)", t3$asym_z, fmt_p(t3$asym_p_1sd)),
                 sprintf("z=%+.2f (p=%s)", t4$asym_z, fmt_p(t4$asym_p_1sd)))

  footer_df <- rbind(fe_row, clust_row, n_row, r2_row,
                     joint_row, main_row, asym_row)
  colnames(footer_df) <- c("Variable","(1)","(2)","(3)","(4)")
  rownames(footer_df) <- NULL

  full_df <- rbind(body_df, footer_df)
  rownames(full_df) <- NULL

  # Phase 2.7 (19 May 2026): Convert from float+scale_down to longtable+
  # ThreePartTable. See H1_sentiment_convexity.R for rationale; identical
  # change applied here for consistency. Matches the H4 builder.
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
  # Defensive: longtable does not emit \begin{table}[!h]; gsub is a no-op.
  tex <- gsub("\\begin{table}[!h]", "\\begin{table}[H]", tex, fixed = TRUE)
  # No-op for longtable output (CamelCase \begin{TableNotes}). Retained.
  tex <- threeparttable_note_after_compact(tex)
  # Phase 2.8: tighten tabcolsep + stretch columns to linewidth.
  # See H1_sentiment_convexity.R for rationale.
  tex <- sub("\\begin{longtable}[t]{",
             "\\setlength{\\tabcolsep}{3pt}\\begin{longtable}[t]{@{\\extracolsep{\\fill}}",
             tex, fixed = TRUE)
  writeLines(tex, output_path)
  cat(sprintf("Wrote %s\n", output_path))

  list(m1 = m1, m2 = m2, m3 = m3, m4 = m4,
       tests = list(t2 = t2, t3 = t3, t4 = t4))
}

# --- 5. Captions and footnotes -----------------------------------------------
cap_primary <- "H2: Disposition / Illusion-of-Control Hypothesis"
fn_primary <- paste0(
  "The dependent variable is the Sirri-Tufano (1998) winsorised proportional ",
  "fund flow (decimal). $t$-statistics in parentheses below each coefficient. ",
  "Performance segments $R^{\\text{LOW}}$, $R^{\\text{MID}}$, $R^{\\text{HIGH}}$ are constructed from ",
  "the lagged within-Lipper-category fractional rank of cumulative 12-month ",
  "gross returns. State variable in column (2) is $D^{\\text{MD,Det}}_t$ (= 1 if the ",
  "residual of $\\\\log$(MD/MCAP) on a linear time trend is in the top 34\\\\%, ",
  "following Daniel-Klos-Pollet 2016 and Rapach-Ringgenberg-Zhou 2016); ",
  "column (3) is $D^{\\\\text{INV-PCR}}_t$ (= 1 in the bottom 34\\\\% of the CBOE ",
  "equity put-call ratio: high call-to-put = high illusion of control). ",
  "Column (4) is the discriminant specification: it includes both ",
  "$D^{\\text{MD,Det}}_t$ and the Baker-Wurgler orthogonalised sentiment regime ",
  "$D^{\\text{SENT}}_t$ (= 1 if SENT$^\\\\perp$ is in the top 34\\\\%) with full rank ",
  "interactions. All controls lagged one period; time-invariant fund ",
  "characteristics (expense ratio, load dummy, turnover) included but ",
  "absorbed by fund FE. Cols (1)-(2) and (4) use the full margin-debt ",
  "sample; col (3) uses the PCR sample (2003-10 to 2019-10). Standard ",
  "errors two-way clustered on Ticker and calendar month (Petersen 2009). ",
  "Stars: $^{*}\\\\,p<0.10$, $^{**}\\\\,p<0.05$, $^{***}\\\\,p<0.01$. ",
  "Sample: actively managed funds, \\\\textcite{Evans2010}-corrected panel, ",
  "no date cap; Entire-Analysis exclusions per flagged\\\\_funds.xlsx applied ",
  "at source."
)

cap_lagged <- "H2 Robustness --- Lagged State Variables"
fn_lagged <- paste0(
  "Same sample, dependent variable, controls, and identification strategy as ",
  "Table~\\\\ref{tab:H2_regression}. State variables are lagged one period: ",
  "column (2) uses $D^{\\text{MD,Det}}_{t-1}$, column (3) uses ",
  "$D^{\\\\text{INV-PCR}}_{t-1}$, column (4) is the discriminant with ",
  "$D^{\\text{MD,Det}}_{t-1}$ and $D^{\\text{SENT}}_{t-1}$. Standard errors two-way ",
  "clustered on Ticker and calendar month (Petersen 2009). ",
  "Stars: $^{*}\\\\,p<0.10$, $^{**}\\\\,p<0.05$, $^{***}\\\\,p<0.01$. ",
  "Sample: actively managed funds, \\\\textcite{Evans2010}-corrected panel, ",
  "no date cap; Entire-Analysis exclusions per flagged\\\\_funds.xlsx applied ",
  "at source."
)

cap_robust <- "H2 Robustness --- Style $\\times$ Time Fixed Effects"
fn_robust <- paste0(
  "Sample, dependent variable, rank construction, and column specifications ",
  "identical to Table~\\\\ref{tab:H2_regression}. State main effects (and ",
  "$D^{\\text{SENT}}_t$ in col 4) are absorbed by the Lipper $\\\\times$ yearmo fixed ",
  "effects and do not appear in the table; time-invariant controls (expense ",
  "ratio, load dummy, turnover) are now identified because there is no fund ",
  "FE. Standard errors two-way clustered on Ticker and calendar month ",
  "(Petersen 2009). The robustness of $\\\\delta^{\\\\text{MD}}_1>0$ across both ",
  "identification strategies points toward a margin-call alternative ",
  "(Brunnermeier \\\\& Pedersen 2009) rather than disposition psychology. ",
  "Stars: $^{*}\\\\,p<0.10$, $^{**}\\\\,p<0.05$, $^{***}\\\\,p<0.01$. ",
  "Sample: actively managed funds, \\\\textcite{Evans2010}-corrected panel, ",
  "no date cap; Entire-Analysis exclusions per flagged\\\\_funds.xlsx applied ",
  "at source."
)

hdr_lagged <- c(" " = 1, "Baseline" = 1, "$D^{\\text{MD,Det}}_{t-1}$" = 1,
                "$D^{\\\\text{INV-PCR}}_{t-1}$" = 1, "Discriminant" = 1)
hdr_cont   <- c(" " = 1, "Baseline" = 1, "$D^{\\text{MD,Det}}$" = 1,
                "$D^{\\\\text{INV-PCR}}$" = 1, "Discriminant" = 1)

# --- 6. Estimate three specifications ----------------------------------------
H2_primary <- build_h2_table(
  samp_md, samp_pcr, "Ticker",
  "D_MD_DETREND", "D_INV_PCR", "D_SENT",
  OUTPUT_PRIMARY, "H2_regression", cap_primary, fn_primary,
  show_lambda = TRUE, col_headers = hdr_cont, spec_id = "PRIMARY"
)
H2_lagged <- build_h2_table(
  samp_md, samp_pcr, "Ticker",
  "D_MD_DETREND_lag", "D_INV_PCR_lag", "D_SENT_lag",
  OUTPUT_LAGGED, "H2_lagged", cap_lagged, fn_lagged,
  show_lambda = TRUE, col_headers = hdr_lagged, spec_id = "TIMING ROBUSTNESS"
)
H2_robust <- build_h2_table(
  samp_md, samp_pcr, "Lipper_Category^yearmo",
  "D_MD_DETREND", "D_INV_PCR", "D_SENT",
  OUTPUT_ROBUST, "H2_robustness", cap_robust, fn_robust,
  show_lambda = FALSE, col_headers = hdr_cont, spec_id = "FE ROBUSTNESS"
)

H2_models <- list(primary = H2_primary, lagged = H2_lagged, robust = H2_robust)
assign("H2_models", H2_models, envir = .GlobalEnv)
