# H4_fee_elasticity.R                                                  v2.1
# =============================================================================
# v2.1 changes vs v2.0 (Family E pre-defense audit):
#   - No filter change. Per data_import_and_cleaning.R Step 8c convention,
#     Entire-Analysis exclusions in flagged_funds.xlsx are dropped from the
#     source panel; H4 fee-elasticity identification operates on the
#     resulting full universe and does not need filter(!excluded_perf) or
#     filter(!excluded_h3).
#   - Sample-source phrasing added to fn_primary, fn_lagged, fn_robust.
#
# v2.0 (May 2026):
# =============================================================================
# Tests H4 (Fee Elasticity / Berk-Green) under three specifications and writes
# three .tex tables (PRIMARY, LAGGED timing-robustness, FE robustness).
# Aligned-sample three-spec pattern of H1-H3 is RELAXED for H4 because the
# focal predictor (ExpRatio) is time-invariant within fund: fund FE perfectly
# absorbs ExpRatio main effects, leaving only the interactions identified.
# The PRIMARY spec therefore uses Lipper x yearmo FE (which absorbs only
# style-month aggregates and leaves cross-fund expense-ratio variation
# intact).
#
# v2.0 changelog:
#   - PRODUCE_LAGGED default flipped to TRUE. Closes the asymmetry with
#     Appendix F.1 (which already contains lagged H1, H2, H3).
#   - Lower-order R^q x STATE (kappa_q) and R^q x ExpRatio (phi_q) coefficients
#     now displayed in the table rather than suppressed. Output is taller and
#     spans more vertical space; longtable used for clean page breaks.
#   - Footnote text revised: removes "suppressed" language and replaces with
#     identification narrative ("STATE main effect absorbed by FE; all
#     lower-order interaction coefficients reported").
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
#   (2) LAGGED TIMING ROBUSTNESS -> table_H4_lagged.tex
#       Lagged sentiment + Lipper x yearmo FE.  Appendix F.1.
#   (3) FE ROBUSTNESS            -> table_H4_robustness.tex
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

PRODUCE_LAGGED <- TRUE   # set FALSE to suppress table_H4_lagged.tex (default ON in v2.0)

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
  # v2.0: lower-order R_q x STATE (kappa_q) and R_q x ExpRatio (phi_q) are
  # now displayed in the table. Row order is grouped logically:
  #   (a) rank main effects;
  #   (b) ExpRatio main + R_q x ExpRatio;
  #   (c) R_q x STATE (H1 channel inside the H4 sample);
  #   (d) headline triple interactions (delta_F + 3 x delta^F_q);
  #   (e) controls.
  # STATE main effect is absorbed by Lipper x yearmo FE (or yearmo FE in the
  # FE-robustness spec) and so does not appear in any column.
  row_specs <- list(
    list(parts = "R_LOW",   label = "$R^{\\text{LOW}}$"),
    list(parts = "R_MID",   label = "$R^{\\text{MID}}$"),
    list(parts = "R_HIGH",  label = "$R^{\\text{HIGH}}$"),
    list(parts = "ExpRatio", label = "Expense ratio"),
    list(parts = "R_LOW_EXP",
         label = "$R^{\\text{LOW}}\\times$ Exp.\\ ratio"),
    list(parts = "R_MID_EXP",
         label = "$R^{\\text{MID}}\\times$ Exp.\\ ratio"),
    list(parts = "R_HIGH_EXP",
         label = "$R^{\\text{HIGH}}\\times$ Exp.\\ ratio"),
    list(parts = "R_LOW_STATE",
         label = "$R^{\\text{LOW}}\\times$ Sent."),
    list(parts = "R_MID_STATE",
         label = "$R^{\\text{MID}}\\times$ Sent."),
    list(parts = "R_HIGH_STATE",
         label = "$R^{\\text{HIGH}}\\times$ Sent."),
    list(parts = "EXP_STATE",
         label = "Exp.\\ ratio $\\times$ Sent."),
    list(parts = "R_LOW_EXP_STATE",
         label = "$R^{\\text{LOW}}\\times$ Exp.\\ $\\times$ Sent."),
    list(parts = "R_MID_EXP_STATE",
         label = "$R^{\\text{MID}}\\times$ Exp.\\ $\\times$ Sent."),
    list(parts = "R_HIGH_EXP_STATE",
         label = "$R^{\\text{HIGH}}\\times$ Exp.\\ $\\times$ Sent."),
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
    # Two-way interactions involving STATE: only present in m2-m4.
    if (token == "EXP_STATE") {
      if (is.null(sv)) return(NA_character_)
      return(c("ExpRatio", sv))
    }
    if (token == "R_LOW_STATE") {
      if (is.null(sv)) return(NA_character_)
      return(c("R_LOW", sv))
    }
    if (token == "R_MID_STATE") {
      if (is.null(sv)) return(NA_character_)
      return(c("R_MID", sv))
    }
    if (token == "R_HIGH_STATE") {
      if (is.null(sv)) return(NA_character_)
      return(c("R_HIGH", sv))
    }
    # R_q x ExpRatio interactions: only included when STATE is in the model
    # (see mk_formula). Baseline column (m1) has no fee-modulated terms.
    if (token == "R_LOW_EXP") {
      if (is.null(sv)) return(NA_character_)
      return(c("R_LOW", "ExpRatio"))
    }
    if (token == "R_MID_EXP") {
      if (is.null(sv)) return(NA_character_)
      return(c("R_MID", "ExpRatio"))
    }
    if (token == "R_HIGH_EXP") {
      if (is.null(sv)) return(NA_character_)
      return(c("R_HIGH", "ExpRatio"))
    }
    # Triple interactions: only present in m2-m4.
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
  # Note: longtable does not use the [!h] table float; the gsub below is a
  # no-op when longtable=TRUE but is retained as defensive in case kableExtra
  # output changes format in future versions.
  tex <- gsub("\\begin{table}[!h]", "\\begin{table}[H]", tex, fixed = TRUE)
  tex <- threeparttable_note_after_compact(tex)  # PHASE B: move note outside float
  # Phase 2.8 (19 May 2026): tighten tabcolsep + stretch columns to linewidth.
  # Brings H4 into alignment-parity with the H1/H2/H3 longtables converted in
  # Phase 2.7. Without @{\extracolsep{\fill}}, columns sit at natural width
  # (less than \linewidth), so the caption (rendered at \linewidth) appears
  # wider than the table. \setlength{\tabcolsep}{3pt} keeps total width within
  # \linewidth on the lagged / Style x Time-FE specs that have widest labels.
  tex <- sub("\\begin{longtable}[t]{",
             "\\setlength{\\tabcolsep}{3pt}\\begin{longtable}[t]{@{\\extracolsep{\\fill}}",
             tex, fixed = TRUE)
  writeLines(tex, output_path)
  cat(sprintf("Wrote %s\n", output_path))

  list(m1 = m1, m2 = m2, m3 = m3, m4 = m4,
       tests = list(t2 = t2, t3 = t3, t4 = t4))
}

# --- 5. Captions and footnotes -----------------------------------------------
cap_primary <- "H4: Fee Elasticity Hypothesis"
fn_primary <- paste0(
  "The dependent variable is the Sirri-Tufano (1998) winsorised proportional ",
  "fund flow (decimal). $t$-statistics in parentheses below each coefficient. ",
  "All lower-order interaction coefficients ($R^q\\\\times$ Sent.\\\\ and ",
  "$R^q\\\\times$ Exp.\\\\ ratio) are reported alongside the headline triple ",
  "interactions to support full interpretation. Sentiment regimes follow ",
  "Baker-Wurgler (2007) construction: $D^{\\text{SENT}}_t$ is the top-34\\\\% indicator ",
  "for orthogonalised sentiment; column (3) substitutes the standardised ",
  "continuous index; column (4) substitutes the AAII bull-bear regime dummy. ",
  "ExpRatio is the time-invariant fund expense ratio (decimal). Lipper-",
  "category $\\\\times$ yearmo fixed effects absorb both sentiment main effects ",
  "($\\\\gamma$) and any style-month aggregate flow shocks; cross-fund expense-",
  "ratio variation within each (style, month) cell identifies $\\\\theta$ and ",
  "the fee interactions. Standard errors two-way clustered on Ticker and ",
  "calendar month (Petersen 2009). The joint $\\\\chi^2_4$ test covers all four ",
  "fee-amplified-sentiment terms ($\\\\delta_F$ and the three $\\\\delta^F_q$). ",
  "Stars: $^{*}\\\\,p<0.10$, $^{**}\\\\,p<0.05$, $^{***}\\\\,p<0.01$. ",
  "Sample: actively managed funds, \\\\textcite{Evans2010}-corrected panel, ",
  "no date cap; Entire-Analysis exclusions per flagged\\\\_funds.xlsx applied ",
  "at source."
)

cap_lagged <- "H4 Robustness --- Lagged Sentiment"
fn_lagged <- paste0(
  "Same sample, dependent variable, controls, identification strategy, ",
  "and FE structure as Table~\\\\ref{tab:H4_regression}. State variables in ",
  "columns (2)--(4) are lagged one period. ExpRatio enters at $t$ (= $t-1$ ",
  "= constant). Standard errors two-way clustered on Ticker and calendar ",
  "month (Petersen 2009). ",
  "Stars: $^{*}\\\\,p<0.10$, $^{**}\\\\,p<0.05$, $^{***}\\\\,p<0.01$. ",
  "Sample: actively managed funds, \\\\textcite{Evans2010}-corrected panel, ",
  "no date cap; Entire-Analysis exclusions per flagged\\\\_funds.xlsx applied ",
  "at source."
)

cap_robust <- "H4 Robustness --- Yearmo-Only Fixed Effects"
fn_robust <- paste0(
  "Same sample, dependent variable, and rank construction as ",
  "Table~\\\\ref{tab:H4_regression}. Yearmo fixed effects absorb all ",
  "time-aggregate variation (including sentiment main effects, market-wide ",
  "flow waves, and risk-on/risk-off cycles) but leave both within- and ",
  "between-style cross-fund variation in expense ratios available for ",
  "identification. Standard errors two-way clustered on Ticker and ",
  "calendar month (Petersen 2009). The joint $\\\\chi^2_4$ test covers all ",
  "four fee-amplified-sentiment terms. ",
  "Stars: $^{*}\\\\,p<0.10$, $^{**}\\\\,p<0.05$, $^{***}\\\\,p<0.01$. ",
  "Sample: actively managed funds, \\\\textcite{Evans2010}-corrected panel, ",
  "no date cap; Entire-Analysis exclusions per flagged\\\\_funds.xlsx applied ",
  "at source."
)

hdr_lagged <- c(" " = 1, "Baseline" = 1, "$D^{\\text{SENT}}_{t-1}$" = 1,
                "$\\\\text{SENT}^\\\\perp_{t-1}$" = 1, "$D^{\\text{AAII}}_{t-1}$" = 1)
hdr_cont   <- c(" " = 1, "Baseline" = 1, "$D^{\\text{SENT}}$" = 1,
                "$\\\\text{SENT}^\\\\perp$" = 1, "$D^{\\text{AAII}}$" = 1)

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
