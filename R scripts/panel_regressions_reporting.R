# panel_regressions_reporting.R                                        v1.2
# =============================================================================
# v1.2 changes (Batch 4, Oct 2026):
#   - "Decision" column split into "Joint test" (joint Wald test at 10%)
#     and "Headline direction" (named only if the one-sided p in that
#     direction is < 0.10; the opposite tail uses 1 - p). v1.1 attached a
#     direction label to every row from the sign of z alone.
#   - H2 opposite tail labelled as the Brunnermeier-Pedersen margin-call
#     alternative; H3 ActR2 row labelled "rational-null control".
#   - Footnote states that each hypothesis has its own aligned sample.
#
# v1.1 changes (Family E pre-defense audit):
#   - No code changes. This script consumes pre-saved H1_models / H2_models /
#     H3_models / H4_models objects (or the corresponding .rds files) and
#     does not touch the fund-month panel directly. The exclusion-ledger
#     filtering applied in the upstream H1-H4 scripts (per Family E v3.1 /
#     v2.1 audit) is inherited automatically. Audit-stamp bump only.
#
# v1.0 (original):
# =============================================================================
# Assembles a dissertation-ready summary of the four behavioral hypothesis
# tests produced by H1-H4 scripts. Reads test results from H1_models /
# H2_models / H3_models / H4_models in the global environment (or from
# H{1,2,3,4}_models.rds in WORKING_DIR if absent), and writes
# table_behavioral_summary.tex to be inputted in dissertation Section 5.6 /
# Section 7 (puzzle synthesis).
#
# Output:
#   table_behavioral_summary.tex   one row per (hypothesis, state proxy)
#                                  with joint chi^2, headline z, N
#
# Notes:
#   * The "headline" statistic is hypothesis-specific:
#       - H1: asymmetry  z = (delta_3 - delta_1) / SE,  1-sided positive
#             (sentiment amplifies convexity at the winner end)
#       - H2: main effect z on delta_1,  1-sided LEFT-tail p (testing
#             disposition prediction delta_1 < 0).  A high p paired with
#             positive z is evidence AGAINST disposition and FOR the
#             Brunnermeier-Pedersen margin-call alternative.
#       - H3: 1-sided positive z on the ActSkew x SENT interaction delta
#             (sentiment amplifies lottery demand if delta > 0)
#       - H4: 1-sided positive z on the ExpRatio x SENT interaction
#             delta_F (sentiment relaxes fee discipline if delta_F > 0)
#
#   * Detailed interpretation of each test is left to the dissertation
#     prose (Sections 5.2-5.5).
# =============================================================================

suppressPackageStartupMessages({
  library(dplyr)
  library(kableExtra)
})

# --- 0. Config ---------------------------------------------------------------
if (!exists("WORKING_DIR")) WORKING_DIR <- getwd()
OUTPUT_SUMMARY <- file.path(WORKING_DIR, "table_behavioral_summary.tex")

# --- 1. Load models from session or RDS --------------------------------------
load_models <- function(name) {
  if (exists(name, envir = .GlobalEnv)) {
    return(get(name, envir = .GlobalEnv))
  }
  rds <- file.path(WORKING_DIR, paste0(name, ".rds"))
  if (file.exists(rds)) {
    cat(sprintf("Loaded %s from %s\n", name, rds))
    return(readRDS(rds))
  }
  warning(sprintf("%s not found in env or as %s; row(s) will be blank.",
                  name, rds))
  NULL
}
H1_models <- load_models("H1_models")
H2_models <- load_models("H2_models")
H3_models <- load_models("H3_models")
H4_models <- load_models("H4_models")

# --- 2. Helpers --------------------------------------------------------------
fmt_p <- function(p) {
  if (is.null(p) || is.na(p)) return("--")
  if (p < 0.001) "$<\\!0.001$" else formatC(p, format = "f", digits = 3)
}
fmt_chi2 <- function(chi, p, df) {
  if (is.null(chi) || is.na(chi)) return("--")
  sprintf("%.2f (df=%d, p=%s)", chi, df, fmt_p(p))
}
fmt_z <- function(z, p) {
  if (is.null(z) || is.na(z)) return("--")
  sprintf("%+.2f (p=%s)", z, fmt_p(p))
}
fmt_n <- function(n) {
  if (is.null(n) || is.na(n)) return("--")
  formatC(n, format = "d", big.mark = ",")
}

# Joint test: rejects "no state dependence" (see footnote for the terms) at 10%.
joint_tag <- function(joint_p) {
  if (is.null(joint_p) || is.na(joint_p)) return("--")
  if (joint_p < 0.10) "Rejected" else "Not rejected"
}
# Direction: tagged only when the one-sided p in a given direction is < 0.10.
# p_pred = one-sided p in the behaviourally predicted direction; the opposite
# tail is 1 - p_pred.
direction_tag <- function(p_pred, lbl_pred, lbl_opp) {
  if (is.null(p_pred) || is.na(p_pred)) return("--")
  if (p_pred < 0.10)       lbl_pred
  else if (1 - p_pred < 0.10) lbl_opp
  else                     "Not significant"
}
add_row <- function(hyp, state, chi, p_joint, df, z, p_pred, n, lbl_pred, lbl_opp) {
  c(hyp, state, fmt_chi2(chi, p_joint, df), fmt_z(z, p_pred), fmt_n(n),
    joint_tag(p_joint), direction_tag(p_pred, lbl_pred, lbl_opp))
}

# --- 3. Build summary rows ---------------------------------------------------
rows <- list()

# ---- H1: Sentiment-Convexity ----
# Headline: asymmetry z = (delta_3 - delta_1)/SE; predicted sign positive.
if (!is.null(H1_models)) {
  pri <- H1_models$primary
  n1 <- nobs(pri$m2)
  for (key in c("t2", "t3", "t4")) {
    tt <- pri$tests[[key]]
    state_lbl <- switch(key,
      "t2" = "$D^{\\text{SENT}}$",
      "t3" = "$\\text{SENT}^\\perp$",
      "t4" = "$D^{\\text{AAII}}$")
    rows[[length(rows) + 1L]] <- add_row(
      "H1: Sent.-Convexity", state_lbl, tt$joint_chi2, tt$joint_p, 3L,
      tt$asym_z, tt$asym_p_1sd, n1,
      "Steeper (asym.\\ $+$)", "Flatter (asym.\\ $-$)")
  }
}

# ---- H2: Disposition / Illusion-of-Control ----
# Headline: delta_1 z; predicted (disposition) sign negative, so p_pred is
# the LEFT tail; the right tail 1 - p_pred is the Brunnermeier-Pedersen
# margin-call alternative.
if (!is.null(H2_models)) {
  pri <- H2_models$primary
  for (key in c("t2", "t3", "t4")) {
    tt <- pri$tests[[key]]
    if (is.null(tt)) next
    state_lbl <- switch(key,
      "t2" = "$D^{\\text{MD,Det}}$",
      "t3" = "$D^{\\text{INV-PCR}}$",
      "t4" = "Discriminant")
    # model for test tK is mK (t2 -> m2, t3 -> m3, t4 -> m4)
    n_used <- if (!is.null(tt$n)) tt$n else nobs(pri[[sub("^t", "m", key)]])
    rows[[length(rows) + 1L]] <- add_row(
      "H2: Disp./IoC", state_lbl, tt$joint_chi2, tt$joint_p, 3L,
      tt$d1_z, tt$main_p_1sd, n_used,
      "Disposition ($\\delta_{\\text{LOW}}<0$)", "Margin-call ($\\delta_{\\text{LOW}}>0$)")
  }
}

# ---- H3: Lottery Demand ----
# Headline: Lottery x D_SENT interaction; predicted sign positive.
# t2 = ActR2 is the rational-null control (activeness, not a lottery feature).
if (!is.null(H3_models)) {
  pri <- H3_models$primary
  n3 <- nobs(pri$m2)
  for (key in c("t2", "t3", "t4")) {
    tt <- pri$tests[[key]]
    state_lbl <- switch(key,
      "t2" = "ActR2 (rational-null control)",
      "t3" = "ActSkew",
      "t4" = "MAX12")
    rows[[length(rows) + 1L]] <- add_row(
      "H3: Lottery", state_lbl, tt$joint_chi2, tt$joint_p, 2L,
      tt$delta_z, tt$delta_p_1sd, n3,
      "Amplified ($\\delta>0$)", "Dampened ($\\delta<0$)")
  }
}

# ---- H4: Fee Elasticity ----
# Headline: ExpRatio x SENT; predicted sign positive (fee discipline relaxes).
if (!is.null(H4_models)) {
  pri <- H4_models$primary
  n4 <- nobs(pri$m2)
  for (key in c("t2", "t3", "t4")) {
    tt <- pri$tests[[key]]
    state_lbl <- switch(key,
      "t2" = "$D^{\\text{SENT}}$",
      "t3" = "$\\text{SENT}^\\perp$",
      "t4" = "$D^{\\text{AAII}}$")
    df_used <- if (!is.null(tt$df)) tt$df else 4L
    rows[[length(rows) + 1L]] <- add_row(
      "H4: Fee Elast.", state_lbl, tt$joint_chi2, tt$joint_p, df_used,
      tt$delta_z, tt$delta_p_1sd, n4,
      "Relaxed ($\\eta>0$)", "Tightened ($\\eta<0$)")
  }
}

if (length(rows) == 0L) {
  stop("No hypothesis test results available. Run H1_*..H4_* scripts first.")
}

summary_df <- as.data.frame(do.call(rbind, rows), stringsAsFactors = FALSE)
colnames(summary_df) <- c("Hypothesis", "State proxy", "Joint $\\chi^2$",
                          "Headline $z$ ($p$, predicted sign)", "$N$",
                          "Joint test", "Headline direction")
rownames(summary_df) <- NULL

# --- 4. Build kableExtra table ------------------------------------------------
caption <- "Behavioral Hypotheses --- Summary of Primary-Spec Test Statistics"
fn <- paste0(
  "Joint $\\\\chi^2$: Wald test of the hypothesis's state-dependent terms, whose ",
  "null is no state dependence: H1, the three rank-segment $\\\\times$ state ",
  "interactions; H2, the three rank-segment $\\\\times$ margin-debt (or put--call) ",
  "interactions, with the $D^{\\\\text{SENT}}$ interactions as controls in the ",
  "discriminant row; H3, the lottery measure and its interaction with ",
  "$D^{\\\\text{SENT}}$; H4, the four fee $\\\\times$ sentiment terms. ``Joint test'' ",
  "reports whether it is rejected at 10\\\\%. A rejection shows that flows depend ",
  "on the state; it does not by itself identify a psychological cause. Headline $z$: hypothesis-specific statistic (H1: ",
  "$\\\\delta_{\\\\text{HIGH}}-\\\\delta_{\\\\text{LOW}}$; H2: $\\\\delta_{\\\\text{LOW}}$; H3: lottery $\\\\times$ ",
  "$D^{\\\\text{SENT}}$; H4: $\\\\eta$) with its one-sided $p$-value in the ",
  "behaviourally predicted direction. ``Headline direction'' names a direction ",
  "only when the one-sided $p$-value in that direction is below 0.10 (for the ",
  "opposite direction, $1-p$); otherwise ``Not significant''. For H2 the ",
  "predicted sign is negative (disposition), and the opposite sign is the ",
  "margin-call alternative of Brunnermeier and Pedersen (2009). The ActR2 row ",
  "of H3 is a rational-null control: activeness is not a lottery feature. ",
  "Each hypothesis is estimated on its own aligned sample (all regressors of its ",
  "specifications non-missing), so $N$ differs across hypotheses; the H2 ",
  "put--call rows use the shorter put--call ratio window. Primary ",
  "specifications use contemporaneous state variables, fund FE for H1--H3 and ",
  "Lipper $\\\\times$ year-month FE for H4, with two-way clustering on Ticker and ",
  "calendar month (Petersen 2009). Coefficients are reported in the underlying tables."
)

# Phase 2.8 (19 May 2026): Extract tablenotes from threeparttable and re-emit
# IMMEDIATELY BEFORE \end{table} (inside the float). See same patch in
# psychological_premium.R for rationale.
threeparttable_note_after_compact <- function(s) {
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
  # Phase 2.9: `\centering` from kableExtra centers notes paragraph; wrap in
  # brace group with `\raggedright` to force left alignment of notes only.
  sub("\\end{table}",
      paste0("{\\raggedright\\noindent\\footnotesize\n",
             "\\begin{singlespace}\n", ni, "\n",
             "\\end{singlespace}\\par}\n",
             "\\end{table}"),
      s, fixed = TRUE)
}

ktab <- kbl(
  summary_df, format = "latex", booktabs = TRUE,
  caption = caption, label = "behavioral_summary",
  align = c("l", "l", "l", "l", "r", "l", "l"),
  escape = FALSE, linesep = ""
) %>%
  kable_styling(latex_options = c("hold_position", "scale_down")) %>%
  footnote(general = fn, general_title = "",
           threeparttable = TRUE, escape = FALSE)

tex <- as.character(ktab)
tex <- gsub("\\begin{table}[!h]", "\\begin{table}[H]", tex, fixed = TRUE)
tex <- threeparttable_note_after_compact(tex)  # PHASE B: move note outside float
writeLines(tex, OUTPUT_SUMMARY)
cat(sprintf("Wrote %s\n", OUTPUT_SUMMARY))

# --- 5. Console echo ---------------------------------------------------------
cat("\n========================== Behavioral Summary =============================\n")
print(summary_df, row.names = FALSE)
cat("===========================================================================\n")

assign("behavioral_summary_df", summary_df, envir = .GlobalEnv)
