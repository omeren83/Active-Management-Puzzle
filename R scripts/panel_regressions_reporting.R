# panel_regressions_reporting.R                                        v1.1
# =============================================================================
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
#   * The "Conclusion" column flags the direction of the headline result.
#     The detailed interpretation of each test is left to the dissertation
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

# Decision tag from joint p and a directional signal. "Reject" if joint p
# crosses a 10% threshold; appended interpretation depends on the
# hypothesis-specific direction stored in `dir_tag`.
decision <- function(joint_p, dir_tag) {
  if (is.null(joint_p) || is.na(joint_p)) return("--")
  base <- if (joint_p < 0.10) "Reject" else "Fail to reject"
  if (!is.null(dir_tag) && nzchar(dir_tag)) paste0(base, " (", dir_tag, ")")
  else base
}

# --- 3. Build summary rows ---------------------------------------------------
rows <- list()

# ---- H1: Sentiment-Convexity ----
if (!is.null(H1_models)) {
  pri <- H1_models$primary
  n1 <- nobs(pri$m2)
  for (key in c("t2", "t3", "t4")) {
    tt <- pri$tests[[key]]
    state_lbl <- switch(
      key,
      "t2" = "$D^{SENT}$",
      "t3" = "$\\text{SENT}^\\perp$",
      "t4" = "$D^{AAII}$"
    )
    dir_tag <- if (!is.na(tt$asym_z) && tt$asym_z > 0) "asym +" else "asym -"
    rows[[length(rows) + 1L]] <- c(
      "H1: Sent.-Convexity",
      state_lbl,
      fmt_chi2(tt$joint_chi2, tt$joint_p, df = 3L),
      fmt_z(tt$asym_z, tt$asym_p_1sd),
      fmt_n(n1),
      decision(tt$joint_p, dir_tag)
    )
  }
}

# ---- H2: Disposition / Illusion-of-Control ----
# Headline test: delta_1 z, 1-sided LEFT tail (disposition prediction).
# Positive z with high left-tail p = BP margin-call alternative.
if (!is.null(H2_models)) {
  pri <- H2_models$primary
  for (key in c("t2", "t3", "t4")) {
    tt <- pri$tests[[key]]
    if (is.null(tt)) next
    state_lbl <- switch(
      key,
      "t2" = "$D^{MD,Det}$",
      "t3" = "$D^{\\text{INV-PCR}}$",
      "t4" = "Discriminant"
    )
    n_used <- if (!is.null(tt$n)) tt$n else nobs(pri$m2)
    dir_tag <- if (is.na(tt$d1_z)) "" else
               if (tt$d1_z > 0) "BP margin-call (delta_1 > 0)"
               else "Disposition (delta_1 < 0)"
    rows[[length(rows) + 1L]] <- c(
      "H2: Disp./IoC",
      state_lbl,
      fmt_chi2(tt$joint_chi2, tt$joint_p, df = 3L),
      fmt_z(tt$d1_z, tt$main_p_1sd),
      fmt_n(n_used),
      decision(tt$joint_p, dir_tag)
    )
  }
}

# ---- H3: Lottery Demand ----
# Headline: 1-sided positive z on Lottery x D_SENT interaction.
# H3 v2.0 varies lottery MEASURE across columns (not sentiment proxy):
# t2=ActR2 (rational-channel null), t3=ActSkew, t4=MAX12.
if (!is.null(H3_models)) {
  pri <- H3_models$primary
  n3 <- nobs(pri$m2)
  for (key in c("t2", "t3", "t4")) {
    tt <- pri$tests[[key]]
    state_lbl <- switch(
      key,
      "t2" = "ActR2",
      "t3" = "ActSkew",
      "t4" = "MAX12"
    )
    dir_tag <- if (is.na(tt$delta_z)) "" else
               if (tt$delta_z > 0) "lottery amp +" else "lottery amp -"
    rows[[length(rows) + 1L]] <- c(
      "H3: Lottery",
      state_lbl,
      fmt_chi2(tt$joint_chi2, tt$joint_p, df = 2L),
      fmt_z(tt$delta_z, tt$delta_p_1sd),
      fmt_n(n3),
      decision(tt$joint_p, dir_tag)
    )
  }
}

# ---- H4: Fee Elasticity ----
# Headline: 1-sided positive z on ExpRatio x SENT.
if (!is.null(H4_models)) {
  pri <- H4_models$primary
  n4 <- nobs(pri$m2)
  for (key in c("t2", "t3", "t4")) {
    tt <- pri$tests[[key]]
    state_lbl <- switch(
      key,
      "t2" = "$D^{SENT}$",
      "t3" = "$\\text{SENT}^\\perp$",
      "t4" = "$D^{AAII}$"
    )
    df_used <- if (!is.null(tt$df)) tt$df else 4L
    dir_tag <- if (is.na(tt$delta_z)) "" else
               if (tt$delta_z > 0) "fee amp +" else "fee amp -"
    rows[[length(rows) + 1L]] <- c(
      "H4: Fee Elast.",
      state_lbl,
      fmt_chi2(tt$joint_chi2, tt$joint_p, df = df_used),
      fmt_z(tt$delta_z, tt$delta_p_1sd),
      fmt_n(n4),
      decision(tt$joint_p, dir_tag)
    )
  }
}

if (length(rows) == 0L) {
  stop("No hypothesis test results available. Run H1_*..H4_* scripts first.")
}

summary_df <- as.data.frame(do.call(rbind, rows), stringsAsFactors = FALSE)
colnames(summary_df) <- c("Hypothesis", "State proxy", "Joint $\\chi^2$",
                          "Headline $z$ (1-sided $p$)", "$N$", "Decision")
rownames(summary_df) <- NULL

# --- 4. Build kableExtra table ------------------------------------------------
caption <- paste0(
  "Behavioral Hypotheses --- Summary of Primary-Spec Test Statistics. ",
  "Joint $\\chi^2$ tests the null that all rank-segment $\\times$ state ",
  "interactions are zero (H1, H2; df=3), the null that ActSkew enters ",
  "with neither main effect nor sentiment modulation (H3; df=2), or the ",
  "null that the four fee-amplified-sentiment terms are zero (H4; df=4). ",
  "The headline $z$ is hypothesis-specific: H1 reports the ",
  "$\\delta_3-\\delta_1$ asymmetry (1-sided positive); H2 reports $\\delta_1$ ",
  "with a 1-sided LEFT-tail $p$ (disposition prediction) --- positive ",
  "$\\delta_1$ paired with high $p$ rejects disposition in favour of the ",
  "Brunnermeier-Pedersen (2009) margin-call alternative; H3 and H4 report ",
  "the focal interaction with 1-sided positive $p$. Detailed coefficients ",
  "and timing-/FE-robustness checks appear in Tables ",
  "\\ref{tab:H1_regression}--\\ref{tab:H4_regression} and Appendix F."
)
fn <- paste0(
  "Stars on individual coefficients are reported in the underlying tables. ",
  "All specifications use the primary-spec aligned sample (contemporaneous ",
  "state variables; fund FE for H1--H3; Lipper $\\times$ yearmo FE for H4) ",
  "with two-way clustering on Ticker and calendar month (Petersen 2009)."
)

ktab <- kbl(
  summary_df, format = "latex", booktabs = TRUE,
  caption = caption, label = "behavioral_summary",
  align = c("l", "l", "l", "l", "r", "l"),
  escape = FALSE, linesep = ""
) %>%
  kable_styling(latex_options = c("hold_position", "scale_down")) %>%
  footnote(general = fn, general_title = "",
           threeparttable = TRUE, escape = FALSE)

tex <- as.character(ktab)
tex <- gsub("\\begin{table}[!h]", "\\begin{table}[H]", tex, fixed = TRUE)
writeLines(tex, OUTPUT_SUMMARY)
cat(sprintf("Wrote %s\n", OUTPUT_SUMMARY))

# --- 5. Console echo ---------------------------------------------------------
cat("\n========================== Behavioral Summary =============================\n")
print(summary_df, row.names = FALSE)
cat("===========================================================================\n")

assign("behavioral_summary_df", summary_df, envir = .GlobalEnv)
