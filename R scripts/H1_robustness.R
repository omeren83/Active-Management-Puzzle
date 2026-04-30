# H1_robustness.R                                                       v1.0
# =============================================================================
# Robustness check for H1: re-estimates the same four sentiment specifications
# under a different identification strategy.
#
#   PRIMARY  (H1_sentiment_convexity.R): fund FE, no time FE.
#       -> within-fund identification of delta_1, delta_2, delta_3.
#       -> sentiment level effect lambda is identified.
#       -> time-invariant controls (ExpRatio, LoadDummy, Turnover)
#          absorbed by alpha_i, not separately reported.
#
#   ROBUST   (this script):  Lipper_Category x yearmo two-way FE, no fund FE.
#       -> within-style-month identification (a la Cheng et al. 2025,
#          Berk & van Binsbergen 2015).
#       -> mu_{c(i),t} absorbs aggregate market shocks, style-baseline
#          shocks, and the sentiment level lambda (since Sent_t is purely
#          time-varying and is constant within any yearmo cell).
#       -> ExpRatio, LoadDummy, Turnover are now identified because
#          there is no fund FE -> their coefficients appear in the table.
#       -> Interaction terms delta_1, delta_2, delta_3 remain identified
#          (they vary cross-sectionally within each style-month cell
#          because R^LOW, R^MID, R^HIGH vary across funds).
#
# If the primary and robust specifications agree on the sign and
# significance of the delta interactions, H1 is identified independently
# of the choice between fund FE and style-time FE.
#
# REFERENCES:
#   Cheng X. et al. (2025). Financial Management 54(3).
#   Berk J.B. & van Binsbergen J.H. (2015). JFE 118(1).
#   Petersen M.A. (2009). RFS 22(1).
#
# Dependencies: dplyr, fixest, kableExtra
# =============================================================================

suppressPackageStartupMessages({
  library(dplyr)
  library(fixest)
  library(kableExtra)
})

# --- 0. Config ---------------------------------------------------------------
if (!exists("WORKING_DIR")) WORKING_DIR <- getwd()
OUTPUT_TEX <- file.path(WORKING_DIR, "table_H1_robustness.tex")
STAR_THR   <- c(`***` = 2.576, `**` = 1.960, `*` = 1.645)

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

# --- 2. Build estimation sample (matches H1 primary sample) -----------------
core_rhs <- c("flow", "R_LOW", "R_MID", "R_HIGH",
              "log_TNA", "log_Age", "ExpRatio", "LoadDummy",
              "ret_vol", "Turnover", "style_flow_lag",
              "D_SENT", "SENT_ORTH", "D_AAII")

samp <- panel_reg %>%
  filter(!is_december) %>%
  filter(if_all(all_of(core_rhs), ~ !is.na(.))) %>%
  mutate(
    SENT_ORTH_z = as.numeric(scale(SENT_ORTH)),
    D_SENT      = as.numeric(D_SENT),
    D_AAII      = as.numeric(D_AAII),
    Ticker      = as.factor(Ticker),
    yearmo      = as.factor(yearmo),
    Lipper_Category = as.factor(Lipper_Category)
  )

cat(sprintf(
  "\nEstimation sample: %d fund-months | %d funds | %d months | %d styles\n",
  nrow(samp), nlevels(samp$Ticker), nlevels(samp$yearmo),
  nlevels(samp$Lipper_Category)
))

# --- 3. Estimate four specifications -----------------------------------------
# In fixest, "A^B" means the two-way interaction (one dummy per unique
# combination of A and B). Lipper_Category^yearmo absorbs all aggregate
# shocks and style-baseline shocks within each style-month cell, AND
# absorbs the sentiment level lambda (since sentiment is constant within
# any single yearmo).
# Static controls (ExpRatio, LoadDummy, Turnover) are now identified
# because we don't have fund FE -- they do not appear in the formula's
# FE part and so their coefficients will be reported.
controls <- c("log_TNA", "log_Age", "ExpRatio", "LoadDummy",
              "ret_vol", "Turnover", "style_flow_lag")
ctrl_str <- paste(controls, collapse = " + ")
fe_part  <- "Lipper_Category^yearmo"

f1 <- as.formula(paste("flow ~ R_LOW + R_MID + R_HIGH +", ctrl_str,
                       "|", fe_part))
f2 <- as.formula(paste("flow ~ R_LOW + R_MID + R_HIGH",
                       "+ R_LOW:D_SENT + R_MID:D_SENT + R_HIGH:D_SENT +",
                       ctrl_str, "|", fe_part))
f3 <- as.formula(paste("flow ~ R_LOW + R_MID + R_HIGH",
                       "+ R_LOW:SENT_ORTH_z + R_MID:SENT_ORTH_z",
                       "+ R_HIGH:SENT_ORTH_z +",
                       ctrl_str, "|", fe_part))
f4 <- as.formula(paste("flow ~ R_LOW + R_MID + R_HIGH",
                       "+ R_LOW:D_AAII + R_MID:D_AAII + R_HIGH:D_AAII +",
                       ctrl_str, "|", fe_part))
# Note: state main effects (D_SENT, SENT_ORTH_z, D_AAII) are absorbed by
# Lipper_Category^yearmo so they are not included as RHS variables.

cat("Estimating Col (1) Baseline (style x time FE)...\n")
m1 <- feols(f1, data = samp, cluster = ~ Ticker + yearmo)
cat("Estimating Col (2) D_SENT (style x time FE)...\n")
m2 <- feols(f2, data = samp, cluster = ~ Ticker + yearmo)
cat("Estimating Col (3) SENT_ORTH continuous (style x time FE)...\n")
m3 <- feols(f3, data = samp, cluster = ~ Ticker + yearmo)
cat("Estimating Col (4) D_AAII (style x time FE)...\n")
m4 <- feols(f4, data = samp, cluster = ~ Ticker + yearmo)

# --- 4. Hypothesis tests -----------------------------------------------------
test_h1 <- function(m, sent_label) {
  b <- coef(m); V <- vcov(m)
  i_low  <- which(names(b) == paste0("R_LOW:",  sent_label))
  i_mid  <- which(names(b) == paste0("R_MID:",  sent_label))
  i_high <- which(names(b) == paste0("R_HIGH:", sent_label))
  if (length(i_low) == 0) {
    i_low  <- which(names(b) == paste0(sent_label, ":R_LOW"))
    i_mid  <- which(names(b) == paste0(sent_label, ":R_MID"))
    i_high <- which(names(b) == paste0(sent_label, ":R_HIGH"))
  }

  pos <- c(i_low, i_mid, i_high)
  bs  <- b[pos]; Vs <- V[pos, pos]
  chi2 <- as.numeric(t(bs) %*% solve(Vs) %*% bs)
  p_joint <- pchisq(chi2, df = length(pos), lower.tail = FALSE)

  diff_b  <- as.numeric(b[i_high]) - as.numeric(b[i_low])
  diff_se <- sqrt(as.numeric(V[i_high, i_high]) + as.numeric(V[i_low, i_low])
                  - 2 * as.numeric(V[i_high, i_low]))
  z_asym  <- diff_b / diff_se
  p_asym_1sd <- pnorm(z_asym, lower.tail = FALSE)

  list(joint_chi2 = chi2, joint_p = p_joint,
       asym_diff  = diff_b, asym_z = z_asym,
       asym_p_1sd = p_asym_1sd)
}

t2 <- test_h1(m2, "D_SENT")
t3 <- test_h1(m3, "SENT_ORTH_z")
t4 <- test_h1(m4, "D_AAII")

cat("\n--- H1 robustness test results (Lipper x yearmo FE) ---\n")
for (nm_pair in list(c("D_SENT (col 2)", "t2"),
                     c("SENT_ORTH cont. (col 3)", "t3"),
                     c("D_AAII (col 4)", "t4"))) {
  tt <- get(nm_pair[2])
  cat(sprintf(
    "%-25s  joint chi2(3) = %6.2f (p=%.4f)  | delta_3-delta_1 = %+.4f, z=%+.2f (1-sd p=%.4f)\n",
    nm_pair[1], tt$joint_chi2, tt$joint_p,
    tt$asym_diff, tt$asym_z, tt$asym_p_1sd))
}

# --- 5. Build LaTeX table ----------------------------------------------------
row_specs <- list(
  list(coef = "R_LOW",    label = "$R^{LOW}$"),
  list(coef = "R_MID",    label = "$R^{MID}$"),
  list(coef = "R_HIGH",   label = "$R^{HIGH}$"),
  list(coef = "R_LOW:S",  label = "$R^{LOW}\\times$ Sent ($\\delta_1$)"),
  list(coef = "R_MID:S",  label = "$R^{MID}\\times$ Sent ($\\delta_2$)"),
  list(coef = "R_HIGH:S", label = "$R^{HIGH}\\times$ Sent ($\\delta_3$)"),
  list(coef = "log_TNA",        label = "$\\log(\\text{TNA})$"),
  list(coef = "log_Age",        label = "$\\log(\\text{Age})$"),
  list(coef = "ExpRatio",       label = "Expense ratio"),
  list(coef = "LoadDummy",      label = "Load dummy"),
  list(coef = "ret_vol",        label = "Return vol.\\ (36m SD)"),
  list(coef = "Turnover",       label = "Turnover"),
  list(coef = "style_flow_lag", label = "Style flow")
)

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

sent_var <- list(m1 = NULL, m2 = "D_SENT",
                 m3 = "SENT_ORTH_z", m4 = "D_AAII")

resolve_coef_name <- function(coef_pat, sv) {
  if (startsWith(coef_pat, "R_") && grepl(":S$", coef_pat)) {
    if (is.null(sv)) return(NA_character_)
    return(paste0(sub(":S$", "", coef_pat), ":", sv))
  }
  coef_pat
}

# Robust coef extraction (named-vector-trap-safe)
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

mods       <- list(m1 = m1, m2 = m2, m3 = m3, m4 = m4)
mod_keys   <- c("m1", "m2", "m3", "m4")
body_rows  <- list()
for (spec in row_specs) {
  vals <- vector("list", length(mod_keys))
  for (j in seq_along(mod_keys)) {
    sv  <- sent_var[[mod_keys[j]]]
    cn  <- resolve_coef_name(spec$coef, sv)
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

# Footer
fmt_p <- function(p) {
  if (is.na(p)) return("")
  if (p < 0.001) "$<\\!0.001$"
  else formatC(p, format = "f", digits = 3)
}

fe_row    <- c("FE: Lipper $\\times$ yearmo", rep("Yes", 4))
clust_row <- c("Cluster (Ticker, yearmo)",     rep("Yes", 4))
n_row     <- c("$N$",
               formatC(nobs(m1), format="d", big.mark=","),
               formatC(nobs(m2), format="d", big.mark=","),
               formatC(nobs(m3), format="d", big.mark=","),
               formatC(nobs(m4), format="d", big.mark=","))
r2_row    <- c("$R^2$ (within)",
               fmt_num(r2(m1, "wr2"), 3),
               fmt_num(r2(m2, "wr2"), 3),
               fmt_num(r2(m3, "wr2"), 3),
               fmt_num(r2(m4, "wr2"), 3))

joint_row <- c("Joint $\\delta_1=\\delta_2=\\delta_3=0$ ($\\chi^2_3$, $p$)",
               "--",
               sprintf("%.2f (%s)", t2$joint_chi2, fmt_p(t2$joint_p)),
               sprintf("%.2f (%s)", t3$joint_chi2, fmt_p(t3$joint_p)),
               sprintf("%.2f (%s)", t4$joint_chi2, fmt_p(t4$joint_p)))
asym_row  <- c("Asym $\\delta_3-\\delta_1$ ($z$, 1-sided $p$)",
               "--",
               sprintf("%+.4f (z=%+.2f, p=%s)", t2$asym_diff, t2$asym_z,
                       fmt_p(t2$asym_p_1sd)),
               sprintf("%+.4f (z=%+.2f, p=%s)", t3$asym_diff, t3$asym_z,
                       fmt_p(t3$asym_p_1sd)),
               sprintf("%+.4f (z=%+.2f, p=%s)", t4$asym_diff, t4$asym_z,
                       fmt_p(t4$asym_p_1sd)))

footer_df <- rbind(fe_row, clust_row, n_row, r2_row, joint_row, asym_row)
colnames(footer_df) <- c("Variable", "(1)", "(2)", "(3)", "(4)")
rownames(footer_df) <- NULL

full_df <- rbind(body_df, footer_df)
rownames(full_df) <- NULL

cap <- paste0(
  "H1 Robustness: Sentiment-Convexity Hypothesis under Style $\\times$ Time ",
  "fixed effects. Same four sentiment specifications as Table~",
  "\\ref{tab:H1_regression}, but with Lipper-category $\\times$ yearmo two-",
  "way fixed effects in place of fund fixed effects. Identification shifts ",
  "from within-fund to within-style-month variation."
)
footnote_text <- paste0(
  "Same sample, dependent variable, and rank construction as ",
  "Table~\\ref{tab:H1_regression}. The fixed-effect strategy here absorbs ",
  "all aggregate market shocks and style-baseline shocks within each ",
  "Lipper-category $\\times$ yearmo cell (a la Cheng et al.\\ 2025). The ",
  "sentiment main effect $\\lambda$ is absorbed by the time component of ",
  "the FE and so does not appear in the table. Time-invariant controls ",
  "(expense ratio, load dummy, turnover) are now identified because there ",
  "is no fund FE. The interaction terms $\\delta_1, \\delta_2, \\delta_3$ ",
  "remain identified through within-style-month cross-sectional variation ",
  "in performance ranks. If the primary specification ",
  "(Table~\\ref{tab:H1_regression}, fund FE) and this robustness ",
  "specification agree on the sign and significance of $\\delta_3$ and the ",
  "asymmetry contrast, H1 is robust to identification strategy. Standard ",
  "errors two-way clustered on Ticker and calendar month (Petersen 2009). ",
  "Stars: $^{*}\\,p<0.10$, $^{**}\\,p<0.05$, $^{***}\\,p<0.01$."
)

ktab <- kbl(
  full_df,
  format    = "latex",
  booktabs  = TRUE,
  caption   = cap,
  label     = "H1_robustness",
  align     = c("l", rep("r", 4)),
  escape    = FALSE,
  linesep   = ""
) %>%
  kable_styling(latex_options = c("hold_position", "scale_down")) %>%
  add_header_above(c(" " = 1, "Baseline" = 1, "$D^{SENT}$" = 1,
                     "$SENT^{\\\\perp}$" = 1, "$D^{AAII}$" = 1),
                   escape = FALSE) %>%
  row_spec(nrow(body_df), hline_after = TRUE) %>%
  footnote(general = footnote_text, general_title = "",
           threeparttable = TRUE, escape = FALSE)

tex <- as.character(ktab)
tex <- gsub("\\begin{table}[!h]", "\\begin{table}[H]", tex, fixed = TRUE)
writeLines(tex, OUTPUT_TEX)
cat(sprintf("\nWrote %s\n", OUTPUT_TEX))

# --- 6. Expose objects for the reporting script ------------------------------
H1_robust_models <- list(baseline = m1, dsent = m2, sent_cont = m3, daaii = m4)
H1_robust_tests  <- list(D_SENT = t2, SENT_ORTH_z = t3, D_AAII = t4)
assign("H1_robust_models", H1_robust_models, envir = .GlobalEnv)
assign("H1_robust_tests",  H1_robust_tests,  envir = .GlobalEnv)
