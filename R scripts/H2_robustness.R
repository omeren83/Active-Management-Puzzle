# H2_robustness.R                                                       v1.1
# =============================================================================
# Robustness check for H2: re-estimates the same four margin-debt
# specifications under Lipper_Category x yearmo two-way FE in place of fund FE.
#
#   PRIMARY  (H2_disposition_control.R): fund FE, no time FE.
#       -> within-fund identification of delta_1, delta_2, delta_3.
#       -> margin-debt level effect lambda is identified.
#       -> time-invariant controls absorbed by alpha_i.
#
#   ROBUST   (this script):  Lipper_Category x yearmo two-way FE, no fund FE.
#       -> within-style-month identification (a la Cheng et al. 2025).
#       -> mu_{c(i),t} absorbs aggregate flow-flood and risk-on confounders,
#          AND the level effect of every state variable (since each State_t
#          is purely time-varying and is constant within any yearmo cell).
#          State main-effect rows therefore appear blank.
#       -> ExpRatio, LoadDummy, Turnover are now identified.
#       -> Interaction terms delta_1, delta_2, delta_3 remain identified
#          through within-style-month variation in performance ranks.
#
# Four-column structure (v1.1, condensed from v1.0's six columns):
#   (1) Baseline
#   (2) D_MD_DETREND  (DKP/RRZ detrended)     primary H2 margin-debt test
#   (3) D_INV_PCR                             illusion-of-control via PCR
#   (4) D_MD_DETREND + D_SENT discriminant
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
OUTPUT_TEX <- file.path(WORKING_DIR, "table_H2_robustness.tex")
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

required_md <- c("D_MD_DETREND", "MD_DETREND")
missing_md  <- setdiff(required_md, names(panel_reg))
if (length(missing_md)) {
  stop("panel_reg is missing column(s): ", paste(missing_md, collapse = ", "),
       ". Re-run panel_regressions_setup.R (v1.1+) and behavioral_state_",
       "variables.R (v1.1+).")
}

# --- 2. Build estimation samples ---------------------------------------------
core_rhs <- c("flow", "R_LOW", "R_MID", "R_HIGH",
              "log_TNA", "log_Age", "ExpRatio", "LoadDummy",
              "ret_vol", "Turnover", "style_flow_lag")

samp_md <- panel_reg %>%
  filter(!is_december) %>%
  filter(if_all(all_of(c(core_rhs, "D_MD_DETREND", "SENT_ORTH")),
                ~ !is.na(.))) %>%
  mutate(
    D_MD_DETREND    = as.numeric(D_MD_DETREND),
    D_SENT          = as.numeric(D_SENT),
    Ticker          = as.factor(Ticker),
    yearmo          = as.factor(yearmo),
    Lipper_Category = as.factor(Lipper_Category)
  )

samp_pcr <- panel_reg %>%
  filter(!is_december) %>%
  filter(if_all(all_of(c(core_rhs, "PUT_CALL_RATIO")), ~ !is.na(.))) %>%
  mutate(
    pcr_thr         = quantile(PUT_CALL_RATIO, 1 - 0.66, na.rm = TRUE),
    D_INV_PCR       = as.numeric(PUT_CALL_RATIO <= pcr_thr),
    Ticker          = as.factor(Ticker),
    yearmo          = as.factor(yearmo),
    Lipper_Category = as.factor(Lipper_Category)
  ) %>%
  select(-pcr_thr)

cat(sprintf(
  "\nPrimary sample (cols 1-2, 4): %d fund-months | %d funds | %d months | %d styles\n",
  nrow(samp_md), nlevels(samp_md$Ticker), nlevels(samp_md$yearmo),
  nlevels(samp_md$Lipper_Category)
))
cat(sprintf(
  "PCR sample (col 3): %d fund-months | %d funds | %d months\n",
  nrow(samp_pcr), nlevels(samp_pcr$Ticker), nlevels(samp_pcr$yearmo)
))

# --- 3. Estimate four specifications -----------------------------------------
controls <- c("log_TNA", "log_Age", "ExpRatio", "LoadDummy",
              "ret_vol", "Turnover", "style_flow_lag")
ctrl_str <- paste(controls, collapse = " + ")
fe_part  <- "Lipper_Category^yearmo"

mk_state_formula <- function(state_var) {
  as.formula(paste("flow ~ R_LOW + R_MID + R_HIGH",
                   "+ R_LOW:", state_var,
                   " + R_MID:", state_var,
                   " + R_HIGH:", state_var, " +", ctrl_str,
                   "|", fe_part, sep = ""))
}

f1 <- as.formula(paste("flow ~ R_LOW + R_MID + R_HIGH +", ctrl_str,
                       "|", fe_part))
f2 <- mk_state_formula("D_MD_DETREND")
f3 <- mk_state_formula("D_INV_PCR")
f4 <- as.formula(paste("flow ~ R_LOW + R_MID + R_HIGH",
                       "+ R_LOW:D_MD_DETREND + R_MID:D_MD_DETREND",
                       "+ R_HIGH:D_MD_DETREND",
                       "+ R_LOW:D_SENT + R_MID:D_SENT + R_HIGH:D_SENT +",
                       ctrl_str, "|", fe_part))

cat("Estimating Col (1) Baseline (style x time FE)...\n")
m1 <- feols(f1, data = samp_md,  cluster = ~ Ticker + yearmo)
cat("Estimating Col (2) D_MD_DETREND (style x time FE)...\n")
m2 <- feols(f2, data = samp_md,  cluster = ~ Ticker + yearmo)
cat("Estimating Col (3) D_INV_PCR (style x time FE)...\n")
m3 <- feols(f3, data = samp_pcr, cluster = ~ Ticker + yearmo)
cat("Estimating Col (4) D_MD_DETREND + D_SENT discriminant (style x time FE)...\n")
m4 <- feols(f4, data = samp_md,  cluster = ~ Ticker + yearmo)

# --- 4. Hypothesis tests -----------------------------------------------------
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
  bs  <- b[pos]; Vs <- V[pos, pos]
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

t2 <- test_h2(m2, "D_MD_DETREND")
t3 <- test_h2(m3, "D_INV_PCR")
t4 <- test_h2(m4, "D_MD_DETREND")

cat("\n--- H2 robustness test results (Lipper x yearmo FE) ---\n")
test_results <- list(
  "D_MD_DETREND  (col 2)" = t2,
  "D_INV_PCR     (col 3)" = t3,
  "D_MD_DETR|H1  (col 4)" = t4
)
for (nm in names(test_results)) {
  tt <- test_results[[nm]]
  cat(sprintf(
    "%-22s joint=%6.2f p=%.4f | d1=%+.4f z=%+.2f p_1sd=%.4f | d3-d1=%+.4f z=%+.2f p_1sd=%.4f\n",
    nm, tt$joint_chi2, tt$joint_p,
    tt$d1_b, tt$d1_z, tt$main_p_1sd,
    tt$asym_diff, tt$asym_z, tt$asym_p_1sd))
}

# --- 5. Build LaTeX table ----------------------------------------------------
# State main-effect row omitted (absorbed by FE).
# Time-invariant controls (ExpRatio, LoadDummy, Turnover) ARE displayed.
row_specs <- list(
  list(coef = "R_LOW",          label = "$R^{LOW}$"),
  list(coef = "R_MID",          label = "$R^{MID}$"),
  list(coef = "R_HIGH",         label = "$R^{HIGH}$"),
  list(coef = "R_LOW:STATE",    label = "$R^{LOW}\\times$ State ($\\delta_1$)"),
  list(coef = "R_MID:STATE",    label = "$R^{MID}\\times$ State ($\\delta_2$)"),
  list(coef = "R_HIGH:STATE",   label = "$R^{HIGH}\\times$ State ($\\delta_3$)"),
  list(coef = "R_LOW:D_SENT",   label = "$R^{LOW}\\times D^{SENT}$"),
  list(coef = "R_MID:D_SENT",   label = "$R^{MID}\\times D^{SENT}$"),
  list(coef = "R_HIGH:D_SENT",  label = "$R^{HIGH}\\times D^{SENT}$"),
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

state_var <- list(m1 = NULL, m2 = "D_MD_DETREND",
                  m3 = "D_INV_PCR", m4 = "D_MD_DETREND")
sent_col  <- list(m1 = FALSE, m2 = FALSE, m3 = FALSE, m4 = TRUE)

resolve_coef_name <- function(coef_pat, sv, has_sent) {
  if (startsWith(coef_pat, "R_") && grepl(":STATE$", coef_pat)) {
    if (is.null(sv)) return(NA_character_)
    return(paste0(sub(":STATE$", "", coef_pat), ":", sv))
  }
  if (coef_pat %in% c("R_LOW:D_SENT", "R_MID:D_SENT", "R_HIGH:D_SENT")) {
    if (!has_sent) return(NA_character_)
    return(coef_pat)
  }
  coef_pat
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

mods       <- list(m1 = m1, m2 = m2, m3 = m3, m4 = m4)
mod_keys   <- c("m1", "m2", "m3", "m4")
body_rows  <- list()
for (spec in row_specs) {
  vals <- vector("list", length(mod_keys))
  for (j in seq_along(mod_keys)) {
    sv  <- state_var[[mod_keys[j]]]
    hs  <- sent_col[[mod_keys[j]]]
    cn  <- resolve_coef_name(spec$coef, sv, hs)
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

main_row  <- c("Main $\\delta_1<0$ ($z$, 1-sided $p$)",
               "--",
               sprintf("z=%+.2f (p=%s)", t2$d1_z, fmt_p(t2$main_p_1sd)),
               sprintf("z=%+.2f (p=%s)", t3$d1_z, fmt_p(t3$main_p_1sd)),
               sprintf("z=%+.2f (p=%s)", t4$d1_z, fmt_p(t4$main_p_1sd)))

asym_row  <- c("Asym $\\delta_3-\\delta_1$ ($z$, 1-sided $p$)",
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

cap <- paste0(
  "H2 Robustness: Disposition / Illusion-of-Control Hypothesis under ",
  "Style $\\times$ Time fixed effects. Same four margin-debt and PCR ",
  "specifications as the primary H2 specification, but with ",
  "Lipper-category $\\times$ yearmo two-way fixed effects in place of fund ",
  "fixed effects. Identification shifts from within-fund to within-style-",
  "month variation, absorbing aggregate flow-flood and risk-on confounders."
)
footnote_text <- paste0(
  # threeparttable=TRUE strips ONE backslash; LaTeX commands need quadruple.
  "Sample, dependent variable, rank construction, and column ",
  "specifications identical to Table~\\\\ref{tab:H2_regression}. State main ",
  "effects (and $D^{SENT}$ in col 4) are absorbed by the Lipper ",
  "$\\\\times$ yearmo fixed effects and do not appear in the table; ",
  "time-invariant controls (expense ratio, load dummy, turnover) are now ",
  "identified because there is no fund FE. Standard errors two-way ",
  "clustered on Ticker and calendar month (Petersen 2009). The robustness ",
  "of $\\\\delta^{MD}_1>0$ across both identification strategies points ",
  "toward a margin-call alternative (Brunnermeier \\\\& Pedersen 2009) ",
  "rather than disposition psychology. ",
  "Stars: $^{*}\\\\,p<0.10$, $^{**}\\\\,p<0.05$, $^{***}\\\\,p<0.01$."
)

ktab <- kbl(
  full_df,
  format    = "latex",
  booktabs  = TRUE,
  caption   = cap,
  label     = "H2_robustness",
  align     = c("l", rep("r", 4)),
  escape    = FALSE,
  linesep   = ""
) %>%
  kable_styling(latex_options = c("hold_position", "scale_down")) %>%
  add_header_above(c(" " = 1, "Baseline" = 1, "$D^{MD,Det}$" = 1,
                     "$D^{\\\\text{INV-PCR}}$" = 1,
                     "Discriminant" = 1),
                   escape = FALSE) %>%
  row_spec(nrow(body_df), hline_after = TRUE) %>%
  footnote(general = footnote_text, general_title = "",
           threeparttable = TRUE, escape = FALSE)

tex <- as.character(ktab)
tex <- gsub("\\begin{table}[!h]", "\\begin{table}[H]", tex, fixed = TRUE)
writeLines(tex, OUTPUT_TEX)
cat(sprintf("\nWrote %s\n", OUTPUT_TEX))

# --- 6. Expose objects for the reporting script ------------------------------
H2_robust_models <- list(baseline = m1, dmd_detrend = m2,
                         dpcr_inv = m3, dmd_det_dsent = m4)
H2_robust_tests  <- test_results
assign("H2_robust_models", H2_robust_models, envir = .GlobalEnv)
assign("H2_robust_tests",  H2_robust_tests,  envir = .GlobalEnv)
