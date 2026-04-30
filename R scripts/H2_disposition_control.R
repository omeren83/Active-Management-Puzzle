# H2_disposition_control.R                                              v2.0
# =============================================================================
# v2.0 changes vs v1.0:
#   - Six columns (was five). Margin-debt regime now appears in three
#     specifications matching the literature:
#       Col (2) D_MD_LEVEL    : MD_RATIO > Q_66 -- accumulated commitment
#       Col (3) D_MD          : DMD_YOY > Q_66  -- year-over-year growth
#       Col (4) D_MD_DETREND  : detrended log(MD_RATIO) > Q_66  --
#                               Daniel-Klos-Pollet (2016) / Rapach-
#                               Ringgenberg-Zhou (2016) methodology,
#                               strips the secular trend in margin debt /
#                               market cap (hedge fund AUM expansion etc.).
#                               Methodologically cleanest H2 regime per the
#                               literature.
#       Col (5) D_INV_PCR     : 1 in BOTTOM 34% of CBOE put-call ratio
#                               (proposal Section 4.2: high call-to-put
#                               reflects illusion of control)
#       Col (6) D_MD_DETREND + D_SENT discriminant: 11 RHS additions --
#                               D_MD_DETREND, D_SENT, plus 3+3 rank
#                               interactions. If delta^MD_1 in col (6)
#                               remains negative and significant after
#                               partialing out the H1 sentiment channel,
#                               H2 is identified independently of H1.
#
#   - Pre-commitment to D_MD_DETREND for the discriminant column is on
#     methodological grounds (DKP 2016, RRZ 2016) and is NOT chosen
#     ex post on the basis of which margin-debt regime performs best.
#
#   - Three hypothesis tests per spec (joint, main delta_1<0, asymmetry
#     delta_3>delta_1) follow proposal Section 4.2.
#
# REFERENCES:
#   Langer E.J. (1975). JPSP 32(2). Illusion of control.
#   Thaler R.H. (1980). JEBO 1(1). Toward a positive theory of consumer choice.
#   Daniel K., Klos A., Pollet J. (2016). NYU Stern WP. Margin Credit and
#     Stock Return Predictability.
#   Rapach D.E., Ringgenberg M.C., Zhou G. (2016). JFE 121(1). Short
#     interest and aggregate stock returns.
#   Domian D.L., Racine M.D. (2006). Int Rev Econ Finance 15(2).
#   Petersen M.A. (2009). RFS 22(1).
#   Proposal Sections 4.2, A.8.
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
OUTPUT_TEX <- file.path(WORKING_DIR, "table_H2_regression.tex")
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

# Verify the new columns are present (they must come from setup v1.1)
required_md <- c("D_MD_LEVEL", "D_MD", "D_MD_DETREND", "MD_DETREND")
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

# Primary sample: requires all three margin-debt regimes + sentiment non-NA
# so cols 1-4 and 6 are estimated on identical observations.
samp_md <- panel_reg %>%
  filter(!is_december) %>%
  filter(if_all(all_of(c(core_rhs, "D_MD", "D_MD_LEVEL",
                         "D_MD_DETREND", "SENT_ORTH")), ~ !is.na(.))) %>%
  mutate(
    D_MD         = as.numeric(D_MD),
    D_MD_LEVEL   = as.numeric(D_MD_LEVEL),
    D_MD_DETREND = as.numeric(D_MD_DETREND),
    D_SENT       = as.numeric(D_SENT),
    Ticker       = as.factor(Ticker),
    yearmo       = as.factor(yearmo)
  )

# PCR sample: own (shorter) window 2003-10 to 2019-10.
samp_pcr <- panel_reg %>%
  filter(!is_december) %>%
  filter(if_all(all_of(c(core_rhs, "PUT_CALL_RATIO")), ~ !is.na(.))) %>%
  mutate(
    pcr_thr   = quantile(PUT_CALL_RATIO, 1 - 0.66, na.rm = TRUE),
    D_INV_PCR = as.numeric(PUT_CALL_RATIO <= pcr_thr),
    Ticker    = as.factor(Ticker),
    yearmo    = as.factor(yearmo)
  ) %>%
  select(-pcr_thr)

cat(sprintf(
  "\nPrimary sample (cols 1-4 & 6): %d fund-months | %d funds | %d months\n",
  nrow(samp_md), nlevels(samp_md$Ticker), nlevels(samp_md$yearmo)
))
cat(sprintf(
  "PCR sample (col 5): %d fund-months | %d funds | %d months\n",
  nrow(samp_pcr), nlevels(samp_pcr$Ticker), nlevels(samp_pcr$yearmo)
))

# --- 3. Estimate six specifications ------------------------------------------
controls <- c("log_TNA", "log_Age", "ExpRatio", "LoadDummy",
              "ret_vol", "Turnover", "style_flow_lag")
ctrl_str <- paste(controls, collapse = " + ")

# Helper: build a single-state-variable spec
mk_state_formula <- function(state_var) {
  as.formula(paste("flow ~ R_LOW + R_MID + R_HIGH",
                   "+", state_var,
                   "+ R_LOW:", state_var, " + R_MID:", state_var,
                   " + R_HIGH:", state_var, " +", ctrl_str,
                   "| Ticker", sep = ""))
}

f1 <- as.formula(paste("flow ~ R_LOW + R_MID + R_HIGH +", ctrl_str,
                       "| Ticker"))
f2 <- mk_state_formula("D_MD_LEVEL")
f3 <- mk_state_formula("D_MD")
f4 <- mk_state_formula("D_MD_DETREND")
f5 <- mk_state_formula("D_INV_PCR")
# Discriminant: D_MD_DETREND + D_SENT, both with 3 rank interactions
f6 <- as.formula(paste("flow ~ R_LOW + R_MID + R_HIGH",
                       "+ D_MD_DETREND + D_SENT",
                       "+ R_LOW:D_MD_DETREND + R_MID:D_MD_DETREND",
                       "+ R_HIGH:D_MD_DETREND",
                       "+ R_LOW:D_SENT + R_MID:D_SENT + R_HIGH:D_SENT +",
                       ctrl_str, "| Ticker"))

cat("Estimating Col (1) Baseline...\n")
m1 <- feols(f1, data = samp_md,  cluster = ~ Ticker + yearmo)
cat("Estimating Col (2) D_MD_LEVEL (level)...\n")
m2 <- feols(f2, data = samp_md,  cluster = ~ Ticker + yearmo)
cat("Estimating Col (3) D_MD (growth)...\n")
m3 <- feols(f3, data = samp_md,  cluster = ~ Ticker + yearmo)
cat("Estimating Col (4) D_MD_DETREND (DKP/RRZ detrended)...\n")
m4 <- feols(f4, data = samp_md,  cluster = ~ Ticker + yearmo)
cat("Estimating Col (5) D_INV_PCR (PCR sample)...\n")
m5 <- feols(f5, data = samp_pcr, cluster = ~ Ticker + yearmo)
cat("Estimating Col (6) D_MD_DETREND + D_SENT discriminant...\n")
m6 <- feols(f6, data = samp_md,  cluster = ~ Ticker + yearmo)

# --- 4. Hypothesis tests -----------------------------------------------------
# (a) Joint:    delta_1 = delta_2 = delta_3 = 0  (Wald chi-square, 3 df)
# (b) Main:     delta_1 < 0   (one-sided z-test)
# (c) Asymmetry: delta_3 > delta_1  (one-sided)
test_h2 <- function(m, state_label) {
  b <- coef(m); V <- vcov(m)
  i_low  <- which(names(b) == paste0("R_LOW:",  state_label))
  i_mid  <- which(names(b) == paste0("R_MID:",  state_label))
  i_high <- which(names(b) == paste0("R_HIGH:", state_label))
  # Fallback for reversed naming (fixest sometimes orders A:B as B:A)
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
  p_main_1sd <- pnorm(z_main, lower.tail = TRUE)   # H_a: delta_1 < 0

  diff_b  <- as.numeric(b[i_high]) - as.numeric(b[i_low])
  diff_se <- sqrt(as.numeric(V[i_high, i_high]) + as.numeric(V[i_low, i_low])
                  - 2 * as.numeric(V[i_high, i_low]))
  z_asym  <- diff_b / diff_se
  p_asym_1sd <- pnorm(z_asym, lower.tail = FALSE)  # H_a: delta_3 > delta_1

  list(joint_chi2 = chi2,    joint_p    = p_joint,
       d1_b       = d1_b,    d1_z       = z_main,    main_p_1sd = p_main_1sd,
       asym_diff  = diff_b,  asym_z     = z_asym,    asym_p_1sd = p_asym_1sd)
}

t2 <- test_h2(m2, "D_MD_LEVEL")
t3 <- test_h2(m3, "D_MD")
t4 <- test_h2(m4, "D_MD_DETREND")
t5 <- test_h2(m5, "D_INV_PCR")
t6 <- test_h2(m6, "D_MD_DETREND")  # discriminant: H2 channel only

cat("\n--- H2 hypothesis test results ---\n")
test_results <- list(
  "D_MD_LEVEL    (col 2)" = t2,
  "D_MD growth   (col 3)" = t3,
  "D_MD_DETREND  (col 4)" = t4,
  "D_INV_PCR     (col 5)" = t5,
  "D_MD_DETR|H1  (col 6)" = t6
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
row_specs <- list(
  list(coef = "R_LOW",          label = "$R^{LOW}$"),
  list(coef = "R_MID",          label = "$R^{MID}$"),
  list(coef = "R_HIGH",         label = "$R^{HIGH}$"),
  list(coef = "STATE",          label = "State ($\\lambda$)"),
  list(coef = "R_LOW:STATE",    label = "$R^{LOW}\\times$ State ($\\delta_1$)"),
  list(coef = "R_MID:STATE",    label = "$R^{MID}\\times$ State ($\\delta_2$)"),
  list(coef = "R_HIGH:STATE",   label = "$R^{HIGH}\\times$ State ($\\delta_3$)"),
  list(coef = "D_SENT",         label = "$D^{SENT}$"),
  list(coef = "R_LOW:D_SENT",   label = "$R^{LOW}\\times D^{SENT}$"),
  list(coef = "R_MID:D_SENT",   label = "$R^{MID}\\times D^{SENT}$"),
  list(coef = "R_HIGH:D_SENT",  label = "$R^{HIGH}\\times D^{SENT}$"),
  list(coef = "log_TNA",        label = "$\\log(\\text{TNA})$"),
  list(coef = "log_Age",        label = "$\\log(\\text{Age})$"),
  list(coef = "ret_vol",        label = "Return vol.\\ (36m SD)"),
  list(coef = "style_flow_lag", label = "Style flow")
)

# Helpers
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

# Per-column state-variable name and whether D_SENT terms exist
state_var <- list(m1 = NULL, m2 = "D_MD_LEVEL", m3 = "D_MD",
                  m4 = "D_MD_DETREND", m5 = "D_INV_PCR", m6 = "D_MD_DETREND")
sent_col  <- list(m1 = FALSE, m2 = FALSE, m3 = FALSE,
                  m4 = FALSE, m5 = FALSE, m6 = TRUE)

resolve_coef_name <- function(coef_pat, sv, has_sent) {
  if (coef_pat == "STATE") return(sv)
  if (startsWith(coef_pat, "R_") && grepl(":STATE$", coef_pat)) {
    if (is.null(sv)) return(NA_character_)
    return(paste0(sub(":STATE$", "", coef_pat), ":", sv))
  }
  if (coef_pat %in% c("D_SENT", "R_LOW:D_SENT", "R_MID:D_SENT",
                      "R_HIGH:D_SENT")) {
    if (!has_sent) return(NA_character_)
    return(coef_pat)
  }
  coef_pat
}

# Robust coef extraction (named-vector-trap-safe; tries reversed name)
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

# Build table body
mods       <- list(m1 = m1, m2 = m2, m3 = m3, m4 = m4, m5 = m5, m6 = m6)
mod_keys   <- c("m1", "m2", "m3", "m4", "m5", "m6")
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
colnames(body_df) <- c("Variable", "(1)", "(2)", "(3)", "(4)", "(5)", "(6)")
rownames(body_df) <- NULL

# Footer rows
fmt_p <- function(p) {
  if (is.na(p)) return("")
  if (p < 0.001) "$<\\!0.001$"
  else formatC(p, format = "f", digits = 3)
}

fe_row    <- c("Fund FE", rep("Yes", 6))
clust_row <- c("Cluster (Ticker, yearmo)", rep("Yes", 6))
n_row     <- c("$N$",
               formatC(nobs(m1), format="d", big.mark=","),
               formatC(nobs(m2), format="d", big.mark=","),
               formatC(nobs(m3), format="d", big.mark=","),
               formatC(nobs(m4), format="d", big.mark=","),
               formatC(nobs(m5), format="d", big.mark=","),
               formatC(nobs(m6), format="d", big.mark=","))
r2_row    <- c("$R^2$ (within)",
               fmt_num(r2(m1, "wr2"), 3),
               fmt_num(r2(m2, "wr2"), 3),
               fmt_num(r2(m3, "wr2"), 3),
               fmt_num(r2(m4, "wr2"), 3),
               fmt_num(r2(m5, "wr2"), 3),
               fmt_num(r2(m6, "wr2"), 3))

joint_row <- c("Joint $\\delta_1=\\delta_2=\\delta_3=0$ ($\\chi^2_3$, $p$)",
               "--",
               sprintf("%.2f (%s)", t2$joint_chi2, fmt_p(t2$joint_p)),
               sprintf("%.2f (%s)", t3$joint_chi2, fmt_p(t3$joint_p)),
               sprintf("%.2f (%s)", t4$joint_chi2, fmt_p(t4$joint_p)),
               sprintf("%.2f (%s)", t5$joint_chi2, fmt_p(t5$joint_p)),
               sprintf("%.2f (%s)", t6$joint_chi2, fmt_p(t6$joint_p)))

main_row  <- c("Main $\\delta_1<0$ ($z$, 1-sided $p$)",
               "--",
               sprintf("z=%+.2f (p=%s)", t2$d1_z, fmt_p(t2$main_p_1sd)),
               sprintf("z=%+.2f (p=%s)", t3$d1_z, fmt_p(t3$main_p_1sd)),
               sprintf("z=%+.2f (p=%s)", t4$d1_z, fmt_p(t4$main_p_1sd)),
               sprintf("z=%+.2f (p=%s)", t5$d1_z, fmt_p(t5$main_p_1sd)),
               sprintf("z=%+.2f (p=%s)", t6$d1_z, fmt_p(t6$main_p_1sd)))

asym_row  <- c("Asym $\\delta_3-\\delta_1$ ($z$, 1-sided $p$)",
               "--",
               sprintf("%+.4f (z=%+.2f, p=%s)",
                       t2$asym_diff, t2$asym_z, fmt_p(t2$asym_p_1sd)),
               sprintf("%+.4f (z=%+.2f, p=%s)",
                       t3$asym_diff, t3$asym_z, fmt_p(t3$asym_p_1sd)),
               sprintf("%+.4f (z=%+.2f, p=%s)",
                       t4$asym_diff, t4$asym_z, fmt_p(t4$asym_p_1sd)),
               sprintf("%+.4f (z=%+.2f, p=%s)",
                       t5$asym_diff, t5$asym_z, fmt_p(t5$asym_p_1sd)),
               sprintf("%+.4f (z=%+.2f, p=%s)",
                       t6$asym_diff, t6$asym_z, fmt_p(t6$asym_p_1sd)))

footer_df <- rbind(fe_row, clust_row, n_row, r2_row,
                   joint_row, main_row, asym_row)
colnames(footer_df) <- c("Variable","(1)","(2)","(3)","(4)","(5)","(6)")
rownames(footer_df) <- NULL

full_df <- rbind(body_df, footer_df)
rownames(full_df) <- NULL

# Caption + footnote
cap <- paste0(
  "H2: Disposition / Illusion-of-Control Hypothesis. Panel regression of ",
  "monthly proportional fund flows on within-Lipper-category lagged ",
  "12-month performance-rank segments interacted with margin-debt and ",
  "put-call illusion-of-control regime indicators."
)
footnote_text <- paste0(
  # kableExtra threeparttable=TRUE strips ONE backslash: use \\\\cmd for \cmd.
  # Bare % is a LaTeX comment character — must be \\\\% to survive stripping.
  "The dependent variable is the Sirri-Tufano (1998) winsorised proportional ",
  "fund flow (decimal). $t$-statistics in parentheses below each coefficient. ",
  "Performance segments $R^{LOW}$, $R^{MID}$, $R^{HIGH}$ are constructed from ",
  "the lagged within-Lipper-category fractional rank of cumulative 12-month ",
  "gross returns. The state variable in column (2) is $D^{MD,Lev}$ (= 1 if ",
  "MD/MCAP is in the top 34\\\\%); column (3) is $D^{MD,Grw}$ (= 1 if YoY change ",
  "in MD/MCAP is in the top 34\\\\%); column (4) is $D^{MD,Det}$ (= 1 if the ",
  "residual of $\\\\log$(MD/MCAP) on a linear time trend is in the top 34\\\\%, ",
  "following Daniel-Klos-Pollet 2016 and Rapach-Ringgenberg-Zhou 2016); ",
  "column (5) is $D^{\\\\text{INV-PCR}}$ (= 1 in the bottom 34\\\\% of the CBOE ",
  "equity put-call ratio: high call-to-put = high illusion of control). ",
  "Column (6) is the discriminant specification: it includes both ",
  "$D^{MD,Det}$ and Baker-Wurgler $D^{SENT}$ with full rank interactions; ",
  "if $\\\\delta^{MD}_1$ remains negative there, H2 is identified ",
  "independently of the H1 sentiment channel. All controls lagged one ",
  "period; time-invariant fund characteristics (expense ratio, load dummy, ",
  "turnover) included but absorbed by fund FE. Cols (1)--(4) and (6) use ",
  "the full margin-debt sample (1998--2023, approximately 190K fund-months); ",
  "col (5) uses the PCR sample (2003-10 to 2019-10). Standard errors ",
  "two-way clustered on Ticker and calendar month (Petersen 2009). ",
  "Stars: $^{*}\\\\,p<0.10$, $^{**}\\\\,p<0.05$, $^{***}\\\\,p<0.01$."
)

ktab <- kbl(
  full_df,
  format    = "latex",
  booktabs  = TRUE,
  caption   = cap,
  label     = "H2_regression",
  align     = c("l", rep("r", 6)),
  escape    = FALSE,
  linesep   = ""
) %>%
  kable_styling(latex_options = "hold_position", font_size = 8) %>%
  add_header_above(c(" " = 1, "Baseline" = 1, "$D^{MD,Lev}$" = 1,
                     "$D^{MD,Grw}$" = 1, "$D^{MD,Det}$" = 1,
                     "$D^{\\\\text{INV-PCR}}$" = 1,
                     "$D^{MD,Det}\\\\,|\\\\,D^{SENT}$" = 1),
                   escape = FALSE) %>%
  row_spec(nrow(body_df), hline_after = TRUE) %>%
  footnote(general = footnote_text, general_title = "",
           threeparttable = TRUE, escape = FALSE)

tex <- as.character(ktab)
tex <- gsub("\\begin{table}[!h]", "\\begin{table}[H]", tex, fixed = TRUE)
writeLines(tex, OUTPUT_TEX)
cat(sprintf("\nWrote %s\n", OUTPUT_TEX))

# --- 6. Expose objects for the reporting script ------------------------------
H2_models <- list(baseline = m1, dmd_level = m2, dmd_growth = m3,
                  dmd_detrend = m4, dpcr_inv = m5, dmd_det_dsent = m6)
H2_tests  <- test_results
assign("H2_models", H2_models, envir = .GlobalEnv)
assign("H2_tests",  H2_tests,  envir = .GlobalEnv)
