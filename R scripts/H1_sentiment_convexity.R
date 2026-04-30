# H1_sentiment_convexity.R                                              v1.0
# =============================================================================
# Tests Hypothesis H1 (proposal Section 4.1, dissertation Section 5.2):
#
#   H1: Investor sentiment amplifies the convexity of the flow-performance
#   relationship. Specifically, sentiment increases the slope of the
#   inflow-chasing-winners segment (delta_3 > 0).
#
#   Auxiliary test (asymmetry, dissertation Section 5.2 footnote):
#   delta_3 > delta_1, i.e. sentiment's amplification of inflows to top
#   performers exceeds its amplification of outflows from bottom performers.
#   This is the correct framing because R^LOW is a positive fractional rank,
#   not a return; level claims about loser outflows do not follow from the
#   sign of delta_1 alone.
#
# SPECIFICATION (proposal Eq. 11, restricted to the sentiment dimension):
#
#   Flow_{i,t} = alpha_i
#                + beta_1 R^LOW_{i,t-1} + beta_2 R^MID_{i,t-1} + beta_3 R^HIGH_{i,t-1}
#                + delta_1 (R^LOW × Sent) + delta_2 (R^MID × Sent) + delta_3 (R^HIGH × Sent)
#                + lambda Sent_t + gamma' Controls_{i,t-1} + epsilon_{i,t}
#
#   - alpha_i: fund fixed effect (absorbs time-invariant fund characteristics)
#   - Standard errors: two-way clustered on Ticker and yearmo (Petersen 2009)
#   - Sample: panel_reg, active funds, non-December, all RHS non-NA
#
# FOUR SPECIFICATIONS estimated:
#   Col (1) Baseline:     no sentiment terms (pure Sirri-Tufano replication)
#   Col (2) D_SENT:       primary spec, regime dummy following Baker-Wurgler 2007
#   Col (3) SENT_ORTH:    continuous sentiment (robustness to discretisation)
#   Col (4) D_AAII:       AAII bull-bear regime (alternative sentiment proxy)
#
# OUTPUT:
#   table_H1_regression.tex   - LaTeX fragment in WORKING_DIR
#
# REFERENCES:
#   Sirri E.R. & Tufano P. (1998). J Finance 53(5), 1589-1622.
#   Baker M. & Wurgler J. (2006). J Finance 61(4); (2007). JEP 21(2).
#   Petersen M.A. (2009). RFS 22(1), 435-480.
#   Berge L. (2018). CREA Discussion Paper 2018-13 (fixest).
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
OUTPUT_TEX <- file.path(WORKING_DIR, "table_H1_regression.tex")

# Two-sided significance thresholds for stars
STAR_THR <- c(`***` = 2.576, `**` = 1.960, `*` = 1.645)

# --- 1. Pre-flight checks ----------------------------------------------------
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

# --- 2. Build estimation sample ----------------------------------------------
# Required controls for ALL four specs. Sentiment vars are added per-spec
# below (cols 2-4). The estimation sample is the intersection of all
# observations needed by the FULL spec (col 2 with D_SENT) so that the
# four specifications are estimated on identical samples.
core_rhs <- c("flow", "R_LOW", "R_MID", "R_HIGH",
              "log_TNA", "log_Age", "ExpRatio", "LoadDummy",
              "ret_vol", "Turnover", "style_flow_lag",
              "D_SENT", "SENT_ORTH", "D_AAII")

samp <- panel_reg %>%
  filter(!is_december) %>%
  filter(if_all(all_of(core_rhs), ~ !is.na(.))) %>%
  mutate(
    # Standardise SENT_ORTH so its coefficient is "effect of 1-SD increase"
    SENT_ORTH_z = as.numeric(scale(SENT_ORTH)),
    # Coerce regime dummies to numeric (fixest interprets factors awkwardly
    # in interactions)
    D_SENT  = as.numeric(D_SENT),
    D_AAII  = as.numeric(D_AAII),
    # Make Ticker a factor for FE absorption efficiency
    Ticker  = as.factor(Ticker),
    yearmo  = as.factor(yearmo)
  )

cat(sprintf(
  "\nEstimation sample: %d fund-months | %d funds | %d months\n",
  nrow(samp), nlevels(samp$Ticker), nlevels(samp$yearmo)
))

# --- 3. Estimate four specifications -----------------------------------------
# Controls common to all specs
controls <- c("log_TNA", "log_Age", "ExpRatio", "LoadDummy",
              "ret_vol", "Turnover", "style_flow_lag")
ctrl_str <- paste(controls, collapse = " + ")

# fixest formula syntax: outcome ~ regressors | fixed_effects
f1 <- as.formula(paste("flow ~ R_LOW + R_MID + R_HIGH +", ctrl_str,
                       "| Ticker"))
f2 <- as.formula(paste("flow ~ R_LOW + R_MID + R_HIGH",
                       "+ D_SENT",
                       "+ R_LOW:D_SENT + R_MID:D_SENT + R_HIGH:D_SENT +",
                       ctrl_str, "| Ticker"))
f3 <- as.formula(paste("flow ~ R_LOW + R_MID + R_HIGH",
                       "+ SENT_ORTH_z",
                       "+ R_LOW:SENT_ORTH_z + R_MID:SENT_ORTH_z",
                       "+ R_HIGH:SENT_ORTH_z +",
                       ctrl_str, "| Ticker"))
f4 <- as.formula(paste("flow ~ R_LOW + R_MID + R_HIGH",
                       "+ D_AAII",
                       "+ R_LOW:D_AAII + R_MID:D_AAII + R_HIGH:D_AAII +",
                       ctrl_str, "| Ticker"))

# Two-way clustered SE on Ticker and yearmo (Petersen 2009)
cat("Estimating Col (1) Baseline...\n")
m1 <- feols(f1, data = samp, cluster = ~ Ticker + yearmo)
cat("Estimating Col (2) D_SENT...\n")
m2 <- feols(f2, data = samp, cluster = ~ Ticker + yearmo)
cat("Estimating Col (3) SENT_ORTH continuous...\n")
m3 <- feols(f3, data = samp, cluster = ~ Ticker + yearmo)
cat("Estimating Col (4) D_AAII...\n")
m4 <- feols(f4, data = samp, cluster = ~ Ticker + yearmo)

# --- 4. Hypothesis tests -----------------------------------------------------
# Joint test: delta_1 = delta_2 = delta_3 = 0 (chi-square form, 3 df)
# Asymmetry test: delta_3 - delta_1 > 0 (one-sided z-test)
test_h1 <- function(m, sent_label) {
  b <- coef(m)
  V <- vcov(m)
  i_low  <- which(names(b) == paste0("R_LOW:",  sent_label))
  i_mid  <- which(names(b) == paste0("R_MID:",  sent_label))
  i_high <- which(names(b) == paste0("R_HIGH:", sent_label))

  # Joint Wald (chi-square, 3 df)
  pos <- c(i_low, i_mid, i_high)
  bs  <- b[pos]; Vs <- V[pos, pos]
  chi2 <- as.numeric(t(bs) %*% solve(Vs) %*% bs)
  p_joint <- pchisq(chi2, df = length(pos), lower.tail = FALSE)

  # Asymmetry (one-sided, H_a: delta_3 > delta_1)
  diff_b  <- b[i_high] - b[i_low]
  diff_se <- sqrt(V[i_high, i_high] + V[i_low, i_low]
                  - 2 * V[i_high, i_low])
  z_asym  <- diff_b / diff_se
  p_asym_1sd <- pnorm(z_asym, lower.tail = FALSE)
  p_asym_2sd <- 2 * pnorm(abs(z_asym), lower.tail = FALSE)

  list(joint_chi2 = chi2, joint_p = p_joint,
       asym_diff  = unname(diff_b), asym_z = unname(z_asym),
       asym_p_1sd = p_asym_1sd, asym_p_2sd = p_asym_2sd)
}

t2 <- test_h1(m2, "D_SENT")
t3 <- test_h1(m3, "SENT_ORTH_z")
t4 <- test_h1(m4, "D_AAII")

cat("\n--- Hypothesis test results ---\n")
for (nm in c("D_SENT (col 2)","SENT_ORTH cont. (col 3)","D_AAII (col 4)")) {
  tt <- list(t2, t3, t4)[[match(nm, c("D_SENT (col 2)",
                                      "SENT_ORTH cont. (col 3)",
                                      "D_AAII (col 4)"))]]
  cat(sprintf(
    "%-25s  joint chi2(3) = %6.2f (p=%.4f)  | delta_3-delta_1 = %+.4f, z=%+.2f (1-sd p=%.4f)\n",
    nm, tt$joint_chi2, tt$joint_p,
    tt$asym_diff, tt$asym_z, tt$asym_p_1sd))
}

# --- 5. Build LaTeX table ----------------------------------------------------
# Coefficient rows we want to show, in order. ExpRatio, LoadDummy, and
# Turnover are time-invariant within fund and absorbed by Ticker FE -- they
# would print as blank rows so we drop them from the display and document
# the absorption in the footnote.
row_specs <- list(
  list(group = "Performance segments", coef = "R_LOW",   label = "$R^{LOW}$"),
  list(group = "Performance segments", coef = "R_MID",   label = "$R^{MID}$"),
  list(group = "Performance segments", coef = "R_HIGH",  label = "$R^{HIGH}$"),
  list(group = "Sentiment level",      coef = "D_SENT/SENT_ORTH_z/D_AAII",
       label = "Sentiment ($\\lambda$)"),
  list(group = "Sentiment x rank",     coef = "R_LOW:S",  label = "$R^{LOW}\\times$ Sent ($\\delta_1$)"),
  list(group = "Sentiment x rank",     coef = "R_MID:S",  label = "$R^{MID}\\times$ Sent ($\\delta_2$)"),
  list(group = "Sentiment x rank",     coef = "R_HIGH:S", label = "$R^{HIGH}\\times$ Sent ($\\delta_3$)"),
  list(group = "Controls",             coef = "log_TNA",        label = "$\\log(\\text{TNA})$"),
  list(group = "Controls",             coef = "log_Age",        label = "$\\log(\\text{Age})$"),
  list(group = "Controls",             coef = "ret_vol",        label = "Return vol.\\ (36m SD)"),
  list(group = "Controls",             coef = "style_flow_lag", label = "Style flow")
)

# Helper: format coefficient with stars + t-stat in parentheses below
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

# Coefficient name lookup per model column.
sent_var <- list(m1 = NULL, m2 = "D_SENT",
                 m3 = "SENT_ORTH_z", m4 = "D_AAII")

# Resolve a row spec's coef name into the actual fixest term name per model
resolve_coef_name <- function(coef_pat, sentvar) {
  if (coef_pat == "D_SENT/SENT_ORTH_z/D_AAII") {
    return(sentvar)
  }
  if (startsWith(coef_pat, "R_") && grepl(":S$", coef_pat)) {
    if (is.null(sentvar)) return(NA_character_)
    return(paste0(sub(":S$", "", coef_pat), ":", sentvar))
  }
  coef_pat
}

# Get coefficient and t-stat from a fixest model, returning ("","") if absent
get_coef <- function(mod, name) {
  if (is.na(name) || is.null(name)) return(c(coef = "", t = ""))
  b <- coef(mod); se <- se(mod)
  i <- which(names(b) == name)
  if (length(i) == 0) return(c(coef = "", t = ""))
  est <- b[i]; tstat <- est / se[i]
  c(coef = add_stars(fmt_num(est, 4), tstat),
    t    = paste0("(", formatC(tstat, format = "f", digits = 2), ")"))
}

# Build the data frame row by row. Each "logical" row produces TWO physical
# rows: one for the coefficient, one for the t-stat in parentheses below.
build_row <- function(spec) {
  vals <- list()
  for (j in seq_along(c("m1","m2","m3","m4"))) {
    mod <- list(m1, m2, m3, m4)[[j]]
    sv  <- sent_var[[c("m1","m2","m3","m4")[j]]]
    cn  <- resolve_coef_name(spec$coef, sv)
    vals[[j]] <- get_coef(mod, cn)
  }
  list(
    coef_row = c(spec$label,
                 vals[[1]]["coef"], vals[[2]]["coef"],
                 vals[[3]]["coef"], vals[[4]]["coef"]),
    t_row    = c("",
                 vals[[1]]["t"], vals[[2]]["t"],
                 vals[[3]]["t"], vals[[4]]["t"])
  )
}

# Assemble the body
body_rows <- list()
for (spec in row_specs) {
  br <- build_row(spec)
  body_rows[[length(body_rows) + 1]] <- br$coef_row
  body_rows[[length(body_rows) + 1]] <- br$t_row
}
body_df <- do.call(rbind, body_rows)
colnames(body_df) <- c("Variable", "(1)", "(2)", "(3)", "(4)")
rownames(body_df) <- NULL

# Footer rows: FE / N / R^2 / Wald tests
fe_row    <- c("Fund FE",                  "Yes", "Yes", "Yes", "Yes")
clust_row <- c("Cluster (Ticker, yearmo)", "Yes", "Yes", "Yes", "Yes")
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

# Hypothesis test rows
fmt_p <- function(p) {
  if (is.na(p)) return("")
  if (p < 0.001) "$<\\!0.001$"
  else formatC(p, format = "f", digits = 3)
}
joint_row <- c("Joint $\\delta_1=\\delta_2=\\delta_3=0$ ($\\chi^2_3$, $p$)",
               "--",
               sprintf("%.2f (%s)", t2$joint_chi2, fmt_p(t2$joint_p)),
               sprintf("%.2f (%s)", t3$joint_chi2, fmt_p(t3$joint_p)),
               sprintf("%.2f (%s)", t4$joint_chi2, fmt_p(t4$joint_p)))
asym_row <- c("Asymmetry $\\delta_3-\\delta_1$ ($z$, 1-sided $p$)",
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

# Caption + footnote (paragraph form)
cap <- paste0(
  "H1: Sentiment-Convexity Hypothesis. Panel regression of monthly ",
  "proportional fund flows on within-Lipper-category lagged 12-month ",
  "performance-rank segments (Sirri-Tufano 1998), interacted with sentiment ",
  "regime indicators."
)
footnote_text <- paste0(
  "The dependent variable is the Sirri-Tufano (1998) winsorised ",
  "proportional fund flow (decimal). Performance segments $R^{LOW}$, ",
  "$R^{MID}$, $R^{HIGH}$ are constructed from the lagged within-Lipper-",
  "category fractional rank of cumulative 12-month gross returns ",
  "(Equations 6--8 of the proposal). Sentiment in column (2) is the ",
  "regime dummy $D^{SENT}$ (= 1 if Baker--Wurgler orthogonalised sentiment ",
  "exceeds its 66th in-sample percentile, following Baker \\& Wurgler 2007); ",
  "in column (3) it is the standardised continuous Baker--Wurgler ",
  "orthogonalised index; in column (4) it is the AAII bull--bear regime ",
  "dummy. All controls are lagged one period. Time-invariant fund ",
  "characteristics (expense ratio, load dummy, turnover ratio) are ",
  "included in the specification but absorbed by the fund fixed effects. ",
  "Standard errors are two-way clustered on Ticker and calendar month ",
  "(Petersen 2009). Stars: $^{*}\\,p<0.10$, $^{**}\\,p<0.05$, ",
  "$^{***}\\,p<0.01$."
)

# Render with kableExtra (booktabs, hold_position) following dissertation style
ktab <- kbl(
  full_df,
  format    = "latex",
  booktabs  = TRUE,
  caption   = cap,
  label     = "H1_regression",
  align     = c("l", "r", "r", "r", "r"),
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

# Apply dissertation convention: replace [!h] -> [H] for stable placement.
tex <- as.character(ktab)
tex <- gsub("\\begin{table}[!h]", "\\begin{table}[H]", tex, fixed = TRUE)

# Write
writeLines(tex, OUTPUT_TEX)
cat(sprintf("\nWrote %s\n", OUTPUT_TEX))

# --- 6. Make summary objects available for the reporting script -------------
H1_models <- list(baseline = m1, dsent = m2, sent_cont = m3, daaii = m4)
H1_tests  <- list(D_SENT = t2, SENT_ORTH_z = t3, D_AAII = t4)
assign("H1_models", H1_models, envir = .GlobalEnv)
assign("H1_tests",  H1_tests,  envir = .GlobalEnv)
