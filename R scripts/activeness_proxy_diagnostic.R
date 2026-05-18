# activeness_proxy_diagnostic.R                                        v1.0
# =============================================================================
# Horse race among four candidate "lottery preference" / "activeness"
# proxies for the H3 regression. Run this AFTER panel_regressions_setup.R
# has produced panel_reg in the session (or panel_reg.rds on disk), and
# while panel_incubation is also still in the session (needed to construct
# the right-tail measures from raw returns).
#
# Candidate measures (all lagged one period):
#   1. ActR2    1 - rolling 36m Carhart R^2.
#               Amihud-Goyenko (2013, RFS) is the foundational paper.
#               Statistical activeness; conflates skill with idiosyncratic
#               departure from a factor benchmark.
#   2. ActSkew  Sample skewness of trailing 36 monthly gross returns.
#               Captures asymmetry in either tail; conflates positive and
#               negative skewness mechanisms.
#   3. MAX12    Max monthly gross return over the trailing 12 months.
#               Bali, Cakici & Whitelaw (2011, JFE) defined MAX on daily
#               returns; for funds with monthly-only return data the
#               trailing-12-month max is the standard adaptation
#               (e.g., Akbas & Genc 2020). Direct right-tail measure.
#   4. UpSkew   Right-tail conditional skewness: standardised cubed
#               deviations of returns above their in-window mean, scaled
#               by the upside semi-deviation. Strictly positive by
#               construction. Custom; user-suggested.
#
# Five tests:
#   A. Sample correlations          (pooled and within-fund)
#   B. Coverage / missingness
#   C. Univariate flow predictability under TWO FE structures:
#        - C1: Lipper x yearmo FE (cross-fund identification)
#        - C2: Ticker + yearmo FE (within-fund identification)
#   D. Horse race regression with all four measures jointly
#   E. Future Carhart-alpha predictability (Amihud-Goyenko 2013 replication)
#
# Output: table_activeness_proxy_diagnostic.tex (one comparison table
#         suitable for an appendix or defense binder) plus comprehensive
#         console output.
# =============================================================================

suppressPackageStartupMessages({
  library(dplyr)
  library(fixest)
  library(slider)
  library(e1071)
  library(kableExtra)
  library(readxl)
})

# --- 0. Config --------------------------------------------------------------
if (!exists("WORKING_DIR")) WORKING_DIR <- getwd()
OUTPUT_TEX <- file.path(WORKING_DIR, "table_activeness_proxy_diagnostic.tex")
ALPHA_ROLL_FILE <- file.path(WORKING_DIR, "alpha_rolling.xlsx")

# --- 1. Pre-flight ----------------------------------------------------------
if (!exists("panel_reg")) {
  rds_path <- file.path(WORKING_DIR, "panel_reg.rds")
  if (file.exists(rds_path)) {
    panel_reg <- readRDS(rds_path)
    cat("Loaded panel_reg from", rds_path, "\n")
  } else {
    stop("panel_reg not in session and ", rds_path, " not found.\n",
         "Run panel_regressions_setup.R first (Phase J setup).")
  }
}
if (!exists("panel_incubation")) {
  stop("panel_incubation not in session.\n",
       "This is a one-time diagnostic; run Phase A ",
       "(data_import_and_cleaning.R + flow_calculation.R) first to ",
       "populate panel_incubation, then this script.")
}

# --- 2. Construct MAX12 and UpSkew from raw monthly returns -----------------
# UpSkew: sample skewness restricted to returns above the in-window mean.
# By construction strictly positive (cube of positive deviations of positive
# numbers is positive); cross-fund variation reflects intensity of right-
# tail mass relative to upside semi-deviation, not sign.
upskew_fun <- function(r) {
  r <- r[!is.na(r)]
  n <- length(r)
  if (n < 24L) return(NA_real_)
  m <- mean(r)
  r_pos <- r[r > m]
  n_pos <- length(r_pos)
  if (n_pos < 8L) return(NA_real_)
  sigma_up <- sqrt(mean((r_pos - m)^2))
  if (sigma_up <= 0) return(NA_real_)
  mean(((r_pos - m) / sigma_up)^3)
}

cat("\nConstructing UpSkew from panel_incubation (MAX12 already in panel_reg via panel_regressions_setup.R)...\n")
extra_proxies <- panel_incubation %>%
  arrange(Ticker, date) %>%
  group_by(Ticker) %>%
  mutate(
    UpSkew_raw = slider::slide_dbl(ret_gross_raw, upskew_fun,
                                   .before = 35, .complete = TRUE),
    UpSkew = dplyr::lag(UpSkew_raw, 1)
  ) %>%
  ungroup() %>%
  select(Ticker, date, UpSkew)

# --- 3. Build working frame -------------------------------------------------
df <- panel_reg %>%
  left_join(extra_proxies, by = c("Ticker", "date")) %>%
  filter(!is_december) %>%
  mutate(
    Ticker          = as.factor(Ticker),
    yearmo          = as.factor(yearmo),
    Lipper_Category = as.factor(Lipper_Category)
  )
cat(sprintf("Working frame: %s rows | %d funds | %d months\n",
            formatC(nrow(df), format="d", big.mark=","),
            nlevels(df$Ticker), nlevels(df$yearmo)))

# Aligned sample for tests C, D: all four measures + core controls non-NA.
core_ctrls <- c("flow", "log_TNA", "log_Age", "ExpRatio", "LoadDummy",
                "ret_vol", "Turnover", "style_flow_lag",
                "R_LOW", "R_MID", "R_HIGH")
samp <- df %>%
  filter(if_all(all_of(c(core_ctrls, "ActR2", "ActSkew",
                         "MAX12", "UpSkew")), ~ !is.na(.)))
cat(sprintf("Aligned sample (all 4 measures + controls non-NA): %s obs\n",
            formatC(nrow(samp), format="d", big.mark=",")))

# Standardise each measure for direct t-stat comparability.
measures <- c("ActR2", "ActSkew", "MAX12", "UpSkew")
for (m in measures) {
  samp[[paste0(m, "_z")]] <- as.numeric(scale(samp[[m]]))
}

# --- TEST A: Sample correlations --------------------------------------------
cat("\n=========================================================================\n")
cat("TEST A: Sample correlations among the four measures\n")
cat("=========================================================================\n")
corr_pool <- cor(samp[, measures], use = "pairwise.complete.obs")
cat("\nPooled correlations:\n")
print(round(corr_pool, 3))

samp_dm <- samp %>%
  group_by(Ticker) %>%
  mutate(across(all_of(measures), ~ . - mean(., na.rm = TRUE),
                .names = "{.col}_dm")) %>%
  ungroup()
corr_within <- cor(samp_dm[, paste0(measures, "_dm")],
                   use = "pairwise.complete.obs")
colnames(corr_within) <- measures; rownames(corr_within) <- measures
cat("\nWithin-fund correlations (after fund demeaning):\n")
print(round(corr_within, 3))

# --- TEST B: Coverage -------------------------------------------------------
cat("\n=========================================================================\n")
cat("TEST B: Coverage and distribution\n")
cat("=========================================================================\n")
coverage_df <- data.frame(
  Measure = measures,
  N       = sapply(measures, function(m) sum(!is.na(df[[m]]))),
  Pct_obs = sapply(measures, function(m) round(100 * mean(!is.na(df[[m]])), 1)),
  Mean    = sapply(measures, function(m) round(mean(df[[m]], na.rm=TRUE), 4)),
  SD      = sapply(measures, function(m) round(sd(df[[m]], na.rm=TRUE), 4)),
  P25     = sapply(measures, function(m) round(quantile(df[[m]], 0.25, na.rm=TRUE), 4)),
  Median  = sapply(measures, function(m) round(quantile(df[[m]], 0.50, na.rm=TRUE), 4)),
  P75     = sapply(measures, function(m) round(quantile(df[[m]], 0.75, na.rm=TRUE), 4)),
  row.names = NULL
)
print(coverage_df)

# --- Helper: pull standardised-coef stats from a fixest model ---------------
extract_coef <- function(mod, name) {
  ct <- fixest::coeftable(mod)
  i <- which(rownames(ct) == name)
  if (length(i) == 0L) return(c(est = NA, se = NA, t = NA))
  est <- as.numeric(ct[i, "Estimate"])
  se  <- as.numeric(ct[i, "Std. Error"])
  c(est = est, se = se, t = est / se)
}

# --- TEST C1: Univariate flow predictability, Lipper x yearmo FE ------------
# This is the FAIR test for proxy comparison: cross-fund identification
# preserves the dominant variation source for each measure (especially
# ActR2, which is near time-invariant within fund).
cat("\n=========================================================================\n")
cat("TEST C1: Univariate flow predictability (Lipper x yearmo FE)\n")
cat("=========================================================================\n")
ctrl_str <- paste(core_ctrls[-1], collapse = " + ")  # drop "flow"

univariate_xstyle <- list()
for (m in measures) {
  f_str <- sprintf("flow ~ %s_z + %s | Lipper_Category^yearmo", m, ctrl_str)
  mod <- feols(as.formula(f_str), data = samp, cluster = ~ Ticker + yearmo)
  cs <- extract_coef(mod, paste0(m, "_z"))
  univariate_xstyle[[m]] <- data.frame(
    Measure = m, Coef = cs["est"], SE = cs["se"], t = cs["t"],
    N = nobs(mod), within_R2 = r2(mod, "wr2"), row.names = NULL
  )
}
univ_xstyle_df <- do.call(rbind, univariate_xstyle); rownames(univ_xstyle_df) <- NULL
print(univ_xstyle_df, row.names = FALSE)

# --- TEST C2: Univariate flow predictability, Ticker + yearmo FE ------------
# This matches the FE structure of the H3 PRIMARY spec. Biased against
# near-time-invariant measures (ActR2) by construction; included to show
# what the actual H3 primary regression "sees".
cat("\n=========================================================================\n")
cat("TEST C2: Univariate flow predictability (Ticker + yearmo FE)\n")
cat("=========================================================================\n")
univariate_ticker <- list()
for (m in measures) {
  f_str <- sprintf("flow ~ %s_z + %s | Ticker + yearmo", m, ctrl_str)
  mod <- feols(as.formula(f_str), data = samp, cluster = ~ Ticker + yearmo)
  cs <- extract_coef(mod, paste0(m, "_z"))
  univariate_ticker[[m]] <- data.frame(
    Measure = m, Coef = cs["est"], SE = cs["se"], t = cs["t"],
    N = nobs(mod), within_R2 = r2(mod, "wr2"), row.names = NULL
  )
}
univ_ticker_df <- do.call(rbind, univariate_ticker); rownames(univ_ticker_df) <- NULL
print(univ_ticker_df, row.names = FALSE)

# --- TEST D: Horse race -----------------------------------------------------
cat("\n=========================================================================\n")
cat("TEST D: Horse race (all 4 measures together, Lipper x yearmo FE)\n")
cat("=========================================================================\n")
race_formula <- as.formula(sprintf(
  "flow ~ ActR2_z + ActSkew_z + MAX12_z + UpSkew_z + %s | Lipper_Category^yearmo",
  ctrl_str
))
race_mod <- feols(race_formula, data = samp, cluster = ~ Ticker + yearmo)
race_results <- do.call(rbind, lapply(measures, function(m) {
  cs <- extract_coef(race_mod, paste0(m, "_z"))
  data.frame(Measure = m, Coef = cs["est"], SE = cs["se"], t = cs["t"],
             row.names = NULL)
}))
rownames(race_results) <- NULL
print(race_results, row.names = FALSE)

# Joint test of all four = 0
joint_terms <- paste0(measures, "_z")
joint_test <- tryCatch({
  fixest::wald(race_mod, joint_terms, print = FALSE)
}, error = function(e) NULL)
if (!is.null(joint_test)) {
  cat(sprintf("Joint test (all 4 = 0): chi^2(%d) = %.2f, p = %.4f\n",
              joint_test$df, joint_test$stat, joint_test$p))
}

# --- TEST E: Future-alpha predictability ------------------------------------
# Amihud-Goyenko (2013) replication. Read alpha_rolling.xlsx, construct
# 12-month-forward alpha within fund, regress on each standardised measure
# with yearmo FE and Ticker clustering. AG predicts: 1 - R^2 should
# positively predict future risk-adjusted return.
cat("\n=========================================================================\n")
cat("TEST E: Future Carhart-alpha predictability (12m forward)\n")
cat("=========================================================================\n")
test_e_df <- NULL
if (!file.exists(ALPHA_ROLL_FILE)) {
  cat("alpha_rolling.xlsx not found; Test E skipped.\n")
} else {
  alpha_xl <- tryCatch(read_xlsx(ALPHA_ROLL_FILE), error = function(e) NULL)
  if (is.null(alpha_xl) || !"alpha_ann" %in% names(alpha_xl)) {
    cat("Could not read alpha_ann column from alpha_rolling.xlsx; skipped.\n")
  } else {
    alpha_xl <- alpha_xl %>%
      mutate(date = as.Date(date)) %>%
      select(Ticker, date, alpha_ann)
    samp_e <- samp %>%
      mutate(Ticker_chr = as.character(Ticker)) %>%
      left_join(alpha_xl, by = c("Ticker_chr" = "Ticker", "date" = "date")) %>%
      arrange(Ticker, date) %>%
      group_by(Ticker) %>%
      mutate(alpha_lead12 = dplyr::lead(alpha_ann, 12)) %>%
      ungroup() %>%
      filter(!is.na(alpha_lead12))
    cat(sprintf("Test E sample (alpha_lead12 non-NA): %s obs\n",
                formatC(nrow(samp_e), format="d", big.mark=",")))
    
    test_e_results <- list()
    for (m in measures) {
      f_str <- sprintf("alpha_lead12 ~ %s_z | yearmo", m)
      mod <- tryCatch(
        feols(as.formula(f_str), data = samp_e, cluster = ~ Ticker + yearmo),
        error = function(e) NULL
      )
      if (is.null(mod)) {
        test_e_results[[m]] <- data.frame(Measure=m, Coef=NA, SE=NA, t=NA, N=NA,
                                          row.names = NULL)
      } else {
        cs <- extract_coef(mod, paste0(m, "_z"))
        test_e_results[[m]] <- data.frame(
          Measure = m, Coef = cs["est"], SE = cs["se"], t = cs["t"],
          N = nobs(mod), row.names = NULL
        )
      }
    }
    test_e_df <- do.call(rbind, test_e_results); rownames(test_e_df) <- NULL
    print(test_e_df, row.names = FALSE)
  }
}

# --- 5. Build summary LaTeX table -------------------------------------------
cat("\nBuilding summary LaTeX table...\n")

fmt_num <- function(x, n=3) {
  if (is.null(x) || length(x) == 0L || is.na(x)) return("--")
  formatC(x, format="f", digits=n)
}

# Pooled correlations vs ActR2 (literature anchor).
corr_vs_actr2 <- corr_pool["ActR2", measures]

univ_xstyle_t <- setNames(univ_xstyle_df$t, univ_xstyle_df$Measure)
univ_ticker_t <- setNames(univ_ticker_df$t, univ_ticker_df$Measure)
race_t        <- setNames(race_results$t,   race_results$Measure)
if (!is.null(test_e_df)) {
  alpha_t <- setNames(test_e_df$t, test_e_df$Measure)
} else {
  alpha_t <- setNames(rep(NA_real_, 4), measures)
}

summary_df <- data.frame(
  Measure = measures,
  Coverage = sapply(measures, function(m) sprintf("%.1f", mean(!is.na(df[[m]]))*100)),
  Corr_ActR2 = sapply(measures, function(m) fmt_num(corr_vs_actr2[m], 3)),
  Univ_xstyle_t = sapply(measures, function(m) fmt_num(univ_xstyle_t[m], 2)),
  Univ_ticker_t = sapply(measures, function(m) fmt_num(univ_ticker_t[m], 2)),
  Race_t = sapply(measures, function(m) fmt_num(race_t[m], 2)),
  Alpha_t = sapply(measures, function(m) fmt_num(alpha_t[m], 2)),
  row.names = NULL
)
colnames(summary_df) <- c("Measure",
                          "Cov.\\ (\\%)",
                          "$\\rho$ vs.\\ ActR2",
                          "Univ.\\ $t$ (style FE)",
                          "Univ.\\ $t$ (fund FE)",
                          "Horserace $t$",
                          "$\\alpha_{t+12}$-pred.\\ $t$")

caption <- "Activeness / Lottery Proxy Diagnostic"
fn <- paste0(
  "Aligned sample restricted to fund-month observations with all four ",
  "measures and core controls non-NA. Two-way clustered standard errors on ",
  "Ticker and calendar month. Coverage in column 2 reports the fraction of ",
  "fund-months in panel\\\\_reg with the measure non-NA (before alignment).\\\\\\\\ ",
  "The ``winning'' proxy is identified by (i) high univariate $|t|$ under ",
  "the FE structure that preserves its dominant variation source, (ii) ",
  "survival in the horse race conditional on the others, and (iii) ",
  "consistency with Amihud--Goyenko's (2013) future-alpha prediction."
)

ktab <- kbl(
  summary_df, format = "latex", booktabs = TRUE,
  caption = caption, label = "activeness_proxy",
  align = c("l", rep("r", 6)), escape = FALSE, linesep = ""
) %>%
  kable_styling(latex_options = c("hold_position", "scale_down")) %>%
  footnote(general = fn, general_title = "",
           threeparttable = TRUE, escape = FALSE)

# Phase B helper: extract tablenotes from a threeparttable float and re-emit
# them AFTER \end{table} as a flowing paragraph (NOT a minipage).
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

tex <- as.character(ktab)
tex <- gsub("\\begin{table}[!h]", "\\begin{table}[H]", tex, fixed = TRUE)
tex <- threeparttable_note_after_compact(tex)  # PHASE B: move note outside float
writeLines(tex, OUTPUT_TEX)
cat(sprintf("Wrote %s\n", OUTPUT_TEX))

# --- 6. Console winner declaration ------------------------------------------
cat("\n========================== INTERPRETIVE SUMMARY ==========================\n")
which_winner <- function(named_vec, label) {
  v <- named_vec[!is.na(named_vec)]
  if (length(v) == 0L) {
    cat(sprintf("  %-32s skipped (no data)\n", paste0(label, ":")))
    return(invisible())
  }
  best <- names(v)[which.max(abs(v))]
  cat(sprintf("  %-32s %s (|t|=%.2f)\n",
              paste0(label, ":"), best, abs(v[best])))
}
which_winner(univ_xstyle_t, "Univariate (style FE)")
which_winner(univ_ticker_t, "Univariate (fund FE)")
which_winner(race_t,        "Horse race")
which_winner(alpha_t,       "Future alpha predictability")
cat("=========================================================================\n")
cat("Save objects: corr_pool, corr_within, coverage_df, univ_xstyle_df,\n")
cat("              univ_ticker_df, race_results, test_e_df.\n")

assign("activeness_proxy_summary", summary_df, envir = .GlobalEnv)
saveRDS(list(corr_pool = corr_pool, corr_within = corr_within,
             coverage = coverage_df, univ_xstyle = univ_xstyle_df,
             univ_ticker = univ_ticker_df, race = race_results,
             alpha_pred = test_e_df, summary = summary_df),
        file.path(WORKING_DIR, "activeness_proxy_diagnostic.rds"))
cat("Saved activeness_proxy_diagnostic.rds\n")
