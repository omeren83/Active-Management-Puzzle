# =============================================================================
# ROBUST APPENDIX TABLES BUILDER (v1.1)
#
# Purpose
# -------
# Generates LaTeX tables for Appendix E (Factor Model Robustness), comparing
# pooled alpha, bootstrap inference, and BSW skill decomposition under three
# factor specifications:
#   - Carhart 4-factor (main text baseline)
#   - Fama-French 6-factor (FF5 + MOM)
#   - Carhart 4-factor + Pastor-Stambaugh traded liquidity (C5)
#
# v1.1 changes vs v1.0
# --------------------
#   (a) Table E.1: Active and Passive groups shown as separate rows per
#       specification (previously Active only). Model name appears on the
#       first row of each spec pair, blank on the second.
#   (b) Prob. Luck display in Table E.2: values that round to 0.0% but are
#       genuinely close to zero are now rendered as "$<$0.1\%" to
#       distinguish "essentially zero" from "exactly zero". Values of
#       exactly 0 (0 runs out of 10,000) are displayed as "0.0\%".
#   (c) Table E.2 footnote expanded to explain the meaning of 0.0% / <0.1%
#       entries at lower percentiles.
#
# Prerequisite for correct VW values in Table E.1
# ------------------------------------------------
# Run patch_mean_tna.R (in same session as data_import_and_cleaning.R) before
# this script. Without it, mean_tna in the alpha xlsx files is NaN and VW
# columns display as "--". See patch_mean_tna.R for the one-time fix.
#
# Workflow
# --------
#   1. Source data_import_and_cleaning.R
#   2. Run patch_mean_tna.R (one-time, until alpha_estimation_robust.R v1.2)
#   3. Run alpha_estimation.R (v2.6) -> alpha_fullperiod.xlsx, bootstrap_results.xlsx
#   4. Run alpha_estimation_robust.R (v1.1+) -> alpha_fullperiod_{FF6,C5}.xlsx etc.
#   5. Run THIS script.
#
# Tables produced
# ---------------
#   table_alpha_comparison_robust.tex     Table E.1 (pooled alpha)
#   table_bootstrap_comparison_robust.tex Table E.2 (bootstrap percentiles)
#   table_bsw_comparison_robust.tex       Table E.3 (BSW decomposition)
#
# Bibliography
# ------------
# Requires entries in dissertation.bib: FamaFrench2015, PastorStambaugh2003,
# FamaFrench1993. BibTeX strings printed at end of script run.
# =============================================================================

library(readxl)
library(dplyr)

# --- 0. CONFIGURATION --------------------------------------------------------
# OUT_DIR: where to write the .tex files. Set to "D:/TEZ/tables" to drop
# outputs directly into your Overleaf-synced repo. If this script is sourced
# from master_pipeline.R with OUT_DIR already defined in the global environment,
# that value is used; otherwise the default below applies.
if (!exists("OUT_DIR", inherits = TRUE) || is.null(OUT_DIR)) {
  OUT_DIR <- "./tables_robust"
}
dir.create(OUT_DIR, showWarnings = FALSE, recursive = TRUE)

SAMPLE_LABEL_MAIN <- "Dec 1994--Feb 2026"   # baseline / FF6 window
SAMPLE_LABEL_C5   <- "Dec 1994--Dec 2024"   # PSL-constrained window

PERCENTILES_TO_SHOW <- c(1, 5, 10, 50, 90, 95, 99)
GAMMA_STAR          <- 20   # BSW threshold for the headline decomposition

# --- 1. LOAD OUTPUTS ---------------------------------------------------------
cat("=== Loading Excel outputs ===\n")

load_spec <- function(spec_suffix, label) {
  fp  <- if (spec_suffix == "") "alpha_fullperiod.xlsx"
  else                    sprintf("alpha_fullperiod_%s.xlsx", spec_suffix)
  bp  <- if (spec_suffix == "") "bootstrap_results.xlsx"
  else                    sprintf("bootstrap_results_%s.xlsx", spec_suffix)
  list(
    alpha_full   = read_excel(fp),
    boot_summary = read_excel(bp, sheet = "summary"),
    bsw_df       = read_excel(bp, sheet = "bsw_decomposition"),
    bsw_meta     = read_excel(bp, sheet = "bsw_meta"),
    label        = label
  )
}

car <- load_spec("",    "Carhart 4-Factor")
ff6 <- load_spec("FF6", "FF 6-Factor")
c5  <- load_spec("C5",  "Carhart + PSL")

cat("Carhart:", nrow(car$alpha_full), "funds | FF6:", nrow(ff6$alpha_full),
    "| C5:", nrow(c5$alpha_full), "\n")

# --- 2. FORMATTING HELPERS (identical convention to build_ff_tables_manual) --
fmt3 <- function(x) {
  v <- suppressWarnings(as.numeric(x))
  ifelse(is.na(v), "--", formatC(round(v, 3), format = "f", digits = 3))
}
fmt2 <- function(x) {
  v <- suppressWarnings(as.numeric(x))
  ifelse(is.na(v), "--", formatC(round(v, 2), format = "f", digits = 2))
}
fmt1 <- function(x) {
  v <- suppressWarnings(as.numeric(x))
  ifelse(is.na(v), "--", formatC(round(v, 1), format = "f", digits = 1))
}
fmt0 <- function(x) {
  v <- suppressWarnings(as.numeric(x))
  ifelse(is.na(v), "--", formatC(round(v, 0), format = "f", digits = 0))
}

# Probability-of-luck formatter.
# Exactly 0 (no bootstrap run was as extreme) -> "0.0\%"
# Greater than 0 but rounds to 0.0           -> "$<$0.1\%"
# Otherwise                                  -> "xx.x\%"
fmt_prob <- function(x) {
  v <- suppressWarnings(as.numeric(x))
  ifelse(is.na(v), "--",
         ifelse(v == 0,            "0.0\\%",
                ifelse(v < 0.05,          "$<$0.1\\%",
                       paste0(formatC(round(v, 1), format = "f", digits = 1), "\\%"))))
}

# TNA-weighted mean
wm <- function(x, w) {
  v <- !is.na(x) & !is.na(w) & w > 0
  if (sum(v) == 0L) return(NA_real_)
  sum(x[v] * w[v]) / sum(w[v])
}

write_tex <- function(lines, fn) {
  path <- file.path(OUT_DIR, fn)
  writeLines(lines, path)
  cat("Written:", path, "\n")
}

# Compute pooled statistics for one spec and one ap_group.
# Returns a named list; n_funds = 0 if group not in spec.
pool_stats <- function(spec, group_filter) {
  dat <- spec$alpha_full %>% filter(ap_group == group_filter)
  if (nrow(dat) == 0L)
    return(list(n_funds=0L, n_months=NA_integer_,
                ew_gross=NA_real_, vw_gross=NA_real_,
                ew_net=NA_real_,   vw_net=NA_real_))
  list(
    n_funds  = nrow(dat),
    n_months = max(dat$n_obs, na.rm = TRUE),
    ew_gross = mean(dat$alpha_ann,     na.rm = TRUE) * 100,
    vw_gross = wm(dat$alpha_ann,     dat$mean_tna)  * 100,
    ew_net   = mean(dat$alpha_net_ann, na.rm = TRUE) * 100,
    vw_net   = wm(dat$alpha_net_ann, dat$mean_tna)  * 100
  )
}

# =============================================================================
# 3. TABLE E.1:  POOLED ALPHA COMPARISON ACROSS FACTOR MODELS
#    v1.1: Active and Passive shown as separate rows per spec. Model label
#    on first row, blank on second. 8 columns: model, group, N, Tmax, EW/VW
#    gross, EW/VW net.
# =============================================================================
cat("\n=== Table E.1: Pooled Alpha Comparison ===\n")

# Build one row; model_label blank on second row of each spec pair.
make_e1_row <- function(model_label, group_label, st) {
  n_str <- if (st$n_funds == 0L) "--"
  else formatC(st$n_funds, format = "d", big.mark = ",")
  t_str <- if (is.na(st$n_months)) "--"
  else formatC(as.integer(st$n_months), format = "d")
  paste0(
    model_label, " & ",
    group_label, " & ",
    n_str,       " & ",
    t_str,       " & ",
    fmt3(st$ew_gross), " & ",
    fmt3(st$vw_gross), " & ",
    fmt3(st$ew_net),   " & ",
    fmt3(st$vw_net),   " \\\\"
  )
}

e1_rows <- c(
  # Carhart
  make_e1_row("Carhart 4-Factor", "Active",  pool_stats(car, "Active")),
  make_e1_row("",                 "Passive", pool_stats(car, "Passive")),
  "\\addlinespace",
  # FF6
  make_e1_row("FF 6-Factor",      "Active",  pool_stats(ff6, "Active")),
  make_e1_row("",                 "Passive", pool_stats(ff6, "Passive")),
  "\\addlinespace",
  # C5
  make_e1_row("Carhart + PSL",    "Active",  pool_stats(c5,  "Active")),
  make_e1_row("",                 "Passive", pool_stats(c5,  "Passive"))
)

e1_tex <- c(
  "\\begin{table}[H]",
  "\\centering",
  "\\caption{Pooled Alpha Comparison Across Factor Models (\\%, Annualised)}",
  "\\label{tab:alpha_comparison_robust}",
  "\\begin{threeparttable}",
  "\\begin{tabular}{llrrrrrr}",
  "\\toprule",
  " & & & & \\multicolumn{2}{c}{Gross Alpha} & \\multicolumn{2}{c}{Net Alpha} \\\\",
  "\\cmidrule(lr){5-6} \\cmidrule(lr){7-8}",
  "Model & Group & $N$ & $T_{\\max}$ & EW & VW & EW & VW \\\\",
  "\\midrule",
  e1_rows,
  "\\bottomrule",
  "\\end{tabular}",
  "\\begin{tablenotes}",
  "\\footnotesize",
  paste0(
    "\\item Pooled annualised alpha (\\%) for active and passive funds under ",
    "three factor specifications. \\textit{Carhart 4-Factor}: MKT-RF, SMB, ",
    "HML, MOM (\\citealt{Carhart1997}; main text baseline). \\textit{FF 6-Factor}: ",
    "MKT-RF, SMB, HML, RMW, CMA, MOM (\\citealt{FamaFrench2015} five-factor ",
    "model plus Carhart momentum). \\textit{Carhart + PSL}: Carhart four-factor ",
    "augmented with the \\textcite{PastorStambaugh2003} traded liquidity factor. ",
    "EW: equal-weighted cross-sectional mean of per-fund alphas. ",
    "VW: value-weighted cross-sectional mean using each fund's time-series ",
    "mean TNA as the weight. ",
    "Net returns are gross returns less one-twelfth of the static annual ",
    "expense ratio each month following \\textcite{BarrasScailletWermers2010}. ",
    "$N$: funds with at least 24 monthly observations; ",
    "$T_{\\max}$: maximum number of monthly observations per fund. ",
    "SMB is held constant across specifications at the \\textcite{FamaFrench1993} ",
    "construction (single $2\\times3$ size--BM sort); the two SMB definitions ",
    "correlate above 0.97 over the sample window. ",
    "Sample: ", SAMPLE_LABEL_MAIN, " for Carhart and FF 6-Factor; ",
    SAMPLE_LABEL_C5, " for Carhart + PSL (constrained by the December 2024 ",
    "Pastor liquidity data release)."
  ),
  "\\end{tablenotes}",
  "\\end{threeparttable}",
  "\\end{table}"
)
write_tex(e1_tex, "table_alpha_comparison_robust.tex")

# =============================================================================
# 4. TABLE E.2:  BOOTSTRAP PERCENTILE COMPARISON
# =============================================================================
cat("\n=== Table E.2: Bootstrap Percentile Comparison ===\n")

# Filter each bootstrap summary to the selected percentiles and order.
bt_car <- car$boot_summary %>% filter(percentile %in% PERCENTILES_TO_SHOW) %>%
  arrange(percentile)
bt_ff6 <- ff6$boot_summary %>% filter(percentile %in% PERCENTILES_TO_SHOW) %>%
  arrange(percentile)
bt_c5  <- c5$boot_summary  %>% filter(percentile %in% PERCENTILES_TO_SHOW) %>%
  arrange(percentile)

stopifnot(identical(bt_car$percentile, bt_ff6$percentile),
          identical(bt_car$percentile, bt_c5$percentile))

make_e2_row <- function(i) {
  paste0(
    formatC(bt_car$percentile[i], width = 2, flag = " "),  " & ",
    fmt3(bt_car$t_alpha_actual[i]),                        " & ",
    fmt3(bt_ff6$t_alpha_actual[i]),                        " & ",
    fmt3(bt_c5$t_alpha_actual[i]),                         " & ",
    fmt_prob(bt_car$pct_runs_below[i]),                    " & ",
    fmt_prob(bt_ff6$pct_runs_below[i]),                    " & ",
    fmt_prob(bt_c5$pct_runs_below[i]),                     " \\\\"
  )
}

e2_rows <- vapply(seq_len(nrow(bt_car)), make_e2_row, character(1))

e2_tex <- c(
  "\\begin{table}[H]",
  "\\small",
  "\\centering",
  "\\caption{Bootstrap Percentile Comparison Across Factor Models}",
  "\\label{tab:bootstrap_comparison_robust}",
  "\\begin{threeparttable}",
  "\\begin{tabular}{rrrrrrr}",
  "\\toprule",
  " & \\multicolumn{3}{c}{Actual $t(\\hat{\\alpha})$} & \\multicolumn{3}{c}{Prob.\\ Luck} \\\\",
  "\\cmidrule(lr){2-4} \\cmidrule(lr){5-7}",
  "Pctile & Carhart & FF6 & C+PSL & Carhart & FF6 & C+PSL \\\\",
  "\\midrule",
  e2_rows,
  "\\bottomrule",
  "\\end{tabular}",
  "\\begin{tablenotes}",
  "\\footnotesize",
  paste0(
    "\\item Percentiles of the empirical cross-sectional distribution of ",
    "Newey--West $t(\\hat{\\alpha})$ for actively managed funds (``Actual''), ",
    "paired with Prob.\\ Luck: the fraction of the $B = 10{,}000$ ",
    "\\textcite{FamaFrench2010} bootstrap iterations in which the simulated ",
    "percentile falls below the actual value. ",
    "Values below 5\\% at lower percentiles indicate underperformance ",
    "inconsistent with the zero-alpha null; values above 95\\% at upper ",
    "percentiles indicate outperformance inconsistent with luck. ",
    "\\textbf{Entries of 0.0\\% at lower percentiles indicate that zero out ",
    "of $B = 10{,}000$ bootstrap iterations produced a simulated $t$-statistic ",
    "as extreme as the actual value.} This is the strongest possible bootstrap ",
    "statement: actual left-tail performance exceeds the worst-luck scenario ",
    "across all simulated samples. Entries shown as $<$0.1\\% indicate ",
    "between 0 and 5 iterations out of $B = 10{,}000$ (i.e.\\ Prob.\\ Luck ",
    "$\\in (0, 0.05)$\\%). Bootstrap methodology is identical across ",
    "specifications: calendar months resampled with replacement, ",
    "factor model re-estimated on zero-alpha null series, Newey--West HAC ",
    "standard errors (6-month lag) applied symmetrically to actual and ",
    "simulated $t$-statistics following the symmetry requirement of ",
    "\\textcite{FamaFrench2010}. ",
    "\\textit{Carhart}: MKT-RF, SMB, HML, MOM. ",
    "\\textit{FF6}: MKT-RF, SMB, HML, RMW, CMA, MOM. ",
    "\\textit{C+PSL}: Carhart plus \\textcite{PastorStambaugh2003} traded ",
    "liquidity. Sample: ", SAMPLE_LABEL_MAIN, " for Carhart and FF6; ",
    SAMPLE_LABEL_C5, " for C+PSL."
  ),
  "\\end{tablenotes}",
  "\\end{threeparttable}",
  "\\end{table}"
)
write_tex(e2_tex, "table_bootstrap_comparison_robust.tex")

# =============================================================================
# 5. TABLE E.3:  BSW DECOMPOSITION COMPARISON AT GAMMA* = 0.20
# =============================================================================
cat("\n=== Table E.3: BSW Decomposition Comparison ===\n")

# Extract the gamma* = 20 row from each spec's BSW decomposition.
get_bsw_row <- function(spec, gamma_val = GAMMA_STAR) {
  row <- spec$bsw_df %>% filter(gamma == gamma_val)
  if (nrow(row) == 0L) stop(sprintf("gamma = %s not found in %s BSW decomposition",
                                    gamma_val, spec$label))
  list(
    pi0       = as.numeric(spec$bsw_meta$pi0_pct[1]),
    S_neg     = as.numeric(row$S_neg_pct[1]),
    S_pos     = as.numeric(row$S_pos_pct[1]),
    F_luck    = as.numeric(row$F_luck_pct[1]),
    T_unskill = as.numeric(row$T_unskilled_pct[1]),
    T_skill   = as.numeric(row$T_skilled_pct[1])
  )
}

br_car <- get_bsw_row(car)
br_ff6 <- get_bsw_row(ff6)
br_c5  <- get_bsw_row(c5)

make_e3_row <- function(label, br) {
  paste0(
    label,               " & ",
    fmt1(br$pi0),        "\\% & ",
    fmt1(br$S_neg),      "\\% & ",
    fmt1(br$S_pos),      "\\% & ",
    fmt1(br$F_luck),     "\\% & ",
    fmt1(br$T_unskill),  "\\% & ",
    fmt1(br$T_skill),    "\\% \\\\"
  )
}

e3_rows <- c(
  make_e3_row(car$label, br_car),
  make_e3_row(ff6$label, br_ff6),
  make_e3_row(c5$label,  br_c5)
)

e3_tex <- c(
  "\\begin{table}[H]",
  "\\centering",
  "\\caption{BSW (2010) Decomposition Comparison at $\\gamma^* = 0.20$}",
  "\\label{tab:bsw_comparison_robust}",
  "\\begin{threeparttable}",
  "\\begin{tabular}{lrrrrrr}",
  "\\toprule",
  "Model & $\\hat{\\pi}_0$ & $S^-_{\\gamma^*}$ & $S^+_{\\gamma^*}$ & $F_{\\gamma^*}$ & $T^-_{\\gamma^*}$ & $T^+_{\\gamma^*}$ \\\\",
  "\\midrule",
  e3_rows,
  "\\bottomrule",
  "\\end{tabular}",
  "\\begin{tablenotes}",
  "\\footnotesize",
  paste0(
    "\\item Four-way decomposition of active funds into zero-alpha, genuinely ",
    "unskilled, and genuinely skilled populations at significance threshold ",
    "$\\gamma^* = 0.20$, following \\textcite{BarrasScailletWermers2010}. ",
    "$\\hat{\\pi}_0$: \\textcite{Storey2002} estimator of the proportion of ",
    "zero-alpha funds at tuning parameter $\\lambda = 0.5$. ",
    "$S^-_{\\gamma^*}$ ($S^+_{\\gamma^*}$): observed fraction of active funds ",
    "with significantly negative (positive) Newey--West $t(\\hat{\\alpha})$ at ",
    "two-sided significance level $\\gamma^*$. ",
    "$F_{\\gamma^*} = \\hat{\\pi}_0 \\cdot \\gamma^*/2$: expected proportion ",
    "of false discoveries per tail. ",
    "$T^-_{\\gamma^*} = S^-_{\\gamma^*} - F_{\\gamma^*}$: genuinely unskilled ",
    "funds. ",
    "$T^+_{\\gamma^*} = S^+_{\\gamma^*} - F_{\\gamma^*}$: genuinely skilled ",
    "funds; negative values indicate right-tail significance does not exceed ",
    "the false-discovery rate at this threshold. ",
    "\\textit{Carhart 4-Factor}: \\citealt{Carhart1997} main-text baseline. ",
    "\\textit{FF 6-Factor}: \\citealt{FamaFrench2015} five-factor plus ",
    "momentum. ",
    "\\textit{Carhart + PSL}: Carhart plus \\textcite{PastorStambaugh2003} traded ",
    "liquidity. Sample: ", SAMPLE_LABEL_MAIN, " for Carhart and FF 6-Factor; ",
    SAMPLE_LABEL_C5, " for Carhart + PSL."
  ),
  "\\end{tablenotes}",
  "\\end{threeparttable}",
  "\\end{table}"
)
write_tex(e3_tex, "table_bsw_comparison_robust.tex")

# =============================================================================
# 6. SUMMARY + BIBLIOGRAPHY NOTE
# =============================================================================
cat("\n=== ALL APPENDIX E TABLES BUILT ===\n")
cat("Files written to:", normalizePath(OUT_DIR), "\n")
cat("  table_alpha_comparison_robust.tex       (Table E.1)\n")
cat("  table_bootstrap_comparison_robust.tex   (Table E.2)\n")
cat("  table_bsw_comparison_robust.tex         (Table E.3)\n")
cat("\n=== BIBLIOGRAPHY ENTRIES REQUIRED ===\n")
cat("Add the following to dissertation.bib if not already present:\n")
cat("
@article{FamaFrench2015,
  author    = {Fama, Eugene F. and French, Kenneth R.},
  title     = {A Five-Factor Asset Pricing Model},
  journal   = {Journal of Financial Economics},
  year      = {2015},
  volume    = {116},
  number    = {1},
  pages     = {1--22}
}

@article{PastorStambaugh2003,
  author    = {P{\\'a}stor, {\\v L}ubo{\\v s} and Stambaugh, Robert F.},
  title     = {Liquidity Risk and Expected Stock Returns},
  journal   = {Journal of Political Economy},
  year      = {2003},
  volume    = {111},
  number    = {3},
  pages     = {642--685}
}

@article{FamaFrench1993,
  author    = {Fama, Eugene F. and French, Kenneth R.},
  title     = {Common Risk Factors in the Returns on Stocks and Bonds},
  journal   = {Journal of Financial Economics},
  year      = {1993},
  volume    = {33},
  number    = {1},
  pages     = {3--56}
}
")