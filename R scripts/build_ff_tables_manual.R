# =============================================================================
# MANUAL FF TABLES BUILDER                                                  v1.2
#
# v1.2 change vs v1.1 (Family B audit + Family C follow-on):
#   (a) SAMPLE_LABEL extended to acknowledge the performance-comparison subsample
#       filter applied upstream by FF_comparison.R v1.3. No code-path change; the
#       extended label flows through the existing footnote concatenations in
#       Sections 1, 4, and 7. Tables 7, 9, 11 FF inherit the updated label
#       automatically.
#   (b) BSW (2010) net-return citation in Table 7 FF footnote (line 183)
#       corrected. Convention attributed to Carhart (1997) and Wermers (2000),
#       the originators; BSW (2010) shares the arithmetic but applies it in
#       the reverse direction. Same fix as alpha_reporting.R v8.3 line 325
#       and FF_comparison.R v1.3 line 565.
#
# v1.1 change vs v1.0:
#   Section 3 (Table 7 FF, file table_perf_aggregate_FF.tex) rewritten to
#   consume alpha_agg (5 groups x 2 weightings, from portfolio_alphas_FF.xlsx)
#   instead of alpha_full per-fund alphas with static mean-TNA weights. Now
#   matches the methodology of the main-body Table 5: portfolio-level Carhart
#   regression with Newey-West HAC SEs, interleaved coefficient + t-stat rows,
#   significance stars. Caption and footnote updated. Tables 9, 10, 11, 13 FF
#   (Sections 4-7) unchanged - they were not affected by the VW methodology
#   switch.
#
# v1.0 initial release:
#
# Generates Tables 7, 9, 10, 11, 13 (FF subperiod) as hand-coded LaTeX,
# bypassing kableExtra entirely. Use this when ff_comparison.R produces
# tables that fail to compile on Overleaf due to the kableExtra/booktabs/
# \noalign/\resizebox/\addlinespace issues we cannot reliably fix from R.
#
# WORKFLOW:
#   1. Run ff_comparison.R first (it does the computation and writes
#      alpha_fullperiod_FF.xlsx, bootstrap_results_FF.xlsx,
#      portfolio_alphas_FF.xlsx).
#   2. Run THIS script. It reads those xlsx files and overwrites the
#      five FF .tex files with manually-built versions.
#   3. Move the .tex files into your tables/ subdirectory and recompile.
#
# Output filenames are identical to ff_comparison.R so existing
# \input{tables/table_..._FF} lines in your master document still work.
# =============================================================================

library(readxl)
library(dplyr)

# =============================================================================
# 1. LOAD COMPUTATION OUTPUTS FROM ff_comparison.R
# =============================================================================
alpha_full   <- read_excel("alpha_fullperiod_FF.xlsx")
boot_summary <- read_excel("bootstrap_results_FF.xlsx", sheet = "summary")
bsw_df       <- read_excel("bootstrap_results_FF.xlsx", sheet = "bsw_decomposition")
bsw_meta     <- read_excel("bootstrap_results_FF.xlsx", sheet = "bsw_meta")
alpha_agg    <- read_excel("portfolio_alphas_FF.xlsx")

SAMPLE_LABEL <- "Jan 1995--Sep 2006 (Fama--French 2010 subperiod), performance-comparison subsample per flagged\\_funds.xlsx"

# =============================================================================
# 2. FORMATTING HELPERS
# =============================================================================
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

# Significance stars from a t-statistic
stars <- function(t) {
  ta <- abs(suppressWarnings(as.numeric(t)))
  if (is.na(ta))            return("")
  if (ta >= 2.576)          return("$^{***}$")
  if (ta >= 1.960)          return("$^{**}$")
  if (ta >= 1.645)          return("$^{*}$")
  ""
}

# TNA-weighted mean
wm <- function(x, w) {
  v <- !is.na(x) & !is.na(w) & w > 0
  if (sum(v) == 0L) return(NA_real_)
  sum(x[v] * w[v]) / sum(w[v])
}

# Write a vector of strings as a .tex file
write_tex <- function(lines, fn) {
  writeLines(lines, fn)
  cat("Written:", fn, "\n")
}

# =============================================================================
# 3. TABLE 7 (FF):  AGGREGATE PORTFOLIO ALPHA BY GROUP  -- NEW v1.1
# =============================================================================
# Consumes alpha_agg from portfolio_alphas_FF.xlsx, which contains portfolio-
# level Carhart alphas for all 5 groups x 2 weightings (10 rows). Uses the
# FF (2010) Table II convention: coefficient row followed by NW t-stat row.
cat("=== Table 7 (FF, v1.1: portfolio regression) ===\n")

# Significance stars helper
add_stars_t <- function(val_str, t_stat) {
  if (is.na(val_str) || val_str %in% c("--", "")) return(val_str)
  t_abs <- suppressWarnings(abs(as.numeric(t_stat)))
  if (is.na(t_abs)) return(val_str)
  if      (t_abs >= 2.576) paste0(val_str, "$^{***}$")
  else if (t_abs >= 1.960) paste0(val_str, "$^{**}$")
  else if (t_abs >= 1.645) paste0(val_str, "$^{*}$")
  else                     val_str
}

# Fund counts per group for the N column (computed from per-fund alpha_full).
n_funds_by_group <- list(
  "Active"           = sum(alpha_full$ap_group == "Active",  na.rm = TRUE),
  "Passive"          = sum(alpha_full$ap_group == "Passive", na.rm = TRUE),
  "Unknown"          = sum(alpha_full$ap_group == "Unknown", na.rm = TRUE),
  "Active + Passive" = sum(alpha_full$ap_group %in% c("Active","Passive"), na.rm = TRUE),
  "Full Sample"      = nrow(alpha_full)
)

make_t7_block <- function(grp) {
  eg <- alpha_agg %>% filter(ap_group == grp, weighting == "EW")
  vg <- alpha_agg %>% filter(ap_group == grp, weighting == "VW")
  if (nrow(eg) == 0L || nrow(vg) == 0L) return(character(0))
  
  coef_row <- paste0(
    grp,                                             " & ",
    formatC(n_funds_by_group[[grp]], format = "d", big.mark = ","),   " & ",
    formatC(eg$n_months, format = "d"),              " & ",
    add_stars_t(fmt3(eg$alpha_car),     eg$t_car),     " & ",
    add_stars_t(fmt3(vg$alpha_car),     vg$t_car),     " & ",
    add_stars_t(fmt3(eg$alpha_car_net), eg$t_car_net), " & ",
    add_stars_t(fmt3(vg$alpha_car_net), vg$t_car_net), " \\\\"
  )
  t_row <- paste0(
    "t(coef)",                                       " & & & ",
    "(", fmt2(eg$t_car),     ")",                    " & ",
    "(", fmt2(vg$t_car),     ")",                    " & ",
    "(", fmt2(eg$t_car_net), ")",                    " & ",
    "(", fmt2(vg$t_car_net), ")",                    " \\\\"
  )
  c(coef_row, t_row)
}

t7_rows <- unlist(lapply(
  c("Active", "Passive", "Unknown", "Active + Passive", "Full Sample"),
  make_t7_block
))

# Midrule after the Unknown t-stat row (row 6 of 10) to separate individual
# groups from the combined aggregates.
t7_rows_with_rule <- c(t7_rows[1:6], "\\midrule", t7_rows[7:10])

t7_tex <- c(
  "\\begin{table}[H]",
  "\\centering",
  "\\caption{Aggregate Portfolio Alpha by Group (\\%, Annualised) -- Fama--French (2010) Subperiod}",
  "\\label{tab:perf_aggregate_FF}",
  "\\begin{threeparttable}",
  "\\begin{tabular}{lrrrrrr}",
  "\\toprule",
  " & & & \\multicolumn{2}{c}{Gross Alpha} & \\multicolumn{2}{c}{Net Alpha} \\\\",
  "\\cmidrule(lr){4-5} \\cmidrule(lr){6-7}",
  "Group & $N$ & $T$ & EW & VW & EW & VW \\\\",
  "\\midrule",
  t7_rows_with_rule,
  "\\bottomrule",
  "\\end{tabular}",
  "\\begin{tablenotes}",
  "\\footnotesize",
  paste0(
    "\\item Annualised \\textcite{Carhart1997} four-factor alpha (\\%) from ",
    "regressing the monthly aggregate portfolio return of each group on the ",
    "market, size, value, and momentum factors, following \\textcite{FamaFrench2010}. ",
    "EW: equal-weighted portfolio (each fund alive in month $t$ contributes $1/N_t$). ",
    "VW: value-weighted portfolio with lagged TNA weights ",
    "$w_{i,t-1} = \\text{TNA}_{i,t-1} / \\sum_j \\text{TNA}_{j,t-1}$. ",
    "Net returns are computed as gross returns less one-twelfth of the static ",
    "annual expense ratio each month, following \\textcite{Carhart1997} and \\textcite{Wermers2000}. ",
    "Newey-West $t$-statistics (6-month lag) in parentheses below each alpha; ",
    "$^{*}$, $^{**}$, $^{***}$: significant at 10\\%, 5\\%, 1\\%. ",
    "$N$: unique funds contributing to the portfolio series; ",
    "$T$: number of monthly observations in the regression. ",
    "The Active + Passive row aggregates only the two classified groups. ",
    "Sample: ", SAMPLE_LABEL, ". The subperiod is the maximum overlap ",
    "between this study's data window (Dec 1994 onwards) and the ",
    "\\textcite{FamaFrench2010} sample (Jan 1984--Sep 2006), enabling ",
    "direct comparison with their Tables I--III."
  ),
  "\\end{tablenotes}",
  "\\end{threeparttable}",
  "\\end{table}"
)
write_tex(t7_tex, "table_perf_aggregate_FF.tex")

# =============================================================================
# 4. TABLE 9 (FF):  BOOTSTRAP PERCENTILES
# =============================================================================
cat("=== Table 9 (FF) ===\n")

bt <- boot_summary[boot_summary$percentile %in% c(1, 5, 10, 50, 90, 95, 99), ]
bt <- bt[order(bt$percentile), ]
n_active_bs <- sum(alpha_full$ap_group == "Active" & !is.na(alpha_full$alpha_t_nw))

interp <- ifelse(bt$percentile <= 10 & bt$pct_runs_below < 5,
                 "Worse than luck (significant)",
                 ifelse(bt$percentile >= 90 & bt$pct_runs_below > 95,
                        "Evidence of genuine skill",
                        ifelse(bt$percentile == 50,
                               "Indistinguishable from luck",
                               "Consistent with zero-skill")))

t9_rows <- paste0(
  formatC(bt$percentile, width = 2, flag = " "), " & ",
  fmt3(bt$t_alpha_actual),    " & ",
  fmt3(bt$t_alpha_sim_mean),  " & ",
  fmt1(bt$pct_runs_below), "\\% & ",
  interp, " \\\\"
)

t9_tex <- c(
  "\\begin{table}[H]",
  "\\small",
  "\\centering",
  "\\caption{Fama--French (2010) Bootstrap: Actual vs.\\ Simulated $t(\\hat{\\alpha})$ Percentiles -- FF Subperiod}",
  "\\label{tab:bootstrap_tails_FF}",
  "\\begin{threeparttable}",
  "\\begin{tabular}{rrrrl}",
  "\\toprule",
  "Percentile & Actual $t(\\hat{\\alpha})$ & Simulated Mean & Prob.\\ Luck & Interpretation \\\\",
  "\\midrule",
  t9_rows,
  "\\bottomrule",
  "\\end{tabular}",
  "\\begin{tablenotes}",
  "\\footnotesize",
  paste0(
    "\\item Bootstrap procedure follows \\textcite{FamaFrench2010}. ",
    "Sample: actively managed funds, ",
    SAMPLE_LABEL, ", minimum 24 monthly observations ($N = ", n_active_bs,
    "$ funds). For each fund, estimated monthly alpha is subtracted from ",
    "the excess return series to construct a zero-alpha null return. ",
    "In each of $B = 10{,}000$ bootstrap iterations, calendar months are ",
    "resampled with replacement, preserving cross-sectional factor return ",
    "dependence. The \\textcite{Carhart1997} four-factor model is re-estimated on ",
    "each resampled series. \\textit{Actual} $t(\\hat{\\alpha})$: percentile ",
    "of the empirical $t$-statistic distribution across active funds. ",
    "\\textit{Simulated Mean}: average of that percentile across all ",
    "iterations. \\textit{Prob.\\ Luck}: fraction of iterations in which ",
    "the simulated percentile falls below the actual value; values below ",
    "5\\% at lower percentiles indicate underperformance unlikely to be ",
    "explained by luck alone. Newey--West standard errors with a 6-month ",
    "lag are used throughout. Compare with \\textcite{FamaFrench2010}, Table~III."
  ),
  "\\end{tablenotes}",
  "\\end{threeparttable}",
  "\\end{table}"
)
write_tex(t9_tex, "table_bootstrap_tails_FF.tex")

# =============================================================================
# 5. TABLE 10 (FF):  pi_0 ESTIMATE
# =============================================================================
cat("=== Table 10 (FF) ===\n")

pi0_pct <- as.numeric(bsw_meta$pi0_pct[1])
total_n <- as.integer(bsw_meta$n_active[1])

interp10 <- (
  if (pi0_pct > 90)      "Industry dominated by luck"
  else if (pi0_pct > 75) "Heterogeneous skill; majority zero-alpha"
  else if (pi0_pct > 50) "Moderate skill heterogeneity"
  else                   "Substantial skilled-fund presence"
)

t10_tex <- c(
  "\\begin{table}[H]",
  "\\centering",
  "\\caption{Aggregate Skill Estimate: Proportion of True Zero-Alpha Active Funds -- FF Subperiod}",
  "\\label{tab:pi0_estimate_FF}",
  "\\begin{threeparttable}",
  "\\begin{tabular}{lrrrl}",
  "\\toprule",
  "Metric & Estimate & $N$ & $\\lambda$ & Interpretation \\\\",
  "\\midrule",
  paste0(
    "$\\hat{\\pi}_0$: Proportion of True Zero-Alpha Active Funds & ",
    fmt1(pi0_pct), "\\% & ",
    total_n, " & ",
    "0.5 & ",
    interp10, " \\\\"
  ),
  "\\bottomrule",
  "\\end{tabular}",
  "\\begin{tablenotes}",
  "\\footnotesize",
  paste0(
    "\\item The proportion of true zero-alpha funds ($\\hat{\\pi}_0$) is ",
    "estimated following \\textcite{Storey2002} and \\textcite{BarrasScailletWermers2010}, ",
    "using p-values from Newey--West ",
    "$t$-tests on full-period \\textcite{Carhart1997} four-factor alphas. The ",
    "estimator is $\\hat{\\pi}_0 = |\\{p_i > \\lambda\\}| \\,/\\, [N(1-\\lambda)]$, ",
    "where $\\lambda = 0.5$ is the standard tuning parameter following ",
    "\\textcite{Storey2002}. Funds with p-values exceeding $\\lambda$ are unlikely to ",
    "have non-zero true alpha; the density of p-values in $(\\lambda, 1]$ ",
    "provides a conservative estimate of the zero-alpha proportion, bounded ",
    "above at 1. Sample: actively managed funds ($N = ", total_n, "$), ",
    SAMPLE_LABEL, ". Passive and Unknown-classified funds are excluded. ",
    "Compare with \\textcite{FamaFrench2010}, ",
    "Table~III, which reports an analogous bootstrap-based assessment over ",
    "Jan 1984--Sep 2006."
  ),
  "\\end{tablenotes}",
  "\\end{threeparttable}",
  "\\end{table}"
)
write_tex(t10_tex, "table_pi0_estimate_FF.tex")

# =============================================================================
# 6. TABLE 11 (FF):  BSW FOUR-WAY DECOMPOSITION
# =============================================================================
cat("=== Table 11 (FF) ===\n")

# Bold the gamma=0.20 row by wrapping its cells in \textbf{}
boldify <- function(s, do_bold) if (do_bold) paste0("\\textbf{", s, "}") else s

t11_rows <- vapply(seq_len(nrow(bsw_df)), function(i) {
  is_bold <- as.numeric(bsw_df$gamma[i]) == 20
  paste0(
    boldify(paste0(fmt0(bsw_df$gamma[i]), "\\%"),     is_bold), " & ",
    boldify(fmt1(bsw_df$S_neg_pct[i]),                 is_bold), " & ",
    boldify(fmt1(bsw_df$S_pos_pct[i]),                 is_bold), " & ",
    boldify(fmt1(bsw_df$F_luck_pct[i]),                is_bold), " & ",
    boldify(fmt1(bsw_df$T_unskilled_pct[i]),           is_bold), " & ",
    boldify(fmt1(bsw_df$T_skilled_pct[i]),             is_bold), " \\\\"
  )
}, character(1))

t11_tex <- c(
  "\\begin{table}[H]",
  "\\small",
  "\\centering",
  "\\caption{BSW (2010) Four-Way Decomposition: Proportions of Skilled, Unskilled, and Lucky Funds (\\%) -- FF Subperiod}",
  "\\label{tab:bsw_decomposition_FF}",
  "\\begin{threeparttable}",
  "\\begin{tabular}{rrrrrr}",
  "\\toprule",
  " & \\multicolumn{2}{c}{Observed Tails} & False Disc. & \\multicolumn{2}{c}{True Proportions} \\\\",
  "\\cmidrule(lr){2-3} \\cmidrule(lr){4-4} \\cmidrule(lr){5-6}",
  "$\\gamma$ & $S^-_\\gamma$ & $S^+_\\gamma$ & $F_\\gamma$ & $T^-_\\gamma$ & $T^+_\\gamma$ \\\\",
  "\\midrule",
  t11_rows,
  "\\bottomrule",
  "\\end{tabular}",
  "\\begin{tablenotes}",
  "\\footnotesize",
  paste0(
    "\\item Decomposition follows \\textcite{BarrasScailletWermers2010}, ",
    "Section~II.B and Table~III. ",
    "$S^-_\\gamma$ ($S^+_\\gamma$): observed fraction of active funds with ",
    "significantly negative (positive) Newey--West $t(\\hat{\\alpha})$ at ",
    "two-sided significance level $\\gamma$, using full-period \\textcite{Carhart1997} ",
    "four-factor alphas. Critical values are from the standard normal ",
    "distribution, consistent with the large-sample approximation in ",
    "\\textcite{BarrasScailletWermers2010}. ",
    "$F_\\gamma = \\hat{\\pi}_0 \\cdot \\gamma/2$: expected proportion ",
    "of false discoveries per tail arising from zero-alpha funds, where ",
    "$\\hat{\\pi}_0 = ", fmt1(pi0_pct), "\\%$ is the \\textcite{Storey2002} estimate ",
    "at $\\lambda = 0.5$ (see Table~\\ref{tab:pi0_estimate_FF}). ",
    "$T^-_\\gamma = S^-_\\gamma - F_\\gamma$: genuinely unskilled funds ",
    "(significant negative alpha net of false discoveries). ",
    "$T^+_\\gamma = S^+_\\gamma - F_\\gamma$: genuinely skilled funds. ",
    "The bolded row ($\\gamma = 0.20$) provides the population-level ",
    "estimates $\\hat{\\pi}^-_A$ and $\\hat{\\pi}^+_A$ following ",
    "\\textcite{BarrasScailletWermers2010}. ",
    "Negative $T^+_\\gamma$ entries indicate right-tail significance does ",
    "not exceed the false-discovery rate at that threshold. Sample: $N = ",
    total_n, "$ actively managed funds, ", SAMPLE_LABEL,
    "; Passive and Unknown funds excluded."
  ),
  "\\end{tablenotes}",
  "\\end{threeparttable}",
  "\\end{table}"
)
write_tex(t11_tex, "table10b_bsw_decomposition_FF.tex")

# =============================================================================
# 7. TABLE 13 (FF):  AGGREGATE PORTFOLIO ALPHAS
# =============================================================================
cat("=== Table 13 (FF) ===\n")

# Build interleaved coefficient / t-stat rows for Active and Passive, EW and VW
make_t13_block <- function(grp) {
  out <- character(0)
  for (wt in c("EW", "VW")) {
    r <- alpha_agg[alpha_agg$ap_group == grp & alpha_agg$weighting == wt, ]
    if (nrow(r) == 0) next
    label <- paste0(grp, " (", wt, ")")
    coef_row <- paste0(
      label, " & ",
      fmt3(r$alpha_capm), stars(r$t_capm), " & ",
      fmt3(r$alpha_ff3),  stars(r$t_ff3),  " & ",
      fmt3(r$alpha_car),  stars(r$t_car),  " \\\\"
    )
    t_row <- paste0(
      "$t$(coef) & ",
      "(", fmt2(r$t_capm), ") & ",
      "(", fmt2(r$t_ff3),  ") & ",
      "(", fmt2(r$t_car),  ") \\\\"
    )
    out <- c(out, coef_row, t_row)
  }
  out
}

t13_rows <- c(make_t13_block("Active"), make_t13_block("Passive"))

t13_tex <- c(
  "\\begin{table}[H]",
  "\\centering",
  "\\caption{Active vs.\\ Passive Aggregate Portfolio Alpha (\\%, Annualised) -- FF Subperiod}",
  "\\label{tab:port_agg_alpha_FF}",
  "\\begin{threeparttable}",
  "\\begin{tabular}{lrrr}",
  "\\toprule",
  "Portfolio & $\\alpha_{\\text{CAPM}}$ & $\\alpha_{\\text{FF3}}$ & $\\alpha_{\\text{Car}}$ \\\\",
  "\\midrule",
  t13_rows,
  "\\bottomrule",
  "\\end{tabular}",
  "\\begin{tablenotes}",
  "\\footnotesize",
  paste0(
    "\\item Monthly EW and VW aggregate portfolio returns regressed on CAPM, ",
    "Fama--French three-factor, and \\textcite{Carhart1997} four-factor models. ",
    "Alphas annualised ($\\times 12$) and expressed as \\%. Newey--West ",
    "$t$-statistics (6-month lag) in parentheses. $^{*}$, $^{**}$, $^{***}$: ",
    "significant at 10\\%, 5\\%, 1\\% respectively. Sample: ", SAMPLE_LABEL,
    ". Compare with \\textcite{FamaFrench2010}, Table~I."
  ),
  "\\end{tablenotes}",
  "\\end{threeparttable}",
  "\\end{table}"
)
write_tex(t13_tex, "table_port_agg_alpha_FF.tex")

# =============================================================================
# 8. SUMMARY
# =============================================================================
cat("\n=== ALL FF TABLES BUILT MANUALLY ===\n")
cat("Files written to current directory:\n")
cat("  table_perf_aggregate_FF.tex      Table 7  (FF)\n")
cat("  table_bootstrap_tails_FF.tex     Table 9  (FF)\n")
cat("  table_pi0_estimate_FF.tex        Table 10 (FF)\n")
cat("  table10b_bsw_decomposition_FF.tex Table 11 (FF)\n")
cat("  table_port_agg_alpha_FF.tex      Table 13 (FF)\n")
cat("\nMove these into your tables/ subdirectory and recompile.\n")
cat("Figure 3 (FF) is unaffected -- ff_comparison.R produces it directly\n")
cat("as a PNG via ggplot2; no kableExtra involvement.\n")