# =============================================================================
# ROBUST APPENDIX TABLES BUILDER (v1.3)
#
# Purpose
# -------
# Generates LaTeX tables for Appendix I (Factor Model Robustness).
# Pure output script: reads all pre-computed results from xlsx, no regressions.
#
# v1.3 changes vs v1.2
# --------------------
#   (a) Table I.1: reads aggregate portfolio alphas from the new port_alpha
#       sheet in bootstrap_results_{spec}.xlsx (written by alpha_estimation_
#       robust.R v1.4). Replaces the old pooled cross-sectional mean approach
#       (pool_stats). Layout gains t-stat rows and an R²-adjusted column,
#       matching Table 4.5 format. Caption updated accordingly.
#   (b) Footnotes added to all three tables using the proven inline
#       \par\medskip + singlespace pattern from the main-text tables (4.5,
#       4.6, 4.8). The move_note_after_table() threeparttable workaround is
#       retired — it was silently dropping footnotes in the output .tex files.
#   (c) Passive fund count in I.1 now reflects the updated flagged_funds.xlsx
#       ledger automatically (the estimation script writes the correct counts).
#
# Workflow
# --------
#   1. Run alpha_estimation_robust.R v1.4 -> produces bootstrap_results_*.xlsx
#      each containing a port_alpha sheet alongside existing sheets.
#   2. Run THIS script (no session panel needed).
#
# Tables produced
# ---------------
#   table_alpha_comparison_robust.tex     Table I.1 (aggregate portfolio alpha)
#   table_bootstrap_comparison_robust.tex Table I.2 (bootstrap percentiles)
#   table_bsw_comparison_robust.tex       Table I.3 (BSW decomposition)
# =============================================================================

library(readxl)
library(dplyr)

# --- 0. CONFIGURATION --------------------------------------------------------
# OUT_DIR: where to write the .tex files. Defaults to "." (current working
# directory = WORKING_DIR set by master_pipeline.R), so that the master
# pipeline's end-of-run sync step picks up table_*.tex files correctly.
# Override by setting OUT_DIR before sourcing this script.
if (!exists("OUT_DIR", inherits = TRUE) || is.null(OUT_DIR)) {
  OUT_DIR <- "."
}
dir.create(OUT_DIR, showWarnings = FALSE, recursive = TRUE)

PERCENTILES_TO_SHOW <- c(1, 5, 10, 50, 90, 95, 99)
GAMMA_STAR          <- 20

# --- 1. LOAD ALL OUTPUTS -----------------------------------------------------
cat("=== Loading Excel outputs ===\n")

load_spec <- function(spec_suffix, label) {
  # spec_suffix: "Carhart", "FF6", or "C5"
  fp <- sprintf("alpha_fullperiod_%s.xlsx",   spec_suffix)
  bp <- sprintf("bootstrap_results_%s.xlsx",  spec_suffix)
  list(
    alpha_full   = read_excel(fp),
    boot_summary = read_excel(bp, sheet = "summary"),
    bsw_df       = read_excel(bp, sheet = "bsw_decomposition"),
    bsw_meta     = read_excel(bp, sheet = "bsw_meta"),
    port_alpha   = read_excel(bp, sheet = "port_alpha"),  # NEW v1.4
    label        = label
  )
}

car <- load_spec("Carhart", "Carhart 4-Factor")
ff6 <- load_spec("FF6",     "FF 6-Factor")
c5  <- load_spec("C5",      "Carhart + PSL")

cat("Carhart port_alpha rows:", nrow(car$port_alpha),
    "| FF6:", nrow(ff6$port_alpha),
    "| C5:",  nrow(c5$port_alpha), "\n")

# --- 2. FORMATTING HELPERS ---------------------------------------------------
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

fmt_prob <- function(x) {
  v <- suppressWarnings(as.numeric(x))
  ifelse(is.na(v), "--",
    ifelse(v == 0,   "0.0\\%",
    ifelse(v < 0.05, "$<$0.1\\%",
           paste0(formatC(round(v, 1), format = "f", digits = 1), "\\%"))))
}

# Significance stars from two-sided t-statistic.
add_stars <- function(val_str, t_stat) {
  if (is.na(val_str) || val_str == "--") return(val_str)
  t_abs <- suppressWarnings(abs(as.numeric(t_stat)))
  if (is.na(t_abs)) return(val_str)
  if      (t_abs >= 2.576) paste0(val_str, "$^{***}$")
  else if (t_abs >= 1.960) paste0(val_str, "$^{**}$")
  else if (t_abs >= 1.645) paste0(val_str, "$^{*}$")
  else                     val_str
}

write_tex <- function(lines, fn) {
  path <- file.path(OUT_DIR, fn)
  writeLines(lines, path)
  cat("Written:", path, "\n")
}

# =============================================================================
# 3. TABLE I.1:  AGGREGATE PORTFOLIO ALPHA COMPARISON ACROSS FACTOR MODELS
#    Reads port_alpha sheet from bootstrap_results_{spec}.xlsx.
#    Two rows per spec (Active + Passive), each followed by a t-stat row.
#    Columns: Model, Group, N, T, EW gross, VW gross, EW net, VW net, R²-adj.
# =============================================================================
cat("\n=== Table I.1: Aggregate Portfolio Alpha Comparison ===\n")

# Extract one group's row from a spec's port_alpha data frame.
# alpha values in port_alpha are stored in annualised decimal form (×12);
# convert to percent (×100) for display.
get_port_row <- function(spec, group) {
  r <- spec$port_alpha %>% filter(Group == group)
  if (nrow(r) == 0L)
    return(list(n_funds=0L, t_months=NA_integer_,
                ew_gross=NA_real_, t_ew_gross=NA_real_,
                vw_gross=NA_real_, t_vw_gross=NA_real_,
                ew_net  =NA_real_, t_ew_net  =NA_real_,
                vw_net  =NA_real_, t_vw_net  =NA_real_,
                r2_adj_ew=NA_real_))
  list(
    n_funds    = as.integer(r$n_funds[1]),
    t_months   = as.integer(r$t_months[1]),
    ew_gross   = as.numeric(r$alpha_ew_gross[1]) * 100,
    t_ew_gross = as.numeric(r$t_ew_gross[1]),
    vw_gross   = as.numeric(r$alpha_vw_gross[1]) * 100,
    t_vw_gross = as.numeric(r$t_vw_gross[1]),
    ew_net     = as.numeric(r$alpha_ew_net[1])   * 100,
    t_ew_net   = as.numeric(r$t_ew_net[1]),
    vw_net     = as.numeric(r$alpha_vw_net[1])   * 100,
    t_vw_net   = as.numeric(r$t_vw_net[1]),
    r2_adj_ew  = as.numeric(r$r2_adj_ew[1])
  )
}

# Build a two-row block (alpha row + t-stat row) for one group × one spec.
make_e1_block <- function(model_label, group_label, st) {
  n_str <- if (st$n_funds == 0L || is.na(st$n_funds)) "--"
            else formatC(st$n_funds, format = "d", big.mark = ",")
  t_str <- if (is.na(st$t_months)) "--"
            else formatC(as.integer(st$t_months), format = "d")

  ew_g <- add_stars(fmt3(st$ew_gross), st$t_ew_gross)
  vw_g <- add_stars(fmt3(st$vw_gross), st$t_vw_gross)
  ew_n <- add_stars(fmt3(st$ew_net),   st$t_ew_net)
  vw_n <- add_stars(fmt3(st$vw_net),   st$t_vw_net)
  r2   <- fmt3(st$r2_adj_ew)

  alpha_row <- paste0(
    model_label, " & ", group_label, " & ",
    n_str, " & ", t_str, " & ",
    ew_g, " & ", vw_g, " & ",
    ew_n, " & ", vw_n, " & ",
    r2, " \\\\"
  )
  # t-stat row: model/group/N/T cells blank; R² cell blank.
  t_row <- paste0(
    "t(coef) &  &  &  & ",
    "(", fmt2(st$t_ew_gross), ") & ",
    "(", fmt2(st$t_vw_gross), ") & ",
    "(", fmt2(st$t_ew_net),   ") & ",
    "(", fmt2(st$t_vw_net),   ") & ",
    " \\\\"
  )
  c(alpha_row, t_row)
}

e1_rows <- c(
  make_e1_block("Carhart 4-Factor", "Active",  get_port_row(car, "Active")),
  make_e1_block("",                 "Passive", get_port_row(car, "Passive")),
  "\\addlinespace",
  make_e1_block("FF 6-Factor",      "Active",  get_port_row(ff6, "Active")),
  make_e1_block("",                 "Passive", get_port_row(ff6, "Passive")),
  "\\addlinespace",
  make_e1_block("Carhart + PSL",    "Active",  get_port_row(c5,  "Active")),
  make_e1_block("",                 "Passive", get_port_row(c5,  "Passive"))
)

e1_tex <- c(
  "\\begin{table}[!htbp]",
  "\\centering",
  "\\sbox{\\tabletempbox}{%",
  "\\footnotesize",
  "\\begin{tabular}{llrrrrrrrrr}",
  "\\toprule",
  " & & & & \\multicolumn{2}{c}{Gross Alpha} & \\multicolumn{2}{c}{Net Alpha} & \\\\",
  "\\cmidrule(lr){5-6} \\cmidrule(lr){7-8}",
  "Model & Group & $N$ & $T$ & EW & VW & EW & VW & $\\bar{R}^2$ \\\\",
  "\\midrule",
  e1_rows,
  "\\bottomrule",
  "\\end{tabular}%",
  "}",
  "\\setlength{\\tabletempwidth}{\\wd\\tabletempbox}",
  "\\ifdim\\tabletempwidth>\\linewidth\\setlength{\\tabletempwidth}{\\linewidth}\\fi",
  "\\begin{minipage}{\\tabletempwidth}",
  "\\captionsetup{width=\\linewidth}",
  "\\caption{\\label{tab:alpha_comparison_robust}Aggregate Portfolio Alpha Comparison Across Factor Models (\\%, Annualised)}",
  "\\ifdim\\wd\\tabletempbox>\\linewidth",
  "  \\resizebox{\\linewidth}{!}{\\usebox{\\tabletempbox}}",
  "\\else",
  "  \\usebox{\\tabletempbox}",
  "\\fi",
  "\\par\\medskip",
  "\\begin{singlespace}\\footnotesize\\noindent",
  paste0(
    "Annualised alpha (\\%) from regressing the monthly aggregate portfolio ",
    "return of each group on the specified factor model, following ",
    "\\textcite{FamaFrench2010}. ",
    "EW: equal-weighted portfolio ($1/N_t$ weight per fund per month). ",
    "VW: value-weighted portfolio with lagged TNA weights ",
    "$w_{i,t-1} = \\text{TNA}_{i,t-1}/\\sum_j \\text{TNA}_{j,t-1}$. ",
    "Net returns are gross returns less one-twelfth of the static annual ",
    "expense ratio each month, following \\textcite{Carhart1997} and ",
    "\\textcite{Wermers2000}. ",
    "Newey-West $t$-statistics (6-month lag) in parentheses below each alpha; ",
    "$^{*}$, $^{**}$, $^{***}$: significant at 10\\%, 5\\%, 1\\%. ",
    "$\\bar{R}^2$: adjusted $R^2$ from the EW gross portfolio regression. ",
    "$N$: unique funds contributing to the portfolio; ",
    "$T$: monthly observations in the regression. ",
    "\\textit{Carhart 4-Factor}: MKT-RF, SMB, HML, MOM ",
    "(\\citealt{Carhart1997}; main-text baseline). ",
    "\\textit{FF 6-Factor}: MKT-RF, SMB, HML, RMW, CMA, MOM ",
    "(\\citealt{FamaFrench2015} five-factor model plus momentum). ",
    "\\textit{Carhart + PSL}: Carhart four-factor augmented with the ",
    "\\textcite{PastorStambaugh2003} traded liquidity factor. ",
    "SMB is held constant at the \\textcite{FamaFrench1993} construction ",
    "(single $2\\times3$ size--BM sort) across all specifications. ",
    "Carhart and FF 6-Factor: Dec 1994--Feb 2026. ",
    "Carhart + PSL: Dec 1994--Dec 2024 (constrained by Pastor liquidity data release). ",
    "Sample: Incubation-corrected panel (\\citealt{Evans2010}), no date cap; ",
    "performance-comparison subsample per flagged\\_funds.xlsx."
  ),
  "\\end{singlespace}",
  "\\end{minipage}",
  "\\end{table}"
)

# Fix the tabular spec: count columns in header row.
# Model & Group & N & T & EW & VW & EW & VW & R2 = 9 columns = ll rrr rrr r = llrrrrrrr
e1_tex[grep("begin\\{tabular\\}", e1_tex)] <- "\\begin{tabular}{llrrrrrrr}"
write_tex(e1_tex, "table_alpha_comparison_robust.tex")

# =============================================================================
# 4. TABLE I.2:  BOOTSTRAP PERCENTILE COMPARISON
# =============================================================================
cat("\n=== Table I.2: Bootstrap Percentile Comparison ===\n")

n_active_boot <- tryCatch(as.integer(car$bsw_meta$n_active[1]),
                          error = function(e) NA_integer_)
n_active_str  <- if (is.na(n_active_boot)) {
  "$N$"
} else {
  paste0("$N = ", formatC(n_active_boot, format="d", big.mark=","), "$")
}

bt_car <- car$boot_summary %>% filter(percentile %in% PERCENTILES_TO_SHOW) %>% arrange(percentile)
bt_ff6 <- ff6$boot_summary %>% filter(percentile %in% PERCENTILES_TO_SHOW) %>% arrange(percentile)
bt_c5  <- c5$boot_summary  %>% filter(percentile %in% PERCENTILES_TO_SHOW) %>% arrange(percentile)

stopifnot(identical(bt_car$percentile, bt_ff6$percentile),
          identical(bt_car$percentile, bt_c5$percentile))

make_e2_row <- function(i) {
  paste0(
    formatC(bt_car$percentile[i], width = 2, flag = " "), " & ",
    fmt3(bt_car$t_alpha_actual[i]),                       " & ",
    fmt3(bt_ff6$t_alpha_actual[i]),                       " & ",
    fmt3(bt_c5$t_alpha_actual[i]),                        " & ",
    fmt_prob(bt_car$pct_runs_below[i]),                   " & ",
    fmt_prob(bt_ff6$pct_runs_below[i]),                   " & ",
    fmt_prob(bt_c5$pct_runs_below[i]),                    " \\\\"
  )
}

e2_rows <- vapply(seq_len(nrow(bt_car)), make_e2_row, character(1))

e2_tex <- c(
  "\\begin{table}[!htbp]",
  "\\centering",
  "\\sbox{\\tabletempbox}{%",
  "\\footnotesize",
  "\\begin{tabular}{rrrrrrr}",
  "\\toprule",
  " & \\multicolumn{3}{c}{Actual $t(\\hat{\\alpha})$} & \\multicolumn{3}{c}{Prob.\\ Luck} \\\\",
  "\\cmidrule(lr){2-4} \\cmidrule(lr){5-7}",
  "Pctile & Carhart & FF6 & C+PSL & Carhart & FF6 & C+PSL \\\\",
  "\\midrule",
  e2_rows,
  "\\bottomrule",
  "\\end{tabular}%",
  "}",
  "\\setlength{\\tabletempwidth}{\\wd\\tabletempbox}",
  "\\ifdim\\tabletempwidth>\\linewidth\\setlength{\\tabletempwidth}{\\linewidth}\\fi",
  "\\begin{minipage}{\\tabletempwidth}",
  "\\captionsetup{width=\\linewidth}",
  "\\caption{\\label{tab:bootstrap_comparison_robust}Bootstrap Percentile Comparison Across Factor Models}",
  "\\ifdim\\wd\\tabletempbox>\\linewidth",
  "  \\resizebox{\\linewidth}{!}{\\usebox{\\tabletempbox}}",
  "\\else",
  "  \\usebox{\\tabletempbox}",
  "\\fi",
  "\\par\\medskip",
  "\\begin{singlespace}\\footnotesize\\noindent",
  paste0(
    "Percentiles of the empirical $t(\\hat{\\alpha})$ distribution across ",
    "actively managed funds (``Actual''), paired with Prob.\\ Luck: the ",
    "fraction of $B = 10{,}000$ \\textcite{FamaFrench2010} bootstrap ",
    "iterations in which the simulated percentile falls below the actual value. ",
    "Bootstrap procedure: for each fund, estimated monthly alpha is subtracted ",
    "from the excess return series to construct a zero-alpha null; calendar ",
    "months are then resampled with replacement, preserving cross-sectional ",
    "factor return dependence; the factor model is re-estimated on each ",
    "resampled series. Newey-West HAC standard errors (6-month lag) are ",
    "applied symmetrically to actual and simulated $t$-statistics, following ",
    "the symmetry requirement of \\textcite{FamaFrench2010}. ",
    "Values of Prob.\\ Luck below 5\\% at lower percentiles indicate ",
    "underperformance inconsistent with the zero-alpha null. ",
    "Entries of 0.0\\% indicate zero out of $B = 10{,}000$ iterations ",
    "produced a simulated $t$-statistic as extreme as the actual value; ",
    "$<$0.1\\% indicates between 1 and 5 iterations. ",
    "\\textit{Carhart}: MKT-RF, SMB, HML, MOM. ",
    "\\textit{FF6}: MKT-RF, SMB, HML, RMW, CMA, MOM. ",
    "\\textit{C+PSL}: Carhart plus \\textcite{PastorStambaugh2003} traded liquidity. ",
    "Sample: actively managed funds (", n_active_str, "), ",
    "Incubation-corrected panel (\\citealt{Evans2010}), no date cap; ",
    "performance-comparison subsample per flagged\\_funds.xlsx; ",
    "minimum 24 monthly observations. ",
    "Full sample window for Carhart and FF6; Dec 1994--Dec 2024 for C+PSL."
  ),
  "\\end{singlespace}",
  "\\end{minipage}",
  "\\end{table}"
)
write_tex(e2_tex, "table_bootstrap_comparison_robust.tex")

# =============================================================================
# 5. TABLE I.3:  BSW DECOMPOSITION COMPARISON AT GAMMA* = 0.20
# =============================================================================
cat("\n=== Table I.3: BSW Decomposition Comparison ===\n")

get_bsw_row <- function(spec, gamma_val = GAMMA_STAR) {
  row <- spec$bsw_df %>% filter(gamma == gamma_val)
  if (nrow(row) == 0L) stop(sprintf("gamma=%s not found in %s", gamma_val, spec$label))
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
pi0_car_str <- paste0(fmt1(br_car$pi0), "\\%")

make_e3_row <- function(label, br) {
  paste0(label, " & ", fmt1(br$pi0), "\\% & ", fmt1(br$S_neg), "\\% & ",
         fmt1(br$S_pos), "\\% & ", fmt1(br$F_luck), "\\% & ",
         fmt1(br$T_unskill), "\\% & ", fmt1(br$T_skill), "\\% \\\\")
}

e3_rows <- c(make_e3_row(car$label, br_car),
             make_e3_row(ff6$label, br_ff6),
             make_e3_row(c5$label,  br_c5))

e3_tex <- c(
  "\\begin{table}[!htbp]",
  "\\centering",
  "\\sbox{\\tabletempbox}{%",
  "\\footnotesize",
  "\\begin{tabular}{lrrrrrr}",
  "\\toprule",
  "Model & $\\hat{\\pi}_0$ & $S^-_{\\gamma^*}$ & $S^+_{\\gamma^*}$ & $F_{\\gamma^*}$ & $T^-_{\\gamma^*}$ & $T^+_{\\gamma^*}$ \\\\",
  "\\midrule",
  e3_rows,
  "\\bottomrule",
  "\\end{tabular}%",
  "}",
  "\\setlength{\\tabletempwidth}{\\wd\\tabletempbox}",
  "\\ifdim\\tabletempwidth>\\linewidth\\setlength{\\tabletempwidth}{\\linewidth}\\fi",
  "\\begin{minipage}{\\tabletempwidth}",
  "\\captionsetup{width=\\linewidth}",
  "\\caption{\\label{tab:bsw_comparison_robust}BSW (2010) Decomposition Comparison at $\\gamma^* = 0.20$}",
  "\\ifdim\\wd\\tabletempbox>\\linewidth",
  "  \\resizebox{\\linewidth}{!}{\\usebox{\\tabletempbox}}",
  "\\else",
  "  \\usebox{\\tabletempbox}",
  "\\fi",
  "\\par\\medskip",
  "\\begin{singlespace}\\footnotesize\\noindent",
  paste0(
    "\\textcite{BarrasScailletWermers2010} four-way decomposition of active funds ",
    "at significance threshold $\\gamma^* = 0.20$. ",
    "$\\hat{\\pi}_0$: \\textcite{Storey2002} estimator of the zero-alpha fund ",
    "proportion at tuning parameter $\\lambda = 0.5$, derived from Newey-West ",
    "$t(\\hat{\\alpha})$ p-values on full-period per-fund alphas. ",
    "$S^-_{\\gamma^*}$ ($S^+_{\\gamma^*}$): observed fraction of active funds with ",
    "significantly negative (positive) $t(\\hat{\\alpha})$ at two-sided level ",
    "$\\gamma^*$; critical values from $N(0,1)$. ",
    "$F_{\\gamma^*} = \\hat{\\pi}_0 \\cdot \\gamma^*/2$: expected false-discovery ",
    "proportion per tail. ",
    "$T^-_{\\gamma^*} = S^-_{\\gamma^*} - F_{\\gamma^*}$: genuinely unskilled funds. ",
    "$T^+_{\\gamma^*} = S^+_{\\gamma^*} - F_{\\gamma^*}$: genuinely skilled funds; ",
    "negative entries indicate right-tail significance does not exceed the ",
    "false-discovery rate. ",
    "All quantities are percentages of the active-fund universe. ",
    "\\textit{Carhart 4-Factor}: \\citealt{Carhart1997} main-text baseline ",
    "($\\hat{\\pi}_0 = ", pi0_car_str, "$). ",
    "\\textit{FF 6-Factor}: \\citealt{FamaFrench2015} five-factor plus momentum. ",
    "\\textit{Carhart + PSL}: Carhart plus \\textcite{PastorStambaugh2003} traded liquidity. ",
    "Sample: Incubation-corrected panel (\\citealt{Evans2010}), no date cap; ",
    "performance-comparison subsample per flagged\\_funds.xlsx; Passive and Unknown funds excluded."
  ),
  "\\end{singlespace}",
  "\\end{minipage}",
  "\\end{table}"
)
write_tex(e3_tex, "table_bsw_comparison_robust.tex")

# =============================================================================
# 6. SUMMARY
# =============================================================================
cat("\n=== ALL APPENDIX I TABLES BUILT ===\n")
cat("Files written to:", normalizePath(OUT_DIR), "\n")
cat("  table_alpha_comparison_robust.tex       (Table I.1)\n")
cat("  table_bootstrap_comparison_robust.tex   (Table I.2)\n")
cat("  table_bsw_comparison_robust.tex         (Table I.3)\n")
