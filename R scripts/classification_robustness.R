# =============================================================================
# CLASSIFICATION ROBUSTNESS: BLOOMBERG FLAG ONLY VS RECLASSIFIED
# classification_robustness.R  (v1.0, Oct 2026; pre-committed 16 Oct 2026)
#
# Headline results under the two fund classifications:
#   "Bloomberg flag only": unflagged funds (mostly pre-2012 closures) left out;
#     the pre-2012 active sample is survivor-only. Results of the 15 Oct 2026
#     run, saved in BASELINE_DIR before the reclassified rerun.
#   "Reclassified": unflagged funds classified by classify_ap() v1.8 (current
#     run, WORKING_DIR).
# The two are the bounds of the classification choice: the reclassified run
# includes every unflagged fund not identified as an index fund.
#
# Requires: the xlsx outputs of both runs. Output:
#   table_classification_robustness.tex (appendix), classification_robustness.xlsx
# =============================================================================

suppressPackageStartupMessages({
  library(dplyr)
  library(readxl)
  library(writexl)
})

if (!exists("WORKING_DIR")) WORKING_DIR <- getwd()
setwd(WORKING_DIR)
BASELINE_DIR <- file.path(WORKING_DIR, "results_bloomberg_only")
if (!dir.exists(BASELINE_DIR))
  stop("Baseline outputs not found in ", BASELINE_DIR, call. = FALSE)

# --- 1. Extract headline numbers from one result folder ----------------------
headline <- function(dir) {
  rd <- function(f, s) read_excel(file.path(dir, f), sheet = s)
  agg <- rd("aggregate_alphas.xlsx", "t7_agg") %>% filter(Group == "Active")
  bsw <- rd("bootstrap_results.xlsx", "bsw_meta")
  sp  <- lapply(c("P1", "P2", "P3"), function(r)
    rd("subperiod_results.xlsx", paste0(r, "_alpha_agg")) %>%
      filter(ap_group == "Active", weighting == "EW"))
  brk <- rd("breaktest_results.xlsx", "Break Dates")
  chg <- rd("same_sample_results.xlsx", "Change") %>% filter(sample == "Full", ret == "ew_net")
  hyp <- rd("panel_regime_results.xlsx", "TNA_floor") %>% filter(floor == 5)
  v <- c(
    funds        = agg$n_funds,
    ew_gross     = 100 * agg$alpha_ew_gross, ew_gross_t = agg$t_ew_gross,
    ew_net       = 100 * agg$alpha_ew_net,   ew_net_t   = agg$t_ew_net,
    vw_gross     = 100 * agg$alpha_vw_gross, vw_gross_t = agg$t_vw_gross,
    vw_net       = 100 * agg$alpha_vw_net,   vw_net_t   = agg$t_vw_net,
    pi0 = bsw$pi0_pct, pi_minus = bsw$pi_A_minus, pi_plus = bsw$pi_A_plus,
    p1_net = sp[[1]]$alpha_car_net, p1_net_t = sp[[1]]$t_car_net,
    p2_net = sp[[2]]$alpha_car_net, p2_net_t = sp[[2]]$t_car_net,
    p3_net = sp[[3]]$alpha_car_net, p3_net_t = sp[[3]]$t_car_net,
    d32 = chg$delta, d32_t = chg$t)
  v <- c(v, setNames(hyp$p, paste0("p_", hyp$id)))
  list(v = v, breaks = paste(format(as.Date(brk[["Point Estimate"]]), "%b %Y"), collapse = ", "))
}
base <- headline(BASELINE_DIR)
new  <- headline(WORKING_DIR)
cmp <- data.frame(item = names(new$v), bloomberg_only = unname(base$v[names(new$v)]),
                  reclassified = unname(new$v))
print(cmp, digits = 3)
cat("Breaks, Bloomberg only:", base$breaks, "\nBreaks, reclassified:", new$breaks, "\n")
write_xlsx(list(Headline = cmp, Breaks = data.frame(classification = c("Bloomberg flag only", "Reclassified"),
                                                   breaks = c(base$breaks, new$breaks))),
           "classification_robustness.xlsx")

# --- 2. LaTeX table ---------------------------------------------------------------
f2 <- function(x) ifelse(is.na(x), "--", ifelse(x < 0, paste0("$-$", formatC(abs(x), format = "f", digits = 2)),
                                                 formatC(x, format = "f", digits = 2)))
fp <- function(x) ifelse(is.na(x), "--", ifelse(x < 0.001, "$<$0.001", formatC(x, format = "f", digits = 3)))
star <- function(t) ifelse(abs(t) >= 2.576, "$^{***}$", ifelse(abs(t) >= 1.960, "$^{**}$",
                    ifelse(abs(t) >= 1.645, "$^{*}$", "")))
g <- function(v, k) v[[k]]
ac <- function(v, k) paste0(f2(g(v, k)), star(g(v, paste0(k, "_t"))))
row <- function(lab, fb, fn) paste0(lab, " & ", fb, " & ", fn, " \\\\")
HYP <- c(H1_DSENT = "H1, $D^{\\text{SENT}}$", H1_SENT = "H1, $\\text{SENT}^\\perp$",
         H1_AAII = "H1, $D^{\\text{AAII}}$", H2_MD = "H2, $D^{\\text{MD,Det}}$",
         H3_MAX12 = "H3, MAX12", H4_DSENT = "H4, $D^{\\text{SENT}}$",
         H4_SENT = "H4, $\\text{SENT}^\\perp$", H4_AAII = "H4, $D^{\\text{AAII}}$")
b <- base$v; n <- new$v
body <- c(" & Bloomberg flag only & Reclassified \\\\", "\\midrule",
  "\\multicolumn{3}{l}{\\textit{A. Aggregate Carhart alpha of active funds (\\% p.a.)}} \\\\",
  row("Active funds", formatC(g(b, "funds"), big.mark = ","), formatC(g(n, "funds"), big.mark = ",")),
  row("EW gross", ac(b, "ew_gross"), ac(n, "ew_gross")), row("EW net", ac(b, "ew_net"), ac(n, "ew_net")),
  row("VW gross", ac(b, "vw_gross"), ac(n, "vw_gross")), row("VW net", ac(b, "vw_net"), ac(n, "vw_net")),
  "\\midrule", "\\multicolumn{3}{l}{\\textit{B. False discovery decomposition (\\%)}} \\\\",
  row("$\\hat\\pi_0$", f2(g(b, "pi0")), f2(g(n, "pi0"))),
  row("$\\hat\\pi_A^-$", f2(g(b, "pi_minus")), f2(g(n, "pi_minus"))),
  row("$\\hat\\pi_A^+$", f2(g(b, "pi_plus")), f2(g(n, "pi_plus"))),
  "\\midrule", "\\multicolumn{3}{l}{\\textit{C. Sub-periods: EW net alpha (\\% p.a.)}} \\\\",
  row("P1", ac(b, "p1_net"), ac(n, "p1_net")), row("P2", ac(b, "p2_net"), ac(n, "p2_net")),
  row("P3", ac(b, "p3_net"), ac(n, "p3_net")), row("P3 $-$ P2", ac(b, "d32"), ac(n, "d32")),
  row("Bai--Perron breaks (BIC)", paste0("\\parbox[t]{5.5cm}{\\raggedright ", base$breaks, "}"),
      paste0("\\parbox[t]{5.5cm}{\\raggedright ", new$breaks, "}")),
  "\\midrule", "\\multicolumn{3}{l}{\\textit{D. Behavioral flow tests: joint test $p$-value}} \\\\",
  vapply(names(HYP), function(k) row(HYP[[k]], fp(g(b, paste0("p_", k))), fp(g(n, paste0("p_", k)))), ""))
note <- paste0(
  "\\textit{Bloomberg flag only}: funds without a Bloomberg active/passive flag are left out; because the ",
  "flag is missing for almost every fund that closed before 2012, the pre-2012 active sample then consists ",
  "of survivors only. \\textit{Reclassified}: those funds are classified by name, Lipper category and ",
  "benchmark tracking, with manual review of boundary cases (Appendix~\\ref{app:data_cleaning}); this is ",
  "the classification used in the main text. Panel A as Table~\\ref{tab:perf_aggregate}; Panel B as ",
  "Table~\\ref{tab:bsw_decomposition} ($\\gamma^*=0.45$); Panel C as Table~\\ref{tab:subperiod_port_agg_alpha}, ",
  "with P3 $-$ P2 from the stacked regression of Table~\\ref{tab:same_sample_alpha}; Panel D as ",
  "Table~\\ref{tab:tna_floor} (USD 5 million floor). ",
  if (identical(base$breaks, new$breaks)) "The Bai--Perron break dates, and hence the sub-period boundaries, are the same under both classifications. "
  else "Each column uses the sub-period boundaries of its own break test. ",
  "$^{*}p<0.10$, $^{**}p<0.05$, $^{***}p<0.01$.")
writeLines(c("\\begin{table}[H]", "\\centering", "\\footnotesize",
             "\\caption{Headline Results Under the Two Fund Classifications}",
             "\\label{tab:classification_robustness}", "\\begin{tabular}{lcc}", "\\toprule", body,
             "\\bottomrule", "\\end{tabular}", "\\end{table}",
             paste0("\\begin{singlespace}\\footnotesize\\noindent\n", note, "\n\\end{singlespace}\n")),
           "table_classification_robustness.tex")
cat("Written: table_classification_robustness.tex, classification_robustness.xlsx\n")
