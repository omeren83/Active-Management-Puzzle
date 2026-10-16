# =============================================================================
# SAME-SAMPLE SUB-PERIOD COMPARISON
# same_sample_subperiods.R  (v1.0, Oct 2026; defense extension, pre-committed 15 Oct 2026)
#
# Question: is the P2 -> P3 deterioration a regime change (the same funds did
# worse) or composition (different funds)? Özsöylev A4.
#
# Design (pre-commitment note, item 2):
#   - S23  = active funds with >= 24 monthly returns in P2 and in P3.
#     S123 = S23 funds that also have >= 24 in P1.
#   - On each fixed sample, regime by regime: aggregate-portfolio EW/VW Carhart
#     alpha (gross, net), FF (2010) bootstrap and BSW decomposition, using the
#     estimate_subperiod() kernel of subperiod_analysis.R unchanged (same B,
#     seed, NW lags, gamma*).
#   - Change test: one stacked regression of the portfolio excess return on
#     regime dummies and regime-specific factor loadings; Wald test of
#     alpha_P3 - alpha_P2 with Newey-West (12 lags).
#   - Decomposition: full-sample change = within (fixed sample) + composition
#     (residual). Within share > 2/3 -> "predominantly regime change";
#     < 1/3 -> "predominantly composition"; otherwise "mixed".
#     Headline quantity: EW net alpha (the sub-period claim in the text).
#   - Caveats (stated in the text): survivors across regimes; lower power.
#
# Requires: panel_incubation in session (Phase A); subperiod_analysis.R in
#   SCRIPTS_DIR (helpers parsed, not re-run); subperiod_results.xlsx and
#   breaktest_results.xlsx (Phase E).
# Outputs: same_sample_results.xlsx, table_same_sample_alpha.tex (Chapter 4),
#   table_same_sample_skill.tex (Appendix H).
# =============================================================================

suppressPackageStartupMessages({
  library(dplyr)
  library(readxl)
  library(writexl)
})

if (!exists("WORKING_DIR")) WORKING_DIR <- getwd()
if (!exists("SCRIPTS_DIR")) SCRIPTS_DIR <- WORKING_DIR
setwd(WORKING_DIR)
if (!exists("panel_incubation"))
  stop("panel_incubation not in session. Run Phase A first.")

MIN_MONTHS_REG <- 24L   # per regime, = MIN_OBS_FULL of the per-fund regressions
NW_LAG_DIFF    <- 12L
GAMMA_ROW      <- 45    # gamma* = 0.45 row of the BSW table

# --- 1. Load subperiod_analysis.R helpers without running its estimation ----
# Evaluates its top-level code up to (not including) the line that runs the
# three sub-periods, so SUBPERIODS, constants and estimate_subperiod() are
# exactly those of the main sub-period analysis.
spe   <- new.env(parent = globalenv())
exprs <- parse(file.path(SCRIPTS_DIR, "subperiod_analysis.R"), keep.source = FALSE)
stop_at <- which(vapply(exprs, function(e) grepl("^results <- lapply\\(SUBPERIODS",
                                                 paste(deparse(e), collapse = "")), logical(1)))
stopifnot(length(stop_at) == 1L)
for (e in exprs[seq_len(stop_at - 1L)]) eval(e, envir = spe)
SP <- spe$SUBPERIODS
if (exists("TEST_B_RUNS")) spe$B_RUNS <- TEST_B_RUNS   # container tests only
cat("Regimes:", vapply(SP, `[[`, "", "window"), "\n")

# --- 2. Fixed samples -----------------------------------------------------------
base <- panel_incubation %>%
  filter(!excluded_perf, ap_group == "Active", !is.na(ret_gross), !is.na(MKT_RF),
         !is.na(SMB), !is.na(HML), !is.na(MOM), !is.na(RF))
regime_of <- function(d) ifelse(d <= SP$P1$date_hi, "P1", ifelse(d <= SP$P2$date_hi, "P2", "P3"))
cnt <- base %>% mutate(reg = regime_of(date)) %>% count(Ticker, reg) %>%
  tidyr::pivot_wider(names_from = reg, values_from = n, values_fill = 0L)
S23  <- cnt$Ticker[cnt$P2 >= MIN_MONTHS_REG & cnt$P3 >= MIN_MONTHS_REG]
S123 <- cnt$Ticker[cnt$P1 >= MIN_MONTHS_REG & cnt$P2 >= MIN_MONTHS_REG & cnt$P3 >= MIN_MONTHS_REG]
cat(sprintf("Active funds with returns: %d | S23: %d | S123: %d\n",
            nrow(cnt), length(S23), length(S123)))
SAMPLES <- list(S23 = list(tk = S23, regs = c("P2", "P3")),
                S123 = list(tk = S123, regs = c("P1", "P2", "P3")))

# --- 3. Regime estimates on the fixed samples (main-analysis kernel) ----------
fixed <- list()
for (s in names(SAMPLES)) {
  src <- panel_incubation %>% filter(Ticker %in% SAMPLES[[s]]$tk)
  for (r in SAMPLES[[s]]$regs) {
    sp <- SP[[r]]; sp$label <- paste0(s, "_", r)   # distinct bootstrap cache file
    fixed[[sp$label]] <- spe$estimate_subperiod(sp, src)
  }
}
rm(src)

# Full-sample regime results from the main sub-period run
full_xl <- "subperiod_results.xlsx"
full <- lapply(c("P1", "P2", "P3"), function(r) list(
  alpha_agg = read_excel(full_xl, sheet = paste0(r, "_alpha_agg")),
  boot      = read_excel(full_xl, sheet = paste0(r, "_boot_summary")),
  bsw       = read_excel(full_xl, sheet = paste0(r, "_bsw")),
  pi0       = read_excel(full_xl, sheet = "pi0_summary") %>% filter(period == r)))
names(full) <- c("P1", "P2", "P3")

# --- 4. Stacked change test: alpha_P3 - alpha_P2 ------------------------------
nw_vcov_mat <- function(X, e, lag) {
  Tn <- nrow(X); Xi <- solve(crossprod(X)); sc <- X * e
  S <- crossprod(sc) / Tn
  for (j in seq_len(lag)) {
    w <- 1 - j / (lag + 1)
    G <- crossprod(sc[(j + 1):Tn, , drop = FALSE], sc[1:(Tn - j), , drop = FALSE]) / Tn
    S <- S + w * (G + t(G))
  }
  Tn * Xi %*% S %*% Xi
}
# Portfolio returns built as in estimate_subperiod() (EW mean; VW on tna_lag)
port_of <- function(tk = NULL) {
  d <- base %>% filter(date >= SP$P2$date_lo, date <= SP$P3$date_hi)
  if (!is.null(tk)) d <- d %>% filter(Ticker %in% tk)
  d %>% group_by(date) %>%
    summarise(ew_gross = mean(ret_gross), ew_net = mean(ret_net, na.rm = TRUE),
              vw_gross = spe$wm_alpha(ret_gross, tna_lag),
              vw_net   = spe$wm_alpha(ret_net, tna_lag),
              MKT_RF = first(MKT_RF), SMB = first(SMB), HML = first(HML),
              MOM = first(MOM), RF = first(RF), .groups = "drop")
}
diff_test <- function(p, col) {
  d  <- p %>% filter(!is.na(.data[[col]]))
  D3 <- as.numeric(d$date > SP$P2$date_hi); D2 <- 1 - D3
  Fm <- as.matrix(d[, c("MKT_RF", "SMB", "HML", "MOM")])
  X  <- cbind(D2, D3, Fm * D2, Fm * D3)
  y  <- d[[col]] - d$RF
  b  <- solve(crossprod(X), crossprod(X, y)); e <- as.vector(y - X %*% b)
  V  <- nw_vcov_mat(X, e, NW_LAG_DIFF)
  g  <- c(-1, 1, rep(0, ncol(X) - 2))
  dlt <- sum(g * b); se <- sqrt(as.numeric(t(g) %*% V %*% g))
  data.frame(ret = col, alpha_P2 = b[1] * 1200, alpha_P3 = b[2] * 1200,
             delta = dlt * 1200, t = dlt / se, p = 2 * pnorm(-abs(dlt / se)))
}
RET_COLS <- c("ew_gross", "ew_net", "vw_gross", "vw_net")
ports <- list(Full = port_of(), S23 = port_of(S23), S123 = port_of(S123))
chg <- bind_rows(lapply(names(ports), function(s)
  bind_rows(lapply(RET_COLS, diff_test, p = ports[[s]])) %>% mutate(sample = s)))
print(chg, digits = 3)

# --- 5. Decomposition: full change = within + composition ---------------------
dec <- bind_rows(lapply(c("S23", "S123"), function(s) {
  f <- chg %>% filter(sample == "Full"); w <- chg %>% filter(sample == s)
  data.frame(sample = s, ret = f$ret, delta_full = f$delta, delta_within = w$delta,
             composition = f$delta - w$delta, within_share = w$delta / f$delta)
})) %>%
  mutate(verdict = case_when(within_share > 2/3 ~ "predominantly regime change",
                             within_share < 1/3 ~ "predominantly composition",
                             TRUE               ~ "mixed"))
print(dec, digits = 3)
cat("Headline (EW net, S23):",
    dec$verdict[dec$sample == "S23" & dec$ret == "ew_net"], "\n")

# --- 6. Collect regime-level rows ----------------------------------------------
agg_row <- function(a) {
  a <- a %>% filter(ap_group == "Active")
  ew <- a %>% filter(weighting == "EW"); vw <- a %>% filter(weighting == "VW")
  data.frame(ew_gross = ew$alpha_car, ew_gross_t = ew$t_car,
             ew_net = ew$alpha_car_net, ew_net_t = ew$t_car_net,
             vw_gross = vw$alpha_car, vw_gross_t = vw$t_car,
             vw_net = vw$alpha_car_net, vw_net_t = vw$t_car_net, months = ew$n_months)
}
skill_row <- function(boot, bsw, pi0, n) {
  b <- bsw %>% filter(gamma == GAMMA_ROW)
  data.frame(n_funds = n, pi0 = 100 * pi0, pi_minus = b$T_unskilled_pct,
             pi_plus = b$T_skilled_pct,
             luck_p5  = boot$pct_runs_below[boot$percentile == 5],
             luck_p95 = boot$pct_runs_below[boot$percentile == 95])
}
rows <- list()
for (r in c("P1", "P2", "P3")) {
  rows[[length(rows) + 1L]] <- cbind(sample = "Full", regime = r, agg_row(full[[r]]$alpha_agg),
    skill_row(full[[r]]$boot, full[[r]]$bsw, full[[r]]$pi0$pi_0, full[[r]]$pi0$n))
}
for (k in names(fixed)) {
  x <- fixed[[k]]; parts <- strsplit(k, "_")[[1]]
  rows[[length(rows) + 1L]] <- cbind(sample = parts[1], regime = parts[2], agg_row(x$alpha_agg),
    skill_row(x$boot_summary, x$bsw_df, x$pi_0, x$pi_0_n))
}
regime_tab <- bind_rows(rows)
print(regime_tab, digits = 3)

write_xlsx(list(Counts = data.frame(sample = c("Active with returns", "S23", "S123"),
                                    n = c(nrow(cnt), length(S23), length(S123))),
                Regimes = regime_tab, Change = chg, Decomposition = dec,
                Fund_months = cnt),
           "same_sample_results.xlsx")

# --- 7. LaTeX tables -----------------------------------------------------------
f2 <- function(x) ifelse(is.na(x), "--", ifelse(x < 0, paste0("$-$", formatC(abs(x), format = "f", digits = 2)),
                                                 formatC(x, format = "f", digits = 2)))
f1 <- function(x) formatC(x, format = "f", digits = 1)
star_t <- function(t) ifelse(is.na(t), "", ifelse(abs(t) >= 2.576, "$^{***}$",
                      ifelse(abs(t) >= 1.960, "$^{**}$", ifelse(abs(t) >= 1.645, "$^{*}$", ""))))
cell <- function(a, t) paste0(f2(a), star_t(t))
note_block <- function(txt) paste0("\\begin{singlespace}\\footnotesize\\noindent\n", txt,
                                   "\n\\end{singlespace}\n")
table_wrap <- function(caption, label, colspec, body, note) {
  c("\\begin{table}[H]", "\\centering", "\\footnotesize",
    paste0("\\caption{", caption, "}"), paste0("\\label{tab:", label, "}"),
    "\\resizebox{\\linewidth}{!}{%", paste0("\\begin{tabular}{", colspec, "}"),
    "\\toprule", body, "\\bottomrule", "\\end{tabular}", "}", "\\end{table}",
    note_block(note))
}
SLBL <- c(Full = "All active funds", S23 = "Same funds, P2 and P3",
          S123 = "Same funds, P1--P3")
RLBL <- c(ew_gross = "EW gross", ew_net = "EW net", vw_gross = "VW gross", vw_net = "VW net")
N_LBL <- c(Full = "", S23 = sprintf(" ($N=%s$)", formatC(length(S23), big.mark = ",")),
           S123 = sprintf(" ($N=%s$)", formatC(length(S123), big.mark = ",")))
get_alpha <- function(s, r, col) {
  x <- regime_tab %>% filter(sample == s, regime == r)
  if (!nrow(x)) return("--")
  cell(x[[col]], x[[paste0(col, "_t")]])
}
body_a <- c("Sample & Return & P1 & P2 & P3 & P3 $-$ P2 & $t$ \\\\", "\\midrule")
for (s in c("Full", "S23", "S123")) {
  for (rc in RET_COLS) {
    ch <- chg %>% filter(sample == s, ret == rc)
    body_a <- c(body_a, paste0(
      if (rc == "ew_gross") paste0(SLBL[[s]], N_LBL[[s]]) else "", " & ", RLBL[[rc]], " & ",
      get_alpha(s, "P1", rc), " & ", get_alpha(s, "P2", rc), " & ", get_alpha(s, "P3", rc),
      " & ", cell(ch$delta, ch$t), " & ", f2(ch$t), " \\\\"))
  }
  if (s != "S123") body_a <- c(body_a, "\\midrule")
}
body_a <- c(body_a, "\\midrule",
  "\\multicolumn{7}{l}{\\textit{Decomposition of the full-sample change, P2 $\\rightarrow$ P3}} \\\\",
  "Fixed sample & Return & Full change & Within & Composition & Within share & \\\\",
  "\\midrule")
for (s in c("S23", "S123")) for (rc in c("ew_net", "ew_gross")) {
  x <- dec %>% filter(sample == s, ret == rc)
  body_a <- c(body_a, paste0(if (rc == "ew_net") SLBL[[s]] else "", " & ", RLBL[[rc]], " & ",
                             f2(x$delta_full), " & ", f2(x$delta_within), " & ",
                             f2(x$composition), " & ", f2(x$within_share), " & \\\\"))
}
COMMON <- paste0(
  "Regimes: P1 ", SP$P1$window, ", P2 ", SP$P2$window, ", P3 ", SP$P3$window,
  " (Table~\\ref{tab:breaktest}). Same funds: active funds with at least 24 monthly ",
  "returns in each listed regime; these funds survived across the regime boundary, ",
  "so their alpha levels are biased upward relative to the full cross-section, and ",
  "the change, not the level, is the object of interest. Sample: active funds, ",
  "\\textcite{Evans2010}-corrected panel, performance-comparison subsample.")
note_a <- paste0(
  "Aggregate-portfolio \\textcite{Carhart1997} alpha (\\% p.a.) of the equal-weighted (EW) and ",
  "value-weighted (VW, lagged TNA) portfolio of the listed funds, estimated separately in each ",
  "regime as in Table~\\ref{tab:subperiod_port_agg_alpha}; stars from Newey--West $t$-statistics ",
  "(6 lags). P3 $-$ P2: difference of the two regime alphas from one stacked regression with ",
  "regime-specific intercepts and factor loadings, Newey--West standard errors with 12 lags. ",
  "Decomposition: full change = P3 $-$ P2 change of all active funds; within = the same change ",
  "for the fixed sample; composition = full $-$ within; within share = within / full. ",
  "$^{*}p<0.10$, $^{**}p<0.05$, $^{***}p<0.01$. ", COMMON)
writeLines(table_wrap("Regime Change or Composition? Aggregate Alpha of the Same Funds Across Regimes",
                      "same_sample_alpha", "llccccc", body_a, note_a),
           "table_same_sample_alpha.tex")

fmt_prob <- spe$fmt_prob
body_b <- c("Sample & Regime & Funds & $\\hat\\pi_0$ & $\\hat\\pi_A^-$ & $\\hat\\pi_A^+$ & ",
            "Prob.\\ luck, 5th pct. & Prob.\\ luck, 95th pct. \\\\")
body_b <- c(paste0(body_b[1], body_b[2]), "\\midrule")
for (s in c("Full", "S23", "S123")) {
  x <- regime_tab %>% filter(sample == s)
  for (i in seq_len(nrow(x))) body_b <- c(body_b, paste0(
    if (i == 1L) SLBL[[s]] else "", " & ", x$regime[i], " & ",
    formatC(x$n_funds[i], big.mark = ","), " & ", f1(x$pi0[i]), " & ", f1(x$pi_minus[i]),
    " & ", f1(x$pi_plus[i]), " & ", fmt_prob(x$luck_p5[i]), " & ", fmt_prob(x$luck_p95[i]), " \\\\"))
  if (s != "S123") body_b <- c(body_b, "\\midrule")
}
note_b <- paste0(
  "Per-fund Carhart alphas on gross returns within each regime (at least 24 observations). ",
  "$\\hat\\pi_0$: \\textcite{Storey2002} zero-alpha proportion ($\\lambda=0.5$); ",
  "$\\hat\\pi_A^-$, $\\hat\\pi_A^+$: \\textcite{BarrasScailletWermers2010} unskilled and skilled ",
  "proportions at $\\gamma^*=0.45$. Prob.\\ luck: share of $B=10{,}000$ ",
  "\\textcite{FamaFrench2010} bootstrap runs in which the simulated 5th (95th) percentile of ",
  "$t(\\hat\\alpha)$ lies below the actual one; below 5\\% (above 95\\%) means worse (better) ",
  "than luck. Settings as in Tables~\\ref{tab:subperiod_bootstrap_tails} and ",
  "\\ref{tab:subperiod_bsw_decomposition}. ", COMMON)
writeLines(table_wrap("Luck Versus Skill of the Same Funds Across Regimes",
                      "same_sample_skill", "llcccccc", body_b, note_b),
           "table_same_sample_skill.tex")
cat("Written: same_sample_results.xlsx, table_same_sample_alpha.tex, table_same_sample_skill.tex\n")
