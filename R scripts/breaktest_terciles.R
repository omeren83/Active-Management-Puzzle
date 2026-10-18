# =============================================================================
# STRUCTURAL BREAK TEST BY PAST-PERFORMANCE TERCILE
# breaktest_terciles.R  (v1.0, Oct 2026; defense extension, pre-committed 15 Oct 2026)
#
# Question: did the Aug 2010 deterioration hit all active funds or only the
# left tail of past performance?
#
# Design (pre-commitment note, item 1):
#   - Each month m, active funds are sorted into terciles on the t-statistic
#     of their 36-month rolling Carhart alpha ending m-1 (alpha_rolling.xlsx:
#     36 calendar months, >= 24 obs; same signal as persistence_testing.R,
#     rank_var = "t_stat"). EW tercile portfolios hold month m (gross returns).
#     Funds that die simply drop out; no look-ahead.
#   - Sorting on the alpha and then averaging the same funds' rolling alphas
#     would overlap sort and outcome windows (35 of 36 months) and is avoided.
#   - Outcome: 36-calendar-month rolling Carhart alpha (>= 24 obs) of each
#     tercile portfolio (annualised %), the T1-T3 spread, and the all-fund
#     portfolio built the same way (bridge to the main break series).
#   - Tests per series, identical to structural_break_test.R: Bai-Perron
#     (BIC + LWZ, up to 8 breaks, min segment 36, HAC lag 35, het.err = FALSE
#     CIs) and sup-F / ave-F. Added: fixed-date mean-shift test at the P2/P3
#     boundary (Aug 2010, taken from breaktest_results.xlsx), HAC lag 35.
#   - Decision rule: "broad" if T1, T2, T3 all have a lower post-break mean
#     with a fixed-date shift significant at 5%; "left tail only" if only T1;
#     otherwise "mixed".
#
# Requires: panel_incubation in session (Phase A); alpha_rolling.xlsx and
#   breaktest_results.xlsx (Phases B, E).
# Outputs: breaktest_terciles_results.xlsx, table_breaktest_terciles.tex
#   (Chapter 4), table_breaktest_terciles_dates.tex (Appendix H),
#   fig_breaktest_terciles.png (Appendix H).
# =============================================================================

suppressPackageStartupMessages({
  library(dplyr)
  library(readxl)
  library(writexl)
  library(strucchange)
  library(sandwich)
  library(ggplot2)
})

if (!exists("WORKING_DIR")) WORKING_DIR <- getwd()
setwd(WORKING_DIR)

# --- 0. Configuration (same values as structural_break_test.R) --------------
ROLL_FILE      <- "alpha_rolling.xlsx"
BREAKTEST_FILE <- "breaktest_results.xlsx"
REGIME_BREAK   <- "2010-08"   # last month of P2; must be a detected break
WIN_MONTHS     <- 36L
MIN_OBS_ROLL   <- 24L
MAX_BREAKS     <- 8L
MIN_SEG_MONTHS <- 36L
NW_LAG         <- MIN_SEG_MONTHS - 1L   # MA(35) overlap of 36-month windows
N_GROUPS       <- 3L

if (!exists("panel_incubation"))
  stop("panel_incubation not in session. Run Phase A first.")

.bd <- format(as.Date(read_excel(BREAKTEST_FILE, sheet = "Break Dates")[["Point Estimate"]]), "%Y-%m")
if (!REGIME_BREAK %in% .bd)
  stop("REGIME_BREAK ", REGIME_BREAK, " is not a break date in ", BREAKTEST_FILE,
       " (", paste(.bd, collapse = ", "), ").", call. = FALSE)

mi_of <- function(d) as.integer(format(d, "%Y")) * 12L + as.integer(format(d, "%m"))
mi_break <- mi_of(as.Date(paste0(REGIME_BREAK, "-01")))

# --- 1. Sorting signal: rolling alpha t-stat at m-1 -------------------------
roll <- read_excel(ROLL_FILE) %>%
  filter(ap_group == "Active", !is.na(alpha_t_nw)) %>%
  transmute(Ticker, mi_sig = mi_of(as.Date(date)), t_sig = alpha_t_nw)

# Holding-month returns: same sample as alpha_rolling (!excluded_perf, Active)
rets <- panel_incubation %>%
  filter(!excluded_perf, ap_group == "Active", !is.na(ret_gross)) %>%
  transmute(Ticker, mi = mi_of(date), ret_gross)

# Terciles formed on every fund with a signal at m-1 (cut-offs do not depend
# on survival into m); portfolio return = mean over those with a return in m
sorted <- roll %>%
  group_by(mi_sig) %>%
  mutate(tercile = ntile(t_sig, N_GROUPS)) %>%
  ungroup() %>%
  mutate(mi = mi_sig + 1L)

port <- sorted %>%
  inner_join(rets, by = c("Ticker", "mi")) %>%
  group_by(mi) %>%
  summarise(T1  = mean(ret_gross[tercile == 1L]),
            T2  = mean(ret_gross[tercile == 2L]),
            T3  = mean(ret_gross[tercile == N_GROUPS]),
            All = mean(ret_gross),
            n_T1 = sum(tercile == 1L), n_T3 = sum(tercile == N_GROUPS),
            n_all = n(), .groups = "drop")

fac <- panel_incubation %>%
  transmute(mi = mi_of(date), MKT_RF, SMB, HML, MOM, RF) %>%
  filter(!is.na(MKT_RF), !is.na(SMB), !is.na(HML), !is.na(MOM), !is.na(RF)) %>%
  distinct(mi, .keep_all = TRUE)

port <- port %>% inner_join(fac, by = "mi") %>% arrange(mi)
cat(sprintf("Tercile portfolios: %d months; mean funds per tercile %.0f (min %d)\n",
            nrow(port), mean(port$n_all) / N_GROUPS, min(port$n_T1, port$n_T3)))

# --- 2. Rolling 36-month Carhart alpha of each portfolio --------------------
roll_alpha <- function(r, d) {
  X  <- cbind(1, d$MKT_RF, d$SMB, d$HML, d$MOM)
  y  <- r - d$RF
  vapply(seq_len(nrow(d)), function(i) {
    w <- which(d$mi >= d$mi[i] - (WIN_MONTHS - 1L) & d$mi <= d$mi[i])
    if (length(w) < MIN_OBS_ROLL) return(NA_real_)
    solve(crossprod(X[w, ]), crossprod(X[w, ], y[w]))[1] * 12 * 100
  }, numeric(1))
}
SERIES <- c("T1", "T2", "T3", "All")
alpha_ts <- data.frame(mi = port$mi)
for (s in SERIES) alpha_ts[[s]] <- roll_alpha(port[[s]], port)
alpha_ts$Spread <- alpha_ts$T1 - alpha_ts$T3   # identical windows: alpha is linear
alpha_ts <- alpha_ts %>%
  filter(if_all(all_of(c(SERIES, "Spread")), ~ !is.na(.))) %>%
  mutate(date = as.Date(sprintf("%d-%02d-01", (mi - 1L) %/% 12L, (mi - 1L) %% 12L + 1L)))
T_obs <- nrow(alpha_ts)
cat(sprintf("Rolling-alpha series: %d months, %s to %s\n", T_obs,
            format(min(alpha_ts$date), "%b %Y"), format(max(alpha_ts$date), "%b %Y")))

# --- 3. Break tests per series (kernel of structural_break_test.R) ----------
nw_vcov <- function(x, ...) NeweyWest(x, lag = NW_LAG, prewhite = FALSE, ...)

bp_one <- function(s) {
  y  <- ts(alpha_ts[[s]], start = c((alpha_ts$mi[1] - 1L) %/% 12L,
                                     (alpha_ts$mi[1] - 1L) %% 12L + 1L), frequency = 12)
  bp <- breakpoints(y ~ 1, h = MIN_SEG_MONTHS / T_obs, breaks = MAX_BREAKS)
  m_bic <- as.integer(which.min(AIC(bp, k = log(T_obs)))) - 1L
  m_lwz <- as.integer(which.min(AIC(bp, k = 0.299 * log(T_obs)^2.1))) - 1L
  dates <- data.frame()
  if (m_bic > 0L) {
    ci <- confint(bp, breaks = m_bic, vcov. = nw_vcov, het.err = FALSE, level = 0.95)$confint
    if (!is.matrix(ci)) ci <- matrix(ci, nrow = 1)
    idx <- function(v) alpha_ts$date[pmax(1L, pmin(T_obs, as.integer(round(v))))]
    dates <- data.frame(series = s, break_n = seq_len(nrow(ci)),
                        lower_95 = idx(ci[, 1]), estimate = idx(ci[, 2]),
                        upper_95 = idx(ci[, 3]))
  }
  fs <- Fstats(y ~ 1, from = 0.15, to = 0.85, vcov. = nw_vcov)
  sup <- sctest(fs, type = "supF"); ave <- sctest(fs, type = "aveF")
  # Fixed-date mean shift at the P2/P3 boundary (window-end dating)
  post <- as.numeric(alpha_ts$mi > mi_break)
  fit  <- lm(alpha_ts[[s]] ~ post)
  se   <- sqrt(diag(NeweyWest(fit, lag = NW_LAG, prewhite = FALSE)))
  list(dates = dates,
       summ = data.frame(series = s, T = T_obs, m_bic = m_bic, m_lwz = m_lwz,
                         supF = unname(sup$statistic), supF_p = sup$p.value,
                         aveF = unname(ave$statistic), aveF_p = ave$p.value,
                         mean_pre = mean(alpha_ts[[s]][post == 0]),
                         mean_post = mean(alpha_ts[[s]][post == 1]),
                         shift = unname(coef(fit)[2]),
                         shift_t = unname(coef(fit)[2] / se[2]),
                         shift_p = 2 * pnorm(-abs(coef(fit)[2] / se[2]))))
}
res <- lapply(c(SERIES, "Spread"), bp_one)
summ  <- bind_rows(lapply(res, `[[`, "summ"))
dates <- bind_rows(lapply(res, `[[`, "dates"))
print(summ, digits = 3)
print(dates)

# Pre-committed decision rule
hit <- with(summ, setNames(shift < 0 & shift_p < 0.05, series))
decision <- if (all(hit[c("T1", "T2", "T3")])) "broad" else
  if (hit["T1"] && !hit["T2"] && !hit["T3"]) "left tail only" else "mixed"
cat("Decision (pre-committed rule):", decision, "\n")

# --- 4. Excel output ----------------------------------------------------------
write_xlsx(list(Summary = summ, `Break Dates` = dates,
                Decision = data.frame(rule = "all three terciles: lower post mean, fixed-date p < 0.05",
                                      decision = decision),
                Series = alpha_ts %>% select(date, all_of(SERIES), Spread),
                Portfolios = port %>% select(mi, T1, T2, T3, All, n_T1, n_T3, n_all)),
           "breaktest_terciles_results.xlsx")

# --- 5. LaTeX tables ------------------------------------------------------------
f2 <- function(x) ifelse(x < 0, paste0("$-$", formatC(abs(x), format = "f", digits = 2)),
                         formatC(x, format = "f", digits = 2))
fp <- function(p) ifelse(p < 0.001, "$<$0.001", formatC(p, format = "f", digits = 3))
star <- function(p) ifelse(p < 0.01, "$^{***}$", ifelse(p < 0.05, "$^{**}$", ifelse(p < 0.10, "$^{*}$", "")))
mon <- function(d) format(d, "%b %Y")
note_block <- function(txt) paste0("\\begin{singlespace}\\footnotesize\\noindent\n", txt,
                                   "\n\\end{singlespace}\n")
table_wrap <- function(caption, label, colspec, body, note) {
  c("\\begin{table}[H]", "\\centering", "\\footnotesize", "\\setstretch{1}",
    paste0("\\caption{", caption, "}"), paste0("\\label{tab:", label, "}"),
    "\\resizebox{\\linewidth}{!}{%", paste0("\\begin{tabular}{", colspec, "}"),
    "\\toprule", body, "\\bottomrule", "\\end{tabular}", "}", "\\end{table}",
    note_block(note))
}
LBL <- c(T1 = "T1 (bottom)", T2 = "T2", T3 = "T3 (top)", All = "All funds",
         Spread = "T1 $-$ T3")
brk_lbl <- format(as.Date(paste0(REGIME_BREAK, "-01")), "%b %Y")

# Nearest BIC break to the regime boundary, with its 95% CI
near_break <- function(s) {
  d <- dates[dates$series == s, ]
  if (!nrow(d)) return("--")
  k <- which.min(abs(mi_of(d$estimate) - mi_break))
  sprintf("%s [%s, %s]", mon(d$estimate[k]), mon(d$lower_95[k]), mon(d$upper_95[k]))
}
body <- c(
  paste0("Series & Mean to ", brk_lbl, " & Mean after & Shift & HAC $t$ & sup-$F$ ($p$) & ",
         "BIC & LWZ & Nearest BIC break [95\\% CI] \\\\"),
  "\\midrule",
  vapply(seq_len(nrow(summ)), function(i) with(summ[i, ], paste0(
    LBL[[series]], " & ", f2(mean_pre), " & ", f2(mean_post), " & ",
    f2(shift), star(shift_p), " & ", f2(shift_t), " & ",
    formatC(supF, format = "f", digits = 2), " (", fp(supF_p), ") & ",
    m_bic, " & ", m_lwz, " & ", near_break(series), " \\\\",
    if (series == "T3") " \\midrule" else "")), character(1)))
COMMON <- paste0(
  "Each month $m$, active funds are sorted into terciles on the $t$-statistic of their ",
  "36-month rolling \\textcite{Carhart1997} alpha ending in $m-1$ (at least 24 observations; ",
  "the persistence-test signal); equal-weighted tercile portfolios of gross returns are held ",
  "in $m$. Series: 36-calendar-month rolling Carhart alpha of each portfolio (at least 24 ",
  "observations, \\% p.a.), ", mon(min(alpha_ts$date)), "--", mon(max(alpha_ts$date)),
  " ($T=", T_obs, "$). \\textit{All funds}: the same construction without the sort. ",
  "Sample: active funds, \\textcite{Evans2010}-corrected panel, performance-comparison subsample.")
note_main <- paste0(
  "\\textit{Shift}: coefficient on a post-", brk_lbl, " dummy in a regression of the series ",
  "on a constant and the dummy (the date is the P2/P3 boundary from Table~\\ref{tab:breaktest}, ",
  "not re-estimated here), Newey--West standard errors with 35 lags. sup-$F$: ",
  "\\textcite{Andrews1993} test of one break at an unknown date. BIC, LWZ: number of ",
  "\\textcite{Bai2003} breaks selected (at most 8, minimum segment 36 months). ",
  "Stars on the shift: $^{*}p<0.10$, $^{**}p<0.05$, $^{***}p<0.01$. ", COMMON)
writeLines(table_wrap("Structural Breaks in Rolling Alpha by Past-Performance Tercile",
                      "breaktest_terciles", "lcccccccc", body, note_main),
           "table_breaktest_terciles.tex")

body_d <- c("Series & Break & Lower 95\\% & Estimate & Upper 95\\% \\\\", "\\midrule",
            if (nrow(dates)) vapply(seq_len(nrow(dates)), function(i) with(dates[i, ], paste0(
              if (break_n == 1L) LBL[[series]] else "", " & ", break_n, " & ", mon(lower_95),
              " & ", mon(estimate), " & ", mon(upper_95), " \\\\")), character(1))
            else "\\multicolumn{5}{c}{No breaks selected} \\\\")
writeLines(table_wrap("Bai--Perron Break Dates by Past-Performance Tercile",
                      "breaktest_terciles_dates", "lcccc", body_d, paste0(
  "Break dates selected by BIC for each series of Table~\\ref{tab:breaktest_terciles}, dated at ",
  "the end of the 36-month window; 95\\% confidence intervals use one full-sample ",
  "Newey--West variance with 35 lags. ", COMMON)),
  "table_breaktest_terciles_dates.tex")

# --- 6. Figure (Appendix H) ------------------------------------------------------
fig_df <- bind_rows(lapply(c("T1", "T2", "T3"), function(s)
  data.frame(date = alpha_ts$date, alpha = alpha_ts[[s]], series = LBL[[s]])))
fig_df$series <- factor(fig_df$series, levels = LBL[c("T1", "T2", "T3")])
p <- ggplot(fig_df, aes(date, alpha, linetype = series, colour = series)) +
  geom_hline(yintercept = 0, colour = "grey60", linewidth = 0.3) +
  geom_vline(xintercept = as.Date(paste0(REGIME_BREAK, "-01")), linetype = "dashed",
             colour = "grey40", linewidth = 0.4) +
  geom_line(linewidth = 0.5) +
  scale_colour_manual(values = c("#B2182B", "grey35", "#2166AC")) +
  labs(x = NULL, y = "Rolling 36-month Carhart alpha (% p.a.)", colour = NULL, linetype = NULL) +
  theme_minimal(base_size = 10) + theme(legend.position = "bottom")
ggsave("fig_breaktest_terciles.png", p, width = 7, height = 4, dpi = 300)
cat("Written: breaktest_terciles_results.xlsx, table_breaktest_terciles.tex,",
    "table_breaktest_terciles_dates.tex, fig_breaktest_terciles.png\n")
