# =============================================================================
# DOLLAR VALUE ADDED (BERK AND VAN BINSBERGEN 2015)
# value_added.R  (v1.0, Oct 2026; defense extension, pre-committed 15 Oct 2026)
#
# V_it = TNA_{i,t-1} x (R^gross_it - rf_t - b_i'F_t): dollars extracted from
# markets before fees (Berk & van Binsbergen 2015, JFE). b_i from the fund's
# full-sample Carhart regression on gross excess returns (>= 24 obs). Net
# counterpart: same with R^net (value delivered to investors).
# Differences from BvB, disclosed in the text: Carhart factors instead of
# Vanguard index-fund benchmarks; in-sample loadings.
#
# Fund level: S_i = mean_t V_it; cross-sectional mean weighted by months
#   alive (BvB), fund-clustered t-statistic; median, % positive.
# Aggregate: monthly sum over funds, time-series mean, Newey-West t (6 lags,
#   as the aggregate alpha regressions).
# By regime P1/P2/P3 (boundaries from breaktest_results.xlsx).
# Units: USD million per year, 2025 dollars (CPI-U, CPIAUCSL, 2025 average
#   = 1), and nominal.
#
# Requires: panel_incubation in session (Phase A); CPIAUCSL.xlsx (FRED,
#   sheet "Monthly"); breaktest_results.xlsx.
# Outputs: value_added_results.xlsx, table_value_added.tex (Chapter 4).
# =============================================================================

suppressPackageStartupMessages({
  library(dplyr)
  library(readxl)
  library(writexl)
})

if (!exists("WORKING_DIR")) WORKING_DIR <- getwd()
setwd(WORKING_DIR)
if (!exists("panel_incubation"))
  stop("panel_incubation not in session. Run Phase A first.")

CPI_FILE         <- "CPIAUCSL.xlsx"
CPI_BASE_YEAR    <- 2025
SUBPERIOD_BREAKS <- c("2006-03", "2010-08")
BREAKTEST_FILE   <- "breaktest_results.xlsx"
MIN_OBS          <- 24L
NW_LAG           <- 6L

.bd <- format(as.Date(read_excel(BREAKTEST_FILE, sheet = "Break Dates")[["Point Estimate"]]), "%Y-%m")
if (!all(SUBPERIOD_BREAKS %in% .bd))
  stop("SUBPERIOD_BREAKS are not all break dates in ", BREAKTEST_FILE, call. = FALSE)

# --- 1. CPI deflator (2025 dollars) ------------------------------------------
cpi <- read_excel(CPI_FILE, sheet = "Monthly") %>%
  mutate(observation_date = if (is.numeric(observation_date))      # Excel serial
           as.Date(observation_date, origin = "1899-12-30") else as.Date(observation_date)) %>%
  transmute(ym = format(observation_date, "%Y-%m"), cpi = as.numeric(CPIAUCSL))
# Oct 2025 was not published (federal shutdown): fill isolated gaps by
# geometric interpolation between the neighbouring months
gap <- which(is.na(cpi$cpi))
stopifnot(all(gap > 1 & gap < nrow(cpi)), !anyNA(cpi$cpi[c(gap - 1, gap + 1)]))
cpi$cpi[gap] <- sqrt(cpi$cpi[gap - 1] * cpi$cpi[gap + 1])
if (length(gap)) cat("CPI interpolated for:", cpi$ym[gap], "\n")
cpi_base <- mean(cpi$cpi[substr(cpi$ym, 1, 4) == as.character(CPI_BASE_YEAR)])
stopifnot(sum(substr(cpi$ym, 1, 4) == as.character(CPI_BASE_YEAR)) == 12L)
cpi$defl <- cpi_base / cpi$cpi

# --- 2. Panel and full-sample Carhart loadings --------------------------------
d <- panel_incubation %>%
  filter(!excluded_perf, ap_group == "Active", !is.na(ret_gross), !is.na(MKT_RF),
         !is.na(SMB), !is.na(HML), !is.na(MOM), !is.na(RF)) %>%
  transmute(Ticker, date, ym = format(date, "%Y-%m"), ret_gross, ret_net, tna_lag,
            MKT_RF, SMB, HML, MOM, RF)
betas <- d %>% group_by(Ticker) %>% filter(n() >= MIN_OBS) %>%
  group_modify(function(x, ...) {
    X <- cbind(1, x$MKT_RF, x$SMB, x$HML, x$MOM)
    b <- solve(crossprod(X), crossprod(X, x$ret_gross - x$RF))
    data.frame(b_mkt = b[2], b_smb = b[3], b_hml = b[4], b_mom = b[5])
  }) %>% ungroup()

va <- d %>%
  inner_join(betas, by = "Ticker") %>%
  left_join(cpi %>% select(ym, defl), by = "ym") %>%
  filter(!is.na(tna_lag), tna_lag > 0) %>%
  mutate(fac  = b_mkt * MKT_RF + b_smb * SMB + b_hml * HML + b_mom * MOM,
         V_gross_nom = tna_lag * (ret_gross - RF - fac),          # USD million per month
         V_net_nom   = tna_lag * (ret_net   - RF - fac),
         V_gross = V_gross_nom * defl, V_net = V_net_nom * defl,
         regime = ifelse(ym <= SUBPERIOD_BREAKS[1], "P1",
                         ifelse(ym <= SUBPERIOD_BREAKS[2], "P2", "P3")))
if (anyNA(va$defl)) stop("CPI missing for: ", paste(unique(va$ym[is.na(va$defl)]), collapse = ", "))
cat(sprintf("Value added panel: %d fund-months, %d funds, %s to %s\n", nrow(va),
            n_distinct(va$Ticker), min(va$ym), max(va$ym)))

# --- 3. Statistics --------------------------------------------------------------
nw_t <- function(y, lag = NW_LAG) {
  e <- y - mean(y); Tn <- length(y); s <- sum(e^2) / Tn
  for (j in seq_len(lag)) s <- s + 2 * (1 - j / (lag + 1)) * sum(e[(j + 1):Tn] * e[1:(Tn - j)]) / Tn
  mean(y) / sqrt(s / Tn)
}
summ_va <- function(x, col, label) {
  agg <- x %>% group_by(ym) %>% summarise(V = sum(.data[[col]]), .groups = "drop")
  fl  <- x %>% group_by(Ticker) %>% summarise(S = mean(.data[[col]]), Tm = n(), .groups = "drop")
  w   <- fl$Tm / sum(fl$Tm); m_w <- sum(w * fl$S)
  se_w <- sqrt(sum(w^2 * (fl$S - m_w)^2))      # fund-clustered SE of the weighted mean
  q <- quantile(fl$S, c(0.05, 0.25, 0.5, 0.75, 0.95)) * 12
  data.frame(series = label, months = nrow(agg), funds = nrow(fl),
             agg_mean = 12 * mean(agg$V), agg_t = nw_t(agg$V),
             agg_total = sum(agg$V),
             fund_wmean = 12 * m_w, fund_t = m_w / se_w,
             fund_p05 = q[[1]], fund_p25 = q[[2]], fund_median = q[[3]],
             fund_p75 = q[[4]], fund_p95 = q[[5]], pct_pos = 100 * mean(fl$S > 0))
}
res <- bind_rows(lapply(c("All", "P1", "P2", "P3"), function(r) {
  x <- if (r == "All") va else va %>% filter(regime == r)
  bind_rows(summ_va(x, "V_gross", "Gross (2025 USD)"), summ_va(x, "V_net", "Net (2025 USD)"),
            summ_va(x, "V_gross_nom", "Gross (nominal)"), summ_va(x, "V_net_nom", "Net (nominal)")) %>%
    mutate(period = r, .before = 1)
}))
print(res %>% select(period, series, funds, agg_mean, agg_t, fund_wmean, fund_t, fund_median, pct_pos),
      digits = 3)

monthly <- va %>% group_by(ym, regime) %>%
  summarise(V_gross = sum(V_gross), V_net = sum(V_net), funds = n(), .groups = "drop")
write_xlsx(list(Summary = res, Monthly = monthly,
                Funds = va %>% group_by(Ticker) %>%
                  summarise(months = n(), S_gross = 12 * mean(V_gross), S_net = 12 * mean(V_net),
                            .groups = "drop")),
           "value_added_results.xlsx")

# --- 4. LaTeX table (Chapter 4) ------------------------------------------------
fnum <- function(x, k = 1) ifelse(x < 0, paste0("$-$", formatC(abs(x), format = "f", digits = k, big.mark = ",")),
                                  formatC(x, format = "f", digits = k, big.mark = ","))
star_t <- function(t) ifelse(abs(t) >= 2.576, "$^{***}$", ifelse(abs(t) >= 1.960, "$^{**}$",
                       ifelse(abs(t) >= 1.645, "$^{*}$", "")))
note_block <- function(txt) paste0("\\begin{singlespace}\\footnotesize\\noindent\n", txt,
                                   "\n\\end{singlespace}\n")
PER <- c("All", "P1", "P2", "P3")
row_of <- function(series, lab, f) {
  x <- res %>% filter(.data$series == !!series) %>% arrange(match(period, PER))
  paste0(lab, " & ", paste(f(x), collapse = " & "), " \\\\")
}
va_panel <- function(series, title) c(
  paste0("\\multicolumn{5}{l}{\\textit{", title, "}} \\\\"),
  row_of(series, "Aggregate, USD m per year", function(x) paste0(fnum(x$agg_mean), star_t(x$agg_t))),
  row_of(series, "\\quad $t$-statistic", function(x) fnum(x$agg_t, 2)),
  row_of(series, "Per fund, life-weighted mean", function(x) paste0(fnum(x$fund_wmean, 2), star_t(x$fund_t))),
  row_of(series, "\\quad $t$-statistic", function(x) fnum(x$fund_t, 2)),
  row_of(series, "Per fund, median", function(x) fnum(x$fund_median, 2)),
  row_of(series, "Funds with positive value added (\\%)", function(x) fnum(x$pct_pos, 1)))
hdr_win <- function(r) {
  x <- va %>% filter(regime == r)
  paste0(r, " (", format(as.Date(paste0(min(x$ym), "-01")), "%b %Y"), "--",
         format(as.Date(paste0(max(x$ym), "-01")), "%b %Y"), ")")
}
body <- c(paste0(" & Full sample & ", paste(vapply(c("P1", "P2", "P3"), hdr_win, ""), collapse = " & "), " \\\\"),
          "\\midrule", va_panel("Gross (2025 USD)", "A. Value added (gross returns)"), "\\midrule",
          va_panel("Net (2025 USD)", "B. Value delivered to investors (net returns)"), "\\midrule",
          row_of("Gross (2025 USD)", "Funds", function(x) formatC(x$funds, big.mark = ",")),
          row_of("Gross (2025 USD)", "Months", function(x) x$months))
note <- paste0(
  "Value added follows \\textcite{BerkVanBinsbergen2015}: $V_{it}=q_{i,t-1}(R_{it}-r^f_t-",
  "\\hat\\beta_i'F_t)$, where $q_{i,t-1}$ is lagged TNA, $R_{it}$ the gross (Panel A) or net ",
  "(Panel B) return and $\\hat\\beta_i$ the fund's full-sample \\textcite{Carhart1997} loadings ",
  "(gross returns, at least 24 months). Unlike \\textcite{BerkVanBinsbergen2015}, the benchmark is ",
  "the Carhart factor portfolios rather than Vanguard index funds, which are not costless to ",
  "track, and the loadings are estimated in sample. Amounts in USD million per year at ",
  CPI_BASE_YEAR, " prices (CPI-U, all items; October 2025, not published, interpolated). Aggregate: monthly sum over funds, time-series ",
  "mean $\\times 12$, Newey--West $t$-statistic (6 lags). Per fund: mean of $V_{it}$ over each ",
  "fund's months $\\times 12$, averaged across funds with weights proportional to months alive; ",
  "$t$-statistic clustered by fund. $^{*}p<0.10$, $^{**}p<0.05$, $^{***}p<0.01$. Regimes as in ",
  "Table~\\ref{tab:breaktest}. Sample: active funds, \\textcite{Evans2010}-corrected panel, ",
  "performance-comparison subsample.")
writeLines(c("\\begin{table}[H]", "\\centering", "\\footnotesize",
             "\\caption{Dollar Value Added by Active Funds}", "\\label{tab:value_added}",
             "\\resizebox{\\linewidth}{!}{%", "\\begin{tabular}{lcccc}", "\\toprule", body,
             "\\bottomrule", "\\end{tabular}", "}", "\\end{table}", note_block(note)),
           "table_value_added.tex")
cat("Written: value_added_results.xlsx, table_value_added.tex\n")
