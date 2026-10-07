# =============================================================================
# BATCH 1 CHECKS (Oct 2026) - verifies the Stage 1 data fixes.
# Run in the same session right after Phase A (data_import_and_cleaning.R +
# flow_calculation.R), from WORKING_DIR. Read-only; prints to the console.
# Needs: dplyr, tidyr, readxl, lubridate. quantreg optional (median regression).
# =============================================================================
suppressPackageStartupMessages({
  library(dplyr); library(tidyr); library(readxl); library(lubridate)
})
stopifnot(exists("panel_incubation"), exists("panel_master"), exists("parse_col_dates"))

# Newey-West t for the intercept (lag 6), alpha in % p.a.
nw_alpha <- function(y, X, L = 6L) {
  XtX_inv <- solve(crossprod(X)); b <- XtX_inv %*% crossprod(X, y)
  e <- as.vector(y - X %*% b); n <- nrow(X); s <- X * e; S <- crossprod(s) / n
  for (j in seq_len(L)) {
    G <- crossprod(s[(j + 1):n, , drop = FALSE], s[1:(n - j), , drop = FALSE]) / n
    S <- S + (1 - j / (L + 1)) * (G + t(G))
  }
  V <- n * XtX_inv %*% S %*% XtX_inv
  c(alpha = b[1] * 1200, t = b[1] / sqrt(V[1, 1]))
}
wm <- function(x, w) { v <- !is.na(x) & !is.na(w) & w > 0
  if (!any(v)) NA_real_ else sum(x[v] * w[v]) / sum(w[v]) }

# --- 1. No clipping left in the main performance series ----------------------
cat("\n[1] Fund-months where ret_net differs from ret_net_raw (expect 0):",
    sum(panel_incubation$ret_net != panel_incubation$ret_net_raw, na.rm = TRUE), "\n")
cs <- panel_incubation %>% filter(!is.na(ret_net_raw)) %>% group_by(date) %>%
  summarise(sh = mean(ret_net_cs != ret_net_raw), .groups = "drop")
cat("    Robustness series ret_net_cs, share clipped per month (expect ~0.02):",
    "min", round(min(cs$sh), 3), "| median", round(median(cs$sh), 3),
    "| max", round(max(cs$sh), 3), "\n")

# --- 2. Gross/net direction: passive tracking difference vs benchmark --------
ym <- function(d) year(d) * 12L + month(d)
bench <- read_excel("fund_data.xlsx", sheet = "bench_returns", col_types = "text") %>%
  rename(Benchmark_Code = 1) %>%
  pivot_longer(-Benchmark_Code, names_to = "date", values_to = "lvl") %>%
  mutate(date = parse_col_dates(date), lvl = suppressWarnings(as.numeric(lvl))) %>%
  filter(!is.na(date), !is.na(lvl)) %>%
  group_by(Benchmark_Code) %>% arrange(date) %>%
  mutate(b = lvl / lag(lvl) - 1, ym = ym(date)) %>% ungroup() %>%
  select(Benchmark_Code, ym, b)
td <- panel_incubation %>%
  filter(ap_group == "Passive", !excluded_perf, !is.na(ret_net_raw)) %>%
  mutate(ym = ym(date), ER = as.numeric(Expense_Ratio)) %>%
  inner_join(bench, by = c("Benchmark_Code", "ym")) %>%
  group_by(Ticker, ER) %>%
  summarise(n = n(), td_net = 1200 * mean(ret_net_raw - b),
            td_gross = 1200 * mean(ret_gross_raw - b, na.rm = TRUE), .groups = "drop") %>%
  filter(n >= 36, !is.na(ER))
cat("\n[2] Passive funds vs benchmark (", nrow(td), " funds, % p.a.)\n", sep = "")
for (v in c("td_net", "td_gross")) {
  slope <- if (requireNamespace("quantreg", quietly = TRUE)) {
    coef(quantreg::rq(as.formula(paste(v, "~ ER")), data = td, tau = 0.5))[2]
  } else coef(lm(as.formula(paste(v, "~ ER")), data = td))[2]
  cat(sprintf("    %-8s median %6.3f | slope on ER %5.2f\n", v, median(td[[v]]), slope))
}
cat("    Expect: td_net slope ~ -1 (fees come out of net), td_gross slope ~ 0.\n")

# --- 3. No flows across TNA source switches ---------------------------------
n_bad <- panel_master %>% group_by(Ticker) %>% arrange(date) %>%
  mutate(sw = tna_source != lag(tna_source)) %>% ungroup() %>%
  filter(sw %in% TRUE, !is.na(flow_calc)) %>% nrow()
cat("\n[3] Non-NA flows on TNA-source switch months (expect 0):", n_bad, "\n")

# --- 4. Excluded funds are gone ----------------------------------------------
gone <- c("MLPAX", "MLPDX", "MLPFX", "MLPLX", "OMLPX", "PRRSX", "IDPIX", "OEPIX")
cat("\n[4] D9 / ProFunds tickers still in panel_master (expect none):",
    paste(intersect(paste(gone, "US Equity"), unique(panel_master$Ticker)), collapse = ", "), "\n")

# --- 5. Preview: aggregate Carhart alpha, Active, !excluded_perf -------------
d  <- panel_incubation %>%
  filter(!excluded_perf, ap_group == "Active", !is.na(ret_gross), !is.na(MKT_RF), !is.na(RF))
pr <- d %>% group_by(date) %>%
  summarise(ewg = mean(ret_gross), ewn = mean(ret_net),
            vwg = wm(ret_gross, tna_lag), vwn = wm(ret_net, tna_lag),
            MKT_RF = MKT_RF[1], SMB = SMB[1], HML = HML[1], MOM = MOM[1], RF = RF[1],
            .groups = "drop") %>%
  filter(!is.na(MOM))
X <- cbind(1, pr$MKT_RF, pr$SMB, pr$HML, pr$MOM)
cat("\n[5] Active aggregate Carhart alpha (% p.a., NW t), N =", n_distinct(d$Ticker),
    "funds, T =", nrow(pr), "months\n")
for (v in c("ewg", "ewn", "vwg", "vwn")) {
  r <- nw_alpha(pr[[v]] - pr$RF, X)
  cat(sprintf("    %-4s %7.3f  (t = %5.2f)\n", v, r[1], r[2]))
}
cat("    Preview only - the committed tables come from aggregate_alphas.R.\n")
