# =============================================================================
# PASSIVE TRACKING-DIFFERENCE CHECK (Appendix K, Table tab:k_td)         v1.0
#
# Tests whether the Bloomberg return index is net of fees: for passive funds,
# the annualised tracking difference (fund minus benchmark) should fall
# one-for-one with the expense ratio if returns are net (slope ~ -1) and be
# unrelated to it if gross (slope ~ 0). LAD (bootstrap SE) and OLS slopes.
#
# Run after Phase A (needs panel_incubation in the session).
# Inputs: fund_data.xlsx sheet "bench_returns" (LSEG benchmark index levels).
# Output: passive_tracking_difference.xlsx in WORKING_DIR.
# Dependencies: dplyr, tidyr, readxl, lubridate, quantreg, writexl
# =============================================================================
suppressPackageStartupMessages({ library(dplyr); library(tidyr); library(readxl)
  library(lubridate); library(quantreg); library(writexl) })
if (!exists("panel_incubation")) stop("Run Phase A first (panel_incubation missing).")
if (!exists("WORKING_DIR")) WORKING_DIR <- getwd()
MIN_MONTHS <- 36L

# Benchmark index levels -> monthly returns keyed by month index
b <- read_excel(file.path(WORKING_DIR, "fund_data.xlsx"), sheet = "bench_returns",
                col_types = "text")
names(b)[1] <- "Benchmark_Code"
parse_hdr_date <- function(v) {                 # Excel serial or ISO text
  n <- suppressWarnings(as.numeric(v)); out <- as.Date(n, origin = "1899-12-30")
  i <- is.na(out); out[i] <- suppressWarnings(as.Date(v[i])); out
}
bl <- b %>%
  pivot_longer(-Benchmark_Code, names_to = "date", values_to = "lvl") %>%
  mutate(date = parse_hdr_date(date), lvl = suppressWarnings(as.numeric(lvl))) %>%
  filter(!is.na(date), !is.na(lvl)) %>%
  group_by(Benchmark_Code) %>% arrange(date) %>%
  mutate(bret = lvl / lag(lvl) - 1, ym = year(date) * 12 + month(date)) %>%
  ungroup() %>% select(Benchmark_Code, ym, bret)

# Fund-level annualised mean tracking difference (net and gross)
td <- panel_incubation %>%
  filter(ap_group == "Passive", !excluded_perf, !is.na(ret_net_raw)) %>%
  mutate(ym = year(date) * 12 + month(date),
         ER = suppressWarnings(as.numeric(Expense_Ratio))) %>%
  inner_join(bl, by = c("Benchmark_Code", "ym")) %>% filter(!is.na(bret)) %>%
  group_by(Ticker, ER) %>%
  summarise(n = n(), td_net = 1200 * mean(ret_net_raw - bret),
            td_gross = 1200 * mean(ret_gross_raw - bret, na.rm = TRUE), .groups = "drop") %>%
  filter(n >= MIN_MONTHS, !is.na(ER))
cat("Passive funds with >=", MIN_MONTHS, "benchmark-matched months:", nrow(td), "\n")

set.seed(42)
res <- bind_rows(lapply(c("td_net", "td_gross"), function(v) {
  f <- as.formula(paste(v, "~ ER"))
  q <- summary(rq(f, data = td, tau = .5), se = "boot", R = 500)$coefficients
  o <- summary(lm(f, data = td))$coefficients
  data.frame(series = v, n_funds = nrow(td), median_td = median(td[[v]]),
             lad_slope = q[2, 1], lad_se = q[2, 2], ols_slope = o[2, 1], ols_se = o[2, 2])
}))
print(res, digits = 3)
write_xlsx(list(slopes = res, fund_td = td),
           file.path(WORKING_DIR, "passive_tracking_difference.xlsx"))
cat("Written: passive_tracking_difference.xlsx\n")
