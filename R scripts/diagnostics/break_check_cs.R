# --- Variant D: per-month cross-sectional 1/99 winsorisation, month window --
# Run in the same session right after break_check_raw.R (reuses roll_one, bp_run).
ap_d <- panel_incubation %>%
  filter(!excluded_perf, !is.na(ret_gross_raw)) %>%
  group_by(date) %>%                                   # cut-offs per calendar month
  mutate(r_cs = pmin(pmax(ret_gross_raw, quantile(ret_gross_raw, .01)),
                     quantile(ret_gross_raw, .99))) %>%
  ungroup() %>%
  filter(ap_group == "Active") %>%
  transmute(Ticker, date, ym = year(date) * 12L + month(date),
            y_win = r_cs - RF, y_raw = r_cs - RF, MKT_RF, SMB, HML, MOM) %>%
  filter(!is.na(y_raw), !is.na(MKT_RF), !is.na(SMB), !is.na(HML), !is.na(MOM)) %>%
  arrange(Ticker, date)
cl <- makeCluster(max(1L, detectCores() - 1L)); invisible(clusterEvalQ(cl, library(lubridate)))
roll_d <- bind_rows(parLapply(cl, split(ap_d, ap_d$Ticker), roll_one, MIN_OBS_ROLL))
stopCluster(cl)
series_A_B_C <- series                                 # keep the earlier series
series <- roll_d %>% group_by(ym) %>%
  summarise(date = max(date), D = 100 * mean(C, na.rm = TRUE), .groups = "drop") %>%
  filter(is.finite(D)) %>% arrange(ym)
r <- bp_run("D")
cat("\nD per-month winsorised, month window\n  BIC-optimal breaks:", r$n_breaks,
    "\n  Break dates:", paste(r$dates, collapse = ", "),
    "\n  Regime means (% p.a.):", paste(r$means, collapse = " | "),
    "\n  P2 vs P3 mean:", paste(round(p_gap("D"), 2), collapse = " / "), "(gap in bp)\n")
print(as.data.frame(series %>% filter(date >= as.Date("2011-07-01"), date < as.Date("2012-02-01")) %>%
                      mutate(D = round(D, 2)) %>% select(date, D)), row.names = FALSE)
series <- series_A_B_C                                 # restore
