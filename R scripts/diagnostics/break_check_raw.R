# =============================================================================
# BREAK CHECK (Oct 2026) - is the Sep 2011 break an artefact of pooled
# winsorisation and/or the date-based rolling window?
# Rebuilds the EW mean of 36-month rolling Carhart gross alphas (Active,
# !excluded_perf) three ways and re-runs Bai-Perron on each:
#   A  current pipeline : winsorised ret_gross, %m-% date window
#   B  raw returns      : ret_gross_raw,        %m-% date window
#   C  raw + month index: ret_gross_raw,        calendar-month window
# Read-only. Writes break_check_raw.xlsx. Run after Phase A, from WORKING_DIR.
# Needs: dplyr, lubridate, strucchange, parallel, writexl.
# =============================================================================
suppressPackageStartupMessages({
  library(dplyr); library(lubridate); library(strucchange); library(parallel); library(writexl)
})
stopifnot(exists("panel_incubation"))
MIN_OBS_ROLL <- 24L; MIN_SEG <- 36L; MAX_BREAKS <- 8L

# --- 1. Active performance sample, same filters as alpha_estimation.R -------
ap <- panel_incubation %>%
  filter(!excluded_perf, ap_group == "Active") %>%
  transmute(Ticker, date, ym = year(date) * 12L + month(date),
            y_win = ret_gross - RF, y_raw = ret_gross_raw - RF,
            MKT_RF, SMB, HML, MOM) %>%
  filter(!is.na(y_win), !is.na(y_raw), !is.na(MKT_RF), !is.na(SMB),
         !is.na(HML), !is.na(MOM)) %>%
  arrange(Ticker, date)
ap_split <- split(ap, ap$Ticker)
cat("Active funds:", length(ap_split), "| fund-months:", nrow(ap), "\n")

# --- 2. Rolling alpha per fund, three variants (intercept only, no SEs) -----
roll_one <- function(d, min_obs) {
  X <- cbind(1, d$MKT_RF, d$SMB, d$HML, d$MOM); n <- nrow(d)
  a <- matrix(NA_real_, n, 3, dimnames = list(NULL, c("A", "B", "C")))
  ols_a <- function(y, w) tryCatch(solve(crossprod(X[w, , drop = FALSE]),
                                        crossprod(X[w, , drop = FALSE], y[w]))[1] * 12,
                                  error = function(e) NA_real_)
  for (i in seq_len(n)) {
    w_date <- which(d$date >= (d$date[i] %m-% months(35)) & d$date <= d$date[i])
    w_mon  <- which(d$ym >= d$ym[i] - 35L & d$ym <= d$ym[i])
    if (length(w_date) >= min_obs) { a[i, "A"] <- ols_a(d$y_win, w_date)
                                     a[i, "B"] <- ols_a(d$y_raw, w_date) }
    if (length(w_mon)  >= min_obs)   a[i, "C"] <- ols_a(d$y_raw, w_mon)
  }
  data.frame(date = d$date, ym = d$ym, a)
}
cl <- makeCluster(max(1L, detectCores() - 1L))
clusterEvalQ(cl, library(lubridate))
roll <- bind_rows(parLapply(cl, ap_split, roll_one, MIN_OBS_ROLL))
stopCluster(cl)

# --- 3. EW cross-sectional mean per month (alpha in % p.a.) -----------------
series <- roll %>% group_by(ym) %>%
  summarise(date = max(date), across(c(A, B, C), ~ 100 * mean(.x, na.rm = TRUE)),
            .groups = "drop") %>%
  filter(is.finite(A), is.finite(B), is.finite(C)) %>% arrange(ym)
cat("Series:", nrow(series), "months,", format(min(series$date), "%b %Y"), "to",
    format(max(series$date), "%b %Y"), "\n")

# --- 4. Bai-Perron on each variant (mean shifts, BIC over 0..8 breaks) ------
bp_run <- function(v) {
  y  <- ts(series[[v]], start = c(year(series$date[1]), month(series$date[1])), frequency = 12)
  bp <- breakpoints(y ~ 1, h = MIN_SEG / length(y), breaks = MAX_BREAKS)
  bic <- AIC(bp, k = log(length(y)))
  m   <- as.integer(which.min(bic)) - 1L
  idx <- if (m == 0L) integer(0) else breakpoints(bp, breaks = m)$breakpoints
  seg <- findInterval(seq_along(y), idx + 1L) + 1L
  list(variant = v, n_breaks = m, bic = bic,
       dates = format(series$date[idx], "%b %Y"),
       means = round(tapply(series[[v]], seg, mean), 2))
}
res <- lapply(c("A", "B", "C"), bp_run)

# --- 5. Report: break dates, regime means, P2 vs P3 gap ---------------------
lbl <- c(A = "A current (winsorised, date window)", B = "B raw returns, date window",
         C = "C raw returns, month window")
p_gap <- function(v) {
  p2 <- mean(series[[v]][series$date >= as.Date("2006-01-01") & series$date < as.Date("2011-10-01")])
  p3 <- mean(series[[v]][series$date >= as.Date("2011-10-01")])
  c(P2 = p2, P3 = p3, gap_bp = 100 * (p2 - p3))
}
for (r in res) {
  cat("\n", lbl[[r$variant]], "\n  BIC-optimal breaks:", r$n_breaks,
      if (r$n_breaks == MAX_BREAKS) "(AT SEARCH LIMIT)" else "",
      "\n  Break dates (segment ends):", paste(r$dates, collapse = ", "),
      "\n  Regime means (% p.a.):", paste(r$means, collapse = " | "),
      "\n  P2 (Jan06-Sep11) vs P3 mean:", paste(round(p_gap(r$variant), 2), collapse = " / "),
      "(last = gap in bp)\n")
}

# Month-by-month jump at the window that drops Oct 2008 (Sep -> Oct 2011)
jmp <- series %>% filter(date >= as.Date("2011-07-01"), date < as.Date("2012-02-01")) %>%
  select(date, A, B, C) %>% mutate(across(c(A, B, C), ~ round(.x, 2)))
cat("\nSeries around the break (% p.a.):\n"); print(as.data.frame(jmp), row.names = FALSE)

write_xlsx(list(series = series,
                breaks = bind_rows(lapply(res, function(r)
                  data.frame(variant = lbl[[r$variant]], n_breaks = r$n_breaks,
                             dates = paste(r$dates, collapse = ", "),
                             means = paste(r$means, collapse = " | "))))),
           "break_check_raw.xlsx")
cat("\nWrote break_check_raw.xlsx\n")
