# =============================================================================
# RETURN-TREATMENT ROBUSTNESS (Appendix K, Table tab:k_returns)          v1.0
#
# Aggregate Carhart alphas (EW and VW, active performance-comparison sample)
# under alternative return treatments: screened raw (baseline), per-month
# cross-sectional winsorisation, pooled 1/99 winsorisation, and the
# "fees deducted twice" series of the pre-audit pipeline (index treated as
# gross). Newey-West t, 6-month lag.
#
# Run after Phase A (needs panel_incubation in the session).
# Output: return_treatment_robustness.xlsx in WORKING_DIR.
# Dependencies: dplyr, writexl
# =============================================================================
suppressPackageStartupMessages({ library(dplyr); library(writexl) })
if (!exists("panel_incubation")) stop("Run Phase A first (panel_incubation missing).")
if (!exists("WORKING_DIR")) WORKING_DIR <- getwd()

# NW alpha (annualised %, x1200) and t
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
pw <- function(x) { q <- quantile(x, c(.01, .99), na.rm = TRUE); pmax(pmin(x, q[2]), q[1]) }

# Alternative series (pooled winsorisation across the whole panel)
p <- panel_incubation %>%
  mutate(ER         = suppressWarnings(as.numeric(Expense_Ratio)),
         net_pool   = pw(ret_net_raw), gross_pool = pw(ret_gross_raw),
         oldnet_raw = ret_net_raw - ER / 1200)        # fees deducted twice
# v-audit 17 Oct: net-return rows, as Table 4.5 (gross rows: funds with an ER)
d <- p %>% filter(!excluded_perf, ap_group == "Active", !is.na(ret_net),
                  !is.na(MKT_RF), !is.na(RF))

# Share of fund-months clipped by pooled winsorisation, by month
clip <- d %>% group_by(date) %>%
  summarise(share_clipped = mean(net_pool != ret_net_raw, na.rm = TRUE), .groups = "drop")
cat("Pooled-winsorisation clip share: median", round(median(clip$share_clipped), 4),
    "| max", round(max(clip$share_clipped), 4), "at",
    format(clip$date[which.max(clip$share_clipped)]), "\n")

specs <- c("Gross, screened raw (baseline)" = "ret_gross",
           "Net, screened raw (baseline)"   = "ret_net",
           "Gross, per-month winsorised"    = "ret_gross_cs",
           "Net, per-month winsorised"      = "ret_net_cs",
           "Gross, pooled winsorised"       = "gross_pool",
           "Net, pooled winsorised"         = "net_pool",
           "Net, fees deducted twice"       = "oldnet_raw")
res <- bind_rows(lapply(names(specs), function(nm) {
  v  <- specs[[nm]]
  pr <- d %>% filter(!is.na(.data[[v]])) %>% group_by(date) %>%
    summarise(ew = mean(.data[[v]]), vw = wm(.data[[v]], tna_lag),
              MKT_RF = MKT_RF[1], SMB = SMB[1], HML = HML[1], MOM = MOM[1],
              RF = RF[1], .groups = "drop") %>%
    filter(!is.na(MOM), !is.na(vw))
  X <- cbind(1, pr$MKT_RF, pr$SMB, pr$HML, pr$MOM)
  e <- nw_alpha(pr$ew - pr$RF, X); w <- nw_alpha(pr$vw - pr$RF, X)
  data.frame(spec = nm, T = nrow(pr), ew_alpha = e[[1]], ew_t = e[[2]],
             vw_alpha = w[[1]], vw_t = w[[2]])
}))
print(res, digits = 3)
write_xlsx(list(alphas = res, clip_by_month = clip),
           file.path(WORKING_DIR, "return_treatment_robustness.xlsx"))
cat("Written: return_treatment_robustness.xlsx\n")
