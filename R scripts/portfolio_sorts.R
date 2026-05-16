# =============================================================================
# PORTFOLIO SORTS — Size, Momentum, Fee Quintiles                            v1.4
#
# Changes from v1.3 (Family D pre-defense audit):
#   - Performance-comparison subsample filter added to the panel-prep stage:
#     port_base <- panel_incubation %>% filter(!excluded_perf) %>% ...
#     Per the flagged_funds.xlsx exclusion ledger wired in by
#     data_import_and_cleaning.R v1.2 Step 8c, the performance/portfolio-sorts
#     scripts must restrict to the !excluded_perf subsample so that funds
#     flagged in the "Exclude from Perf Comparison" sheet do not contaminate
#     quintile cutoffs or the Q5-Q1 spread alphas. Mirrors the v2.7 fix in
#     alpha_estimation.R, the v1.3 fix in FF_comparison.R, and the v1.5 fix
#     in subperiod_analysis.R.
#   - factors_ts pull (line 265 region) is left unfiltered because it takes
#     distinct(date, MKT_RF, ..., RF), and these factor values are constant
#     within a date across the fund cross-section. Filtering would not change
#     the factor matrix; skipping the filter avoids a redundant pass.
#   - Sample-source sentence in fn_d1, fn_d2, and fn_base footnotes appended
#     with "performance-comparison subsample per flagged\_funds.xlsx" to match
#     the convention adopted in alpha_reporting.R v8.4 and FF_comparison.R v1.4.
#
# Changes from v1.2:
#   - Panel switch: source panel is now panel_incubation (Evans 2010 36-month
#     correction, no date cap) rather than panel_trimmed. Extends coverage
#     through February 2026 without artificially capping at 2023. Sample-source
#     sentences in Table D1, D2, and D5 (fee) footnotes updated accordingly.
#   - Sharpe ratio added to Table D1 (Table 9 in the dissertation): one new
#     column "EW Sharpe" reports the annualised Sharpe ratio of the quintile
#     portfolio's equal-weighted gross excess return,
#     sqrt(12)*mean(r_EW_gross - RF)/sd(r_EW_gross - RF). Column count D1_NCOLS
#     increases from 7 to 8; col.names and align updated; footnote fn_d1
#     extended. Sharpe is computed on the full monthly time series per
#     (ap_group, quintile) bucket using factors_ts$RF (inner-joined on date).
#     Active Q5 fee panel shows the Sharpe penalty of carrying the highest
#     expense ratios directly.
# =============================================================================
#
# Changes from v1.1:
#   - RF subtraction fix: run_models() gains subtract_rf parameter (default
#     TRUE). Long-short spreads (quintile 6L) pass subtract_rf=FALSE because
#     the risk-free rate cancels in a self-financing long-short construction:
#     (r5 - RF) - (r1 - RF) = r5 - r1. The prior code regressed (r5-r1)-RF,
#     over-subtracting RF once per period. Footnote updated accordingly.
#   - Beta t-statistics added: run_models() now returns t_mkt, t_smb, t_hml,
#     t_mom (Newey-West, 6-month lag). Tables 14-16 report these in the
#     t-statistic row alongside alpha t-statistics.
#   - Significance stars: add_stars() helper appends $^{*}$/$^{**}$/$^{***}$
#     (10%/5%/1%) as LaTeX superscripts to all estimated coefficients in
#     Tables 13-16. Stars use |t| thresholds 1.645/1.960/2.576.
#   - Table 13 line-spacing fix: linesep="" added to kbl() call; kableExtra
#     default inserts \addlinespace[0.3em] after every 5th row with
#     booktabs=TRUE, landing between rows 5 and 6 of the 8-row table.
#   - Fee quintile integrity fix: fee_q now uses group_by(Ticker, ap_group) +
#     summarise(fee_sort = first(na.omit(fee_sort))) instead of
#     distinct(Ticker, ap_group, fee_sort), preventing many-to-many joins
#     when Expense_Ratio varies across rows for the same fund.
#
# Produces:
#   portfolio_returns.xlsx        ??? monthly EW/VW return series per portfolio
#   portfolio_alphas.xlsx         ??? regression results per portfolio
#   table_port_chars.tex          ??? Table D1: characteristics by sort/quintile
#   table_port_agg_alpha.tex      ??? Table D2: Active vs Passive aggregate alpha
#   table_port_size_alpha.tex     ??? Table D3: Size quintile alphas
#   table_port_mom_alpha.tex      ??? Table D4: Momentum quintile alphas
#   table_port_fee_alpha.tex      ??? Table D5: Fee quintile alphas
#
# Requires (in session): panel_incubation with tna, tna_lag, ret_gross, ret_net,
#   flow_calc_pct_win, Expense_Ratio, Turnover, MKT_RF, SMB, HML, MOM, RF.
#
# Dependencies: dplyr, tidyr, purrr, zoo, lubridate, writexl,
#               knitr, kableExtra, stringr
# =============================================================================

library(dplyr)
library(tidyr)
library(purrr)
library(zoo)
library(lubridate)
library(writexl)
library(knitr)
library(kableExtra)
library(stringr)

N_QUINT <- 5L
NW_LAG  <- 6L
MIN_OBS <- 24L

# =============================================================================
# 1. HELPERS
# =============================================================================

fmt <- function(x, d = 3) {
  ifelse(is.na(x) | is.nan(x), "--",
         formatC(round(as.numeric(x), d), format = "f", digits = d,
                 big.mark = ","))
}

wm <- function(x, w) {
  v <- !is.na(x) & !is.na(w) & w > 0
  if (sum(v) == 0L) return(NA_real_)
  sum(x[v] * w[v]) / sum(w[v])
}

safe_ntile <- function(x, n = N_QUINT) {
  out   <- rep(NA_integer_, length(x))
  valid <- !is.na(x)
  if (sum(valid) < n) return(out)
  out[valid] <- dplyr::ntile(x[valid], n)
  out
}

# Append significance stars as LaTeX superscripts.
# Uses |t| thresholds: 1.645 (10%), 1.960 (5%), 2.576 (1%).
# Returns val_str unchanged if t_stat is NA or val_str is "--".
# escape=FALSE in kbl() is required for these to render in Overleaf.
add_stars <- function(val_str, t_stat) {
  if (is.na(val_str) || val_str %in% c("--", "")) return(val_str)
  t_abs <- suppressWarnings(abs(as.numeric(t_stat)))
  if (is.na(t_abs)) return(val_str)
  stars <- if      (t_abs >= 2.576) "$^{***}$"
  else if (t_abs >= 1.960) "$^{**}$"
  else if (t_abs >= 1.645) "$^{*}$"
  else                     ""
  paste0(val_str, stars)
}

# --- LaTeX helpers -----------------------------------------------------------

clean_latex <- function(x, resize = TRUE, small = FALSE) {
  # Phase 2.4 SBE consistency pass (May 2026).
  # Replaces kableExtra's default output with the savebox+minipage pattern
  # for floating tables, or @{\extracolsep{\fill}} stretched longtable layout.
  # Args resize/small retained for signature compatibility, no effect now.

  find_brace_end <- function(s, brace_start) {
    depth <- 0L
    n <- nchar(s)
    for (i in brace_start:n) {
      ch <- substr(s, i, i)
      if (ch == "{") depth <- depth + 1L
      else if (ch == "}") {
        depth <- depth - 1L
        if (depth == 0L) return(i)
      }
    }
    NA_integer_
  }

  # Generic kableExtra fixes.
  x <- gsub("\\\\end[{]threeparttable[}][}]", "\\\\end{threeparttable}", x)
  x <- gsub("\\begin{table}[!h]", "\\begin{table}[H]", x, fixed = TRUE)

  # ---- Longtable branch (Phase 2.3 stretching + Phase 2.4 12pt gap) -------
  if (grepl("\\\\begin\\{longtable\\}", x)) {
    if (!grepl("@\\{\\\\extracolsep\\{\\\\fill\\}\\}", x, perl = TRUE)) {
      x <- sub(
        "(\\\\begin\\{longtable\\}(?:\\[[^]]*\\])?)\\{",
        "\\1{@{\\\\extracolsep{\\\\fill}}",
        x, perl = TRUE
      )
    }
    cap_pattern <- paste0(
      "(\\\\caption(?:\\[[^]]*\\])?",
      "\\{(?:[^{}]|\\{(?:[^{}]|\\{[^{}]*\\})*\\})*\\}",
      ")\\\\\\\\(?!\\[)"
    )
    x <- gsub(cap_pattern, "\\1\\\\\\\\[12pt]", x, perl = TRUE)
    return(x)
  }

  # ---- Floating-table branch (savebox + minipage) -------------------------
  cap_anchor <- regexpr("\\\\caption(?=\\{)", x, perl = TRUE)
  if (cap_anchor == -1) return(x)
  cap_start <- as.integer(cap_anchor)
  brace_start <- cap_start + attr(cap_anchor, "match.length")
  cap_end <- find_brace_end(x, brace_start)
  if (is.na(cap_end)) return(x)
  caption_text <- substring(x, cap_start, cap_end)

  tab_m <- regexpr(
    "(?s)\\\\begin\\{tabular\\}(?:\\[[^]]*\\])?\\{[^}]*\\}.*?\\\\end\\{tabular\\}",
    x, perl = TRUE
  )
  if (tab_m == -1) return(x)
  tabular <- substring(x, tab_m, tab_m + attr(tab_m, "match.length") - 1L)

  notes_m <- regexpr(
    "(?s)\\\\begin\\{tablenotes\\}\\s*\\\\item\\s+(.*?)\\\\end\\{tablenotes\\}",
    x, perl = TRUE
  )
  notes <- ""
  if (notes_m != -1) {
    full_block <- substring(x, notes_m, notes_m + attr(notes_m, "match.length") - 1L)
    inner <- sub("^(?s)\\\\begin\\{tablenotes\\}\\s*\\\\item\\s+", "",
                 full_block, perl = TRUE)
    inner <- sub("(?s)\\\\end\\{tablenotes\\}\\s*$", "", inner, perl = TRUE)
    notes <- trimws(inner)
  }

  out <- paste0(
    "\\begin{table}[H]\n",
    "\\centering\n",
    "\\sbox{\\tabletempbox}{%\n",
    "\\footnotesize\n",
    tabular, "%\n",
    "}\n",
    "\\setlength{\\tabletempwidth}{\\wd\\tabletempbox}\n",
    "\\ifdim\\tabletempwidth>\\linewidth\\setlength{\\tabletempwidth}{\\linewidth}\\fi\n",
    "\\begin{minipage}{\\tabletempwidth}\n",
    "\\captionsetup{width=\\linewidth}\n",
    caption_text, "\n",
    "\\ifdim\\wd\\tabletempbox>\\linewidth\n",
    "  \\resizebox{\\linewidth}{!}{\\usebox{\\tabletempbox}}\n",
    "\\else\n",
    "  \\usebox{\\tabletempbox}\n",
    "\\fi\n"
  )
  if (nzchar(notes)) {
    out <- paste0(out,
      "\\par\\medskip\n",
      "\\begin{singlespace}\\footnotesize\\noindent\n",
      notes, "\n",
      "\\end{singlespace}\n"
    )
  }
  out <- paste0(out, "\\end{minipage}\n\\end{table}\n")
  out
}

write_tex <- function(s, fn, resize = TRUE, small = FALSE) {
  writeLines(clean_latex(s, resize = resize, small = small), fn)
  cat("Written:", fn, "\n")
}

# Append a note paragraph AFTER \end{longtable} (not inside the body).
longtable_note <- function(s, note, n_cols) {
  note_para <- paste0(
    "{\\footnotesize\\noindent\\textit{Note:} ", note, "}\n\\par\\medskip\n\n"
  )
  parts <- strsplit(s, "\\end{longtable}", fixed = TRUE)[[1]]
  paste0(parts[1], "\\end{longtable}\n", note_para,
         if (length(parts) > 1)
           paste0(parts[-1], collapse = "\\end{longtable}") else "")
}

# Wrap a longtable in footnotesize + tighter tabcolsep for compact body text.
# \captionsetup{font=normalsize} is injected BEFORE \begin{longtable} inside
# the {\footnotesize} group so that the \caption{} command (which is inside
# the longtable body) inherits normalsize (12 pt) rather than footnotesize.
# Without this override the global captionsetup[table]{font=normalsize} is
# defeated by the enclosing {\footnotesize} scope — visible as a 9 pt caption
# in the compiled PDF. This fix is self-contained: no manual post-edit needed.
wrap_lt_small <- function(s, tabcolsep = "3pt") {
  opener <- paste0(
    "{\\setlength{\\tabcolsep}{", tabcolsep, "}\\footnotesize\n",
    "\\captionsetup{font=normalsize}\\begin{longtable}"   # ← caption override
  )
  parts_open <- strsplit(s, "\\begin{longtable}", fixed = TRUE)[[1]]
  s <- paste0(parts_open[1], opener,
              if (length(parts_open) > 1) parts_open[2] else "")
  parts_close <- strsplit(s, "\\end{longtable}", fixed = TRUE)[[1]]
  paste0(parts_close[1], "\\end{longtable}\n}",
         if (length(parts_close) > 1)
           paste0(parts_close[-1], collapse = "\\end{longtable}") else "")
}

# OLS helper
fast_ols <- function(y, X) {
  tryCatch({
    XtX  <- crossprod(X)
    beta <- solve(XtX, crossprod(X, y))
    e    <- as.vector(y - X %*% beta)
    n    <- length(y); k <- ncol(X)
    list(beta = as.vector(beta), e = e,
         r2   = 1 - sum(e^2) / sum((y - mean(y))^2),
         n = n, k = k)
  }, error = function(err) NULL)
}

# Newey-West standard errors
nw_se <- function(X, e, lag) {
  T <- nrow(X)
  tryCatch({
    XtX_inv <- solve(crossprod(X))
    scores  <- X * as.vector(e)
    S       <- crossprod(scores) / T
    if (lag > 0L) {
      for (j in seq_len(lag)) {
        w  <- 1 - j / (lag + 1)
        Gj <- crossprod(scores[(j + 1):T, , drop = FALSE],
                        scores[1:(T - j),  , drop = FALSE]) / T
        S  <- S + w * (Gj + t(Gj))
      }
    }
    sqrt(pmax(diag(T * XtX_inv %*% S %*% XtX_inv), 0))
  }, error = function(err) rep(NA_real_, ncol(X)))
}

# Run CAPM, FF3, Carhart on a single return column.
# subtract_rf: set TRUE for individual portfolios, FALSE for long-short spreads.
# A self-financing long-short portfolio already nets out the risk-free rate in
# construction: (r5 - RF) - (r1 - RF) = r5 - r1. Subtracting RF again would
# over-penalise the alpha by ~RF per period (~0.4-0.5% annualised).
run_models <- function(ret_col, port_df, factors_df, subtract_rf = TRUE) {
  d <- port_df %>%
    select(date, ret = all_of(ret_col)) %>%
    left_join(factors_df, by = "date") %>%
    filter(!is.na(ret), !is.na(MKT_RF), !is.na(RF)) %>%
    mutate(excess = if (subtract_rf) ret - RF else ret)
  
  n <- nrow(d)
  
  # NA row returned when insufficient observations
  na_row <- data.frame(
    n_months   = n,
    alpha_capm = NA_real_, t_capm = NA_real_,
    alpha_ff3  = NA_real_, t_ff3  = NA_real_,
    alpha_car  = NA_real_, t_car  = NA_real_,
    b_mkt = NA_real_, t_mkt = NA_real_,
    b_smb = NA_real_, t_smb = NA_real_,
    b_hml = NA_real_, t_hml = NA_real_,
    b_mom = NA_real_, t_mom = NA_real_,
    adj_r2 = NA_real_
  )
  
  if (n < MIN_OBS) return(na_row)
  
  y  <- d$excess
  X1 <- cbind(1, d$MKT_RF)
  f1 <- fast_ols(y, X1); if (is.null(f1)) return(na_row)
  s1 <- nw_se(X1, f1$e, NW_LAG)
  
  X3 <- cbind(1, d$MKT_RF, d$SMB, d$HML)
  f3 <- fast_ols(y, X3); if (is.null(f3)) return(na_row)
  s3 <- nw_se(X3, f3$e, NW_LAG)
  
  X4 <- cbind(1, d$MKT_RF, d$SMB, d$HML, d$MOM)
  f4 <- fast_ols(y, X4); if (is.null(f4)) return(na_row)
  s4 <- nw_se(X4, f4$e, NW_LAG)
  
  data.frame(
    n_months   = n,
    alpha_capm = f1$beta[1] * 12,  t_capm = f1$beta[1] / s1[1],
    alpha_ff3  = f3$beta[1] * 12,  t_ff3  = f3$beta[1] / s3[1],
    alpha_car  = f4$beta[1] * 12,  t_car  = f4$beta[1] / s4[1],
    b_mkt = f4$beta[2],  t_mkt = f4$beta[2] / s4[2],
    b_smb = f4$beta[3],  t_smb = f4$beta[3] / s4[3],
    b_hml = f4$beta[4],  t_hml = f4$beta[4] / s4[4],
    b_mom = f4$beta[5],  t_mom = f4$beta[5] / s4[5],
    adj_r2 = 1 - (1 - f4$r2) * (n - 1) / (n - 5)
  )
}

# =============================================================================
# 2. DATA PREPARATION
# =============================================================================
cat("=== 2. Data preparation ===\n")

factors_ts <- panel_incubation %>%
  distinct(date, MKT_RF, SMB, HML, MOM, RF) %>%
  filter(!is.na(MKT_RF)) %>%
  arrange(date)

port_base <- panel_incubation %>%
  filter(!excluded_perf) %>%      # v1.4: performance-comparison subsample
  filter(ap_group %in% c("Active", "Passive")) %>%
  arrange(Ticker, date) %>%
  group_by(Ticker) %>%
  mutate(
    size_sort = log(pmax(tna_lag, 1e-6)),
    # 11-month cumulative return, skip t-1 (Jegadeesh & Titman 1993)
    mom_raw   = rollapplyr(1 + ret_gross, width = 11,
                           FUN = prod, fill = NA, align = "right") - 1,
    mom_sort  = lag(mom_raw, 2)
  ) %>%
  ungroup() %>%
  mutate(fee_sort = suppressWarnings(as.numeric(Expense_Ratio)))

# Monthly within-group quintiles for SIZE and MOM
port_base <- port_base %>%
  group_by(date, ap_group) %>%
  mutate(q_size = safe_ntile(size_sort),
         q_mom  = safe_ntile(mom_sort)) %>%
  ungroup()

# Fixed within-group quintiles for FEE (static expense ratio).
# Use one canonical fee per fund (first non-NA value) to prevent
# many-to-many joins if Expense_Ratio varies across rows for the same fund.
fee_q <- port_base %>%
  group_by(Ticker, ap_group) %>%
  summarise(fee_sort = first(na.omit(fee_sort)), .groups = "drop") %>%
  filter(!is.na(fee_sort)) %>%
  group_by(ap_group) %>%
  mutate(q_fee = safe_ntile(fee_sort)) %>%
  ungroup() %>%
  select(Ticker, q_fee)

port_base <- left_join(port_base, fee_q, by = "Ticker")

cat("  Active:",  sum(port_base$ap_group == "Active",  na.rm = TRUE), "fund-months\n")
cat("  Passive:", sum(port_base$ap_group == "Passive", na.rm = TRUE), "fund-months\n")

# =============================================================================
# 3. PORTFOLIO RETURN CONSTRUCTION
# =============================================================================
cat("=== 3. Portfolio construction ===\n")

build_port <- function(data, q_col, sort_name) {
  data %>%
    filter(!is.na(.data[[q_col]]), !is.na(ret_gross)) %>%
    group_by(date, ap_group, quintile = .data[[q_col]]) %>%
    summarise(
      ret_ew_gross = mean(ret_gross,         na.rm = TRUE),
      ret_ew_net   = mean(ret_net,           na.rm = TRUE),
      ret_vw_gross = wm(ret_gross,           tna_lag),
      ret_vw_net   = wm(ret_net,             tna_lag),
      n_funds      = n(),
      mean_tna     = mean(tna_lag,           na.rm = TRUE),
      mean_er      = mean(fee_sort,          na.rm = TRUE),
      mean_turn    = mean(suppressWarnings(as.numeric(Turnover)), na.rm = TRUE),
      .groups      = "drop"
    ) %>%
    mutate(sort_type = sort_name)
}

port_size <- build_port(port_base, "q_size", "Size")
port_mom  <- build_port(port_base, "q_mom",  "Momentum")
port_fee  <- build_port(port_base, "q_fee",  "Fee")

port_agg <- port_base %>%
  filter(!is.na(ret_gross)) %>%
  group_by(date, ap_group) %>%
  summarise(
    ret_ew_gross = mean(ret_gross, na.rm = TRUE),
    ret_ew_net   = mean(ret_net,   na.rm = TRUE),
    ret_vw_gross = wm(ret_gross, tna_lag),
    ret_vw_net   = wm(ret_net,   tna_lag),
    n_funds      = n(),
    .groups      = "drop"
  ) %>%
  mutate(sort_type = "Aggregate", quintile = 0L)

# =============================================================================
# 4. REGRESSIONS
# =============================================================================
cat("=== 4. Factor model regressions ===\n")

# Build Q5 - Q1 spread return series for a given group/weighting
make_spread <- function(port_df, grp, ret_col) {
  d5 <- port_df %>% filter(ap_group == grp, quintile == 5L) %>%
    select(date, r5 = all_of(ret_col))
  d1 <- port_df %>% filter(ap_group == grp, quintile == 1L) %>%
    select(date, r1 = all_of(ret_col))
  inner_join(d5, d1, by = "date") %>%
    transmute(date, !!ret_col := r5 - r1)
}

regress_sort <- function(port_df, sort_name) {
  rows <- list()
  for (grp in c("Active", "Passive")) {
    qs <- sort(unique(port_df$quintile[port_df$ap_group == grp &
                                         !is.na(port_df$quintile)]))
    for (q in c(qs, 6L)) {
      for (wt in c("EW", "VW")) {
        rcol <- if (wt == "EW") "ret_ew_gross" else "ret_vw_gross"
        d    <- if (q == 6L) make_spread(port_df, grp, rcol) else
          port_df %>% filter(ap_group == grp, quintile == q)
        
        # subtract_rf=FALSE for long-short spreads: RF cancels in construction
        r <- tryCatch(
          run_models(rcol, d, factors_ts, subtract_rf = (q != 6L)),
          error = function(e) NULL
        )
        if (!is.null(r))
          rows[[paste(grp, q, wt)]] <-
          r %>% mutate(ap_group = grp, quintile = as.integer(q),
                       weighting = wt, sort_type = sort_name)
      }
    }
  }
  bind_rows(rows)
}

alpha_size <- regress_sort(port_size, "Size")
alpha_mom  <- regress_sort(port_mom,  "Momentum")
alpha_fee  <- regress_sort(port_fee,  "Fee")

# Aggregate regressions (gross + net) for Table D2
regress_agg_one <- function(grp, wt) {
  d     <- port_agg %>% filter(ap_group == grp)
  g_col <- if (wt == "EW") "ret_ew_gross" else "ret_vw_gross"
  n_col <- if (wt == "EW") "ret_ew_net"   else "ret_vw_net"
  rg    <- run_models(g_col, d, factors_ts)
  rn    <- run_models(n_col, d, factors_ts)
  rg %>% mutate(alpha_car_net = rn$alpha_car, t_car_net = rn$t_car,
                ap_group = grp, weighting = wt,
                sort_type = "Aggregate", quintile = 0L)
}

alpha_agg <- bind_rows(
  regress_agg_one("Active",  "EW"),
  regress_agg_one("Active",  "VW"),
  regress_agg_one("Passive", "EW"),
  regress_agg_one("Passive", "VW")
)

# =============================================================================
# 5. EXCEL EXPORT
# =============================================================================
cat("=== 5. Exporting to Excel ===\n")

write_xlsx(
  list(all      = bind_rows(port_size, port_mom, port_fee, port_agg),
       size     = port_size, momentum = port_mom,
       fee      = port_fee,  aggregate = port_agg),
  "portfolio_returns.xlsx"
)
write_xlsx(
  list(all      = bind_rows(alpha_size, alpha_mom, alpha_fee, alpha_agg),
       size     = alpha_size, momentum = alpha_mom,
       fee      = alpha_fee,  aggregate = alpha_agg),
  "portfolio_alphas.xlsx"
)
cat("Written: portfolio_returns.xlsx\nWritten: portfolio_alphas.xlsx\n")

# =============================================================================
# 6. TABLE D1: PORTFOLIO CHARACTERISTICS (8 columns, longtable)
# =============================================================================
cat("=== 6. Table D1 ===\n")

# Compute annualised Sharpe on the EW gross return time series of each
# (sort_type, ap_group, quintile) bucket. Excess return uses the monthly RF
# from the merged Fama-French factors. MIN_OBS reused as the minimum sample
# requirement to avoid reporting Sharpe ratios on near-empty panels.
sharpe_ann <- function(r_ew_gross, rf) {
  ex <- r_ew_gross - rf
  v  <- !is.na(ex)
  if (sum(v) < MIN_OBS) return(NA_real_)
  sd_ex <- sd(ex[v])
  if (is.na(sd_ex) || sd_ex == 0) return(NA_real_)
  sqrt(12) * mean(ex[v]) / sd_ex
}

# Build a (sort_type, ap_group, quintile) -> EW gross Sharpe lookup.
# port_size/_mom/_fee are date-level frames with ret_ew_gross; inner-join to
# factors_ts for RF, then collapse per bucket.
compute_sharpe_lookup <- function(port_df) {
  port_df %>%
    inner_join(factors_ts %>% select(date, RF), by = "date") %>%
    filter(!is.na(quintile)) %>%
    group_by(sort_type, ap_group, quintile) %>%
    summarise(ew_sharpe = sharpe_ann(ret_ew_gross, RF), .groups = "drop")
}

sharpe_lu <- bind_rows(
  compute_sharpe_lookup(port_size),
  compute_sharpe_lookup(port_mom),
  compute_sharpe_lookup(port_fee)
)

make_char_panel <- function(port_df, grp) {
  sort_nm <- unique(port_df$sort_type)
  port_df %>%
    filter(ap_group == grp, !is.na(quintile)) %>%
    group_by(quintile) %>%
    summarise(
      N_Avg    = fmt(mean(n_funds,            na.rm = TRUE), 0),
      Mean_TNA = fmt(mean(mean_tna,           na.rm = TRUE), 1),
      Mean_ER  = fmt(mean(mean_er,            na.rm = TRUE), 2),
      Mean_Turn= fmt(mean(mean_turn,          na.rm = TRUE), 1),
      EW_Gross = fmt(mean(ret_ew_gross * 100, na.rm = TRUE)),
      VW_Gross = fmt(mean(ret_vw_gross * 100, na.rm = TRUE)),
      .groups  = "drop"
    ) %>%
    # Join Sharpe values from pre-computed lookup (keyed by sort/group/quintile)
    left_join(
      sharpe_lu %>%
        filter(sort_type == sort_nm, ap_group == grp) %>%
        select(quintile, ew_sharpe),
      by = "quintile"
    ) %>%
    mutate(Q = paste0("Q", quintile),
           EW_Sharpe = fmt(ew_sharpe, 2)) %>%
    select(Q, N_Avg, Mean_TNA, Mean_ER, Mean_Turn, EW_Gross, VW_Gross, EW_Sharpe)
}

D1_NCOLS <- 8L

d1_panels <- list(
  list(df = port_size, grp = "Active",  label = "Panel A: Size --- Active"),
  list(df = port_size, grp = "Passive", label = "Panel B: Size --- Passive"),
  list(df = port_mom,  grp = "Active",  label = "Panel C: Momentum --- Active"),
  list(df = port_mom,  grp = "Passive", label = "Panel D: Momentum --- Passive"),
  list(df = port_fee,  grp = "Active",  label = "Panel E: Fee --- Active"),
  list(df = port_fee,  grp = "Passive", label = "Panel F: Fee --- Passive")
)

d1_data <- bind_rows(lapply(d1_panels, function(p)
  make_char_panel(p$df, p$grp) %>% mutate(panel_label = p$label)))

rows_pp  <- N_QUINT
pack_tab <- data.frame(
  label = vapply(d1_panels, `[[`, character(1), "label"),
  start = (seq_along(d1_panels) - 1) * rows_pp + 1,
  end   = seq_along(d1_panels) * rows_pp,
  stringsAsFactors = FALSE
)

fn_d1 <- paste(
  "Quintile portfolios sorted monthly within group on $\\log(\\text{TNA}_{t-1})$",
  "(Size), cumulative 11-month gross return ending $t-2$ skipping $t-1$",
  "(Momentum; \\citealt{JegadeeshTitman1993}), and static annual Expense Ratio (Fee).",
  "EW: equal-weighted; VW: lagged-TNA-weighted. Returns in monthly \\%.",
  "EW Sharpe is the annualised Sharpe ratio of the quintile portfolio's",
  # Single \\ in the R string → single \ in the .tex file → correct LaTeX command.
  # Previously \\\\textit produced \\textit in the file, which LaTeX reads as
  # a line-break (\\) followed by literal text "textit{...}" — visible artefact.
  "equal-weighted \\textit{gross} excess return,",
  "$\\sqrt{12}\\cdot\\overline{r^{\\text{EW,g}}_{q,t}-r^{f}_{t}}/\\sigma(r^{\\text{EW,g}}_{q,t}-r^{f}_{t})$.",
  "Sample: Incubation-corrected panel (Evans 2010), no date cap;",
  "performance-comparison subsample per flagged\\_funds.xlsx."
)

lt_d1 <- d1_data %>%
  select(-panel_label) %>%
  kbl(
    format     = "latex",
    booktabs   = TRUE,
    linesep    = "",
    escape     = FALSE,
    longtable  = TRUE,
    row.names  = FALSE,          # prevent bind_rows() row-key leakage
    caption    = "Portfolio Characteristics by Sort Variable and Quintile",
    label      = "port_characteristics",
    col.names  = c("$Q$", "$N_{\\text{avg}}$", "TNA", "ER", "Turn.",
                   "EW Gross", "VW Gross", "EW Sharpe"),
    align      = c("l", "r", "r", "r", "r", "r", "r", "r")
  ) %>%
  kable_styling(latex_options = c("hold_position", "repeat_header"))

for (i in seq_len(nrow(pack_tab)))
  lt_d1 <- lt_d1 %>%
  pack_rows(pack_tab$label[i], pack_tab$start[i], pack_tab$end[i],
            bold = FALSE, italic = FALSE,
            hline_before = (i > 1), hline_after = FALSE)

lt_d1_str <- as.character(lt_d1)
lt_d1_str <- longtable_note(lt_d1_str, fn_d1, D1_NCOLS)
lt_d1_str <- wrap_lt_small(lt_d1_str)
writeLines(lt_d1_str, "table_port_chars.tex")
cat("Written: table_port_chars.tex\n")

# =============================================================================
# 7. TABLE D2: ACTIVE VS PASSIVE AGGREGATE ALPHA  (Table 13)
# =============================================================================
cat("=== 7. Table D2 ===\n")

d2_scaled <- alpha_agg %>%
  mutate(across(c(alpha_capm, alpha_ff3, alpha_car, alpha_car_net), ~ .x * 100))

# Build interleaved coefficient / t-stat rows.
# Stars appended to coefficient cells using add_stars().
make_d2_rows <- function(grp) {
  out <- list()
  for (wt in c("EW", "VW")) {
    r     <- d2_scaled %>% filter(ap_group == grp, weighting == wt)
    label <- paste0(grp, " (", wt, ")")
    if (nrow(r) == 0) next
    out[[paste0(label, "_c")]] <- data.frame(
      Portfolio = label,
      a_capm = add_stars(fmt(r$alpha_capm), r$t_capm),
      a_ff3  = add_stars(fmt(r$alpha_ff3),  r$t_ff3),
      a_car  = add_stars(fmt(r$alpha_car),  r$t_car),
      stringsAsFactors = FALSE
    )
    out[[paste0(label, "_t")]] <- data.frame(
      Portfolio = "t(coef)",
      a_capm = paste0("(", fmt(r$t_capm, 2), ")"),
      a_ff3  = paste0("(", fmt(r$t_ff3,  2), ")"),
      a_car  = paste0("(", fmt(r$t_car,  2), ")"),
      stringsAsFactors = FALSE
    )
  }
  df <- do.call(rbind, out)
  rownames(df) <- NULL
  df
}

d2_table <- bind_rows(make_d2_rows("Active"), make_d2_rows("Passive"))

fn_d2 <- paste(
  "Monthly EW and VW aggregate portfolio returns regressed on CAPM,",
  "Fama-French three-factor, and \\\\textcite{Carhart1997} four-factor models.",
  "Alphas annualised ($\\\\times 12$) and expressed as \\\\%.",
  "Newey-West $t$-statistics (6-month lag) in parentheses.",
  "$^{*}$, $^{**}$, $^{***}$: significant at 10\\\\%, 5\\\\%, 1\\\\% respectively.",
  "Sample: Incubation-corrected panel (Evans 2010), no date cap;",
  "performance-comparison subsample per flagged\\\\_funds.xlsx."
)

# linesep="" suppresses kableExtra's default \addlinespace[0.3em] after every
# 5th row, which would otherwise fall between rows 5 and 6 (Passive EW coef
# and its t-stat) in this 8-row table.
latex_d2 <- d2_table %>%
  kbl(
    format    = "latex",
    booktabs  = TRUE,
    linesep   = "",
    escape    = FALSE,
    caption   = "Active vs.\\ Passive Aggregate Portfolio Alpha (\\%, Annualised)",
    label     = "port_agg_alpha",
    col.names = c("Portfolio",
                  "$\\alpha_{\\text{CAPM}}$",
                  "$\\alpha_{\\text{FF3}}$",
                  "$\\alpha_{\\text{Car}}$"),
    align     = c("l", "r", "r", "r")
  ) %>%
  kable_styling(latex_options = "hold_position") %>%
  footnote(
    general        = fn_d2,
    general_title  = "",
    escape         = FALSE,
    threeparttable = TRUE
  )

d2_str <- as.character(latex_d2)
write_tex(d2_str, "table_port_agg_alpha.tex",
          resize = FALSE, small = FALSE)

# =============================================================================
# 8-10. TABLES D3-D5: QUINTILE ALPHA TABLES (longtable, 9 columns)
#       Now includes t-statistics for all beta coefficients and significance
#       stars on all estimated parameters (alphas and factor loadings).
# =============================================================================
cat("=== 8-10. Tables D3-D5 ===\n")

q_labels <- list(
  Size     = c("Q1 (Small)",    "Q2", "Q3", "Q4", "Q5 (Large)",   "Q5$-$Q1"),
  Momentum = c("Q1 (Losers)",   "Q2", "Q3", "Q4", "Q5 (Winners)", "Q5$-$Q1"),
  Fee      = c("Q1 (Low Fee)",  "Q2", "Q3", "Q4", "Q5 (High Fee)","Q5$-$Q1")
)

D35_NCOLS <- 9L

# Build one 12-row block (6 quintiles x [coef row + t-stat row]).
# Coefficient row: all values carry significance stars via add_stars().
# T-statistic row: alpha AND beta t-stats reported; adj-R2 cell left blank.
build_block <- function(alpha_df, grp, wt, sort_name) {
  qs     <- c(1:5, 6L)
  labels <- q_labels[[sort_name]]
  rows   <- list()
  
  for (i in seq_along(qs)) {
    q <- qs[i]
    r <- alpha_df %>% filter(ap_group == grp, quintile == q, weighting == wt)
    
    if (nrow(r) == 0) {
      rows[[paste0("c", i)]] <- data.frame(
        Q   = labels[i], a_c = "--", a_f = "--", a_r = "--",
        b_m = "--", b_s = "--", b_h = "--", b_o = "--", ar2 = "--",
        stringsAsFactors = FALSE)
      rows[[paste0("t", i)]] <- data.frame(
        Q = "t(coef)", a_c = "", a_f = "", a_r = "",
        b_m = "", b_s = "", b_h = "", b_o = "", ar2 = "",
        stringsAsFactors = FALSE)
      next
    }
    
    # Coefficient row ??? stars appended to all estimated values
    rows[[paste0("c", i)]] <- data.frame(
      Q   = labels[i],
      a_c = add_stars(fmt(r$alpha_capm * 100), r$t_capm),
      a_f = add_stars(fmt(r$alpha_ff3  * 100), r$t_ff3),
      a_r = add_stars(fmt(r$alpha_car  * 100), r$t_car),
      b_m = add_stars(fmt(r$b_mkt),            r$t_mkt),
      b_s = add_stars(fmt(r$b_smb),            r$t_smb),
      b_h = add_stars(fmt(r$b_hml),            r$t_hml),
      b_o = add_stars(fmt(r$b_mom),            r$t_mom),
      ar2 = fmt(r$adj_r2),
      stringsAsFactors = FALSE
    )
    # T-statistic row ??? all coefficient t-stats; adj-R2 cell blank
    rows[[paste0("t", i)]] <- data.frame(
      Q   = "t(coef)",
      a_c = paste0("(", fmt(r$t_capm, 2), ")"),
      a_f = paste0("(", fmt(r$t_ff3,  2), ")"),
      a_r = paste0("(", fmt(r$t_car,  2), ")"),
      b_m = paste0("(", fmt(r$t_mkt,  2), ")"),
      b_s = paste0("(", fmt(r$t_smb,  2), ")"),
      b_h = paste0("(", fmt(r$t_hml,  2), ")"),
      b_o = paste0("(", fmt(r$t_mom,  2), ")"),
      ar2 = "",
      stringsAsFactors = FALSE
    )
  }
  
  tbl <- do.call(rbind, rows)
  rownames(tbl) <- NULL
  list(data  = tbl,
       label = paste0(grp, " --- ", wt))
}

build_alpha_table <- function(alpha_df, sort_name, cap, lab, fn_text) {
  panels <- list()
  for (grp in c("Active", "Passive"))
    for (wt in c("EW", "VW"))
      panels[[paste(grp, wt)]] <- build_block(alpha_df, grp, wt, sort_name)
  
  full_data  <- bind_rows(lapply(panels, `[[`, "data"))
  rows_pp    <- 12L  # 6 quintiles ?? (coef + t-stat)
  
  pack_tab2 <- data.frame(
    label = vapply(panels, `[[`, character(1), "label"),
    start = (seq_along(panels) - 1) * rows_pp + 1,
    end   = seq_along(panels) * rows_pp,
    stringsAsFactors = FALSE
  )
  
  k <- full_data %>%
    kbl(
      format    = "latex",
      booktabs  = TRUE,
      linesep   = "",
      escape    = FALSE,
      longtable = TRUE,
      caption   = cap,
      label     = lab,
      col.names = c("Quintile",
                    "$\\alpha_{\\text{CAPM}}$",
                    "$\\alpha_{\\text{FF3}}$",
                    "$\\alpha_{\\text{Car}}$",
                    "$\\beta_{\\text{MKT}}$",
                    "$\\beta_{\\text{SMB}}$",
                    "$\\beta_{\\text{HML}}$",
                    "$\\beta_{\\text{MOM}}$",
                    "$\\bar{R}^2$"),
      align     = c("l", "r", "r", "r", "r", "r", "r", "r", "r")
    ) %>%
    kable_styling(latex_options = c("hold_position", "repeat_header"))
  
  for (i in seq_len(nrow(pack_tab2)))
    k <- k %>%
    pack_rows(pack_tab2$label[i], pack_tab2$start[i], pack_tab2$end[i],
              bold = FALSE, italic = FALSE,
              hline_before = (i > 1), hline_after = FALSE)
  
  s <- as.character(k)
  s <- longtable_note(s, fn_text, D35_NCOLS)
  s <- wrap_lt_small(s, tabcolsep = "2pt")
  s
}

# Shared footnote: updated to reflect (a) beta t-stats now shown,
# (b) RF omission for spreads, (c) significance stars.
fn_base <- paste(
  "\\textcite{Carhart1997} four-factor time-series regressions on monthly portfolio returns.",
  "Alphas annualised ($\\times 12$, \\%). Q5$-$Q1: long-short spread; the risk-free",
  "rate is omitted from the spread return because it cancels in the self-financing",
  "construction: $(r_5 - R_f) - (r_1 - R_f) = r_5 - r_1$.",
  "EW: equal-weighted; VW: lagged-TNA-weighted.",
  "Newey-West $t$-statistics (6-month lag) in parentheses below each coefficient.",
  "$^{*}$, $^{**}$, $^{***}$: significant at 10\\%, 5\\%, 1\\% respectively.",
  "Sample: Incubation-corrected panel (Evans 2010), no date cap;",
  "performance-comparison subsample per flagged\\_funds.xlsx."
)

fn_d3 <- paste(fn_base,
               "Size sorted monthly on $\\log(\\text{TNA}_{t-1})$; Q1 = smallest.")

fn_d4 <- paste(fn_base,
               "Momentum sorted monthly on cumulative gross return over $t-12$ to $t-2$,",
               "skipping $t-1$ (\\citealt{JegadeeshTitman1993}); Q1 = past losers.")

fn_d5 <- paste(fn_base,
               "Fee sorted once on static annual Expense Ratio; Q1 = lowest fee.",
               "Leveraged and derivative-based passive funds excluded prior to sorting",
               "(see Cleaning Step 6 in the data construction notes).")

s_d3 <- build_alpha_table(alpha_size, "Size",
                          "Alpha and Factor Loadings by Size Quintile (\\%, Annualised)",
                          "port_size_alpha", fn_d3)

s_d4 <- build_alpha_table(alpha_mom, "Momentum",
                          "Alpha and Factor Loadings by Momentum Quintile (\\%, Annualised)",
                          "port_mom_alpha", fn_d4)

s_d5 <- build_alpha_table(alpha_fee, "Fee",
                          "Alpha and Factor Loadings by Fee Quintile (\\%, Annualised)",
                          "port_fee_alpha", fn_d5)

writeLines(s_d3, "table_port_size_alpha.tex")
writeLines(s_d4, "table_port_mom_alpha.tex")
writeLines(s_d5, "table_port_fee_alpha.tex")
cat("Written: table_port_size_alpha.tex\n")
cat("Written: table_port_mom_alpha.tex\n")
cat("Written: table_port_fee_alpha.tex\n")

cat("\n[SUCCESS] portfolio_sorts.R v1.4 complete.\n")