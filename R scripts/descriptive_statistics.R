# =============================================================================
# DESCRIPTIVE STATISTICS - LATEX TABLE OUTPUT (v2.5 - PASSIVE_INDEX retirement)
#
# v2.5 changes vs v2.4 (filter-methodology revision, May 2026):
#   No code change. flagged_funds.xlsx ledger updated upstream: PASSIVE_INDEX
#   flag retired from "Exclude from Entire Analysis" (313 funds) and "Exclude
#   from Perf Comparison" (293 funds). The post-Step-8c panel now carries
#   ~290 passive funds (vs ~28-39 under v1.2), so the Active vs Passive rows
#   in Tables 2-4, the return-distribution rows in Table 3, and the
#   fund-flows series in Figure 1 now describe the full US domestic passive
#   index-fund universe rather than a small residual cohort. Caption text
#   and footnotes are unchanged; the methodology section (Chapter 3) is the
#   single source of truth for the exclusion framework. See v2.3 note below
#   for the legacy state.
#
# v2.4 changes vs v2.3 (Family C audit follow-on):
#   Table 2 footnote BSW citation corrected. The net-return-derivation
#   convention (gross - ER/12) originates in Carhart (1997) and Wermers
#   (2000), used throughout the literature including Pastor and Stambaugh
#   (2002) and BSW (2010). BSW (2010) observe net returns from CRSP and
#   derive gross by adding back ER/12; this pipeline observes gross only and
#   derives net by subtracting ER/12. Same arithmetic, opposite direction.
#   Citation now points to Carhart (1997) and Wermers (2000), the convention
#   originators.
#
# v2.3 changes vs v2.2:
#   - Figure 1 (fig_fund_flows): in-figure title, subtitle, and source caption
#     removed. Only the y-axis label and the Active/Passive legend remain
#     embedded in the PNG/PDF. Title and source description belong in the
#     LaTeX caption so they are searchable, editable, and not rasterised.
#   - Compatibility with data_import_and_cleaning.R v1.2: panels now carry
#     excluded_perf and excluded_h3 boolean columns from Step 8c. This script
#     does not need to filter them - it describes the analysis universe as
#     produced by the cleaning pipeline.
#     [Legacy v2.3 note, SUPERSEDED by v2.5: under the v1.2 ledger, Tables
#     2-4 showed a near-zero Passive count (~28-39) because the
#     PASSIVE_INDEX flag dropped the bulk of passives at source. As of v2.5
#     PASSIVE_INDEX has been retired from flagged_funds.xlsx; the passive
#     cohort now appears at its full ~290-fund size in this script's
#     outputs. The Table 1 pre-8c snapshot remains the full universe and
#     is unaffected by either ledger revision.]
#
# v2.2 changes vs v2.1:
#   (1) Added annualised gross Sharpe ratio (fund-level time-series Sharpe,
#       summarised cross-sectionally) to Tables 2, A.1 and A.2. Sharpe is
#       computed in fund_means() as sqrt(12)*mean(ret_gross - RF)/sd(ret_gross - RF),
#       on gross (pre-fee) returns, requiring at least 24 monthly excess-return
#       observations. "Gross Sharpe (ann.)" row added to build_t2(). Footnote
#       fn_t2_base extended accordingly.
#   (2) Panel switch: Table 2 (main text) now reports the Incubation-Corrected
#       panel (previously Trimmed 1995--2023). Table A.2 (appendix) now reports
#       the Trimmed panel for reference. Table A.1 (Master) is unchanged.
#       This removes the artificial 2023 date cap from the primary descriptive
#       statistics and extends coverage through February 2026 while retaining
#       the Evans (2010) incubation correction.
#   - fund_means(): gains sharpe_g column (requires RF in panel).
#   - build_t2(): vars/var_labels extended to include Sharpe.
#   - Table 2 / Table A.2 caption text updated; routing in latex_t2 /
#     latex_t2_incub swapped; fn_t2 is now fn_t2_incub (base + Evans note);
#     fn_t2_trimmed is the new base+trimmed footnote for Table A.2.
#
# v2.1 change vs v2.0:
#   Table 2 (all three variants: Trimmed, Master, Incubation-Corrected) drops
#   the VW Mean column. The static mean-TNA weight used in v2.0 is inconsistent
#   with the FF (2010) lagged-TNA convention now adopted throughout the alpha
#   tables and is not a natural object to apply to fund-level summary
#   statistics. Table 3 (fund-month distribution of gross returns) retains its
#   VW Mean column because it already uses the correct contemporaneous lagged-
#   TNA weight on fund-month observations.
#   - fund_means(): mean_tna_raw column removed.
#   - build_t2(): VW_Mean column and wt variable removed.
#   - fn_t2_base: VW Mean sentences removed from footnote.
#   - make_t2_kbl(): col.names and align updated (one fewer column).
#
# v2.0 changes (deprecated in v2.1):
# Produces six .tex files (table fragments, no preamble):
#   table_fund_counts.tex        ??? Table 1: fund sample composition
#   table_desc_stats.tex         ??? Table 2: cross-sectional summary stats (main)
#   table_desc_stats_master.tex  ??? Table 2: master panel (appendix)
#   table_desc_stats_incub.tex   ??? Table 2: incubation-corrected panel (appendix)
#   table_return_dist.tex        ??? Table 3: return distribution by group
#   table_coverage.tex           ??? Table 4: time series coverage
#
# v2.0 changes vs v1.x:
#   - Tables 2 and 3 each gain a "VW Mean" column (lagged-TNA-weighted mean)
#     alongside the existing equal-weighted Mean.
#   - fund_means() gains mean_tna_raw (used as Table 2 VW weight).
#   - wm() helper added (shared with portfolio_sorts.R convention).
#
# In Overleaf use: \input{table_fund_counts} etc.
# Required preamble: \usepackage{booktabs, threeparttable, multirow, array, graphicx}
#
# Assumes:
#   - data_import_and_cleaning.R has been run (ap_group, ret_gross, ret_net present)
#   - flow_calculation.R has been run (flow_calc_pct_win, tna present)
#
# Produces two figure files:
#   fig_fund_flows.pdf  ??? LaTeX-ready vector figure
#   fig_fund_flows.png  ??? 300 dpi raster fallback
#
# Dependencies: dplyr, tidyr, purrr, knitr, kableExtra, ggplot2, zoo, scales,
#               lubridate, stringr, e1071, slider
# Install (run once):
#   install.packages(c("knitr", "kableExtra", "purrr", "ggplot2", "zoo",
#                      "scales", "lubridate", "stringr", "e1071", "slider"))
# =============================================================================

library(dplyr)
library(tidyr)
library(purrr)
library(knitr)
library(kableExtra)
library(ggplot2)
library(zoo)
library(scales)
library(lubridate)
library(stringr)
library(e1071)    # for kurtosis()
library(slider)   # for slide_index_dbl() centred rolling mean



# =============================================================================
# 1. HELPERS
# =============================================================================

pct <- function(x, p) quantile(x, probs = p, na.rm = TRUE)

# TNA-weighted mean ??? shared by Tables 2 and 3
wm <- function(x, w) {
  v <- !is.na(x) & !is.na(w) & w > 0
  if (sum(v) == 0L) return(NA_real_)
  sum(x[v] * w[v]) / sum(w[v])
}

fmt <- function(x, digits = 3) {
  ifelse(is.na(x), "--",
         formatC(round(x, digits), format = "f", digits = digits))
}
fmt2 <- function(x) {
  ifelse(is.na(x), "--",
         formatC(round(x, 0), format = "d", big.mark = ","))
}

# Clean kableExtra LaTeX output
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

write_tex <- function(latex_str, filename, resize = TRUE, small = FALSE) {
  writeLines(clean_latex(latex_str, resize = resize, small = small), filename)
  cat("Written:", filename, "\n")
}

# =============================================================================
# 2. TABLE 1: FUND COUNTS BY PANEL AND GROUP
# =============================================================================

# Guard: Table 1 (Fund Counts) reports the UNIVERSE composition and
# must use the pre-Step-8c panel snapshots. All other tables in this
# script use the post-8c (analytical-sample) panels.
stopifnot(
  "panel_master_pre8c missing — re-run data_import_and_cleaning.R v1.3+"     = exists("panel_master_pre8c"),
  "panel_incubation_pre8c missing — re-run data_import_and_cleaning.R v1.3+" = exists("panel_incubation_pre8c"),
  "panel_trimmed_pre8c missing — re-run data_import_and_cleaning.R v1.3+"    = exists("panel_trimmed_pre8c")
)

fund_counts <- function(panel, label) {
  panel %>%
    group_by(Ticker, ap_group) %>%
    slice(1) %>%
    ungroup() %>%
    count(ap_group) %>%
    mutate(Panel = label)
}

counts_raw <- bind_rows(
  fund_counts(panel_master_pre8c,     "Master"),
  fund_counts(panel_incubation_pre8c, "Incubation-Corrected"),
  fund_counts(panel_trimmed_pre8c,    "Trimmed (1995--2023)")
) %>%
  pivot_wider(names_from = ap_group, values_from = n, values_fill = 0) %>%
  mutate(Total = Active + Passive + Unknown) %>%
  select(Panel, Active, Passive, Unknown, Total)

latex_t1 <- counts_raw %>%
  kbl(
    format    = "latex",
    booktabs  = TRUE,
    caption   = "Fund Sample Composition by Panel",
    label     = "fund_counts",
    col.names = c("Panel", "Active", "Passive", "Unknown", "Total"),
    align     = c("l", "r", "r", "r", "r"),
    format.args = list(big.mark = ",")        # SBE Part 7 (p.36): commas for thousands
  ) %>%
  kable_styling(latex_options = "hold_position") %>%
  footnote(
    general = paste(
      "Active and Passive classifications use the \\\\textit{Actively Managed}",
      "field from LSEG static data. Unknown funds are excluded from all",
      "regression analyses. The Incubation-Corrected panel removes the first",
      "36 months of each fund's return history following \\\\textcite{Evans2010}.",
      "The Trimmed panel additionally restricts the sample to 1995--2023.",
      "Counts reflect the cleaned data universe before analytical-scope",
      "exclusions documented in Section~\\\\ref{sec:panel_construction}",
      "(long/short, market-neutral, bear-market or inverse strategies,",
      "non-U.S.\\\\ mandate, and confirmed data-error funds tagged in",
      "flagged\\\\_funds.xlsx as Entire-Analysis exclusions): 110, 84,",
      "and 80 share classes are dropped from the Master,",
      "Incubation-Corrected, and Trimmed panels respectively, yielding",
      "analytical samples of 3,581 / 3,138 / 3,046. Per-hypothesis sample",
      "sizes (further restricted by the Perf-Comparison and H3 flag columns)",
      "are reported in each subsequent table."
    ),
    general_title  = "",
    escape         = FALSE,
    threeparttable = TRUE
  )
write_tex(latex_t1, "table_fund_counts.tex", resize = FALSE)

# =============================================================================
# 3. TABLE 2: CROSS-SECTIONAL SUMMARY STATISTICS
#    Fund-level time-series means -> cross-sectional statistics across funds
# =============================================================================
fund_means <- function(panel) {
  # Guard: tna is created by flow_calculation.R. If that script has not been
  # run in the current session, reconstruct tna from raw asset columns so
  # descriptive_statistics.R can run standalone for diagnostics.
  if (!"tna" %in% names(panel)) {
    warning("'tna' column not found ??? reconstructing from class_assets/total_assets. ",
            "Run flow_calculation.R for the canonical tna definition.")
    panel <- panel %>%
      mutate(tna = if_else(!is.na(class_assets) & class_assets > 0,
                           class_assets, total_assets))
  }
  if (!"flow_calc_pct_win" %in% names(panel)) {
    warning("'flow_calc_pct_win' not found ??? flow column will be NA. ",
            "Run flow_calculation.R to populate fund flow variables.")
    panel <- panel %>% mutate(flow_calc_pct_win = NA_real_)
  }
  # RF is merged from factors sheet by data_import_and_cleaning.R. Guard so the
  # script still runs if factors have not been merged; Sharpe column becomes NA.
  if (!"RF" %in% names(panel)) {
    warning("'RF' column not found ??? Sharpe will be NA. ",
            "Merge Fama-French factors (including RF) in data_import_and_cleaning.R.")
    panel <- panel %>% mutate(RF = NA_real_)
  }
  panel %>%
    group_by(Ticker, ap_group) %>%
    summarise(
      gross_ret  = mean(ret_gross * 100,          na.rm = TRUE),
      net_ret    = mean(ret_net   * 100,          na.rm = TRUE),
      # Filter strictly for positive TNA before logging to prevent -Inf
      log_tna    = mean(log(tna[tna > 0]),        na.rm = TRUE),
      flow       = mean(flow_calc_pct_win * 100,  na.rm = TRUE),
      expense_r  = first(na.omit(as.numeric(Expense_Ratio))),
      turnover_r = first(na.omit(as.numeric(Turnover))),
      n_months   = n(),
      # Annualised gross Sharpe: sqrt(12) * mean(ex) / sd(ex). Pre-fee, matches
      # gross alpha framework. Minimum 24 monthly excess-return observations
      # required; otherwise NA (avoids spurious SR from noise on short series).
      sharpe_g   = {
        ex <- ret_gross - RF
        v  <- !is.na(ex)
        if (sum(v) < 24L) NA_real_
        else {
          sd_ex <- sd(ex[v])
          if (is.na(sd_ex) || sd_ex == 0) NA_real_
          else sqrt(12) * mean(ex[v]) / sd_ex
        }
      },
      .groups    = "drop"
    )
}

var_stats <- function(x) {
  c(
    N      = sum(!is.na(x)),
    Mean   = mean(x,   na.rm = TRUE),
    Median = median(x, na.rm = TRUE),
    SD     = sd(x,     na.rm = TRUE),
    P10    = unname(pct(x, 0.10)),
    P90    = unname(pct(x, 0.90))
  )
}

build_t2 <- function(panel, panel_label) {
  fm <- fund_means(panel)
  # Winsorise turnover cross-sectionally at 1/99 pct before computing stats.
  # Raw turnover is a point-in-time static field and contains extreme outliers
  # (ETF creation/redemption events, LSEG classification contamination) that
  # inflate the mean and SD without reflecting genuine active trading activity.
  fm <- fm %>% mutate(turnover_r = winsorise(turnover_r))
  vars <- c("gross_ret", "net_ret", "log_tna",
            "flow", "expense_r", "turnover_r", "n_months", "sharpe_g")
  var_labels <- c(
    "Gross Return (\\%, monthly)",
    "Net Return (\\%, monthly)",
    "Log TNA (USD millions)",
    "Proportional Flow (\\% of TNA, monthly)",
    "Expense Ratio (\\%)",
    "Turnover (\\%)",
    "Months of Coverage",
    "Gross Sharpe Ratio (annualised)"
  )
  groups <- c("Active", "Passive", "Full")
  
  map_dfr(seq_along(vars), function(vi) {
    v <- vars[vi]
    map_dfr(groups, function(g) {
      x  <- if (g == "Full") fm[[v]] else fm[[v]][fm$ap_group == g]
      s  <- var_stats(x)
      tibble(
        Variable = var_labels[vi],
        Group    = g,
        N        = fmt2(s["N"]),
        Mean     = fmt(s["Mean"]),
        Median   = fmt(s["Median"]),
        SD       = fmt(s["SD"]),
        P10      = fmt(s["P10"]),
        P90      = fmt(s["P90"])
      )
    })
  }) %>%
    mutate(Panel = panel_label)
}

t2_all <- bind_rows(
  build_t2(panel_master,     "Master"),
  build_t2(panel_incubation, "Incubation-Corrected"),
  build_t2(panel_trimmed,    "Trimmed (1995--2023)")
)

# Shared base footnote for all three Table 2 variants
fn_t2_base <- paste(
  "Fund-level time-series means are computed first; cross-sectional statistics",
  "are then computed across fund-level means. $N$ is the number of funds with at",
  "least one valid observation.",
  "Expense Ratio and Turnover are point-in-time LSEG static values; Turnover is",
  "winsorised at the 1st/99th percentiles cross-sectionally.",
  "Flow is the \\\\textcite{SirriTufano1998} measure scaled by lagged TNA (\\\\%),",
  "winsorised at 1st/99th percentiles, with December excluded.",
  "Net returns approximate gross returns less one-twelfth of the static annual expense ratio each month, following \\\\textcite{Carhart1997} and \\\\textcite{Wermers2000}.",
  "Returns and flows are in \\\\%.",
  "Gross Sharpe ratio is the fund-level annualised Sharpe on excess gross returns,",
  "$\\\\sqrt{12}\\\\cdot\\\\overline{r^{g}_{i,t}-r^{f}_{t}}/\\\\sigma(r^{g}_{i,t}-r^{f}_{t})$,",
  "computed only for funds with at least 24 monthly excess-return observations."
)

# Table 2 (Incubation-Corrected, MAIN TEXT): base + Evans note
fn_t2_incub <- paste(
  fn_t2_base,
  "\\\\textcite{Evans2010} correction removes the first 36 months of each fund's history.",
  "Mean returns are higher than in the Master panel because the 36-month minimum",
  "disproportionately eliminates short-lived, poorly performing funds \\\\parencite{Evans2010}.",
  "No date trimming applied; sample runs through February 2026."
)

# Table 2 routing uses the Incubation-Corrected footnote
fn_t2 <- fn_t2_incub

# Table A.1 (Master): base + unfiltered note
fn_t2_master <- paste(
  fn_t2_base,
  "No incubation correction or date trimming applied; full cleaned sample",
  "December 1994--February 2026."
)

# Table A.2 (Trimmed 1995-2023): base + trimmed note
fn_t2_trimmed <- paste(
  fn_t2_base,
  "\\\\textcite{Evans2010} 36-month incubation correction applied and sample restricted",
  "to 1995--2023. This is the panel used for panel-level regressions (H1--H4) where",
  "alignment with factor and sentiment series availability motivates the date cap."
)

make_t2_kbl <- function(data, cap, lab, add_footnote = FALSE, fn_text = fn_t2) {
  k <- data %>%
    kbl(
      format    = "latex",
      booktabs  = TRUE,
      caption   = cap,
      label     = lab,
      col.names = c("Variable", "Group", "$N$", "Mean",
                    "Median", "SD", "$P_{10}$", "$P_{90}$"),
      align     = c("l", "l", "r", "r", "r", "r", "r", "r"),
      escape    = FALSE
    ) %>%
    kable_styling(latex_options = "hold_position") %>%
    collapse_rows(columns = 1, latex_hline = "major")
  
  if (add_footnote) {
    k <- k %>%
      footnote(
        general        = fn_text,
        general_title  = "",
        escape         = FALSE,
        threeparttable = TRUE
      )
  }
  k
}

latex_t2 <- t2_all %>%
  filter(Panel == "Incubation-Corrected") %>% select(-Panel) %>%
  make_t2_kbl(
    cap          = "Descriptive Statistics: Fund Characteristics (Incubation-Corrected Panel)",
    lab          = "desc_stats",
    add_footnote = TRUE,
    fn_text      = fn_t2_incub
  )
write_tex(latex_t2, "table_desc_stats.tex", small = TRUE)

latex_t2_master <- t2_all %>%
  filter(Panel == "Master") %>% select(-Panel) %>%
  make_t2_kbl(
    cap          = "Descriptive Statistics: Master Panel (No Corrections)",
    lab          = "desc_stats_master",
    add_footnote = TRUE,
    fn_text      = fn_t2_master
  )
write_tex(latex_t2_master, "table_desc_stats_master.tex", small = TRUE)

latex_t2_trimmed <- t2_all %>%
  filter(Panel == "Trimmed (1995--2023)") %>% select(-Panel) %>%
  make_t2_kbl(
    cap          = "Descriptive Statistics: Trimmed Panel (1995--2023, Evans-Corrected)",
    lab          = "desc_stats_trimmed",
    add_footnote = TRUE,
    fn_text      = fn_t2_trimmed
  )
write_tex(latex_t2_trimmed, "table_desc_stats_trimmed.tex", small = TRUE)

# =============================================================================
# 4. TABLE 3: PANEL-LEVEL GROSS RETURN DISTRIBUTION
# =============================================================================
ret_dist <- function(panel, panel_label) {
  panel %>%
    filter(ap_group %in% c("Active", "Passive")) %>%
    group_by(ap_group) %>%
    summarise(
      N        = fmt2(sum(!is.na(ret_gross))),
      Mean     = fmt(mean(ret_gross   * 100, na.rm = TRUE)),
      VW_Mean  = fmt(wm(ret_gross, tna_lag) * 100),
      Median   = fmt(median(ret_gross * 100, na.rm = TRUE)),
      SD       = fmt(sd(ret_gross     * 100, na.rm = TRUE)),
      P10      = fmt(pct(ret_gross    * 100, 0.10)),
      P90      = fmt(pct(ret_gross    * 100, 0.90)),
      Skew     = fmt((mean(ret_gross, na.rm = TRUE) -
                        median(ret_gross, na.rm = TRUE)) /
                       sd(ret_gross, na.rm = TRUE)),
      # e1071::kurtosis with type=2 gives excess kurtosis (Fisher, subtract 3)
      Kurt     = fmt(e1071::kurtosis(ret_gross, na.rm = TRUE, type = 2),
                     digits = 4),
      .groups = "drop"
    ) %>%
    mutate(Panel = panel_label)
}

t3 <- bind_rows(
  ret_dist(panel_master,     "Master"),
  ret_dist(panel_incubation, "Incubation-Corrected"),
  ret_dist(panel_trimmed,    "Trimmed (1995--2023)")
) %>%
  select(Panel, ap_group, N, Mean, VW_Mean, Median, SD, P10, P90, Skew, Kurt)

latex_t3 <- t3 %>%
  kbl(
    format    = "latex",
    booktabs  = TRUE,
    caption   = "Distribution of Monthly Gross Returns: Active vs.\\ Passive Funds",
    label     = "return_dist",
    col.names = c("Panel", "Group", "$N$", "Mean", "VW Mean", "Median",
                  "SD", "$P_{10}$", "$P_{90}$", "Skewness", "Ex.\\ Kurtosis"),
    align     = c("l", "l", "r", "r", "r", "r", "r", "r", "r", "r", "r"),
    escape    = FALSE
  ) %>%
  kable_styling(latex_options = "hold_position") %>%
  collapse_rows(columns = 1, latex_hline = "major") %>%
  footnote(
    general = paste(
      "Statistics computed at the fund-month level across all valid gross",
      "return observations, expressed as percentages. $N$ is the number of",
      "fund-month observations. VW Mean is the lagged-TNA-weighted cross-sectional",
      "mean return, representing the return earned by a representative invested dollar.",
      "Skewness is Pearson's second coefficient:",
      "$(\\\\bar{x}-\\\\tilde{x})/s$.",
      "Excess kurtosis is the fourth standardised central moment minus 3.",
      "Unknown funds are excluded."
    ),
    general_title  = "",
    escape         = FALSE,
    threeparttable = TRUE
  )

write_tex(latex_t3, "table_return_dist.tex")

# =============================================================================
# 5. TABLE 4: TIME SERIES COVERAGE
# =============================================================================
coverage <- function(panel, panel_label) {
  panel %>%
    filter(ap_group %in% c("Active", "Passive")) %>%
    group_by(Ticker, ap_group) %>%
    summarise(n_obs = n(), .groups = "drop") %>%
    group_by(ap_group) %>%
    summarise(
      Funds  = fmt2(n()),
      Min    = fmt(min(n_obs),       0),
      P25    = fmt(pct(n_obs, 0.25), 0),
      Median = fmt(median(n_obs),    0),
      P75    = fmt(pct(n_obs, 0.75), 0),
      Max    = fmt(max(n_obs),       0),
      .groups = "drop"
    ) %>%
    mutate(Panel = panel_label)
}

t4 <- bind_rows(
  coverage(panel_master,     "Master"),
  coverage(panel_incubation, "Incubation-Corrected"),
  coverage(panel_trimmed,    "Trimmed (1995--2023)")
) %>% select(Panel, ap_group, Funds, Min, P25, Median, P75, Max)

t4_wide <- t4 %>% select(-Panel)

latex_t4 <- t4_wide %>%
  kbl(
    format    = "latex",
    booktabs  = TRUE,
    caption   = "Time Series Coverage: Months of Observation per Fund",
    label     = "coverage",
    col.names = c("Group", "Funds", "Min", "P25", "Median", "P75", "Max"),
    align     = c("l", "r", "r", "r", "r", "r", "r"),
    escape    = FALSE
  ) %>%
  kable_styling(latex_options = "hold_position") %>%
  pack_rows("Master",               1, 2, bold = FALSE, italic = FALSE,
            hline_before = FALSE, hline_after = TRUE) %>%
  pack_rows("Incubation-Corrected", 3, 4, bold = FALSE, italic = FALSE,
            hline_before = FALSE, hline_after = TRUE) %>%
  pack_rows("Trimmed (1995--2023)", 5, 6, bold = FALSE, italic = FALSE,
            hline_before = FALSE, hline_after = FALSE) %>%
  footnote(
    general        = "Distribution of monthly observation counts per fund. Unknown funds excluded.",
    general_title  = "",
    escape         = FALSE,
    threeparttable = TRUE
  )

write_tex(latex_t4, "table_coverage.tex", resize = FALSE)

# =============================================================================
# 6. FIGURE 1: AVERAGE MONTHLY PROPORTIONAL FUND FLOWS
#    12-month centred rolling mean via slide_index_dbl() to avoid
#    end-of-month arithmetic errors (e.g. Feb 31 -> NA with zoo::rollmean).
# =============================================================================
flow_ts <- panel_master %>%
  filter(ap_group %in% c("Active", "Passive"),
         !is.na(flow_calc_pct_win)) %>%
  group_by(date, ap_group) %>%
  summarise(mean_flow = mean(flow_calc_pct_win * 100, na.rm = TRUE),
            .groups   = "drop") %>%
  arrange(ap_group, date)

flow_ts <- flow_ts %>%
  group_by(ap_group) %>%
  mutate(roll_flow = slider::slide_index_dbl(
    .x        = mean_flow,
    .i        = floor_date(date, "month"),
    .f        = mean,
    .before   = months(6),
    .after    = months(5),
    .complete = TRUE
  )) %>%
  ungroup()

pal <- c("Active" = "#2166AC", "Passive" = "#D6604D")

date_max_data <- max(flow_ts$date, na.rm = TRUE)
x_upper       <- ceiling_date(date_max_data, unit = "year")

p_flows <- ggplot(flow_ts, aes(x = date, colour = ap_group)) +
  geom_hline(yintercept = 0, linewidth = 0.35, colour = "grey55",
             linetype = "dashed") +
  geom_line(aes(y = mean_flow), linewidth = 0.22, alpha = 0.22) +
  geom_line(aes(y = roll_flow, linetype = ap_group),
            linewidth = 0.85, na.rm = TRUE) +
  scale_colour_manual(values = pal, name = NULL) +
  scale_linetype_manual(
    values = c("Active" = "solid", "Passive" = "dashed"), name = NULL) +
  scale_x_date(
    breaks = seq(
      as.Date(paste0(
        floor(as.integer(format(min(flow_ts$date), "%Y")) / 5) * 5,
        "-01-01")),
      x_upper, by = "5 years"),
    date_labels = "%Y",
    expand      = expansion(mult = c(0.01, 0.01))
  ) +
  scale_y_continuous(
    labels = label_number(suffix = "%"),
    breaks = pretty_breaks(n = 8)
  ) +
  labs(
    # v2.3: title, subtitle, and source caption moved to the LaTeX
    # \caption{} block so they are searchable, editable, and not
    # rasterised into the PNG. Only the y-axis label and the
    # Active/Passive legend remain in the figure itself.
    title    = NULL,
    subtitle = NULL,
    caption  = NULL,
    x        = NULL,
    y        = "Mean Proportional Flow (% of Lagged TNA)"
  ) +
  theme_classic(base_size = 11) +
  theme(
    axis.title.y       = element_text(size = 9, margin = margin(r = 6)),
    axis.text          = element_text(size = 8.5),
    legend.position    = c(0.88, 0.90),
    legend.background  = element_rect(fill = "white", colour = NA),
    legend.key.width   = unit(1.6, "cm"),
    legend.text        = element_text(size = 9),
    panel.grid.major.y = element_line(colour = "grey90", linewidth = 0.3),
    panel.grid.major.x = element_blank(),
    plot.margin        = margin(10, 14, 8, 8)
  )

ggsave("fig_fund_flows.pdf", plot = p_flows, width = 7.5, height = 4.2,
       device = "pdf")
ggsave("fig_fund_flows.png", plot = p_flows, width = 7.5, height = 4.2,
       dpi = 300)

cat("Done. All outputs written to working directory.\n")