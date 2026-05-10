# =============================================================================
# PERFORMANCE REPORTING: TABLES 5-10b AND FIGURES 2-3 (v8.5)
#
# v8.5 changes vs v8.4 (R-source backslash + Phase B caption architecture):
#   (a) fn_t7 (Table 7 footnote) and fn_t9 (Table 9 footnote): flagged_funds.xlsx
#       backslash count corrected from 2 source-file backslashes to 4 so that
#       after kableExtra threeparttable=TRUE strips one level, the .tex file
#       carries the correct \_ escape (was producing _ → math-mode cascade).
#   (b) threeparttable_note_after() helper added: extracts tablenotes from
#       inside the threeparttable float and re-emits them as a free-flowing
#       paragraph after \end{table}, fixing Overfull \vbox where long notes
#       overflowed the page bottom margin (collided with page numbers).
#       Applied to Tables 7, 9, 10b. Note paragraph uses {\footnotesize\par}
#       grouping (NOT \begin{minipage}, which would still be unbreakable).
#   (c) "The reference row" → "The reference row" in fn_t10b: stale text now
#       that BSW_BOLD_ROW is permanently FALSE.
#   (d) add_header_above() calls now carry bold = FALSE (kableExtra default
#       was bold = TRUE, which was the source of bold "Gross Alpha" / "Net
#       Alpha" / "Observed Tails" / "True Proportions" headers).
#
# v8.4 changes vs v8.3 (figure inline-text strip):
#   Figure 2 (fig_rolling_alphas) and Figure 3 (fig_luck_vs_skill_combined)
#   inline narrative text stripped per project-wide convention. All
#   non-essential text (titles, subtitles, source captions, sample
#   descriptions, methodology paragraphs) moved to the LaTeX \caption{}
#   block in dissertation_main.tex so it is searchable, editable, and not
#   rasterised into the PNG. Each figure now retains only:
#     - axis labels (essential: identifies the variable plotted)
#     - legend entries (essential: identifies series/groups)
#     - Panel A / Panel B identifiers in Figure 3 (essential: structural
#       reference handles for the LaTeX caption to point at)
#   plot_annotation() removed entirely from Figure 3 (its overall title
#   and methodology subtitle were both narrative). The N(0,1) dashed-line
#   subtitle in Panel B is now described in the LaTeX caption.
#
# v8.3 changes vs v8.2 (Family C audit follow-on):
#   (a) Backslash-escaping fix: 5 of the 7 footnote strings introduced in
#       v8.2 used 2 source backslashes for "flagged\_funds.xlsx", which
#       kableExtra threeparttable=TRUE strips to 1, breaking the LaTeX
#       underscore escape and rendering the filename as "flagged_funds.xlsx"
#       (with _ interpreted as math-mode subscript). Per the convention
#       documented at line 528 of this script, kableExtra footnotes require
#       4 source backslashes for any LaTeX command. Fixed at lines 331,
#       424, 534, 636, 737. The two ggplot2 caption strings (Figures 2
#       and 3 at lines 485, 847) are NOT kableExtra footnotes; they are
#       passed directly to the graphics device which has no LaTeX
#       interpretation, so the underscore is left unescaped.
#   (b) BSW (2010) citation correction at line 325. The net-return-derivation
#       convention (gross - ER/12) originates in Carhart (1997) and Wermers
#       (2000) and is shared with BSW (2010), but applied in the opposite
#       direction (BSW observe net and derive gross; this pipeline observes
#       gross and derives net). Citation now points to the convention's
#       originators. Same correction was applied to data_cleaning_methodology.docx
#       in Family A and now propagated here.
#
# v8.2 changes vs v8.1 (Family B audit):
#   Six footnote strings updated (Table 7, Table 8, Figure 2, Table 6 fn_t9,
#   Table 7 BSW, Table 8 BSW) to acknowledge that the upstream pipeline now
#   filters on the performance-comparison subsample defined by
#   flagged_funds.xlsx. The phrasing appended is "performance-comparison
#   subsample per flagged_funds.xlsx" wherever the prior label
#   "Incubation-corrected panel (Evans 2010); no date cap" appeared. No
#   panel-level computation is performed in this script (it reads the xlsx
#   outputs of alpha_estimation.R and aggregate_alphas.R), so the structural
#   change is upstream; only the descriptive footnotes are touched here.
#
# v8.1 changes vs v8.0:
#   Footnote correction across Tables 5, 6, 7, 8 and Figures 2, 3: the
#   hardcoded sample label "Trimmed (1995--2023) Evans-corrected panel"
#   has been replaced with "Incubation-corrected panel (Evans 2010); no date
#   cap" (and syntactic variants thereof where the original was mid-sentence).
#   The Trimmed label was a legacy string from v7.x that was not updated when
#   aggregate_alphas.R switched its input panel from panel_trimmed to
#   panel_incubation; the resulting tables have been computed on
#   panel_incubation all along (hence T = 373 monthly observations in Table 5,
#   which requires data through Feb 2026 and is impossible from the Trimmed
#   1995-2023 panel), but carried a footnote that misdescribed the sample.
#   Seven strings replaced across lines 302 (Table 5), 395 (Table 8 Lipper),
#   456 (Figure 2 caption), 505 (Table 6 fn_t9), 607 (Table 7), 708 (Table 8
#   BSW), and 818 (Figure 3 PDF/CDF caption). No behavioral change; no
#   recomputation needed beyond a Phase C re-run to regenerate the .tex
#   files. The label now matches the convention already in use in
#   portfolio_sorts.R for Tables 11-13.
#
# v8.0 changes vs v7.8:
#   Tables 7, 8, and Figure 2 switched from per-fund-alpha --> cross-sectional
#   weighted mean (with static mean-TNA weight) to the FF (2010) portfolio
#   regression methodology. Each group (ap_group for Table 7; Lipper x ap_group
#   for Table 8) now produces a monthly EW/VW portfolio return series (VW uses
#   contemporaneous lagged TNA), which is regressed on the Carhart (1997) four-
#   factor model with Newey-West HAC SEs. Table 7 now reports point estimates
#   with t-statistics on a second row (FF 2010 Table II convention); Table 8
#   reports point estimates with significance stars. Figure 2 replaces the
#   cross-sectional mean of per-fund rolling alphas with a 36-month rolling
#   Carhart regression on the monthly EW portfolio return; the 12-month
#   trailing smoothing is dropped since rolling regression already smooths.
#   Requires aggregate_alphas.xlsx from aggregate_alphas.R (new script that
#   runs between alpha_estimation.R and this one).
#   - wm_alpha() helper removed (no longer used).
#   - Section 3 (Table 7), Section 4 (Table 8), Section 5 (Figure 2) rewritten.
#   - add_stars() helper imported (matches portfolio_sorts.R convention).
#
# v7.8 changes vs v7.7:
#   (a) TABLE 5 RESTRUCTURED: now 5 rows in fixed order:
#       Active | Passive | Unknown | [midrule] | Active+Passive | Full Sample.
#       Source switched from alpha_full_cleaned (Lipper-filtered) to
#       alpha_full_bs (no Lipper filter), which is the correct population for
#       an aggregate alpha summary. summarise_alpha() replaced by the simpler
#       summarise_row() helper; row_spec(3, hline_after=TRUE) adds a visual
#       separator after the Unknown row.
#   (b) FOOTNOTE FIX (Table 5 + descriptive_statistics.R): "per month" removed
#       from the expense ratio deduction description. Corrected phrasing:
#       "deducts one-twelfth of the static annual expense ratio from each
#       fund's monthly gross return". The old wording was technically accurate
#       but ambiguous -- "per month" could be read as modifying "expense ratio"
#       rather than the deduction frequency.
#
# v7.6 changes vs v7.5:
#   BUG FIX: gsub patterns at lines ~370 (Table 9) and ~580 (Table 10b) that
#   stripped \addlinespace replaced with "\n" instead of "". Replacing with
#   "\n" leaves a blank line inside the tabular environment, which is invalid
#   LaTeX and triggers "Misplaced \noalign", "Missing }", and "Missing \cr"
#   cascades. Latent because triggering depends on which row position
#   kableExtra emits \addlinespace at, which varies by row count and
#   kableExtra version. Both gsub calls now use empty replacement.
#
# v7.5 changes vs v7.4:
#   (a) clean_latex() regex fully corrected (backslash count parity with
#       descriptive_statistics.R v2.0). ThreePartTable (capital) handled.
#       longtable_note() and wrap_lt_small() helpers added.
#   (b) Table 7: VW Mean added; Medians dropped; 7-col layout with
#       add_header_above; resize=TRUE; escape=FALSE.
#   (c) Table 8: VW Mean added; longtable_note + wrap_lt_small applied;
#       label added; footnote trimmed.
#   (d) wm_alpha() helper added. Requires mean_tna from alpha_estimation v2.4.
#       when the backslash is stripped in footnote context.
# =============================================================================

library(dplyr)
library(readxl)
library(tidyr)
library(ggplot2)
library(lubridate)
library(slider)
library(knitr)
library(kableExtra)
library(scales)
library(stringr)
library(patchwork)

# =============================================================================
# 1. LOAD DATA
# =============================================================================
alpha_full   <- read_excel("alpha_fullperiod.xlsx")
alpha_roll   <- read_excel("alpha_rolling.xlsx")
boot_summary <- read_excel("bootstrap_results.xlsx", sheet = "summary")

# FF (2010)-style aggregate portfolio alphas (Tables 7, 8, Figure 2)
agg_t7       <- read_excel("aggregate_alphas.xlsx", sheet = "t7_agg")
agg_t8       <- read_excel("aggregate_alphas.xlsx", sheet = "t8_lipper")
agg_fig2     <- read_excel("aggregate_alphas.xlsx", sheet = "fig2_rolling")

# =============================================================================
# 2. CLEAN AND PREPARE
# =============================================================================

# --- 2a. Two distinct cleaned datasets ---

# For Table 8 (Lipper breakdown): requires valid Lipper category.
clean_data <- function(df) {
  df %>%
    filter(!is.na(lipper), lipper != "#N/A", lipper != "#N/A N/A") %>%
    mutate(ap_group = gsub("Agtive", "Active", ap_group))
}
alpha_full_cleaned <- clean_data(alpha_full)

# For Tables 9-10b and Figure 3: only the ap_group typo fix applied.
# Lipper is irrelevant to bootstrap, pi0, and BSW analyses; excluding funds
# with missing Lipper here would silently shrink the population without
# methodological justification.
alpha_full_bs <- alpha_full %>%
  mutate(ap_group = gsub("Agtive", "Active", ap_group))

# --- 2b. Formatting helpers ---
fmt <- function(x, digits = 3) {
  if (is.null(x) || is.na(x) || x == "NaN") return("--")
  val <- suppressWarnings(as.numeric(x))
  if (is.na(val)) return("--")
  formatC(round(val, digits), format = "f", digits = digits)
}

fmt1 <- function(x) formatC(round(as.numeric(x), 1), format = "f", digits = 1)

# Fixes kableExtra LaTeX bugs:
#   1. Extra closing brace on \end{threeparttable}.
#   2. Malformed \resizebox with \ifdim construct.
#   3. Float placement: replaces [!h] with [H] so tables stay at insertion
#      point. Requires \usepackage{float} in your Overleaf preamble.
clean_latex <- function(x, resize = TRUE, small = FALSE) {
  # Fix kableExtra double-brace bug (both env-name cases kableExtra may emit)
  x <- gsub("\\\\end[{]threeparttable[}][}]", "\\\\end{threeparttable}", x)
  x <- gsub("\\\\end[{]ThreePartTable[}][}]",  "\\\\end{ThreePartTable}", x)
  # Fix malformed \resizebox with \ifdim construct
  x <- gsub("\\\\resizebox[{]\\\\ifdim[^}]*[}][{]![}][{]",
            "\\\\resizebox{\\\\linewidth}{!}{", x)
  # Force exact float placement
  x <- gsub("\\begin{table}[!h]", "\\begin{table}[H]", x, fixed = TRUE)
  
  if (resize && !grepl("resizebox", x, fixed = TRUE)) {
    # Wrap ThreePartTable first (it encloses the tabular when present)
    if (grepl("ThreePartTable", x, fixed = TRUE)) {
      x <- sub("(\\\\begin[{]ThreePartTable[}])",
               "\\\\resizebox{\\\\linewidth}{!}{\n\\1", x)
      x <- sub("(\\\\end[{]ThreePartTable[}])", "\\1\n}", x)
    } else if (grepl("threeparttable", x, fixed = TRUE)) {
      x <- sub("(\\\\begin[{]threeparttable[}])",
               "\\\\resizebox{\\\\linewidth}{!}{\n\\1", x)
      x <- sub("(\\\\end[{]threeparttable[}])", "\\1\n}", x)
    } else {
      x <- sub("(\\\\begin[{]tabular[}])",
               "\\\\resizebox{\\\\linewidth}{!}{\n\\1", x)
      x <- sub("(\\\\end[{]tabular[}])", "\\1\n}", x)
    }
  }
  if (small) x <- sub("(\\\\begin\\{table\\}[^\n]*\n)", "\\1\\\\small\n", x)
  x
}

# Inject a note row inside a longtable before \end{longtable}
longtable_note <- function(s, note, n_cols) {
  note_row <- paste0(
    "\\midrule\n",
    "\\multicolumn{", n_cols, "}{p{0.97\\linewidth}}{",
    "\\footnotesize\\textit{Note:} ", note, "}\\\\[3pt]\n"
  )
  parts <- strsplit(s, "\\end{longtable}", fixed = TRUE)[[1]]
  paste0(parts[1], note_row, "\\end{longtable}",
         if (length(parts) > 1)
           paste0(parts[-1], collapse = "\\end{longtable}") else "")
}

# Alternative to longtable_note() that places the note as a standalone
# paragraph AFTER \end{longtable} instead of as a \multicolumn row inside it.
# Rationale: an in-table multicolumn note with p{0.97\linewidth} forces all
# columns to span 97% of textwidth regardless of content, producing visible
# whitespace between narrow numeric columns (Table B.1 symptom). Placing the
# note outside lets the longtable size to its natural content width.
longtable_note_after <- function(s, note) {
  replacement <- paste0(
    "\\end{longtable}\n",
    "\\begin{minipage}{0.92\\linewidth}\n",
    "\\footnotesize\\textit{Note:} ", note, "\n",
    "\\end{minipage}\n"
  )
  sub("\\end{longtable}", replacement, s, fixed = TRUE)
}

# Phase B helper (v8.5): extract tablenotes from a threeparttable float and
# re-emit them AFTER \end{table} as a flowing paragraph (NOT a minipage --
# minipage is unbreakable and overflowed page bottoms when notes were long).
# Call BEFORE clean_latex(); after the helper removes the threeparttable
# wrapper, clean_latex() wraps the bare tabular instead.
threeparttable_note_after <- function(s) {
  note_rx <- "\\\\begin\\{tablenotes\\}.*?\\\\end\\{tablenotes\\}"
  nb <- regmatches(s, regexpr(note_rx, s, perl = TRUE))
  if (!length(nb)) return(s)
  ni <- nb
  ni <- sub("^\\\\begin\\{tablenotes\\}(\\[para\\])?\\s*\n?", "", ni, perl = TRUE)
  ni <- sub("\\\\end\\{tablenotes\\}\\s*$", "", ni, perl = TRUE)
  ni <- sub("^\\\\footnotesize\\s*\n?", "", ni, perl = TRUE)
  ni <- sub("^\\\\item\\s*", "", ni, perl = TRUE)
  ni <- trimws(ni)
  s <- gsub(note_rx, "", s, perl = TRUE)
  s <- gsub("\\\\begin\\{threeparttable\\}\\s*\n?", "", s)
  s <- gsub("\\\\end\\{threeparttable\\}\\}?\\s*\n?", "", s)
  note_block <- paste0("\\end{table}\n",
                       "{\\footnotesize\\noindent\\textit{Note:} ",
                       ni, "\\par}\n")
  sub("\\end{table}", note_block, s, fixed = TRUE)
}

# Wrap a longtable in footnotesize group with tight column spacing. Also
# sets \LTleft=\LTright=\fill to force natural-width centering (default
# longtable positioning that kableExtra preserves but which callers may
# want to enforce explicitly when a previous wrapper altered it).
wrap_lt_small <- function(s, tabcolsep = "3pt") {
  opener <- paste0("{\\setlength{\\tabcolsep}{", tabcolsep,
                   "}\\setlength{\\LTleft}{\\fill}",
                   "\\setlength{\\LTright}{\\fill}",
                   "\\footnotesize\n\\begin{longtable}")
  parts_open <- strsplit(s, "\\begin{longtable}", fixed = TRUE)[[1]]
  s <- paste0(parts_open[1], opener,
              if (length(parts_open) > 1) parts_open[2] else "")
  parts_close <- strsplit(s, "\\end{longtable}", fixed = TRUE)[[1]]
  paste0(parts_close[1], "\\end{longtable}\n}",
         if (length(parts_close) > 1)
           paste0(parts_close[-1], collapse = "\\end{longtable}") else "")
}

# --- 2c. Scaling to Percentages ---
alpha_full_cleaned <- alpha_full_cleaned %>%
  mutate(across(c(alpha_ann, alpha_net_ann), ~ .x * 100))

alpha_full_bs <- alpha_full_bs %>%
  mutate(across(c(alpha_ann, alpha_net_ann), ~ .x * 100))

alpha_roll_cleaned <- alpha_roll %>%
  mutate(alpha_ann = alpha_ann * 100)

# =============================================================================
# 3. TABLE 5: AGGREGATE PERFORMANCE (N-TRANSPARENCY)
#
# Row order:
#   1. Active
#   2. Passive
#   3. Unknown
#   ---- midrule ----
#   4. Active + Passive  (classified funds only; excludes Unknown)
#   5. Full Sample       (all three groups including Unknown)
#
# Source: alpha_full_bs (ap_group typo-corrected, no Lipper filter).
# Table 5 is an aggregate alpha summary -- Lipper classification is irrelevant.
# Using alpha_full_cleaned (Lipper-filtered) would silently drop funds lacking
# a valid Lipper code, biasing group counts (especially Unknown).
# =============================================================================

# Append significance stars as LaTeX superscripts. Matches portfolio_sorts.R
# convention: |t| thresholds 1.645 (10%), 1.960 (5%), 2.576 (1%).
# Used in Tables 7 and 8. escape=FALSE required in kbl() for stars to render.
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

# -----------------------------------------------------------------------------
# TABLE 7 - FF (2010)-STYLE AGGREGATE PORTFOLIO ALPHA
# -----------------------------------------------------------------------------
# Input: agg_t7 from aggregate_alphas.xlsx. Two rows per group, following
# FF (2010) Table II convention:
#   Row A: Group, N funds, T months, four annualised alphas (with stars).
#   Row B: "t(coef)", blanks, four Newey-West t-stats in parentheses.

# Scale alphas to percent.
t7 <- agg_t7 %>%
  mutate(across(c(alpha_ew_gross, alpha_ew_net,
                  alpha_vw_gross, alpha_vw_net), ~ .x * 100))

# Preserve pre-v8.0 row order.
t7$Group <- factor(t7$Group,
                   levels = c("Active", "Passive", "Unknown",
                              "Active + Passive", "Full Sample"))
t7 <- t7 %>% arrange(Group)

# Build interleaved (coefficient, t-statistic) rows.
make_t7_rows <- function(r) {
  coef_row <- data.frame(
    Group    = as.character(r$Group),
    N        = formatC(r$n_funds, format = "d", big.mark = ","),
    T        = formatC(r$t_months, format = "d"),
    EW_Gross = add_stars(fmt(r$alpha_ew_gross), r$t_ew_gross),
    VW_Gross = add_stars(fmt(r$alpha_vw_gross), r$t_vw_gross),
    EW_Net   = add_stars(fmt(r$alpha_ew_net),   r$t_ew_net),
    VW_Net   = add_stars(fmt(r$alpha_vw_net),   r$t_vw_net),
    stringsAsFactors = FALSE
  )
  t_row <- data.frame(
    Group    = "t(coef)",
    N        = "",
    T        = "",
    EW_Gross = paste0("(", fmt(r$t_ew_gross, 2), ")"),
    VW_Gross = paste0("(", fmt(r$t_vw_gross, 2), ")"),
    EW_Net   = paste0("(", fmt(r$t_ew_net,   2), ")"),
    VW_Net   = paste0("(", fmt(r$t_vw_net,   2), ")"),
    stringsAsFactors = FALSE
  )
  rbind(coef_row, t_row)
}

t7_display <- do.call(rbind,
                      lapply(seq_len(nrow(t7)), function(i) make_t7_rows(t7[i, ])))
rownames(t7_display) <- NULL

# Horizontal rule after Unknown's t-stat row (row 6) separates individual
# groups from the combined aggregates below.
unknown_tstat_row <- 2L * which(levels(t7$Group) == "Unknown")

fn_t7 <- paste(
  "Annualised \\\\textcite{Carhart1997} four-factor alpha (\\\\%) from regressing",
  "the monthly aggregate portfolio return of each group on the market, size,",
  "value, and momentum factors, following \\\\textcite{FamaFrench2010}.",
  "EW: equal-weighted portfolio (each fund alive in month $t$ contributes",
  "$1/N_t$). VW: value-weighted portfolio with lagged TNA weights",
  "$w_{i,t-1} = \\\\text{TNA}_{i,t-1} / \\\\sum_j \\\\text{TNA}_{j,t-1}$.",
  "Net returns are computed as gross returns less one-twelfth of the static",
  "annual expense ratio each month, following \\\\textcite{Carhart1997} and \\\\textcite{Wermers2000}.",
  "Newey-West $t$-statistics (6-month lag) in parentheses below each alpha;",
  "$^{*}$, $^{**}$, $^{***}$: significant at 10\\\\%, 5\\\\%, 1\\\\%.",
  "$N$: unique funds contributing to the portfolio series;",
  "$T$: number of monthly observations in the regression.",
  "The Active + Passive row aggregates only the two classified groups.",
  "Sample: Incubation-corrected panel (Evans 2010), no date cap; performance-comparison subsample per flagged\\\\_funds.xlsx."
)

latex_t7 <- t7_display %>%
  kbl(format    = "latex",
      booktabs  = TRUE,
      linesep   = "",
      escape    = FALSE,
      caption   = "Aggregate Portfolio Alpha by Group (\\%, Annualised)",
      label     = "perf_aggregate",
      col.names = c("Group", "$N$", "$T$",
                    "EW", "VW", "EW", "VW"),
      align     = "lrrrrrr") %>%
  kable_styling(latex_options = "hold_position") %>%
  add_header_above(c(" " = 3, "Gross Alpha" = 2, "Net Alpha" = 2), bold = FALSE) %>%
  row_spec(unknown_tstat_row, hline_after = TRUE) %>%
  footnote(general        = fn_t7,
           general_title  = "",
           escape         = FALSE,
           threeparttable = TRUE)

t7_str <- threeparttable_note_after(as.character(latex_t7))  # PHASE B: move note outside float
writeLines(clean_latex(t7_str, resize = TRUE), "table_perf_aggregate.tex")
cat("Written: table_perf_aggregate.tex\n")

# =============================================================================
# 4. TABLE 8: LIPPER BREAKDOWN
# =============================================================================
# -----------------------------------------------------------------------------
# TABLE 8 / B.1 - AGGREGATE PORTFOLIO GROSS ALPHA BY LIPPER CLASS
# -----------------------------------------------------------------------------
# Input: agg_t8 from aggregate_alphas.xlsx. Gross alpha is reported (instead of
# net) to avoid the static expense-ratio approximation contaminating this
# breakdown, consistent with the thesis convention for performance-attribution
# exhibits. Structure uses pack_rows to place each Lipper class as a visual
# group header, with Active/Passive as plain rows beneath it. This avoids the
# collapse_rows rendering artefacts observed in v8.0.
T8_NCOLS <- 5L

# Fix for escape bug in v8.0: LaTeX special chars must be escaped with a
# single backslash (\&, \%, \_, \#). The v8.0 escape function inserted a
# double backslash, which LaTeX reads as a tabular newline \\ followed by a
# column separator, breaking every S&P and "Science & Technology" row.
# gsub() with fixed=TRUE treats the replacement as a literal string, so the
# R source "\\&" produces the memory value "\&" which is what LaTeX needs.
escape_latex_cell <- function(x) {
  x <- gsub("\\", "\\textbackslash{}", x, fixed = TRUE)  # must come first
  x <- gsub("&",  "\\&", x, fixed = TRUE)
  x <- gsub("%",  "\\%", x, fixed = TRUE)
  x <- gsub("_",  "\\_", x, fixed = TRUE)
  x <- gsub("#",  "\\#", x, fixed = TRUE)
  x <- gsub("$",  "\\$", x, fixed = TRUE)
  x
}

t8 <- agg_t8 %>%
  mutate(across(c(alpha_ew_gross, alpha_vw_gross,
                  alpha_ew_net,   alpha_vw_net), ~ .x * 100)) %>%
  arrange(lipper, Group)

# One visible row per (Lipper, Group) combination. The Lipper class itself
# becomes a pack_rows header below, so it is NOT a column in t8_display.
t8_display <- t8 %>%
  rowwise() %>%
  mutate(
    Group    = Group,
    N        = formatC(n_funds,  format = "d", big.mark = ","),
    T        = formatC(t_months, format = "d"),
    `EW`     = add_stars(fmt(alpha_ew_gross), t_ew_gross),
    `VW`     = add_stars(fmt(alpha_vw_gross), t_vw_gross)
  ) %>%
  ungroup() %>%
  select(Group, N, T, EW, VW)

# Build the pack_rows index: one entry per Lipper class with its row span.
lipper_spans <- t8 %>%
  group_by(lipper) %>%
  summarise(n = n(), .groups = "drop") %>%
  arrange(lipper) %>%
  mutate(lipper_escaped = vapply(lipper, escape_latex_cell, character(1)))

fn_t8 <- paste(
  "Annualised \\textcite{Carhart1997} four-factor gross alpha (\\%) from",
  "regressing the monthly aggregate portfolio gross return of each",
  "(Lipper class $\\times$ group) cell on the market, size, value, and momentum",
  "factors, following \\textcite{FamaFrench2010}. EW: equal-weighted portfolio;",
  "VW: lagged-TNA-weighted portfolio. Significance stars based on Newey-West",
  "$t$-statistics (6-month lag): $^{*}$, $^{**}$, $^{***}$ denote 10\\%, 5\\%,",
  "1\\% significance. $N$: unique funds contributing to the cell's portfolio",
  "series; $T$: number of monthly observations in the regression. Cells with",
  "fewer than 3 funds are suppressed. Gross returns are preferred here over",
  "net returns because the static annual expense ratio used to derive net",
  "returns introduces a class-specific approximation error that would",
  "contaminate the style-class breakdown.",
  "Sample: Incubation-corrected panel (Evans 2010), no date cap; performance-comparison subsample per flagged\\_funds.xlsx."
)

latex_t8 <- t8_display %>%
  kbl(format    = "latex",
      longtable = TRUE,
      booktabs  = TRUE,
      escape    = FALSE,
      linesep   = "",
      caption   = "Aggregate Portfolio Gross Alpha by Lipper Style Class (\\%, Annualised)",
      label     = "perf_by_lipper",
      col.names = c("Group", "$N$", "$T$", "EW", "VW"),
      align     = "lrrrr") %>%
  kable_styling(latex_options = c("hold_position", "repeat_header")) %>%
  add_header_above(c(" " = 3, "Gross Alpha" = 2), bold = FALSE)

# Apply pack_rows for each Lipper class. Row ranges are cumulative.
cum_end <- cumsum(lipper_spans$n)
cum_start <- c(1L, head(cum_end, -1) + 1L)
for (i in seq_len(nrow(lipper_spans))) {
  latex_t8 <- latex_t8 %>%
    pack_rows(lipper_spans$lipper_escaped[i],
              cum_start[i], cum_end[i],
              bold = FALSE, italic = TRUE,
              hline_before = (i > 1L), hline_after = FALSE,
              latex_gap_space = "0.25em",
              escape = FALSE)
}

t8_str <- as.character(latex_t8)
t8_str <- longtable_note_after(t8_str, fn_t8)
t8_str <- wrap_lt_small(t8_str)
writeLines(t8_str, "table_perf_by_lipper.tex")
cat("Written: table_perf_by_lipper.tex\n")

# =============================================================================
# 5. FIGURE 2: ROLLING AGGREGATE PORTFOLIO ALPHA (FF 2010 methodology)
# =============================================================================
# 36-month rolling Carhart regression on the monthly equal-weighted portfolio
# return for Active and Passive groups separately. Replaces the pre-v8.0
# version which took a cross-sectional mean of per-fund rolling alphas and
# then applied an additional 12-month trailing mean. The rolling regression
# already provides the necessary smoothing; the extra trailing mean is dropped.
alpha_ts <- agg_fig2 %>%
  mutate(alpha_ann = alpha_ann * 100,
         ap_group  = gsub("Agtive", "Active", ap_group)) %>%
  filter(ap_group %in% c("Active", "Passive"))

p_alpha <- ggplot(alpha_ts, aes(x = date, y = alpha_ann, color = ap_group)) +
  geom_hline(yintercept = 0, color = "grey55", linetype = "dashed", linewidth = 0.35) +
  geom_line(aes(linetype = ap_group), linewidth = 0.85, na.rm = TRUE) +
  scale_color_manual(values = c("Active" = "#2166AC", "Passive" = "#D6604D"), name = NULL) +
  scale_linetype_manual(values = c("Active" = "solid", "Passive" = "dashed"), name = NULL) +
  scale_y_continuous(labels = label_number(suffix = "%")) +
  theme_classic(base_size = 11) +
  labs(
    # v8.4: title, subtitle, and source caption moved to the LaTeX
    # \caption{} block so they are searchable, editable, and not
    # rasterised into the PNG. Only the y-axis label and the
    # Active/Passive legend remain in the figure itself.
    title    = NULL,
    subtitle = NULL,
    caption  = NULL,
    y        = "Annualized Alpha (%)",
    x        = NULL
  ) +
  theme(
    legend.position = "bottom"
  )

ggsave("fig_rolling_alphas.png", plot = p_alpha, width = 7.5, height = 4.2, dpi = 300)

# =============================================================================
# 6. TABLE 9: BOOTSTRAP PERCENTILES
# =============================================================================
# FIXED: n_active_bs from alpha_full_bs to match the actual bootstrap
# population in alpha_estimation.R (which uses alpha_full, no Lipper filter).
n_active_bs <- sum(
  alpha_full_bs$ap_group == "Active" & !is.na(alpha_full_bs$alpha_t_nw)
)

boot_tab <- boot_summary %>%
  filter(percentile %in% c(1, 5, 10, 50, 90, 95, 99)) %>%
  mutate(
    Actual_t  = sapply(t_alpha_actual,   fmt, digits = 3),
    Sim_Mean  = sapply(t_alpha_sim_mean, fmt, digits = 3),
    # Pre-format with \% so escape=FALSE passes it through correctly
    Prob_Luck = paste0(formatC(pct_runs_below, format = "f", digits = 1), "\\%"),
    Interpretation = case_when(
      percentile <= 10 & pct_runs_below < 5  ~ "Worse than luck (significant)",
      percentile >= 90 & pct_runs_below > 95 ~ "Evidence of genuine skill",
      percentile == 50                        ~ "Indistinguishable from luck",
      TRUE                                    ~ "Consistent with zero-skill"
    )
  )

# Rule: kableExtra strips one backslash from threeparttable footnote text.
# Every LaTeX command \X in the footnote therefore needs \\X in the R string,
# i.e. \\\\X in R source. Plain text, $...$, and {,} are unaffected.
fn_t9 <- paste(
  "Bootstrap procedure follows \\\\textcite{FamaFrench2010}.",
  "Sample: actively managed funds, Incubation-corrected (Evans 2010) panel (no date cap), performance-comparison subsample per flagged\\\\_funds.xlsx,",
  paste0("minimum 24 monthly observations ($N = ", n_active_bs, "$ funds)."),
  "For each fund, estimated monthly alpha is subtracted from the excess return",
  "series to construct a zero-alpha null return.",
  "In each of $B = 10{,}000$ bootstrap iterations, calendar months are resampled",
  "with replacement, preserving cross-sectional factor return dependence.",
  "The \\\\textcite{Carhart1997} four-factor model is re-estimated on each resampled series.",
  "\\\\textit{Actual} $t(\\\\hat{\\\\alpha})$: percentile of the empirical $t$-statistic",
  "distribution across active funds.",
  "\\\\textit{Simulated Mean}: average of that percentile across all iterations.",
  "\\\\textit{Prob.\\\\ Luck}: fraction of iterations in which the simulated percentile",
  "falls below the actual value; values below 5\\\\% at lower percentiles indicate",
  "underperformance unlikely to be explained by luck alone.",
  "Newey-West standard errors with a 6-month lag are used throughout."
)

# 7 rows in boot_tab after filter: P1, P5, P10, P50, P90, P95, P99.
# small=TRUE: long footnote risks "Float too large" without font reduction.
latex_boot <- boot_tab %>%
  select(percentile, Actual_t, Sim_Mean, Prob_Luck, Interpretation) %>%
  kbl(format    = "latex",
      booktabs  = TRUE,
      escape    = FALSE,
      caption   = "Fama--French (2010) Bootstrap: Actual vs.\\ Simulated $t(\\hat{\\alpha})$ Percentiles",
      label     = "bootstrap_tails",
      col.names = c("Percentile", "Actual $t(\\hat{\\alpha})$",
                    "Simulated Mean", "Prob.\\ Luck", "Interpretation"),
      align     = c("r", "r", "r", "r", "l")) %>%
  kable_styling(latex_options = "hold_position") %>%
  column_spec(5, width = "15em") %>%
  footnote(general        = fn_t9,
           general_title  = "",
           escape         = FALSE,
           threeparttable = TRUE)

# BUG FIX (v7.6): replacement is "" not "\n". Replacing \addlinespace lines
# with "\n" leaves a BLANK LINE inside the tabular environment, which is
# invalid LaTeX and triggers "Misplaced \noalign" / "Missing \cr" errors.
# This was latent because kableExtra only emits \addlinespace at row positions
# where the row count interacts with booktabs defaults; row-count or version
# changes can expose it. Empty replacement deletes the line cleanly.
boot_str <- gsub("\\\\addlinespace[^\n]*\n", "", as.character(latex_boot))
boot_str <- threeparttable_note_after(boot_str)  # PHASE B: move note outside float
# resize=FALSE: wrapping threeparttable in \resizebox is fragile on TeX Live
# 2024+ (Overleaf default). The hbox-restricted mode breaks the \noalign
# expansion in booktabs' \bottomrule, producing "Misplaced \noalign" errors
# at the closing brace of \resizebox. Table 9 has 5 narrow columns and fits
# within \linewidth without resizing.
writeLines(clean_latex(boot_str, resize = FALSE, small = TRUE),
           "table_bootstrap_tails.tex")
cat("Written: table_bootstrap_tails.tex\n")

# =============================================================================
# 7. TABLE 10: pi_0 AGGREGATE SKILL ESTIMATE
# =============================================================================
lambda <- 0.5

# FIXED: uses alpha_full_bs ??? Lipper filter removed; population matches the
# bootstrap and BSW decomposition exactly.
active_pi0    <- alpha_full_bs %>%
  filter(ap_group == "Active", !is.na(alpha_p_nw))

actual_p      <- active_pi0$alpha_p_nw
total_n       <- length(actual_p)
num_above_lam <- sum(actual_p > lambda, na.rm = TRUE)
pi_0_val      <- min(1.0, num_above_lam / (total_n * (1 - lambda)))

interp <- case_when(
  pi_0_val > 0.90 ~ "Industry dominated by luck",
  pi_0_val > 0.75 ~ "Heterogeneous skill; majority zero-alpha",
  pi_0_val > 0.50 ~ "Moderate skill heterogeneity",
  TRUE            ~ "Substantial skilled-fund presence"
)

# kableExtra with threeparttable=TRUE strips one backslash level from footnote
# text. Two separate strings are therefore needed:
#   pi0_pct : for the table cell (escape=FALSE, no stripping) ??? \% renders as %
#   pi0_str : for footnote text ??? \\% in R string ??? \% in LaTeX after stripping
pi0_pct <- paste0(formatC(pi_0_val * 100, format = "f", digits = 1), "\\%")
pi0_str <- paste0(formatC(pi_0_val * 100, format = "f", digits = 1), "\\\\%")

pi0_table <- data.frame(
  Metric         = "$\\hat{\\pi}_0$: Proportion of True Zero-Alpha Active Funds",
  Estimate       = pi0_pct,
  N_Funds        = as.character(total_n),
  Lambda         = formatC(lambda, format = "f", digits = 1),
  Interpretation = interp
)

# FIXED: \#\{...\} (fragile in threeparttable across LaTeX engines) replaced
# with |{...\}| (absolute-value bars), which is robust and equally standard.
# All backslash commands doubled per the kableExtra stripping rule above.
fn_t10 <- paste(
  "The proportion of true zero-alpha funds ($\\\\hat{\\\\pi}_0$) is estimated",
  "following \\\\textcite{Storey2002} and \\\\textcite{BarrasScailletWermers2010},",
  "using p-values from Newey-West $t$-tests on",
  "full-period \\\\textcite{Carhart1997} four-factor alphas.",
  "The estimator is $\\\\hat{\\\\pi}_0 = |\\\\{p_i > \\\\lambda\\\\}| \\\\,/\\\\, [N(1-\\\\lambda)]$,",
  "where $\\\\lambda = 0.5$ is the standard tuning parameter following \\\\textcite{Storey2002}.",
  "Funds with p-values exceeding $\\\\lambda$ are unlikely to have non-zero true alpha;",
  "the density of p-values in $(\\\\lambda, 1]$ provides a conservative estimate",
  "of the zero-alpha proportion, bounded above at 1.",
  paste0("Sample: actively managed funds ($N = ", total_n, "$),"),
  "Incubation-corrected (Evans 2010) panel, no date cap; performance-comparison subsample per flagged\\\\_funds.xlsx.",
  "Passive and Unknown-classified funds are excluded.",
  "The four-way decomposition of skilled, unskilled, and lucky fund proportions",
  "implied by this estimate is reported in Table~\\\\ref{tab:bsw_decomposition}."
)

latex_pi0 <- pi0_table %>%
  kbl(format    = "latex",
      booktabs  = TRUE,
      escape    = FALSE,
      caption   = "Aggregate Skill Estimate: Proportion of True Zero-Alpha Active Funds",
      label     = "pi0_estimate",
      col.names = c("Metric", "Estimate", "$N$", "$\\lambda$", "Interpretation"),
      align     = c("l", "r", "r", "r", "l")) %>%
  kable_styling(latex_options = "hold_position") %>%
  column_spec(1, width = "16em") %>%
  column_spec(5, width = "14em") %>%
  footnote(general        = fn_t10,
           general_title  = "",
           escape         = FALSE,
           threeparttable = TRUE)

writeLines(clean_latex(latex_pi0, resize = TRUE), "table_pi0_estimate.tex")
cat("Written: table_pi0_estimate.tex\n")
cat("pi_0 estimate:", formatC(pi_0_val * 100, format = "f", digits = 1), "%\n")
cat("Active funds (pi0/BSW population):", total_n, "\n")

# =============================================================================
# 7b. TABLE 10b: BSW (2010) FOUR-WAY DECOMPOSITION
#
# Barras, Scaillet & Wermers (2010, JF), Section II.B & Table III.
# All inputs already in memory from Section 7; zero additional computation.
#
# Note on critical values: S^- and S^+ are thresholded against qnorm(1-g/2),
# i.e. the normal approximation, consistent with BSW (2010). The pi_0_val
# itself is derived from t-distribution p-values (alpha_p_nw), which is more
# precise for individual funds; the discrepancy between the two approaches is
# negligible given sample sizes in the hundreds.
# =============================================================================
cat("=== 7b. BSW Four-Way Decomposition Table ===\n")

GAMMA_GRID_REP <- c(0.05, 0.10, 0.15, 0.20, 0.25, 0.30, 0.35, 0.40, 0.45, 0.50)
t_stats_rep    <- active_pi0$alpha_t_nw   # same population as pi_0_val

bsw_rep_df <- do.call(rbind, lapply(GAMMA_GRID_REP, function(g) {
  t_thresh <- qnorm(1 - g / 2)
  S_neg    <- mean(t_stats_rep < -t_thresh, na.rm = TRUE)
  S_pos    <- mean(t_stats_rep >  t_thresh, na.rm = TRUE)
  F_luck   <- pi_0_val * g / 2
  data.frame(
    gamma           = g * 100,
    S_neg_pct       = S_neg    * 100,
    S_pos_pct       = S_pos    * 100,
    F_luck_pct      = F_luck   * 100,
    T_unskilled_pct = (S_neg - F_luck) * 100,
    T_skilled_pct   = (S_pos - F_luck) * 100
  )
}))

# Population estimates at gamma* = 0.20 (BSW recommended reference)
pi_A_minus_rep <- bsw_rep_df$T_unskilled_pct[bsw_rep_df$gamma == 20]
pi_A_plus_rep  <- bsw_rep_df$T_skilled_pct[bsw_rep_df$gamma == 20]
cat(sprintf(
  "Population estimates (gamma* = 0.20):\n  pi^-_A (Genuinely Unskilled): %.1f%%\n  pi^+_A (Genuinely Skilled):   %.1f%%\n",
  pi_A_minus_rep, pi_A_plus_rep
))

bsw_display <- bsw_rep_df %>%
  mutate(
    gamma_fmt       = paste0(formatC(gamma, format = "f", digits = 0), "\\%"),
    S_neg_fmt       = sapply(S_neg_pct,       fmt1),
    S_pos_fmt       = sapply(S_pos_pct,       fmt1),
    F_luck_fmt      = sapply(F_luck_pct,      fmt1),
    T_unskilled_fmt = sapply(T_unskilled_pct, fmt1),
    T_skilled_fmt   = sapply(T_skilled_pct,   fmt1)
  ) %>%
  select(gamma_fmt, S_neg_fmt, S_pos_fmt, F_luck_fmt, T_unskilled_fmt, T_skilled_fmt)

fn_t10b <- paste(
  "Decomposition follows \\\\textcite{BarrasScailletWermers2010}, Section~II.B and Table~III.",
  "$S^-_\\\\gamma$ ($S^+_\\\\gamma$): observed fraction of active funds with",
  "significantly negative (positive) Newey-West $t(\\\\hat{\\\\alpha})$ at",
  "two-sided significance level $\\\\gamma$, using full-period \\\\textcite{Carhart1997}",
  "four-factor alphas. Critical values are from the standard normal distribution,",
  "consistent with the large-sample approximation in \\\\textcite{BarrasScailletWermers2010}.",
  "$F_\\\\gamma = \\\\hat{\\\\pi}_0 \\\\cdot \\\\gamma/2$: expected proportion of false",
  "discoveries per tail arising from zero-alpha funds,",
  paste0("where $\\\\hat{\\\\pi}_0 = ", pi0_str, "$ is the \\\\textcite{Storey2002} estimate"),
  "at $\\\\lambda = 0.5$ (see Table~\\\\ref{tab:pi0_estimate}).",
  "$T^-_\\\\gamma = S^-_\\\\gamma - F_\\\\gamma$: genuinely unskilled funds",
  "(significant negative alpha net of false discoveries).",
  "$T^+_\\\\gamma = S^+_\\\\gamma - F_\\\\gamma$: genuinely skilled funds",
  "(significant positive alpha net of false discoveries).",
  "The bolded row ($\\\\gamma = 0.20$) provides the population-level estimates",
  "$\\\\hat{\\\\pi}^-_A$ and $\\\\hat{\\\\pi}^+_A$ following \\\\textcite{BarrasScailletWermers2010}.",
  "Negative $T^+_\\\\gamma$ entries indicate right-tail significance does not",
  "exceed the false-discovery rate at that threshold.",
  "The Storey estimator is conservative: it overestimates $\\\\hat{\\\\pi}_0$",
  "when truly skilled funds exist, so $T^+_\\\\gamma$ values are lower bounds.",
  "All quantities are percentages of the active-fund universe.",
  paste0("Sample: $N = ", total_n, "$ actively managed funds,"),
  "Incubation-corrected (Evans 2010) panel, no date cap; performance-comparison subsample per flagged\\\\_funds.xlsx; Passive and Unknown funds excluded."
)



# SPACING FIXES:
#   (1) add_header_above() lifts the descriptive labels "(Unskilled)"/"(Skilled)"
#       out of the column-name row, shortening the two widest headers and
#       eliminating horizontal crowding across 6 narrow numeric columns.
#   (2) \addlinespace[0.5em] after the reference row (gamma=0.20) ??? provides
#       a visual break between the recommended reference level and the
#       supplementary higher-threshold rows.
#   (3) small=TRUE ??? 10-row table with a long math-heavy footnote risks
#       "Float too large" without font reduction.
latex_t10b <- bsw_display %>%
  kbl(format    = "latex",
      booktabs  = TRUE,
      escape    = FALSE,
      caption   = paste0(
        "BSW (2010) Four-Way Decomposition: ",
        "Proportions of Skilled, Unskilled, and Lucky Funds (\\%)"
      ),
      label     = "bsw_decomposition",
      col.names = c(
        "$\\gamma$",
        "$S^-_\\gamma$",
        "$S^+_\\gamma$",
        "$F_\\gamma$",
        "$T^-_\\gamma$",      # descriptive label moved to group header below
        "$T^+_\\gamma$"
      ),
      align = c("r", "r", "r", "r", "r", "r")) %>%
  kable_styling(latex_options = "hold_position") %>%
  # Group header: moves "(Unskilled)" / "(Skilled)" labels up one row,
  # freeing horizontal space in the main column-name row.
  add_header_above(c(
    " "               = 1,
    "Observed Tails"  = 2,
    "False Disc."     = 1,
    "True Proportions" = 2
  ), escape = FALSE, bold = FALSE) %>%
  footnote(general        = fn_t10b,
           general_title  = "",
           escape         = FALSE,
           threeparttable = TRUE)

# BUG FIX (v7.6): see Table 9 gsub comment above. Empty replacement, not "\n".
t10b_str <- gsub("\\\\addlinespace[^\n]*\n", "", as.character(latex_t10b))
t10b_str <- threeparttable_note_after(t10b_str)  # PHASE B: move note outside float
writeLines(clean_latex(t10b_str, resize = FALSE, small = TRUE),
           "table10b_bsw_decomposition.tex")
cat("Written: table10b_bsw_decomposition.tex\n")

# =============================================================================
# 8. FIGURE 3: DUAL-PANEL DISTRIBUTION (CDF & PDF)
# =============================================================================
# FIXED: actual_t sourced from alpha_full_bs to match the pi0/BSW population.
actual_t <- alpha_full_bs %>%
  filter(ap_group == "Active", !is.na(alpha_t_nw)) %>%
  pull(alpha_t_nw)

actual_data <- data.frame(t_stat = actual_t, Type = "Actual Distribution")
sim_data    <- data.frame(
  t_stat = boot_summary$t_alpha_sim_mean,
  prob   = boot_summary$percentile / 100,
  Type   = "Simulated (Zero-Skill)"
)

# Panel A: CDF
p_cdf <- ggplot() +
  stat_ecdf(data = actual_data, aes(x = t_stat, color = Type), linewidth = 1) +
  geom_line(data = sim_data,
            aes(x = t_stat, y = prob, color = Type, linetype = Type), linewidth = 1) +
  scale_color_manual(
    values = c("Actual Distribution" = "#2166AC", "Simulated (Zero-Skill)" = "black")
  ) +
  theme_classic(base_size = 10) +
  labs(title = "Panel A: Cumulative Distribution (CDF)",
       y = "Cumulative Probability", x = NULL) +
  theme(legend.position = "none") +
  coord_cartesian(xlim = c(-4, 4))

# Panel B: PDF
p_pdf <- ggplot() +
  geom_density(data = actual_data, aes(x = t_stat, fill = Type),
               alpha = 0.2, color = "#2166AC") +
  stat_function(fun = dnorm, args = list(mean = 0, sd = 1),
                color = "black", linetype = "dashed", linewidth = 1) +
  scale_fill_manual(values = c("Actual Distribution" = "#2166AC")) +
  theme_classic(base_size = 10) +
  labs(title    = "Panel B: Probability Density (PDF)",
       y = "Density",
       x = expression(italic(t)*"-Statistic of "*hat(alpha))) +
  theme(legend.position = "bottom", legend.title = element_blank()) +
  coord_cartesian(xlim = c(-4, 4))

combined_plot <- (p_cdf / p_pdf)

ggsave("fig_luck_vs_skill_combined.png", plot = combined_plot,
       width = 7.5, height = 8, dpi = 300)

cat("\n[SUCCESS] Script v7.3 complete.\n")