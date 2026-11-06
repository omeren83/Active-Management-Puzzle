# =============================================================================
# SBE TABLE FORMAT POST-PROCESSOR                                          v1.0
# sbe_table_format.R  (Nov 2026)
#
# Rewrites every table_*.tex in a folder into one SBE-compliant layout:
#   - one non-breaking float (SBE: tables must start and finish on one page;
#     kableExtra longtables are converted to floats);
#   - title immediately above the table, left-aligned to the table edge
#     (savebox + minipage, as clean_latex() in the estimation scripts);
#   - table body at the uniform table size (\footnotesize, set by the
#     preamble); per-file font sizes (\fontsize{9}{11}) removed;
#   - shrunk only if wider than the text block (former longtables are set
#     at full width); wide tables get two-line headings to stay >= 8pt;
#   - the note immediately below the table, inside the float, so it cannot
#     be separated from the table and the double space after the float
#     (\intextsep) applies after the note;
#   - longtable-style "z=+2.43 (p=0.993)" cells shortened to "+2.43 (0.993)".
# Files already in the target layout are left unchanged (idempotent).
#
# Tables listed in SPLIT_TABLES are too tall for one page: they are split at
# the given panel row into two numbered tables (second caption and label as
# given).
#
# Usage: sbe_format_folder(dir) -- called by master_pipeline.R before the
# sync step; can also be run on its own.
# =============================================================================

SBE_MARK <- "% sbe_table_format v1.0"

# Tables too tall for one page, split into two numbered tables at a panel
# row: `at` = fixed string starting the first row of the second part;
# caption1 (optional) replaces the first caption; caption/label = second table
.qcap <- function(what, extra = "") paste0("Alpha and Factor Loadings by ", what, extra, " (\\%, Annualised)")
SPLIT_TABLES <- list(
  table_persistence = list(
    at = "\\multicolumn{13}{l}{\\textit{Panel C:",
    caption1 = "Persistence in Active Mutual Fund Performance, Full Sample: Carhart Test with Bootstrap Inference",
    caption  = "Persistence in Active Mutual Fund Performance by Regime: Carhart Test with Bootstrap Inference",
    label    = "tab:persistence_regime"),
  table_activeness_alpha_r2 = list(
    at = "\\multicolumn{9}{l}{Panel C:",
    caption1 = .qcap("Activeness Quintile ($1-R^2$), Gross Returns", ""),
    caption  = .qcap("Activeness Quintile ($1-R^2$), Net Returns", ""),
    label    = "tab:activeness_alpha_r2_net"),
  table_activeness_alpha_te = list(
    at = "\\multicolumn{9}{l}{Panel C:",
    caption1 = .qcap("Activeness Quintile (Tracking Error), Gross Returns", ""),
    caption  = .qcap("Activeness Quintile (Tracking Error), Net Returns", ""),
    label    = "tab:activeness_alpha_te_net"),
  table_port_size_alpha = list(at = "\\multicolumn{9}{l}{Passive --- EW}",
    caption1 = .qcap("Size Quintile", ", Active Funds"), caption = .qcap("Size Quintile", ", Passive Funds"),
    label = "tab:port_size_alpha_passive"),
  table_port_mom_alpha = list(at = "\\multicolumn{9}{l}{Passive --- EW}",
    caption1 = .qcap("Momentum Quintile", ", Active Funds"), caption = .qcap("Momentum Quintile", ", Passive Funds"),
    label = "tab:port_mom_alpha_passive"),
  table_port_fee_alpha = list(at = "\\multicolumn{9}{l}{Passive --- EW}",
    caption1 = .qcap("Fee Quintile", ", Active Funds"), caption = .qcap("Fee Quintile", ", Passive Funds"),
    label = "tab:port_fee_alpha_passive"),
  table_perf_by_lipper = list(at = "\\multicolumn{5}{l}{\\textit{Multi-Cap Core Funds}}",
    caption1 = "Aggregate Portfolio Gross Alpha by Lipper Style Class: Equity Income, Large-Cap and Mid-Cap (\\%, Annualised)",
    caption  = "Aggregate Portfolio Gross Alpha by Lipper Style Class: Multi-Cap, Small-Cap, Index and Specialty (\\%, Annualised)",
    label    = "tab:perf_by_lipper_2")
)

# Regression tables (coefficient row + t-statistic row): rows set at 0.87 of
# the single-spaced row height (0.87) so the H4 tables, the tallest, fit one page
TIGHT_ROWS <- "^table_H[1-4]_"

# Wide tables whose column headings are set on two lines (otherwise they
# would have to be shrunk below the 8pt minimum)
WRAP_HEAD <- c("table_return_dist", "table_breaktest_terciles", "table_value_added",
               "table_behavioral_summary", "table_stepwise", "table_unc_H3",
               "table_desc_stats", "table_desc_stats_master", "table_desc_stats_trimmed",
               "table_same_sample_alpha", "table_regime_pooled", "table_regime_pooled_fe",
               "table_regime_separate", "table_tna_floor")
# shorter headings also wrapped in these tables
WRAP_MIN <- c(table_return_dist = 6L)
DEFAULT_TABCOLSEP <- "4pt"   # as clean_latex() in the estimation scripts

# --- helpers -----------------------------------------------------------------
# Position of the brace closing the group that opens at position `open`
.brace_end <- function(s, open) {
  ch <- strsplit(substring(s, open), "")[[1]]
  depth <- 0L
  for (i in seq_along(ch)) {
    if (ch[i] == "\\") next
    if (ch[i] == "{" && (i == 1L || ch[i - 1L] != "\\")) depth <- depth + 1L
    if (ch[i] == "}" && (i == 1L || ch[i - 1L] != "\\")) {
      depth <- depth - 1L
      if (depth == 0L) return(open + i - 1L)
    }
  }
  NA_integer_
}

# First \caption{...} argument (without \label) and the label, if any
.get_caption <- function(s) {
  m <- regexpr("\\\\caption(\\[[^]]*\\])?\\{", s, perl = TRUE)
  if (m == -1L) return(NULL)
  open <- m + attr(m, "match.length") - 1L
  close <- .brace_end(s, open)
  txt <- substring(s, open + 1L, close - 1L)
  lab <- regmatches(s, regexpr("\\\\label\\{[^}]*\\}", s))
  lab <- if (length(lab)) lab[1] else ""
  txt <- trimws(gsub("\\\\label\\{[^}]*\\}", "", txt))
  list(text = txt, label = lab, start = m, end = close)
}

# Text of a note block: contents of singlespace / tablenotes / \item, wrappers removed
.clean_note <- function(x) {
  x <- gsub("\\\\begin\\{(singlespace|TableNotes|tablenotes)\\}(\\[para\\])?", "", x)
  x <- gsub("\\\\end\\{(singlespace|TableNotes|tablenotes)\\}", "", x)
  x <- gsub("\\\\begin\\{minipage\\}(\\[[^]]*\\])?\\{[^}]*\\}|\\\\end\\{minipage\\}", "", x)
  x <- gsub("\\\\(par|noindent|footnotesize|raggedright|small|medskip|smallskip)\\b", " ", x, perl = TRUE)
  x <- gsub("\\\\vspace\\*?\\{[^}]*\\}", "", x)
  x <- gsub("\\\\item\\s*", "", x)
  x <- gsub("\\\\textit\\{Note:\\}\\s*", "", x)
  x <- gsub("^[\\s{}]+|[\\s{}]+$", "", x, perl = TRUE)
  # keep \par\smallskip paragraph breaks written by the scripts as blank lines
  x <- gsub("[ \t]+\n", "\n", x)
  trimws(x)
}

# Assemble the SBE float
.sbe_float <- function(tabular, caption, label, note, tabcolsep = NULL,
                       full = NULL, stretch = NULL) {
  tcs <- if (is.null(tabcolsep)) "" else paste0("\\setlength{\\tabcolsep}{", tabcolsep, "}%\n")
  if (!is.null(stretch)) tcs <- paste0(tcs, "\\renewcommand{\\arraystretch}{", stretch, "}%\n")
  # full: the same rows as tabular* stretched to the text width, used when
  # the natural table is narrower (former longtables were full width)
  widen <- if (is.null(full)) "" else paste0(
    "\\ifdim\\wd\\tabletempbox<\\linewidth\n",
    "\\sbox{\\tabletempbox}{%\n\\footnotesize\n", tcs, full, "%\n}\n\\fi\n")
  out <- paste0(
    SBE_MARK, "\n",
    "\\begin{table}[!htbp]\n",
    "\\centering\n",
    "\\sbox{\\tabletempbox}{%\n",
    "\\footnotesize\n", tcs,
    trimws(tabular), "%\n",
    "}\n", widen,
    "\\setlength{\\tabletempwidth}{\\wd\\tabletempbox}\n",
    "\\ifdim\\tabletempwidth>\\linewidth\\setlength{\\tabletempwidth}{\\linewidth}\\fi\n",
    "\\begin{minipage}{\\tabletempwidth}\n",
    "\\captionsetup{width=\\linewidth}\n",
    "\\caption{", caption, "}", label, "\n",
    "\\ifdim\\wd\\tabletempbox>\\linewidth\n",
    "  \\resizebox{\\linewidth}{!}{\\usebox{\\tabletempbox}}\n",
    "\\else\n",
    "  \\usebox{\\tabletempbox}\n",
    "\\fi\n")
  if (nzchar(note))
    out <- paste0(out, "\\par\\medskip\n",
                  "\\begin{singlespace}\\footnotesize\\noindent\n",
                  note, "\n\\end{singlespace}\n")
  paste0(out, "\\end{minipage}\n\\end{table}\n")
}

# Shorten "z=+2.43 (p=0.993)" to "+2.43 (0.993)" (meaning stated in the row label)
.short_zp <- function(s) gsub("z=([+-]?[0-9.]+) \\(p=([0-9.]+)\\)", "\\1 (\\2)", s, perl = TRUE)


# Two-line heading cell: break before " (" or at the space nearest the middle
.wrap_cell <- function(cell, min_chars = 9L) {
  core <- trimws(cell)
  if (nchar(core) < min_chars || grepl("\\\\multicolumn|\\\\makecell|^Panel", core) || !grepl(" ", core)) return(cell)
  # spaces outside $...$ and braces
  ch <- strsplit(core, "")[[1]]; depth <- 0L; math <- FALSE; sp <- integer(0)
  for (i in seq_along(ch)) {
    if (ch[i] == "$") math <- !math
    if (ch[i] == "{") depth <- depth + 1L
    if (ch[i] == "}") depth <- depth - 1L
    if (ch[i] == " " && depth == 0L && !math) sp <- c(sp, i)
  }
  if (!length(sp)) return(cell)
  par <- sp[ch[pmin(sp + 1L, length(ch))] == "("]
  k <- if (length(par)) par[length(par)] else sp[which.min(abs(sp - nchar(core) / 2))]
  # a control space ("Nonpar.\\ Skew") loses its backslash at the break
  cut <- if (k > 1L && ch[k - 1L] == "\\") k - 2L else k - 1L
  paste0(" \\makecell{", substring(core, 1L, cut), "\\\\", substring(core, k + 1L), "} ")
}
# Wrap the cells of the heading row just above the first \midrule
.wrap_head <- function(tabular, min_chars = 9L) {
  pos <- regexpr("\\\\midrule", tabular)
  if (pos == -1L) return(tabular)
  pre <- substring(tabular, 1L, pos - 1L); post <- substring(tabular, pos)
  rows <- strsplit(pre, "\\\\\\\\", perl = TRUE)[[1]]
  i <- max(which(grepl("&", rows)))
  cells <- strsplit(rows[i], "&", fixed = TRUE)[[1]]
  lead <- sub("^(\\s*(\\\\(toprule|midrule|cmidrule[^\n]*)\\s*)*).*$", "\\1", cells[1], perl = TRUE)
  cells[1] <- paste0(lead, .wrap_cell(substring(cells[1], nchar(lead) + 1L), min_chars))
  if (length(cells) > 1L) cells[-1] <- vapply(cells[-1], .wrap_cell, "", min_chars = min_chars)
  rows[i] <- paste(cells, collapse = "&")
  paste0(paste(rows, collapse = "\\\\"), post)
}
# Full-width \multicolumn{N}{l}{long text} row inside the table -> note
.row_to_note <- function(tabular) {
  rx <- "\\\\multicolumn\\{\\d+\\}\\{l\\}\\{([^\n]{100,})\\}\\s*\\\\\\\\\\s*\\n"
  m <- regmatches(tabular, regexec(rx, tabular, perl = TRUE))[[1]]
  if (!length(m)) return(list(tabular = tabular, note = ""))
  list(tabular = sub(rx, "", tabular, perl = TRUE), note = m[2])
}

# --- converters --------------------------------------------------------------
# kableExtra longtable -> list(tabular, caption, label, note, tabcolsep)
.from_longtable <- function(s) {
  cap <- .get_caption(s)
  spec_m <- regexpr("\\\\begin\\{longtable\\}(\\[[^]]*\\])?\\{", s, perl = TRUE)
  open <- spec_m + attr(spec_m, "match.length") - 1L
  spec <- substring(s, open + 1L, .brace_end(s, open) - 1L)
  spec <- gsub("@\\{\\\\extracolsep\\{\\\\fill\\}\\}", "", spec)
  # header: after the first caption row up to \endfirsthead
  after_cap <- substring(s, cap$end + 1L)
  after_cap <- sub("^\\s*\\\\\\\\(\\[[^]]*\\])?", "", after_cap, perl = TRUE)
  head <- sub("\\\\endfirsthead.*$", "", after_cap)
  rest <- sub("(?s)^.*?\\\\endlastfoot", "", after_cap, perl = TRUE)
  if (identical(rest, after_cap)) rest <- sub("(?s)^.*?\\\\endhead", "", after_cap, perl = TRUE)
  body <- sub("\\\\end\\{longtable\\}.*$", "", rest)
  body <- gsub("\\\\\\\\\\*", "\\\\\\\\", body)
  if (!grepl("\\\\bottomrule", body)) body <- paste0(trimws(body), "\n\\bottomrule")
  tail <- sub("^.*\\\\end\\{longtable\\}", "", s)
  notes <- c(regmatches(s, regexpr("(?s)\\\\begin\\{TableNotes\\}.*?\\\\end\\{TableNotes\\}", s, perl = TRUE)),
             regmatches(tail, regexpr("(?s)\\\\begin\\{singlespace\\}.*?\\\\end\\{singlespace\\}", tail, perl = TRUE)))
  tcs <- regmatches(s, regexpr("(?<=\\\\setlength\\{\\\\tabcolsep\\}\\{)[^}]+", s, perl = TRUE))
  list(head = trimws(head), body = trimws(body), spec = spec, caption = cap$text,
       label = cap$label, note = .clean_note(paste(notes, collapse = "\n\n")),
       tabcolsep = if (length(tcs)) tcs else NULL)
}

# Float with a tabular and a note inside or after it -> same list
.from_float <- function(s) {
  cap <- .get_caption(s)
  tb_s <- regexpr("\\\\begin\\{tabular\\}", s)
  tb_e <- regexpr("\\\\end\\{tabular\\}", s)
  tabular <- substring(s, tb_s, tb_e + attr(tb_e, "match.length") - 1L)
  tabular <- sub("^\\\\begin\\{tabular\\}\\[t\\]", "\\\\begin{tabular}", tabular)
  inside <- substring(s, tb_e + attr(tb_e, "match.length"))
  inside <- sub("\\\\end\\{table\\}.*$", "", inside)
  after  <- sub("(?s)^.*?\\\\end\\{table\\}", "", s, perl = TRUE)
  note   <- .clean_note(paste(inside, after, sep = "\n"))
  tcs <- regmatches(s, regexpr("(?<=\\\\setlength\\{\\\\tabcolsep\\}\\{)[^}]+", s, perl = TRUE))
  list(tabular = tabular, caption = cap$text, label = cap$label, note = note,
       tabcolsep = if (length(tcs)) tcs else NULL)
}

# --- main ----------------------------------------------------------------------
sbe_format_string <- function(s, name = "") {
  if (startsWith(s, SBE_MARK)) return(s)                      # already done
  s <- gsub("\\\\begingroup\\\\fontsize\\{[^}]*\\}\\{[^}]*\\}\\\\selectfont", "", s)
  s <- gsub("\\\\endgroup\\{\\}", "", s)
  s <- gsub("\\\\captionsetup\\{font=normalsize\\}", "", s)
  if (grepl("\\\\begin\\{longtable\\}", s)) {
    x <- .from_longtable(s)
    rows <- .short_zp(x$body)
    if (is.null(x$tabcolsep)) x$tabcolsep <- DEFAULT_TABCOLSEP
    sp <- SPLIT_TABLES[[name]]
    st <- if (grepl(TIGHT_ROWS, name)) "0.87" else NULL
    mk <- function(b, caption, label, note) {
      rows_tex <- paste0(x$head, "\n", trimws(b), "\n")
      .sbe_float(paste0("\\begin{tabular}{", x$spec, "}\n", rows_tex, "\\end{tabular}"),
                 caption, label, note, x$tabcolsep,
                 full = paste0("\\begin{tabular*}{\\linewidth}{@{\\extracolsep{\\fill}}", x$spec, "}\n",
                               rows_tex, "\\end{tabular*}"),
                 stretch = st)
    }
    if (!is.null(sp) && grepl(sp$at, rows, fixed = TRUE)) {
      cut <- regexpr(sp$at, rows, fixed = TRUE)
      b1 <- paste0(sub("\\\\midrule\\s*$", "", trimws(substring(rows, 1L, cut - 1L))), "\n\\bottomrule")
      b2 <- substring(rows, cut)
      note2 <- paste0("Construction, columns and notation as in Table~\\ref{",
                      sub("^\\\\label\\{([^}]*)\\}$", "\\1", x$label), "}.")
      cap1 <- if (is.null(sp$caption1)) x$caption else sp$caption1
      return(paste0(mk(b1, cap1, x$label, x$note), "\n",
                    mk(b2, sp$caption, paste0("\\label{", sp$label, "}"), note2)))
    }
    return(mk(rows, x$caption, x$label, x$note))
  }
  if (grepl("\\\\begin\\{table\\}", s) && !grepl("\\\\sbox\\{\\\\tabletempbox\\}", s)) {
    x <- .from_float(s)
    tab <- .short_zp(x$tabular)
    mv <- .row_to_note(tab); tab <- mv$tabular
    note <- if (nzchar(mv$note)) paste0(sub("\\.?$", ".", mv$note), " ", x$note) else x$note
    if (name %in% WRAP_HEAD) tab <- .wrap_head(tab, if (is.na(WRAP_MIN[name])) 9L else WRAP_MIN[[name]])
    tcs <- if (is.null(x$tabcolsep)) DEFAULT_TABCOLSEP else x$tabcolsep
    return(.sbe_float(tab, x$caption, x$label, note, tcs))
  }
  if (grepl("\\\\sbox\\{\\\\tabletempbox\\}", s)) {                # clean_latex layout
    s <- sub("\\\\begin\\{table\\}\\[[^]]*\\]", "\\\\begin{table}[!htbp]", s)
    if (name %in% WRAP_HEAD) {
      tb <- regexpr("(?s)\\\\begin\\{tabular\\}.*?\\\\end\\{tabular\\}", s, perl = TRUE)
      tab <- regmatches(s, tb)
      regmatches(s, tb) <- .wrap_head(tab, if (is.na(WRAP_MIN[name])) 9L else WRAP_MIN[[name]])
    }
    return(paste0(SBE_MARK, "\n", s))
  }
  s
}

sbe_format_folder <- function(dir, pattern = "^table[_0-9].*\\.tex$") {
  files <- list.files(dir, pattern = pattern, full.names = TRUE)
  n <- 0L
  for (f in files) {
    s   <- paste(readLines(f, warn = FALSE, encoding = "UTF-8"), collapse = "\n")
    out <- sbe_format_string(s, sub("\\.tex$", "", basename(f)))
    if (!identical(out, s)) { writeLines(out, f, useBytes = TRUE); n <- n + 1L }
  }
  cat(sprintf("sbe_table_format: %d of %d table files reformatted in %s\n", n, length(files), dir))
  invisible(n)
}

# Run directly: Rscript sbe_table_format.R <folder>
if (sys.nframe() == 0L) {
  a <- commandArgs(trailingOnly = TRUE)
  sbe_format_folder(if (length(a)) a[1] else ".")
}
