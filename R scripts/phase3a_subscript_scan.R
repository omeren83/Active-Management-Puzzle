# =============================================================================
# phase3a_subscript_scan.R  --  Phase 3.A SAFETY-SCAN script  (v1.1, R port)
#
# Purpose:
#   Detect any remaining raw multi-letter math subscripts/superscripts in
#   table .tex files that are NOT wrapped in \text{} or \mathrm{}.
#
# Patch 1 (preamble \renewcommand{\mathrm}{\text}) already neutralises any
#   \mathrm{...} occurrences. This scan catches the OTHER residual pattern:
#   raw uppercase-led subscripts/superscripts that produce math-italic
#   multi-letter sequences (e.g. $\alpha_{CAPM}$, $\beta_{MKT}$,
#   $R^{LOW}$, $D^{SENT}$).
#
# How to run (PC, from RStudio):
#
#   Option A -- RStudio CONSOLE (recommended; matches your R-only convention):
#     setwd("D:/TEZ")
#     source("R scripts/phase3a_subscript_scan.R")
#     scan_tables("tables")
#
#   Option B -- RStudio TERMINAL (Git Bash), command-line:
#     cd /d/TEZ
#     Rscript "R scripts/phase3a_subscript_scan.R" tables
#
# Base R only. No package installation required.
# =============================================================================


# -----------------------------------------------------------------------------
# Acronym heuristic
#   Returns TRUE iff `s` is a content string that should be wrapped in \text{}.
#   Rules: at least one uppercase letter AND total letter count >= 2.
#   Rejects pure digits, pure lowercase, single-letter content.
# -----------------------------------------------------------------------------
.is_real_acronym <- function(s) {
  s <- trimws(s)
  if (!nzchar(s))               return(FALSE)
  if (grepl("^[0-9]+$", s))     return(FALSE)   # pure digits e.g. _{20}
  if (!grepl("[A-Z]", s))       return(FALSE)   # no uppercase letter
  letters_only <- gsub("[^A-Za-z]", "", s)
  nchar(letters_only) >= 2L                     # need 2+ letters
}


# -----------------------------------------------------------------------------
# Single-file scanner
#   Returns a data.frame with columns: line, match, content (zero rows if no
#   findings).
# -----------------------------------------------------------------------------
.phase3a_scan_file <- function(path) {

  # Match _{...} or ^{...} where the contents do not themselves contain a
  # closing brace. This handles the typical 1-level subscript/superscript
  # form found in our tables. For nested cases the captured content is
  # everything up to the first }, which is sufficient: we then check whether
  # the captured content STARTS with \text{ or \mathrm{, and skip if so.
  rx_outer <- "([_\\^])\\{([^}]+)\\}"

  lines <- tryCatch(
    readLines(path, warn = FALSE, encoding = "UTF-8"),
    error = function(e) { warning(sprintf("Cannot read %s: %s", path, e$message)); NULL }
  )
  if (is.null(lines)) {
    return(data.frame(line = integer(), match = character(),
                      content = character(), stringsAsFactors = FALSE))
  }

  findings <- list()

  for (i in seq_along(lines)) {
    m <- gregexpr(rx_outer, lines[i], perl = TRUE)[[1]]
    if (m[1] == -1L) next

    ml   <- attr(m, "match.length")
    cs   <- attr(m, "capture.start")
    cl   <- attr(m, "capture.length")
    full <- substring(lines[i], m, m + ml - 1L)
    cont <- substring(lines[i], cs[, 2], cs[, 2] + cl[, 2] - 1L)

    for (k in seq_along(full)) {
      content <- cont[k]

      # Filter 1: skip if content begins with \text{ or \mathrm{
      # (the wrap we want is already in place).
      if (grepl("^\\\\(?:text|mathrm)\\{", content, perl = TRUE)) next

      # Filter 2: acronym heuristic.
      if (!.is_real_acronym(content)) next

      findings[[length(findings) + 1L]] <- data.frame(
        line    = i,
        match   = full[k],
        content = content,
        stringsAsFactors = FALSE
      )
    }
  }

  if (!length(findings)) {
    return(data.frame(line = integer(), match = character(),
                      content = character(), stringsAsFactors = FALSE))
  }
  do.call(rbind, findings)
}


# -----------------------------------------------------------------------------
# Directory walker -- the function you call from the Console
# -----------------------------------------------------------------------------
scan_tables <- function(directory = "tables", ext = ".tex") {

  if (!dir.exists(directory)) {
    stop("Not a directory: ", normalizePath(directory, mustWork = FALSE))
  }

  pattern   <- paste0("\\", ext, "$")
  tex_files <- sort(list.files(directory, pattern = pattern, full.names = TRUE))

  cat(sprintf("Scanning %d files in %s\n\n",
              length(tex_files), normalizePath(directory)))

  if (!length(tex_files)) {
    cat(sprintf("No %s files found.\n", ext))
    return(invisible(list(n_findings = 0L,
                          n_files_with_findings = 0L,
                          n_files_scanned = 0L,
                          files = list())))
  }

  total_findings      <- 0L
  files_with_findings <- 0L
  per_file            <- list()

  for (tex in tex_files) {
    df <- .phase3a_scan_file(tex)
    if (!nrow(df)) next

    files_with_findings <- files_with_findings + 1L
    total_findings      <- total_findings + nrow(df)
    per_file[[basename(tex)]] <- df

    cat(sprintf("-- %s --\n", basename(tex)))
    for (k in seq_len(nrow(df))) {
      preview <- df$match[k]
      if (nchar(preview) > 60L) preview <- paste0(substr(preview, 1L, 57L), "...")
      cat(sprintf("  L%5d  %s\n", df$line[k], preview))
    }
    cat("\n")
  }

  bar <- paste(rep("=", 64L), collapse = "")
  cat(sprintf("%s\n", bar))
  cat(sprintf("Scan complete: %d finding(s) across %d file(s) (of %d scanned).\n",
              total_findings, files_with_findings, length(tex_files)))
  cat(sprintf("%s\n", bar))

  if (total_findings == 0L) {
    cat("\nResult: NO raw multi-letter sub/superscripts found.\n")
    cat("Patch 1 alone resolves the math-heaviness issue for all tables.\n")
  } else {
    cat("\nResult: per-file edits or R-script regen prevention REQUIRED.\n")
    cat("Reported items are NOT wrapped in \\text{} or \\mathrm{}. With\n")
    cat("Patch 1 applied, \\mathrm{} is aliased to \\text{}, so any rendering\n")
    cat("relying on \\mathrm{} is already corrected; the items above are bare\n")
    cat("math italic and render as a product of single italic letters.\n")
  }

  invisible(list(n_findings            = total_findings,
                 n_files_with_findings = files_with_findings,
                 n_files_scanned       = length(tex_files),
                 files                 = per_file))
}


# -----------------------------------------------------------------------------
# Rscript entry point (only fires when invoked as `Rscript ... tables`)
# -----------------------------------------------------------------------------
if (!interactive() && length(commandArgs(trailingOnly = TRUE))) {
  .args <- commandArgs(trailingOnly = TRUE)
  scan_tables(directory = .args[1])
}
