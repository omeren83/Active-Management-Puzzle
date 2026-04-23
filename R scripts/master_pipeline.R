# =============================================================================
# MASTER PIPELINE ORCHESTRATOR                                             v1.1
#
# Runs the full dissertation analysis pipeline in the correct sequence.
# Each phase can be toggled ON/OFF via the CONFIG section below.
#
# v1.1 changes vs v1.0:
#   - Added Phase G (factor model robustness): alpha_estimation_robust.R +
#     build_robust_tables.R for Appendix E (FF6 and Carhart+PSL specifications).
#   - Added TABLES_OUT_DIR config variable and automatic post-pipeline sync
#     step that copies all table_*.tex files from WORKING_DIR (Drive folder)
#     to TABLES_OUT_DIR (GitHub repo / Overleaf sync folder). Eliminates the
#     manual copy-paste step previously required between the two folders.
#
# USAGE:
#   1. Set WORKING_DIR and TABLES_OUT_DIR below (per machine).
#   2. Toggle phases via the RUN_* flags.
#   3. source("master_pipeline.R") from RStudio, or click "Source" in editor.
#
# EXECUTION SEQUENCE:
#   Phase A  Data construction           (session-level, REQUIRED for most)
#   Phase B  Core alpha estimation       (produces alpha_*.xlsx files)
#   Phase C  Tables & reporting          (Table 5, Figure 2, descriptive)
#   Phase D  FF(2010) benchmark          (independent replication track)
#   Phase E  Sub-period analysis         (Bai-Perron + bootstrap with cache)
#   Phase F  Sorts & persistence         (portfolio sorts, alpha persistence)
#   Phase G  Factor robustness (NEW)     (FF6 + Carhart+PSL for Appendix E)
#   Utility  Lipper category build       (standalone, independent)
#   SYNC     Copy .tex files -> GitHub   (automatic at end if TABLES_OUT_DIR set)
#
# DEPENDENCIES BETWEEN PHASES:
#   Phase A -> required by B, C, D, E (subperiod), F, G
#   Phase B -> required by C, E (structural break reads alpha_rolling.xlsx),
#              G (build_robust_tables reads Carhart baseline xlsx)
#   Phase D -> required by build_ff_tables_manual (reads FF xlsx outputs)
#   Phase E -> structural_break_test MUST run before subperiod_analysis
#   Phase G -> alpha_estimation_robust MUST run before build_robust_tables
#
# All scripts assumed to live in WORKING_DIR alongside data files.
# =============================================================================


# =============================================================================
# CONFIG - EDIT THESE BEFORE RUNNING
# =============================================================================

# Working directory containing all .R scripts and input .xlsx files.
# Use forward slashes on both Windows and Mac.
WORKING_DIR <- "G:/Drive'ım/TEZ-YENI/data/R import"                     # PC Drive folder
# WORKING_DIR <- "/Users/omersmba/Library/CloudStorage/GoogleDrive-omer.eren.2019@gmail.com/Drive'ım/TEZ-YENI/data/R import"  # Mac Drive folder


# Target directory for LaTeX table files (will be synced automatically from
# WORKING_DIR after the pipeline finishes). Should be the tables/ subfolder
# of the GitHub repo that syncs with Overleaf.
# Set to NA to disable auto-sync (tables will stay in WORKING_DIR only).
TABLES_OUT_DIR <- "D:/TEZ/tables"                      # PC GitHub repo
# TABLES_OUT_DIR <- "~/Active-Management-Puzzle/tables"  # Mac GitHub clone
# TABLES_OUT_DIR <- NA                                   # disable auto-sync

# Phase toggles - set to FALSE to skip a phase
RUN_PHASE_A_DATA          <- TRUE   # data_import + flow_calculation
RUN_PHASE_B_ALPHA         <- FALSE   # alpha_estimation + aggregate_alphas
RUN_PHASE_C_REPORTING     <- FALSE   # alpha_reporting + descriptive_statistics
RUN_PHASE_D_FF_BENCHMARK  <- FALSE   # FF_comparison + build_ff_tables_manual
RUN_PHASE_E_SUBPERIODS    <- TRUE   # structural_break_test + subperiod_analysis
RUN_PHASE_F_SORTS_PERSIST <- TRUE   # portfolio_sorts + persistence_testing
RUN_PHASE_G_FACTOR_ROBUST <- FALSE  # alpha_estimation_robust + build_robust_tables
RUN_UTILITY_LIPPER        <- FALSE  # build_lipper_category (rarely re-run)

# Stop on first error (TRUE) or keep going and report failures at end (FALSE)
STOP_ON_ERROR <- TRUE


# =============================================================================
# SETUP - do not edit below unless you know what you're doing
# =============================================================================

setwd(WORKING_DIR)
cat("Working directory set to:", getwd(), "\n")
if (!is.na(TABLES_OUT_DIR)) {
  cat("Tables will be synced to:", TABLES_OUT_DIR, "\n")
}
cat("\n")

# Tracks timing and status for each script run
pipeline_log <- list()

# Snapshot of .tex files BEFORE the pipeline runs, used by the sync step at
# end to identify which .tex files are new / modified.
tex_snapshot_before <- {
  files <- list.files(WORKING_DIR, pattern = "^table_.*\\.tex$", full.names = FALSE)
  mtimes <- file.info(file.path(WORKING_DIR, files))$mtime
  setNames(mtimes, files)
}

# Helper: source a script with timing and error handling
run_script <- function(script_name, phase_label) {
  cat(sprintf("\n%s\n", strrep("=", 79)))
  cat(sprintf("[%s]  %s\n", phase_label, script_name))
  cat(sprintf("%s\n", strrep("=", 79)))
  
  t0 <- Sys.time()
  ok <- tryCatch({
    source(script_name, echo = FALSE, max.deparse.length = Inf)
    TRUE
  }, error = function(e) {
    cat("\nERROR in", script_name, ":\n")
    cat(conditionMessage(e), "\n")
    if (STOP_ON_ERROR) stop(e)
    FALSE
  })
  t1 <- Sys.time()
  dt <- round(as.numeric(difftime(t1, t0, units = "secs")), 1)
  
  pipeline_log[[script_name]] <<- list(
    phase   = phase_label,
    status  = if (ok) "OK" else "FAILED",
    seconds = dt
  )
  cat(sprintf("\n-> %s (%.1fs)\n", if (ok) "OK" else "FAILED", dt))
  invisible(ok)
}

# Helper: verify a session object exists before proceeding to dependent phases
require_session_object <- function(obj_name, needed_by) {
  if (!exists(obj_name, envir = .GlobalEnv)) {
    msg <- sprintf("Required session object '%s' not found. Needed by %s. Run Phase A first.",
                   obj_name, needed_by)
    if (STOP_ON_ERROR) stop(msg) else cat("WARNING:", msg, "\n")
    return(FALSE)
  }
  TRUE
}

pipeline_start <- Sys.time()


# =============================================================================
# PHASE A - DATA CONSTRUCTION (session-level panels)
# =============================================================================
if (RUN_PHASE_A_DATA) {
  run_script("data_import_and_cleaning.R", "Phase A")
  run_script("flow_calculation.R",         "Phase A")
}


# =============================================================================
# PHASE B - CORE ALPHA ESTIMATION (Excel outputs)
# =============================================================================
if (RUN_PHASE_B_ALPHA) {
  require_session_object("panel_incubation", "alpha_estimation.R")
  run_script("alpha_estimation.R",  "Phase B")
  run_script("aggregate_alphas.R",  "Phase B")
}


# =============================================================================
# PHASE C - TABLES & REPORTING
# =============================================================================
if (RUN_PHASE_C_REPORTING) {
  run_script("alpha_reporting.R",        "Phase C")
  run_script("descriptive_statistics.R", "Phase C")
}


# =============================================================================
# PHASE D - FAMA-FRENCH (2010) BENCHMARK REPLICATION
# =============================================================================
if (RUN_PHASE_D_FF_BENCHMARK) {
  require_session_object("panel_trimmed", "FF_comparison.R")
  run_script("FF_comparison.R",          "Phase D")
  run_script("build_ff_tables_manual.R", "Phase D")
}


# =============================================================================
# PHASE E - SUB-PERIOD ANALYSIS
# =============================================================================
if (RUN_PHASE_E_SUBPERIODS) {
  require_session_object("panel_incubation", "subperiod_analysis.R")
  run_script("structural_break_test.R", "Phase E")
  run_script("subperiod_analysis.R",    "Phase E")
}


# =============================================================================
# PHASE F - PORTFOLIO SORTS & PERSISTENCE
# =============================================================================
if (RUN_PHASE_F_SORTS_PERSIST) {
  require_session_object("panel_incubation", "portfolio_sorts.R / persistence_testing.R")
  run_script("portfolio_sorts.R",     "Phase F")
  run_script("persistence_testing.R", "Phase F")
}


# =============================================================================
# PHASE G - FACTOR MODEL ROBUSTNESS  (Appendix E)
# =============================================================================
# alpha_estimation_robust.R (v1.1) re-estimates full-period alphas, FF(2010)
# bootstrap, and BSW decomposition under two alternative factor specifications:
#   FF6 : MKT_RF + SMB + HML + RMW + CMA + MOM
#   C5  : MKT_RF + SMB + HML + MOM + PSL (Pastor-Stambaugh traded liquidity)
# Inputs:  panel_incubation (session); Carhart baseline xlsx files from Phase B
#          (alpha_fullperiod.xlsx, bootstrap_results.xlsx).
# Outputs: alpha_fullperiod_{FF6,C5}.xlsx, bootstrap_results_{FF6,C5}.xlsx,
#          robust_alpha_summary.xlsx.  build_robust_tables.R then consumes
#          these and writes three Appendix E .tex files directly to
#          TABLES_OUT_DIR (bypassing the sync step below).
# Runtime: ~60-90 minutes total (2 x bootstrap cost, ~30-45 min each spec).
# Prerequisite: RMW, CMA, PSL rows must be present in the 'factors' sheet of
# fund_data.xlsx before running Phase A in the same session.
# =============================================================================
if (RUN_PHASE_G_FACTOR_ROBUST) {
  require_session_object("panel_incubation",
                         "alpha_estimation_robust.R")
  # Pre-flight: Carhart baseline xlsx files must exist (Phase B must have run).
  baseline_ok <- file.exists("alpha_fullperiod.xlsx") &&
    file.exists("bootstrap_results.xlsx")
  if (!baseline_ok) {
    msg <- paste("Phase G requires Carhart baseline xlsx files",
                 "(alpha_fullperiod.xlsx, bootstrap_results.xlsx).",
                 "Run Phase B first.")
    if (STOP_ON_ERROR) stop(msg) else cat("WARNING:", msg, "\n")
  } else {
    run_script("alpha_estimation_robust.R", "Phase G")
    # Direct the Appendix E tables straight to TABLES_OUT_DIR so they don't
    # need to go through the sync step (which would be a no-op for them
    # anyway, but cleaner to avoid the round-trip).
    if (!is.na(TABLES_OUT_DIR)) {
      OUT_DIR <- TABLES_OUT_DIR
    }  # else OUT_DIR falls back to "./tables_robust" default in the script
    run_script("build_robust_tables.R", "Phase G")
  }
}


# =============================================================================
# UTILITY - LIPPER CATEGORY BUILDER (standalone)
# =============================================================================
if (RUN_UTILITY_LIPPER) {
  run_script("build_lipper_category.R", "Utility")
}


# =============================================================================
# SYNC - COPY NEW / MODIFIED .tex FILES TO TABLES_OUT_DIR
# =============================================================================
# All main-text and FF appendix .tex files are written by individual scripts
# to WORKING_DIR (the current R working directory). This step copies every
# table_*.tex file that is new or has been modified during this pipeline run
# to TABLES_OUT_DIR (the GitHub repo tables/ folder, which syncs to Overleaf).
# Robustness tables from Phase G are excluded from this step because they
# were written directly to TABLES_OUT_DIR.
# =============================================================================
if (!is.na(TABLES_OUT_DIR)) {
  cat(sprintf("\n%s\n", strrep("=", 79)))
  cat("SYNC  Copy table_*.tex files -> TABLES_OUT_DIR\n")
  cat(sprintf("%s\n", strrep("=", 79)))
  
  # Ensure target exists.
  if (!dir.exists(TABLES_OUT_DIR)) {
    dir.create(TABLES_OUT_DIR, recursive = TRUE, showWarnings = FALSE)
    cat("Created target directory:", TABLES_OUT_DIR, "\n")
  }
  
  # Scan WORKING_DIR for table_*.tex files now.  Compare mtimes against the
  # pre-pipeline snapshot to find those that were created or modified.
  tex_now <- list.files(WORKING_DIR, pattern = "^table_.*\\.tex$",
                        full.names = FALSE)
  to_copy <- character(0)
  for (f in tex_now) {
    mt_now <- file.info(file.path(WORKING_DIR, f))$mtime
    mt_before <- tex_snapshot_before[f]
    if (is.na(mt_before) || mt_now > mt_before) {
      to_copy <- c(to_copy, f)
    }
  }
  
  if (length(to_copy) == 0L) {
    cat("No new or modified table_*.tex files to sync.\n")
  } else {
    cat(sprintf("Syncing %d file(s) to %s:\n", length(to_copy), TABLES_OUT_DIR))
    sync_results <- vapply(to_copy, function(f) {
      src <- file.path(WORKING_DIR, f)
      dst <- file.path(TABLES_OUT_DIR, f)
      ok  <- file.copy(src, dst, overwrite = TRUE, copy.date = TRUE)
      cat(sprintf("  %s  %s\n", if (ok) "OK" else "FAIL", f))
      ok
    }, logical(1))
    n_ok <- sum(sync_results)
    cat(sprintf("Sync complete: %d/%d file(s) copied.\n",
                n_ok, length(sync_results)))
  }
} else {
  cat("\nSYNC disabled (TABLES_OUT_DIR is NA). Copy tables manually if needed.\n")
}


# =============================================================================
# PIPELINE SUMMARY
# =============================================================================
pipeline_end <- Sys.time()
total_mins <- round(as.numeric(difftime(pipeline_end, pipeline_start,
                                        units = "mins")), 1)

cat(sprintf("\n\n%s\n", strrep("=", 79)))
cat("PIPELINE SUMMARY\n")
cat(sprintf("%s\n", strrep("=", 79)))
cat(sprintf("Total runtime: %.1f minutes\n\n", total_mins))

if (length(pipeline_log) > 0) {
  log_df <- do.call(rbind, lapply(names(pipeline_log), function(s) {
    x <- pipeline_log[[s]]
    data.frame(script  = s,
               phase   = x$phase,
               status  = x$status,
               seconds = x$seconds,
               stringsAsFactors = FALSE)
  }))
  print(log_df, row.names = FALSE)
  
  n_failed <- sum(log_df$status == "FAILED")
  if (n_failed > 0) {
    cat(sprintf("\nWARNING: %d script(s) failed. Review errors above.\n",
                n_failed))
  } else {
    cat("\nAll scripts completed successfully.\n")
  }
} else {
  cat("No phases were run. Check your RUN_* toggles in CONFIG.\n")
}

cat(sprintf("%s\n", strrep("=", 79)))