# =============================================================================
# MASTER PIPELINE ORCHESTRATOR                                             v1.0
#
# Runs the full dissertation analysis pipeline in the correct sequence.
# Each phase can be toggled ON/OFF via the CONFIG section below.
#
# USAGE:
#   1. Set your WORKING_DIR below
#   2. Toggle phases via the RUN_* flags
#   3. source("master_pipeline.R") from RStudio, or click "Source" in editor
#
# EXECUTION SEQUENCE:
#   Phase A  Data construction       (session-level, REQUIRED for most phases)
#   Phase B  Core alpha estimation   (produces alpha_*.xlsx files)
#   Phase C  Tables & reporting      (Table 5, Figure 2, descriptive tables)
#   Phase D  FF(2010) benchmark      (independent replication track)
#   Phase E  Sub-period analysis     (Bai-Perron + bootstrap with cache)
#   Phase F  Sorts & persistence     (portfolio sorts, alpha persistence)
#   Utility  Lipper category build   (standalone, independent)
#
# DEPENDENCIES BETWEEN PHASES:
#   Phase A -> required by B, C, D, E (subperiod), F
#   Phase B -> required by C, E (structural break reads alpha_rolling.xlsx)
#   Phase D -> required by build_ff_tables_manual (reads FF xlsx outputs)
#   Phase E -> structural_break_test MUST run before subperiod_analysis
#
# All scripts assumed to live in the WORKING_DIR alongside data files.
# =============================================================================


# =============================================================================
# CONFIG - EDIT THESE BEFORE RUNNING
# =============================================================================

# Working directory containing all .R scripts and input .xlsx files.
# Use forward slashes on both Windows and Mac.
WORKING_DIR <- "D:/TEZ/R scripts"      # PC
# WORKING_DIR <- "~/Active-Management-Puzzle/R scripts"   # Mac

# Phase toggles - set to FALSE to skip a phase
RUN_PHASE_A_DATA          <- TRUE   # data_import + flow_calculation
RUN_PHASE_B_ALPHA         <- TRUE   # alpha_estimation + aggregate_alphas
RUN_PHASE_C_REPORTING     <- TRUE   # alpha_reporting + descriptive_statistics
RUN_PHASE_D_FF_BENCHMARK  <- TRUE   # FF_comparison + build_ff_tables_manual
RUN_PHASE_E_SUBPERIODS    <- TRUE   # structural_break_test + subperiod_analysis
RUN_PHASE_F_SORTS_PERSIST <- TRUE   # portfolio_sorts + persistence_testing
RUN_UTILITY_LIPPER        <- FALSE  # build_lipper_category (rarely re-run)

# Stop on first error (TRUE) or keep going and report failures at end (FALSE)
STOP_ON_ERROR <- TRUE


# =============================================================================
# SETUP - do not edit below unless you know what you're doing
# =============================================================================

setwd(WORKING_DIR)
cat("Working directory set to:", getwd(), "\n\n")

# Tracks timing and status for each script run
pipeline_log <- list()

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
# Produces: panel_master, panel_incubation, panel_trimmed (in memory)
# Required by Phases B, C, D, E (subperiod), F
# =============================================================================
if (RUN_PHASE_A_DATA) {
  run_script("data_import_and_cleaning.R", "Phase A")
  run_script("flow_calculation.R",         "Phase A")
}


# =============================================================================
# PHASE B - CORE ALPHA ESTIMATION (Excel outputs)
# =============================================================================
# Inputs:  panel_incubation (session), panel_trimmed (session)
# Outputs: alpha_fullperiod.xlsx, alpha_rolling.xlsx, rank_data.xlsx,
#          aggregate_alphas.xlsx
# Notes:   alpha_estimation.R is the heaviest step (parallel bootstrap).
#          Uses panel_incubation per v2.6. Runtime highly dependent on cores.
# =============================================================================
if (RUN_PHASE_B_ALPHA) {
  require_session_object("panel_incubation", "alpha_estimation.R")
  run_script("alpha_estimation.R",  "Phase B")
  run_script("aggregate_alphas.R",  "Phase B")
}


# =============================================================================
# PHASE C - TABLES & REPORTING
# =============================================================================
# Inputs:  alpha_fullperiod.xlsx, alpha_rolling.xlsx, aggregate_alphas.xlsx
#          panel_incubation, panel_trimmed (session)
# Outputs: Table 5, Figure 2 (rolling), Table 8 (Lipper), descriptive tables
# =============================================================================
if (RUN_PHASE_C_REPORTING) {
  run_script("alpha_reporting.R",        "Phase C")
  run_script("descriptive_statistics.R", "Phase C")
}


# =============================================================================
# PHASE D - FAMA-FRENCH (2010) BENCHMARK REPLICATION
# =============================================================================
# Independent track: runs its own alpha estimation on panel_trimmed,
# produces portfolio-level alphas for the FF(2010)-style replication Tables
# (C.1, B.1). Does NOT touch Phase B outputs.
# Outputs: alpha_fullperiod_FF.xlsx, bootstrap_results_FF.xlsx,
#          portfolio_alphas_FF.xlsx, Tables C.1 and B.1 .tex files
# =============================================================================
if (RUN_PHASE_D_FF_BENCHMARK) {
  require_session_object("panel_trimmed", "FF_comparison.R")
  run_script("FF_comparison.R",          "Phase D")
  run_script("build_ff_tables_manual.R", "Phase D")
}


# =============================================================================
# PHASE E - SUB-PERIOD ANALYSIS
# =============================================================================
# structural_break_test.R reads alpha_rolling.xlsx (Phase B output) and
# identifies Bai-Perron breaks. Its output SUBPERIODS must be consumed by
# subperiod_analysis.R, which re-runs the FF(2010) bootstrap within each
# sub-period using the SHA-1 cache (~30s on hit vs ~30min cold).
# =============================================================================
if (RUN_PHASE_E_SUBPERIODS) {
  require_session_object("panel_incubation", "subperiod_analysis.R")
  run_script("structural_break_test.R", "Phase E")
  run_script("subperiod_analysis.R",    "Phase E")
}


# =============================================================================
# PHASE F - PORTFOLIO SORTS & PERSISTENCE
# =============================================================================
# portfolio_sorts.R produces decile-sorted portfolio alphas (H2/H3 tests).
# persistence_testing.R runs alpha persistence regressions (rolling decile
# winners vs losers); heavy bootstrap, uses SHA-1 cache.
# =============================================================================
if (RUN_PHASE_F_SORTS_PERSIST) {
  require_session_object("panel_incubation", "portfolio_sorts.R / persistence_testing.R")
  run_script("portfolio_sorts.R",     "Phase F")
  run_script("persistence_testing.R", "Phase F")
}


# =============================================================================
# UTILITY - LIPPER CATEGORY BUILDER (standalone)
# =============================================================================
# Reads lipper.xlsx and builds lipper_category_output.xlsx.
# Independent of all other phases; only re-run when the raw Lipper mapping
# changes.
# =============================================================================
if (RUN_UTILITY_LIPPER) {
  run_script("build_lipper_category.R", "Utility")
}


# =============================================================================
# PIPELINE SUMMARY
# =============================================================================
pipeline_end <- Sys.time()
total_mins <- round(as.numeric(difftime(pipeline_end, pipeline_start, units = "mins")), 1)

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
    cat(sprintf("\nWARNING: %d script(s) failed. Review errors above.\n", n_failed))
  } else {
    cat("\nAll scripts completed successfully.\n")
  }
} else {
  cat("No phases were run. Check your RUN_* toggles in CONFIG.\n")
}

cat(sprintf("%s\n", strrep("=", 79)))
