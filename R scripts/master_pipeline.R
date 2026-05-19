# =============================================================================
# MASTER PIPELINE ORCHESTRATOR                                             v1.4
#
# Runs the full dissertation analysis pipeline in the correct sequence.
# Each phase can be toggled ON/OFF via the CONFIG section below.
#
# v1.5 (May 8, 2026): Sync block extended to also copy fig_*.png files. 
# Architecture is now: every script writes table_*.tex and fig_*.png to WORKING_DIR; 
# sync step copies both classes to TABLES_OUT_DIR.
#
# v1.4 changes vs v1.3:
#   - Phase J now runs psychological_premium.R after panel_regressions_-
#     reporting.R. Computes shadow prices (proposal eq. 12) from the joint
#     H1+H3 estimation, with delta-method SEs and parametric MVN bootstrap.
#     Output: table_psychological_premium.tex.
#   - Phase J now expects panel_regressions_setup.R (>= v1.3) to populate
#     MAX12 in panel_reg as a permanent column. H3 v2.0 uses MAX12 across
#     columns; PPF script also requires it.
#
# v1.3 changes vs v1.2:
#   - Phase J: smart fallback. If panel_incubation + behavioral_state_vars
#     are present in the session, panel_regressions_setup.R runs as usual.
#     If not but panel_reg.rds exists in WORKING_DIR, the setup is SKIPPED
#     and H1-H4 load the panel via their built-in .rds fallback. This lets
#     a fresh RStudio session re-run the regression layer without first
#     re-running Phase A + Phase I, provided panel_reg.rds is current.
#   - Added FORCE_PANEL_SETUP CONFIG flag to override the fallback (forces
#     a fresh setup run even if panel_reg.rds exists; needed when upstream
#     panels have changed since the last setup).
#
# v1.2 changes vs v1.1:
#   - Added Phase I (behavioral state variables): behavioral_state_variables.R
#     produces behavioral_state_vars.xlsx (sentiment, margin debt, regime
#     dummies) used by H1-H4 panel regressions.
#   - Added Phase J (panel regressions): panel_regressions_setup.R +
#     H1_sentiment_convexity.R + H2_disposition_control.R +
#     H3_lottery_demand.R + H4_fee_elasticity.R + panel_regressions_reporting.R.
#     Default OFF until the underlying scripts are written.
#   - run_script() now checks file existence and skips with a warning rather
#     than erroring if a script file is missing (needed because Phase J
#     references scripts that may not exist yet on disk).
#
# v1.1 changes vs v1.0:
#   - Added Phase H (factor model robustness): alpha_estimation_robust.R +
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
#   Phase G  Activeness & performance    (Relationship between degree of activeness and future performance)
#   Phase H  Factor robustness           (FF6 + Carhart+PSL for Appendix E)
#   Phase I  Behavioral state variables  (sentiment, margin debt, regime dummies)
#   Phase J  Panel regressions           (setup + H1-H4 + reporting)
#   Utility  Lipper category build       (standalone, independent)
#   SYNC     Copy .tex .png files -> GitHub   (automatic at end if TABLES_OUT_DIR set)
#
# DEPENDENCIES BETWEEN PHASES:
#   Phase A -> required by B, C, D, E (subperiod), F, G, H, J
#   Phase B -> required by C, E (structural break reads alpha_rolling.xlsx),
#              G (activeness uses alpha_rolling),
#              H (build_robust_tables reads Carhart baseline xlsx),
#              J (panel_regressions_setup uses alpha_rolling for activeness)
#   Phase D -> required by build_ff_tables_manual (reads FF xlsx outputs)
#   Phase E -> structural_break_test MUST run before subperiod_analysis
#   Phase H -> alpha_estimation_robust MUST run before build_robust_tables
#   Phase I -> required by J (behavioral_state_vars merged into panel_reg)
#   Phase J -> internal order is enforced: setup -> H1 -> H2 -> H3 -> H4 -> reporting
#
# All scripts assumed to live in WORKING_DIR alongside data files.
# =============================================================================


# =============================================================================
# CONFIG - EDIT THESE BEFORE RUNNING
# =============================================================================

# Working directory = where DATA lives (input xlsx, cached rds, output xlsx/tex).
# Scripts directory = where the .R files live (git-tracked repo clone).
# These are separated so scripts can live in a Git-connected folder while data
# continues to live in the Google Drive folder (the two are no longer required
# to be the same location).
# Use forward slashes on both Windows and Mac.
WORKING_DIR <- "D:/TEZ/data/R import"
# WORKING_DIR <- "/Users/omersmba/Library/CloudStorage/GoogleDrive-omer.eren.2019@gmail.com/Drive'ım/TEZ-YENI/data/R import"  # Mac Drive folder (data)

 SCRIPTS_DIR <- "D:/TEZ/R scripts"                                       # PC repo clone (scripts)
#SCRIPTS_DIR <- "~/Active-Management-Puzzle/R scripts"                   # Mac repo clone (scripts)


# Target directory for LaTeX table files (will be synced automatically from
# WORKING_DIR after the pipeline finishes). Should be the tables/ subfolder
# of the GitHub repo that syncs with Overleaf.
# Set to NA to disable auto-sync (tables will stay in WORKING_DIR only).
  TABLES_OUT_DIR <- "D:/TEZ/tables"                      # PC GitHub repo
# TABLES_OUT_DIR <- "~/Active-Management-Puzzle/tables"  # Mac GitHub clone
# TABLES_OUT_DIR <- NA                                   # disable auto-sync

# Phase toggles - set to FALSE to skip a phase
RUN_PHASE_A_DATA          <- FALSE    # data_import + flow_calculation
RUN_PHASE_B_ALPHA         <- FALSE   # alpha_estimation + aggregate_alphas
RUN_PHASE_C_REPORTING     <- TRUE   # alpha_reporting + descriptive_statistics
RUN_PHASE_D_FF_BENCHMARK  <- FALSE   # FF_comparison + build_ff_tables_manual
RUN_PHASE_E_SUBPERIODS    <- FALSE    # structural_break_test + subperiod_analysis
RUN_PHASE_F_SORTS_PERSIST <- FALSE   # portfolio_sorts + persistence_testing
RUN_PHASE_G_ACTIVENESS    <- FALSE   # activeness_analysis
RUN_PHASE_H_FACTOR_ROBUST <- FALSE   # alpha_estimation_robust + build_robust_tables
RUN_PHASE_I_BEHAVIORAL    <- FALSE   # behavioral_state_variables (NEW)
RUN_PHASE_J_PANEL_REG     <- TRUE    # panel_regressions_setup + H1..H4 + reporting (NEW; OFF until scripts exist)
RUN_UTILITY_LIPPER        <- FALSE   # build_lipper_category (rarely re-run)

# Phase J sub-toggle: force re-running panel_regressions_setup.R even if
# panel_reg.rds is on disk. Default FALSE = use cached panel_reg.rds when
# upstream session objects are not available. Set TRUE when upstream panels
# (panel_incubation, behavioral_state_vars) have changed since the last
# setup run, so the cached panel_reg.rds is stale.
FORCE_PANEL_SETUP         <- FALSE

# Stop on first error (TRUE) or keep going and report failures at end (FALSE)
STOP_ON_ERROR <- TRUE


# =============================================================================
# SETUP - do not edit below unless you know what you're doing
# =============================================================================

setwd(WORKING_DIR)
if (!dir.exists(SCRIPTS_DIR)) {
  stop("SCRIPTS_DIR does not exist: ", SCRIPTS_DIR,
       "\nCheck the path and the comment/uncomment toggle for your machine.")
}
cat("Working directory set to:", getwd(), "\n")
cat("Scripts directory      :", normalizePath(SCRIPTS_DIR), "\n")
if (!is.na(TABLES_OUT_DIR)) {
  cat("Tables will be synced to:", TABLES_OUT_DIR, "\n")
}
cat("\n")

# Tracks timing and status for each script run
pipeline_log <- list()

# Snapshot of artifacts BEFORE the pipeline runs, used by the sync step at
# end to identify which files are new / modified. Two file classes are tracked:
#   table_*.tex  (LaTeX tables)
#   fig_*.png    (annotated figures)
.snapshot_files <- function(pattern) {
  files  <- list.files(WORKING_DIR, pattern = pattern, full.names = FALSE)
  mtimes <- file.info(file.path(WORKING_DIR, files))$mtime
  setNames(mtimes, files)
}
tex_snapshot_before <- .snapshot_files("^table_.*\\.tex$")
fig_snapshot_before <- .snapshot_files("^fig_.*\\.png$")

# Helper: source a script with timing and error handling.
# Skips gracefully (warning, not error) if the script file is missing.
run_script <- function(script_name, phase_label) {
  cat(sprintf("\n%s\n", strrep("=", 79)))
  cat(sprintf("[%s]  %s\n", phase_label, script_name))
  cat(sprintf("%s\n", strrep("=", 79)))
  
  script_path <- file.path(SCRIPTS_DIR, script_name)
  if (!file.exists(script_path)) {
    cat("\nSKIPPED (file not found):", script_path, "\n")
    pipeline_log[[script_name]] <<- list(
      phase = phase_label, status = "SKIPPED", seconds = 0
    )
    return(invisible(FALSE))
  }
  
  t0 <- Sys.time()
  ok <- tryCatch({
    source(script_path, echo = FALSE, max.deparse.length = Inf)
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

# =============================================================================
# PRE-FLIGHT INTEGRITY CHECK
# Verifies each pipeline script's first content line matches its expected
# topic. Catches silent corruption like the 15 May 2026 incident, where
# descriptive_statistics.R content was accidentally saved into
# aggregate_alphas.R during a copy-paste, producing a clean-looking pipeline
# run with stale aggregate_alphas.xlsx (Table 4.5, Table F.1, Fig 2 all
# reported pre-v1.3 numbers).
#
# Keyword matching is case-sensitive and uses fixed strings; if a script's
# header convention changes, update the SCRIPT_INTEGRITY map below.
# Set RUN_INTEGRITY_CHECK <- FALSE in the config block to disable.
# =============================================================================
if (!exists("RUN_INTEGRITY_CHECK")) RUN_INTEGRITY_CHECK <- TRUE

SCRIPT_INTEGRITY <- c(
  "data_import_and_cleaning.R"    = "FUND DATA IMPORT",
  "flow_calculation.R"            = "FUND FLOW",
  "alpha_estimation.R"            = "ALPHA ESTIMATION",
  "alpha_estimation_robust.R"     = "ROBUSTNESS ALPHA",
  "aggregate_alphas.R"            = "AGGREGATE PORTFOLIO ALPHAS",
  "alpha_reporting.R"             = "PERFORMANCE REPORTING",
  "descriptive_statistics.R"      = "DESCRIPTIVE STATISTICS",
  "portfolio_sorts.R"             = "PORTFOLIO SORTS",
  "persistence_testing.R"         = "PERSISTENCE TESTING",
  "activeness_analysis.R"         = "ACTIVENESS ANALYSIS",
  "activeness_persistence.R"      = "ACTIVENESS-CONDITIONED PERSISTENCE",
  "subperiod_analysis.R"          = "SUB-PERIOD ANALYSIS",
  "structural_break_test.R"       = "STRUCTURAL BREAK",
  "FF_comparison.R"               = "FAMA-FRENCH",
  "build_ff_tables_manual.R"      = "MANUAL FF TABLES",
  "build_robust_tables.R"         = "ROBUST APPENDIX",
  "build_lipper_category.R"       = "LIPPER CATEGORY",
  "panel_regressions_setup.R"     = "panel_regressions_setup",
  "panel_regressions_reporting.R" = "panel_regressions_reporting",
  "behavioral_state_variables.R"  = "behavioral_state_variables",
  "H1_sentiment_convexity.R"      = "H1_sentiment_convexity",
  "H2_disposition_control.R"      = "H2_disposition_control",
  "H3_lottery_demand.R"           = "H3_lottery_demand",
  "H4_fee_elasticity.R"           = "H4_fee_elasticity",
  "psychological_premium.R"       = "psychological_premium"
)

verify_pipeline_integrity <- function(scripts_dir = SCRIPTS_DIR,
                                      expected    = SCRIPT_INTEGRITY) {
  fails   <- character(0)
  checked <- 0L
  for (script in names(expected)) {
    path <- file.path(scripts_dir, script)
    if (!file.exists(path)) next     # tolerate missing optional scripts
    checked <- checked + 1L
    lines <- readLines(path, n = 10, warn = FALSE)
    # First non-empty, non-border (===) content line.
    content_lines <- lines[!grepl("^# *=+ *$|^ *$|^# *$", lines)]
    if (length(content_lines) == 0L) {
      fails <- c(fails,
                 sprintf("  %s -> no content header found in first 10 lines", script))
      next
    }
    keyword       <- expected[[script]]
    first_content <- content_lines[1]
    if (!grepl(keyword, first_content, fixed = TRUE)) {
      fails <- c(fails, sprintf(
        "  %s\n      expected keyword: '%s'\n      found header:     %s",
        script, keyword, trimws(sub("^# ", "", first_content))
      ))
    }
  }
  if (length(fails) > 0L) {
    stop(
      "\n\n*** PIPELINE INTEGRITY CHECK FAILED ***\n",
      "The following script(s) have content not matching their expected\n",
      "purpose. This usually means a file was overwritten with content\n",
      "from another script (silent clobber, no error at save time).\n",
      "Fix the file(s) before re-running the pipeline.\n\n",
      paste(fails, collapse = "\n\n"),
      "\n"
    )
  }
  cat(sprintf("\n[OK] Pipeline integrity check: %d/%d scripts verified.\n",
              checked, length(expected)))
  invisible(TRUE)
}

if (RUN_INTEGRITY_CHECK) verify_pipeline_integrity()

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
  run_script("alpha_reporting.R",         "Phase C")
  run_script("descriptive_statistics.R",  "Phase C")
}


# =============================================================================
# PHASE D - FF(2010) BENCHMARK
# =============================================================================
if (RUN_PHASE_D_FF_BENCHMARK) {
  run_script("FF_comparison.R",           "Phase D")
  run_script("build_ff_tables_manual.R",  "Phase D")
}


# =============================================================================
# PHASE E - SUB-PERIOD ANALYSIS
# =============================================================================
if (RUN_PHASE_E_SUBPERIODS) {
  run_script("structural_break_test.R", "Phase E")
  run_script("subperiod_analysis.R",    "Phase E")
}


# =============================================================================
# PHASE F - SORTS & PERSISTENCE
# =============================================================================
if (RUN_PHASE_F_SORTS_PERSIST) {
  run_script("portfolio_sorts.R",     "Phase F")
  run_script("persistence_testing.R", "Phase F")        
  run_script("activeness_persistence.R", "Phase F")      
}


# =============================================================================
# PHASE G - ACTIVENESS & PERFORMANCE
# =============================================================================
if (RUN_PHASE_G_ACTIVENESS) {
  run_script("activeness_analysis.R", "Phase G")
}


# =============================================================================
# PHASE H - FACTOR MODEL ROBUSTNESS (Appendix E)
# =============================================================================
if (RUN_PHASE_H_FACTOR_ROBUST) {
  run_script("alpha_estimation_robust.R", "Phase H")
  run_script("build_robust_tables.R",     "Phase H")
}


# =============================================================================
# PHASE I - BEHAVIORAL STATE VARIABLES (NEW)
# Reads Sentiment sheet of fund_data.xlsx; produces behavioral_state_vars.xlsx
# and leaves the data frame `behavioral_state_vars` in the global environment
# for Phase J to consume. No dependency on panel objects.
# =============================================================================
if (RUN_PHASE_I_BEHAVIORAL) {
  run_script("behavioral_state_variables.R", "Phase I")
}


# =============================================================================
# PHASE J - PANEL REGRESSIONS H1-H4 (NEW)
# Pipeline:
#   panel_regressions_setup.R     -> builds panel_reg (ranks, controls, lags,
#                                    activeness, merges behavioral_state_vars)
#   H1_sentiment_convexity.R      -> Table H1
#   H2_disposition_control.R      -> Table H2
#   H3_lottery_demand.R           -> Table H3 (4-col horserace: ActR2/ActSkew/MAX12)
#   H4_fee_elasticity.R           -> Table H4 (fee elasticity)
#   panel_regressions_reporting.R -> behavioral hypothesis summary table
#   psychological_premium.R       -> Table PPF (shadow price / proposal eq. 12)
# =============================================================================
if (RUN_PHASE_J_PANEL_REG) {
  panel_reg_rds <- file.path(WORKING_DIR, "panel_reg.rds")
  setup_required <- TRUE
  
  # Decide whether panel_regressions_setup.R needs to run.
  if (FORCE_PANEL_SETUP) {
    cat("\nPhase J: FORCE_PANEL_SETUP=TRUE -> will run panel_regressions_setup.R.\n")
    require_session_object("panel_incubation",       "panel_regressions_setup.R")
    require_session_object("behavioral_state_vars",  "panel_regressions_setup.R")
  } else if (exists("panel_incubation",      envir = .GlobalEnv) &&
             exists("behavioral_state_vars", envir = .GlobalEnv)) {
    cat("\nPhase J: upstream session objects available -> running setup as usual.\n")
  } else if (file.exists(panel_reg_rds)) {
    cat(sprintf(
      "\nPhase J: upstream session objects not in env, but %s exists.\n",
      panel_reg_rds))
    cat("        -> SKIPPING panel_regressions_setup.R; H1-H4 will load from RDS.\n")
    cat("        -> If upstream panels have changed, set FORCE_PANEL_SETUP <- TRUE\n")
    cat("           and rerun Phase A + Phase I first.\n")
    setup_required <- FALSE
  } else {
    stop("Phase J cannot proceed. Need either:\n",
         "  (a) panel_incubation + behavioral_state_vars in the session ",
         "(run Phase A and Phase I first), or\n",
         "  (b) ", panel_reg_rds, " on disk.\n",
         "Neither was found.")
  }
  
  if (setup_required) {
    run_script("panel_regressions_setup.R", "Phase J")
  }
  run_script("H1_sentiment_convexity.R",      "Phase J")
  run_script("H2_disposition_control.R",      "Phase J")
  run_script("H3_lottery_demand.R",           "Phase J")
  run_script("H4_fee_elasticity.R",           "Phase J")
  run_script("panel_regressions_reporting.R", "Phase J")
  run_script("psychological_premium.R",       "Phase J")
}


# =============================================================================
# UTILITY - LIPPER CATEGORY BUILD (standalone)
# =============================================================================
if (RUN_UTILITY_LIPPER) {
  run_script("build_lipper_category.R", "Utility")
}

# SYNC - COPY UPDATED ARTIFACTS TO TABLES_OUT_DIR (GitHub / Overleaf)
# This step copies every table_*.tex AND fig_*.png file that is new or has
# been modified during this pipeline run to TABLES_OUT_DIR (the GitHub repo
# tables/ folder, which syncs to Overleaf). Robustness tables from Phase H
# are excluded from this step because they were written directly to
# TABLES_OUT_DIR.

if (!is.na(TABLES_OUT_DIR)) {
  cat(sprintf("\n%s\n", strrep("=", 79)))
  cat("SYNC  Copy artifacts (table_*.tex + fig_*.png) -> TABLES_OUT_DIR\n")
  cat(sprintf("%s\n", strrep("=", 79)))
  
  # Ensure target exists.
  if (!dir.exists(TABLES_OUT_DIR)) {
    dir.create(TABLES_OUT_DIR, recursive = TRUE, showWarnings = FALSE)
    cat("Created target directory:", TABLES_OUT_DIR, "\n")
  }
  
  # Helper: copy any file matching `pattern` from WORKING_DIR to
  # TABLES_OUT_DIR if its mtime is newer than the pre-run snapshot. Returns
  # the count of files copied.
  .sync_class <- function(pattern, snapshot_before, label) {
    files_now <- list.files(WORKING_DIR, pattern = pattern, full.names = FALSE)
    to_copy <- character(0)
    for (f in files_now) {
      mt_now    <- file.info(file.path(WORKING_DIR, f))$mtime
      mt_before <- snapshot_before[f]
      if (is.na(mt_before) || mt_now > mt_before) {
        to_copy <- c(to_copy, f)
      }
    }
    if (length(to_copy) == 0L) {
      cat(sprintf("No new or modified %s files to sync.\n", label))
      return(0L)
    }
    cat(sprintf("Syncing %d %s file(s):\n", length(to_copy), label))
    res <- vapply(to_copy, function(f) {
      src <- file.path(WORKING_DIR, f)
      dst <- file.path(TABLES_OUT_DIR, f)
      ok  <- file.copy(src, dst, overwrite = TRUE, copy.date = TRUE)
      cat(sprintf("  %s  %s\n", if (ok) "OK" else "FAIL", f))
      ok
    }, logical(1))
    sum(res)
  }
  
  n_tex <- .sync_class("^table_.*\\.tex$", tex_snapshot_before, "table_*.tex")
  n_fig <- .sync_class("^fig_.*\\.png$",   fig_snapshot_before, "fig_*.png")
  
  cat(sprintf("Sync complete: %d table(s), %d figure(s) copied.\n",
              n_tex, n_fig))
} else {
  cat("\nSYNC disabled (TABLES_OUT_DIR is NA). Copy artifacts manually if needed.\n")
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
  
  n_failed  <- sum(log_df$status == "FAILED")
  n_skipped <- sum(log_df$status == "SKIPPED")
  if (n_failed > 0) {
    cat(sprintf("\nWARNING: %d script(s) failed. Review errors above.\n",
                n_failed))
  } else if (n_skipped > 0) {
    cat(sprintf("\nNote: %d script(s) skipped (file not found).\n",
                n_skipped))
  } else {
    cat("\nAll scripts completed successfully.\n")
  }
} else {
  cat("No phases were run. Check your RUN_* toggles in CONFIG.\n")
}

cat(sprintf("%s\n", strrep("=", 79)))
