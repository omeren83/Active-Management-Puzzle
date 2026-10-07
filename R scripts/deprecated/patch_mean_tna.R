# =============================================================================
# PATCH MEAN_TNA IN ALPHA XLSX FILES (v1.1 - DEPRECATED)
#
# DEPRECATION NOTICE (May 2026)
# -----------------------------
# This script is no longer required for fresh pipeline runs. As of
# alpha_estimation.R v2.7 and alpha_estimation_robust.R v1.2, mean_tna is
# computed correctly at source by coalescing class_assets and total_assets.
# alpha_fullperiod*.xlsx files produced by these script versions already
# carry valid mean_tna values, so build_robust_tables.R can run directly
# without invoking this patch.
#
# This script is RETAINED for backward compatibility: if you need to
# reproduce results from alpha_fullperiod*.xlsx files generated with
# alpha_estimation.R v2.6 or earlier (where mean_tna was NaN due to a
# d$tna NULL reference), source this script in the same R session as the
# panel_incubation object and the patch will fix the legacy xlsx files
# in place.
#
# Original v1.0 documentation
# ---------------------------
# Problem
# -------
# alpha_estimation.R and alpha_estimation_robust.R both reference d$tna to
# compute per-fund mean TNA. However panel_incubation carries TNA under the
# column names class_assets (primary) and total_assets (fallback) — there is
# no column named tna. d$tna silently returns NULL in R, so mean_tna is NaN
# in all alpha_fullperiod*.xlsx files. This has no effect on the main pipeline
# (aggregate_alphas.R builds VW portfolios directly from the live panel), but
# breaks build_robust_tables.R which reads mean_tna for VW cross-sectional
# weighted averages.
#
# Fix
# ---
# Re-computes correct mean_tna for every fund in panel_incubation using
# coalesce(class_assets, total_assets) and patches it into all three alpha
# xlsx files in place. Bootstrap results are untouched — they do not use
# mean_tna. The patch takes seconds.
#
# Prerequisite
# ------------
# Run in the same R session as data_import_and_cleaning.R (panel_incubation
# must exist). The three xlsx files must exist in the working directory.
# =============================================================================

library(readxl)
library(writexl)
library(dplyr)

cat("=== patch_mean_tna.R ===\n")

# Verify session object exists
if (!exists("panel_incubation")) {
  stop("panel_incubation not found. Source data_import_and_cleaning.R first.")
}

# --- 1. Compute correct mean_tna per fund -----------------------------------
cat("[1] Computing correct mean_tna from panel_incubation...\n")

if (!"class_assets" %in% names(panel_incubation)) {
  stop("class_assets column not found in panel_incubation. Check data_import_and_cleaning.R.")
}

tna_by_fund <- panel_incubation %>%
  mutate(tna_val = coalesce(class_assets, total_assets)) %>%
  group_by(Ticker) %>%
  summarise(
    mean_tna = mean(tna_val[!is.na(tna_val) & tna_val > 0], na.rm = TRUE),
    .groups = "drop"
  )

cat(sprintf("   Funds with valid mean_tna: %d / %d\n",
            sum(!is.na(tna_by_fund$mean_tna)), nrow(tna_by_fund)))
cat(sprintf("   mean_tna range: %.1f -- %.1f USD mn\n",
            min(tna_by_fund$mean_tna, na.rm = TRUE),
            max(tna_by_fund$mean_tna, na.rm = TRUE)))

# --- 2. Patch each alpha xlsx file ------------------------------------------
files_to_patch <- c(
  "alpha_fullperiod.xlsx",
  "alpha_fullperiod_FF6.xlsx",
  "alpha_fullperiod_C5.xlsx"
)

for (f in files_to_patch) {
  if (!file.exists(f)) {
    cat(sprintf("   SKIP  %-40s (not found)\n", f))
    next
  }
  df <- read_excel(f)
  n_before <- sum(!is.na(df$mean_tna))

  # Drop old (NaN) mean_tna and join fresh values
  df_patched <- df %>%
    select(-any_of("mean_tna")) %>%
    left_join(tna_by_fund, by = "Ticker")

  n_after <- sum(!is.na(df_patched$mean_tna))
  write_xlsx(df_patched, f)
  cat(sprintf("   OK    %-40s  mean_tna valid: %d -> %d funds\n",
              f, n_before, n_after))
}

cat("\nPatch complete. Re-run build_robust_tables.R to regenerate Appendix E tables.\n")
