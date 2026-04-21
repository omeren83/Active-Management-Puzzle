# =============================================================================
# LIPPER CATEGORY ASSIGNMENT
#
# Priority logic:
#   1. LSEG Category — authoritative, use directly
#   2. New_Benchmark code — deterministic map for specific style benchmarks
#   3. Mcap_Focus + Fund_Strategy — Bloomberg field derivation
#      (a) Both present → [Size]-Cap [Style] Funds
#      (b) Mcap only (Strategy missing) → [Size]-Cap Core Funds
#      (c) Strategy only (Mcap = Broad Market or missing) → Multi-Cap [Style] Funds
#   4. NA — genuinely unresolvable (sector/L-S/Unknown-AM funds; excluded anyway)
#
# Output: lipper_category_output.xlsx
#   Two columns: Ticker + Lipper_Category + Lipper_Source (audit trail)
#   Copy-paste Lipper_Category into the static sheet of fund_data.xlsx.
#
# Dependencies: readxl, writexl
# Install (run once): install.packages(c("readxl", "writexl"))
# =============================================================================

library(readxl)
library(writexl)

INPUT_FILE  <- "lipper.xlsx"    # adjust path if needed
OUTPUT_FILE <- "lipper_category_output.xlsx"

# =============================================================================
# 1. READ INPUT
# =============================================================================
df <- read_excel(INPUT_FILE, sheet = "Sheet1")

cat("Loaded:", nrow(df), "funds\n")
cat("Columns:", paste(names(df), collapse = ", "), "\n\n")

# Normalise LSEG Category: treat "#N/A", "N/A", blank as missing
df$LSEG_cat_clean <- trimws(df$`LSEG Category`)
df$LSEG_cat_clean[df$LSEG_cat_clean %in% c("#N/A", "N/A", "#N/A N/A", "")] <- NA

# Normalise Bloomberg fields the same way
na_strings <- c("#N/A N/A", "#N/A", "N/A", "")
clean_field <- function(x) {
  x <- trimws(x)
  x[x %in% na_strings] <- NA
  x
}

df$bench  <- clean_field(df$New_Benchmark)
df$mcap   <- clean_field(df$Mcap_Focus)
df$strat  <- clean_field(df$Fund_Strategy)

# =============================================================================
# 2. BENCHMARK → LIPPER MAP (Priority 2)
#    Only specific-style benchmarks are included here.
#    RUA (Russell 3000) is intentionally excluded — it is a catch-all default
#    with no style information; those funds fall through to priority 3.
#    Sector/REIT/MLP/NASDAQ benchmarks are also excluded → NA.
# =============================================================================
bench_map <- c(
  # Russell 1000 family — Large-Cap
  "RLV"    = "Large-Cap Value Funds",
  "RUI"    = "Large-Cap Core Funds",
  "RLG"    = "Large-Cap Growth Funds",
  # Russell Midcap family — Mid-Cap
  "RMCV"   = "Mid-Cap Value Funds",
  "RMCC"   = "Mid-Cap Core Funds",
  "RMCG"   = "Mid-Cap Growth Funds",
  # Russell 2000 family — Small-Cap
  "RUJ"    = "Small-Cap Value Funds",
  "RUT"    = "Small-Cap Core Funds",
  "RUO"    = "Small-Cap Growth Funds",
  # Russell 3000 style sub-indices — Multi-Cap (but NOT RUA itself)
  "RAV"    = "Multi-Cap Value Funds",
  "RAG"    = "Multi-Cap Growth Funds",
  # Russell 2500 family — proxy to Multi-Cap (closest available)
  "R2500V" = "Multi-Cap Value Funds",
  "R2500"  = "Multi-Cap Core Funds",
  "R2500G" = "Multi-Cap Growth Funds",
  # S&P alternatives
  "MID"    = "Mid-Cap Core Funds",     # S&P MidCap 400
  "SML"    = "Small-Cap Core Funds"    # S&P SmallCap 600
)

# =============================================================================
# 3. MCAP + STRATEGY → LIPPER MAP (Priority 3)
# =============================================================================
mcap_prefix_map <- c(
  "Large-cap"    = "Large-Cap",
  "Mid-cap"      = "Mid-Cap",
  "Small-cap"    = "Small-Cap",
  "Broad Market" = "Multi-Cap"
)

strat_suffix_map <- c(
  "Value"  = "Value Funds",
  "Blend"  = "Core Funds",
  "Growth" = "Growth Funds"
)

# =============================================================================
# 4. APPLY CLASSIFICATION LOGIC
# =============================================================================
n <- nrow(df)
lipper_out <- character(n)
source_out <- character(n)

for (i in seq_len(n)) {

  lseg  <- df$LSEG_cat_clean[i]
  bench <- df$bench[i]
  mcap  <- df$mcap[i]
  strat <- df$strat[i]

  # --- Priority 1: LSEG Category directly -----------------------------------
  if (!is.na(lseg)) {
    lipper_out[i] <- lseg
    source_out[i] <- "LSEG_Direct"
    next
  }

  # --- Priority 2: Specific benchmark code ----------------------------------
  if (!is.na(bench) && bench %in% names(bench_map)) {
    lipper_out[i] <- bench_map[bench]
    source_out[i] <- paste0("Benchmark_Map (", bench, ")")
    next
  }

  # --- Priority 3a: Both Mcap and Strategy present --------------------------
  has_mcap  <- !is.na(mcap)  && mcap  %in% names(mcap_prefix_map)
  has_strat <- !is.na(strat) && strat %in% names(strat_suffix_map)

  if (has_mcap && has_strat) {
    lipper_out[i] <- paste(mcap_prefix_map[mcap], strat_suffix_map[strat])
    source_out[i] <- paste0("Derived_McapStrat (", mcap, " / ", strat, ")")
    next
  }

  # --- Priority 3b: Mcap only — treat Strategy as Blend (Core) -------------
  if (has_mcap && !has_strat) {
    lipper_out[i] <- paste(mcap_prefix_map[mcap], "Core Funds")
    source_out[i] <- paste0("Derived_McapOnly (", mcap, " / Strategy=NA→Core)")
    next
  }

  # --- Priority 3c: Strategy only — treat Mcap as Broad Market (Multi-Cap) -
  if (!has_mcap && has_strat) {
    lipper_out[i] <- paste("Multi-Cap", strat_suffix_map[strat])
    source_out[i] <- paste0("Derived_StratOnly (Multi-Cap / ", strat, ")")
    next
  }

  # --- Priority 4: Unresolvable ---------------------------------------------
  lipper_out[i] <- NA_character_
  source_out[i] <- paste0("Unresolvable (bench=", ifelse(is.na(bench), "NA", bench),
                           " mcap=NA strat=NA)")
}

# =============================================================================
# 5. ASSEMBLE OUTPUT
# =============================================================================
out <- data.frame(
  Ticker          = df$Ticker,
  Lipper_Category = lipper_out,
  Lipper_Source   = source_out,
  stringsAsFactors = FALSE
)

# Replace empty strings from character() initialisation with NA
out$Lipper_Category[out$Lipper_Category == ""] <- NA
out$Lipper_Source[out$Lipper_Source   == ""] <- NA

# =============================================================================
# 6. SUMMARY
# =============================================================================
cat("=== Classification Summary ===\n")

total       <- nrow(out)
n_lseg      <- sum(grepl("^LSEG_Direct", out$Lipper_Source, na.rm = TRUE))
n_bench     <- sum(grepl("^Benchmark_Map", out$Lipper_Source, na.rm = TRUE))
n_both      <- sum(grepl("^Derived_McapStrat", out$Lipper_Source, na.rm = TRUE))
n_mcap      <- sum(grepl("^Derived_McapOnly", out$Lipper_Source, na.rm = TRUE))
n_strat     <- sum(grepl("^Derived_StratOnly", out$Lipper_Source, na.rm = TRUE))
n_unres     <- sum(grepl("^Unresolvable", out$Lipper_Source, na.rm = TRUE))
n_classified <- sum(!is.na(out$Lipper_Category))

cat(sprintf("  Total funds              : %d\n", total))
cat(sprintf("  Classified (total)       : %d (%.1f%%)\n",
            n_classified, 100 * n_classified / total))
cat(sprintf("  -- LSEG Direct           : %d\n", n_lseg))
cat(sprintf("  -- Benchmark map         : %d\n", n_bench))
cat(sprintf("  -- Mcap + Strategy       : %d\n", n_both))
cat(sprintf("  -- Mcap only (→ Core)    : %d\n", n_mcap))
cat(sprintf("  -- Strategy only (→ MC.) : %d\n", n_strat))
cat(sprintf("  Unresolvable (NA)        : %d (%.1f%%)\n",
            n_unres, 100 * n_unres / total))

cat("\nFinal Lipper_Category distribution:\n")
tbl <- sort(table(out$Lipper_Category, useNA = "always"), decreasing = TRUE)
print(tbl)

# =============================================================================
# 7. SAVE OUTPUT
# =============================================================================
write_xlsx(out, OUTPUT_FILE)
cat(sprintf("\nSaved: %s (%d rows)\n", OUTPUT_FILE, nrow(out)))
cat("Copy the Lipper_Category column (matched by Ticker) into the static sheet.\n")
cat("Keep Lipper_Source as an audit trail — do not paste it into the static sheet.\n")
