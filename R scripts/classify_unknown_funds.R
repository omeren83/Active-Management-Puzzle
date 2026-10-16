# =============================================================================
# UNKNOWN-FUND CLASSIFICATION (utility)
# classify_unknown_funds.R  (v1.0, Oct 2026; pre-committed 16 Oct 2026)
#
# Bloomberg's Actively_Managed_New flag is missing for almost every fund that
# closed before ~2012, so those funds were "Unknown" and left out, which made
# the pre-2012 active sample survivor-only. This script classifies them.
#
# Classification (claude/precommit_unknown_reclassification_2026-10-16.md):
#   Rule: PASSIVE if (1) index keyword in the name, or (2) Lipper S&P 500 /
#   S&P MidCap 400 Index category, or (3) R2 >= 0.98 and TE <= 1% against the
#   assigned benchmark (>= 24 joint months); otherwise ACTIVE.
#   Manual review of boundary cases: unknown_classification_ledger.xlsx
#   (Proposed_class, or Omer_decision when filled) overrides the rule.
#   Validation of the rule on Bloomberg-flagged funds is written alongside.
#
# Exclusions, same criteria as flagged_funds.xlsx (Legend sheet), detected
# from Lipper category, benchmark and name because Fund_Strategy and Lipper
# are often missing for these funds:
#   SECTOR_FUND        -> excluded from performance comparison and H3
#   GLOBAL_MANDATE, LONG_SHORT (incl. market neutral, bear) -> entire analysis
#   COVERED_CALL_OVERLAY -> H3 only
# Name matches that are fund-family names, and sector/mandate funds the name
# patterns miss, are handled in the explicit lists below (reviewed 16 Oct).
#
# Run once after Phase A (needs panel_master in the session, any
# classification) whenever fund_data.xlsx or the ledger changes. Output is
# read by data_import_and_cleaning.R (v1.8): unknown_classification.xlsx.
# =============================================================================

suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(readxl)
  library(writexl)
})

if (!exists("WORKING_DIR")) WORKING_DIR <- getwd()
setwd(WORKING_DIR)
if (!exists("panel_master")) stop("panel_master not in session: run Phase A first.")

FILE        <- "fund_data.xlsx"
LEDGER_FILE <- "unknown_classification_ledger.xlsx"
OUT_FILE    <- "unknown_classification.xlsx"
MIN_JOINT   <- 24L
R2_MIN      <- 0.98
TE_MAX      <- 0.01

# --- 1. Rule ------------------------------------------------------------------
NAME_RX <- paste0("INDEX|INDX|\\bIDX\\b|S&P ?(500|400|600)|\\bSP 500|TOTAL (STOCK )?(MARKET|MKT)|",
                  "EXTENDED (MARKET|MKT)|RUSSELL ?(1000|2000|3000|MIDCAP|MID CAP)|WILSHIRE ?(4500|5000)|",
                  "NASDAQ[- ]?100|DOW 30|DJIA|TRACKER|\\bETF\\b")
IDX_LIPPER <- c("S&P 500 Index Funds", "S&P Midcap 400 Index Funds")

# Benchmark total-return levels -> monthly returns
bench <- read_excel(FILE, sheet = "bench_returns", col_types = "text")
names(bench)[1] <- "Benchmark_Code"
bench <- bench %>%
  pivot_longer(-Benchmark_Code, names_to = "date", values_to = "lvl") %>%
  mutate(date = parse_col_dates(date), lvl = suppressWarnings(as.numeric(lvl))) %>%
  filter(!is.na(date), !is.na(lvl), !is.na(Benchmark_Code)) %>%
  group_by(Benchmark_Code) %>% arrange(date) %>%
  mutate(bret = lvl / lag(lvl) - 1) %>% ungroup() %>%
  filter(!is.na(bret)) %>% transmute(Benchmark_Code, ym = format(date, "%Y-%m"), bret)

fund <- panel_master %>%
  group_by(Ticker) %>%
  summarise(Name = first(Name), Lipper_Category = first(Lipper_Category),
            Benchmark_Code = first(Benchmark_Code), flag = first(Actively_Managed_New),
            ap_group = first(ap_group),
            first_month = format(min(date), "%Y-%m"),
            last_return = format(suppressWarnings(max(date[!is.na(ret_net)])), "%Y-%m"),
            .groups = "drop")
fit <- panel_master %>%
  filter(!is.na(ret_gross)) %>%
  transmute(Ticker, Benchmark_Code, ym = format(date, "%Y-%m"), r = ret_gross) %>%
  inner_join(bench, by = c("Benchmark_Code", "ym")) %>%
  group_by(Ticker) %>%
  summarise(joint = n(),
            r2 = if (n() >= MIN_JOINT) summary(lm(r ~ bret))$r.squared else NA_real_,
            te = if (n() >= MIN_JOINT) sd(r - bret) * sqrt(12) else NA_real_, .groups = "drop")
fund <- fund %>% left_join(fit, by = "Ticker") %>%
  mutate(L_name = grepl(NAME_RX, toupper(Name)), L_lipper = Lipper_Category %in% IDX_LIPPER,
         L_fit = !is.na(r2) & r2 >= R2_MIN & te <= TE_MAX,
         rule_class = ifelse(L_name | L_lipper | L_fit, "Passive", "Active"))

# Validation on Bloomberg-flagged funds (overrides MOJAX/GENDX kept out)
lab <- fund %>% filter(flag %in% c("Y", "N")) %>%
  mutate(bloomberg = ifelse(flag == "Y", "Active", "Passive"))
validation <- lab %>% count(bloomberg, rule_class) %>%
  pivot_wider(names_from = rule_class, values_from = n, values_fill = 0, names_prefix = "rule_")
cat("Validation (Bloomberg flag x rule):\n"); print(validation)

# --- 2. Unknown funds: rule + manual review -------------------------------------
ledger <- read_excel(LEDGER_FILE, sheet = "Ledger") %>%
  mutate(Omer_decision = trimws(coalesce(as.character(Omer_decision), "")),
         reviewed = ifelse(Omer_decision %in% c("Active", "Passive"), Omer_decision, Proposed_class)) %>%
  select(Ticker, reviewed, review_reason = Reason)
unk <- fund %>% filter(ap_group == "Unknown") %>%
  left_join(ledger, by = "Ticker") %>%
  mutate(final_class = coalesce(reviewed, rule_class),
         class_source = ifelse(is.na(reviewed), "rule",
                               ifelse(reviewed == rule_class, "rule, reviewed", "manual review")))

# --- 3. Exclusions (criteria of flagged_funds.xlsx) -----------------------------
SECT_LIP <- c("Basic Materials Funds", "Consumer Goods Funds", "Consumer Services Funds",
              "Energy MLP Funds", "Financial Services Funds", "Health/Biotechnology Funds",
              "Industrials Funds", "Natural Resources Funds", "Precious Metals Equity Funds",
              "Real Estate Funds", "Science & Technology Funds", "Telecommunication Funds",
              "Utility Funds")
SECT_BM  <- c("AMZ", "MXUS0EN", "MXUS0HC", "MXUS0IN", "MXUS0IT", "MXUS0TC", "RMZ")
SECT_RX  <- paste0("TECHNOLOG|TECHNLG|TECHNOL|\\bTECH\\b|SCI ?& ?TECH|SCITECH|INTERNET|TELECOM|",
                   "COMMUNICATION|REAL EST|\\bREIT|HEALTH|BIOTECH|PHARM|MEDICAL|UTILIT|ENERGY|",
                   "NATURAL RES|PRECIOUS|\\bGOLD\\b|FINANCIAL SERV|\\bBANK|INFRASTR|\\bMLP\\b|LEISURE|MEDIA")
GLOB_RX  <- paste0("\\bINTL\\b|INTERNATIONAL|\\bGLOBAL\\b|\\bGLBL\\b|\\bWORLD\\b|EMERGING MARKETS|",
                   "EMERG(ING)? MKT|\\bEAFE\\b|EUROPE|\\bASIA|PACIFIC|JAPAN|LATIN")
LS_RX    <- "LONG[- ]?SHORT|\\bL/S\\b|MARKET NEUTRAL|MKT NEUTRAL|\\bBEAR\\b"
CC_RX    <- "COVERED CALL|BUY[- ]?WRITE"
# Reviewed 16 Oct: family names, not mandates (patterns would misfire)
NOT_GLOBAL <- c("1088344D US Equity", "PCIGX US Equity", "PCVYX US Equity",   # Pacific Capital (US funds)
                "1011832D US Equity", "1011833D US Equity", "668966Q US Equity") # Murphy New World (sector, below)
# Reviewed 16 Oct: sector / mandate funds the patterns miss (abbreviations)
SECTOR_ADD <- c("CRUTX US Equity",     # Cappiello-Rushmore Utility Income
                "LLREX US Equity",     # Longleaf Partners Realty
                "GVRGX US Equity",     # FBR Realty Growth
                "TITNX US Equity",     # Titan Financial Services
                "FSVAX US Equity",     # Morgan Stanley Financial Services
                "0153472D US Equity",  # Constellation TIP Financial Services
                "SENBX US Equity",     # Senbanc (bank stocks)
                "1655846D US Equity",  # TCW Health Sciences
                "2655529Q US Equity",  # IPS iFund (internet)
                "ADFFX US Equity")     # Alpha Analytics Digital Future (technology)
LS_ADD     <- c("95502Q US Equity")    # AXA Rosenberg Double Alpha (market neutral)

nm <- toupper(unk$Name)
unk <- unk %>% mutate(
  x_sector = (Lipper_Category %in% SECT_LIP | Benchmark_Code %in% SECT_BM | grepl(SECT_RX, nm) |
                Ticker %in% SECTOR_ADD),
  x_global = grepl(GLOB_RX, nm) & !Ticker %in% NOT_GLOBAL,
  x_ls     = grepl(LS_RX, nm) | Ticker %in% LS_ADD,
  x_cc     = grepl(CC_RX, nm),
  exclusion_flag = trimws(paste(ifelse(x_global, "GLOBAL_MANDATE", ""), ifelse(x_ls, "LONG_SHORT", ""),
                                ifelse(x_sector, "SECTOR_FUND", ""), ifelse(x_cc, "COVERED_CALL_OVERLAY", ""))),
  exclusion_flag = gsub(" +", " | ", exclusion_flag),
  exclude_entire = x_global | x_ls,
  exclude_perf   = exclude_entire | x_sector,
  exclude_h3     = exclude_entire | x_sector | x_cc,
  # Lipper "#N/A N/A": no style category (no flow rank); set to missing
  Lipper_Category = ifelse(Lipper_Category %in% c("#N/A N/A", "#N/A", "N/A", ""), NA, Lipper_Category))

cat("\nUnknown funds:", nrow(unk), "\n"); print(table(final_class = unk$final_class, source = unk$class_source))
cat("Exclusions: entire", sum(unk$exclude_entire), "| perf", sum(unk$exclude_perf),
    "| H3", sum(unk$exclude_h3), "| no Lipper category", sum(is.na(unk$Lipper_Category)), "\n")

out <- unk %>% transmute(Ticker, Name, Lipper_Category, Benchmark_Code, first_month, last_return,
                         joint_months = joint, R2 = round(r2, 3), TE_pct = round(100 * te, 2),
                         rule_class, final_class, class_source, review_reason,
                         exclusion_flag, exclude_entire, exclude_perf, exclude_h3) %>%
  arrange(Ticker)
readme <- data.frame(Item = c("Purpose", "final_class", "exclude_*", "Validation"), Description = c(
  "Classification of funds without a Bloomberg active/passive flag (pre-2012 closures). Read by data_import_and_cleaning.R v1.8. Built by classify_unknown_funds.R.",
  "Rule (name / Lipper index category / R2 >= 0.98 & TE <= 1%) overridden by the manual review in unknown_classification_ledger.xlsx.",
  "Same scopes as flagged_funds.xlsx: entire = GLOBAL_MANDATE or LONG_SHORT; perf = entire or SECTOR_FUND; H3 = perf or COVERED_CALL_OVERLAY.",
  "Rule applied to Bloomberg-flagged funds (sheet Validation)."))
write_xlsx(list(README = readme, Classification = out, Validation = validation), OUT_FILE)
cat("Written:", OUT_FILE, "\n")
