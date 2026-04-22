# The Active Management Puzzle
**PhD Dissertation — Management Department, Boğaziçi University**  
**Author:** Ömer Eren  
**Supervisor:** Prof. Vedat Akgiray  
**Committee:** Han Özsöylev, Cenk Cevat Karahan  

---

## Overview

This dissertation investigates why investors persistently allocate capital to actively managed mutual funds despite documented underperformance — the "Active Management Puzzle." Using a panel of ~3,764 U.S. domestic equity mutual fund share classes (LSEG + Bloomberg, December 1994–February 2026), the study tests four behavioral hypotheses against the rational null of Berk & Green (2004).

**Hypotheses tested:**
- H1: Sentiment-Convexity — investor sentiment amplifies flow-performance convexity
- H2: Disposition-Control / Illusion of Control
- H3: Lottery Demand / Skewness Preference
- H4: Fee Elasticity

**Theoretical contribution:** Shadow price / Psychological Premium Framework (PPF) — quantifies the implicit premium investors pay for the psychological utility of active management.

---

## Pipeline Execution Order

**Critical:** Scripts are interdependent. Run in strict sequence. Skipping steps causes downstream failures.

| Step | Script | Version | Output |
|------|--------|---------|--------|
| 1 | `data_import_and_cleaning.R` | — | Cleaned fund panel |
| 2 | `flow_calculation.R` | — | Fund flow variables |
| 3 | `alpha_estimation.R` | v2.5 | Carhart 4-factor alphas, NW-HAC SEs |
| 4 | `aggregate_alphas.R` | — | Gross and net alpha aggregation |
| 5 | `alpha_reporting.R` | v7.8 | Table 5, Figure 2 (rolling alphas) |
| 6 | `FF_comparison.R` | v1.1 | FF (2010) replication benchmark |
| 7 | `build_ff_tables_manual.R` | v1.1 | Tables C.1, B.1 |
| 8 | `descriptive_statistics.R` | v2.1 | Table 1 |
| 9 | `structural_break_test.R` | v1.0 | Bai-Perron structural breaks |
| 10 | `subperiod_analysis.R` | v1.2 | Bootstrap with SHA-1 caching |
| 11 | `portfolio_sorts.R` | — | Portfolio sort tables |
| 12 | `persistence_testing.R` | — | Alpha persistence tests |

After step 12: recompile dissertation on Overleaf.

---

## Key Methodological Decisions

### Alpha Estimation
- **Model:** Carhart (1997) four-factor model
- **SE estimator:** Newey-West HAC, applied symmetrically to both actual and simulated t-statistics (v2.5 fix — prior v2.4 used OLS for simulated, creating asymmetric comparison with no precedent in FF 2010)
- **Returns:** LSEG total return index → gross returns first; net = gross − (1/12 × annual expense ratio); funds with missing expense ratios receive NA (no zero-fee fallback)
- **VW alpha:** Value-weighted portfolio construction using per-fund mean-TNA weights (not per-fund mean-TNA as cross-sectional weight on fund alpha)
- **Incubation bias:** Evans (2010) correction applied

### Sample
- **Universe:** ~3,764 U.S. domestic equity mutual fund share classes
- **Bootstrap population:** 1,899 active funds (Lipper filter removed for FF/BSW analysis)
- **Table 8 only:** `alpha_full_cleaned` (retains Lipper filter)
- **Period:** December 1994 – February 2026

### Bootstrap / FDR (BSW 2010)
- π̂₀ = 80.4% (zero-alpha funds); π̂⁻_A = 14.9% unskilled; π̂⁺_A = 0.6% skilled at γ* = 0.20
- P(Luck): proportion of simulated percentile values below actual t-statistic
- Skill threshold: P(Luck) > 95% (right tail); P(Luck) < 5% (left tail)
- Bootstrap cache: `subperiod_bootstrap_cache_{P1,P2,P3}.rds` (SHA-1 hashed, ~30s on hit vs ~30min cold)
- **Note:** π̂₀ Storey estimator has known upward bias; π̂⁺ and π̂⁻ are downward-biased lower bounds

### Sub-periods (Bai-Perron)
| Period | Dates | Characterization |
|--------|-------|-----------------|
| P1 | Jan 1995 – Jan 2006 | Volatile early era |
| P2 | Feb 2006 – Nov 2011 | Positive-alpha era (~+0.94%/yr mean); "genuine skill era" |
| P3 | Dec 2011 – Dec 2023 | Structural compression (~−1.23%/yr mean) |

### Core Empirical Results
| Metric | Value |
|--------|-------|
| Active gross alpha (EW) | −0.655%/yr |
| Active gross alpha (VW) | +0.676%/yr |
| Active net alpha (EW) | −1.777%/yr |
| Active net alpha (VW) | ≈ −0.036%/yr |

VW net alpha ≈ 0 validates Berk-Green (2004) equilibrium. The EW–VW gap is where the behavioral argument lives.

---

## Data Sources

| Source | Variables |
|--------|-----------|
| LSEG Workspace | Fund returns (total return index), TNA, expense ratios |
| Bloomberg | Active/passive classification, fund strategy |
| French Data Library | Carhart four factors (MKT, SMB, HML, MOM) |
| Baker & Wurgler (2006) | Investor sentiment index (primary behavioral state variable) |
| FRED (UMCSENT) | University of Michigan Consumer Sentiment |
| AAII | Investor sentiment survey |
| CBOE | Put/call ratio |

**Note:** Raw data files (`.xlsx`, `.csv`, `.rds`) are excluded from this repository via `.gitignore`. Data is available from the sources above or upon request.

---

## Key Literature

| Paper | Role |
|-------|------|
| Berk & Green (2004), *JPE* | Rational null hypothesis |
| Fama & French (2010), *JF* | Primary bootstrap benchmark |
| Barras, Scaillet & Wermers (2010), *JF* | FDR decomposition benchmark |
| Carhart (1997), *JF* | Four-factor model |
| Sirri & Tufano (1998), *JF* | Piecewise rank flow regression |
| Baker & Wurgler (2006), *JF* | Sentiment index |
| Evans (2010), *JF* | Incubation bias correction |
| Pástor, Stambaugh & Taylor (2015), *JF* | Industry-scale decreasing returns |
| Cheng et al. (2025) | H1 empirical grounding |

---

## R Environment

Package versions are locked in `R scripts/renv.lock`. To restore the exact environment:

```r
install.packages("renv")
renv::restore()
```

**Key packages:** `tidyverse`, `kableExtra`, `sandwich` (Newey-West), `strucchange` (Bai-Perron), `parallel`, `e1071`, `slider`, `zoo`, `ggplot2`, `scales`, `lubridate`

---

## LaTeX / Overleaf

- Compiled on Overleaf (Standard plan); synced to this repo via native GitHub integration
- Bibliography: `natbib` with Chicago style (`\citet{}`, `\citep{}`, `\citealt{}`)
- Tables: `kableExtra` with `longtable` + `threeparttable`; separate `.tex` files in `tables/`
- **Backslash convention in footnotes:**
  - `\\\\cmd` inside `threeparttable` footnote strings (R source)
  - `\\cmd` inside `longtable_note()` footnote strings
- `escape=FALSE` only when math appears in column headers; `escape=TRUE` for cells containing `%`
- t-statistics (not p-values or SEs) as reporting standard; formatted as plain `(...)`

---

## Repository Structure

```
Active-Management-Puzzle/
├── R scripts/
│   ├── data_import_and_cleaning.R
│   ├── flow_calculation.R
│   ├── alpha_estimation.R
│   ├── aggregate_alphas.R
│   ├── alpha_reporting.R
│   ├── FF_comparison.R
│   ├── build_ff_tables_manual.R
│   ├── build_lipper_category.R
│   ├── descriptive_statistics.R
│   ├── structural_break_test.R
│   ├── subperiod_analysis.R
│   ├── portfolio_sorts.R
│   ├── persistence_testing.R
│   └── renv.lock
├── tables/          <- .tex table outputs (from Overleaf sync)
├── figures/         <- ggplot figure outputs
├── .gitignore
└── README.md
```

---

## Daily Workflow

**After an R scripting session:**
```bash
git add "R scripts/"
git commit -m "script_name vX.X: brief description of change"
git push
```

**After editing LaTeX on Overleaf:**
Overleaf -> Menu -> GitHub -> Push to GitHub

**Switching to MacBook:**
```bash
git pull
```
