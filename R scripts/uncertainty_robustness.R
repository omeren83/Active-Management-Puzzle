# uncertainty_robustness.R                                               v1.1
# =============================================================================
# SENTIMENT OR UNCERTAINTY? Horse race between the behavioral state variables
# of H1/H2 and the macroeconomic uncertainty index of Jurado, Ludvigson & Ng
# (2015, AER). Added at a jury member's request (Oct 2026).
#
# Rationale: in rational learning models (Berk & Green 2004) flows respond
# less to past returns when returns are noisier, so a flatter flow-performance
# relation in high-sentiment months could reflect high uncertainty instead.
# Each H1/H2 state variable S is re-estimated with and without the
# corresponding uncertainty interactions U x R^q on one common sample:
#   (a) S only    -- the primary H1/H2 specification
#   (b) U only
#   (c) S and U together
# Matched forms: dummies with D_UNC (top 34%), the continuous BW index with
# the standardised continuous JLN index.
#
# Pre-committed choices (fixed before any results were seen):
#   - Main horizon h = 1 month (JLN 2015; Ludvigson, Ma & Ng 2021 headline);
#     h = 3 and h = 12 reported as robustness.
#   - Specification otherwise identical to the H1/H2 primary specification:
#     contemporaneous states, fund FE, two-way clustering (Ticker, month),
#     December excluded, lagged TNA >= $5M (inherited from panel_reg).
#   - Placement: compact summary in the main text, full tables in the appendix.
#
# Outputs (WORKING_DIR):
#   table_unc_summary.tex   main text: joint tests with/without uncertainty
#   table_unc_H1.tex        appendix: H1 coefficients (D_SENT, SENT_perp)
#   table_unc_AAII.tex      appendix: H1 coefficients (D_AAII)
#   table_unc_H2.tex        appendix: H2 coefficients (D_MD,Det)
#   table_unc_horizons.tex  appendix: horizons h = 1, 3, 12
#   uncertainty_robustness.rds
#
# Dependencies: dplyr, fixest
# =============================================================================

suppressPackageStartupMessages({ library(dplyr); library(fixest) })

# --- 0. Config ---------------------------------------------------------------
if (!exists("WORKING_DIR")) WORKING_DIR <- getwd()
MAIN_H   <- "1"                       # pre-committed main horizon
HORIZONS <- c("1", "3", "12")
FE       <- "Ticker"                  # as the H1/H2 primary specification
CONTROLS <- c("log_TNA", "log_Age", "ExpRatio", "LoadDummy",
              "ret_vol", "Turnover", "style_flow_lag")
STAR_THR <- c(`***` = 2.576, `**` = 1.960, `*` = 1.645)

# --- 1. Data -----------------------------------------------------------------
if (!exists("panel_reg")) {
  rds_path <- file.path(WORKING_DIR, "panel_reg.rds")
  if (!file.exists(rds_path)) stop("panel_reg not found; run Phase J setup first.")
  panel_reg <- readRDS(rds_path)
}
unc_cols <- c(paste0("JLN_UNC_", HORIZONS, "M"), paste0("D_UNC", HORIZONS))
miss <- setdiff(unc_cols, names(panel_reg))
if (length(miss))
  stop("panel_reg lacks uncertainty columns (", paste(miss, collapse = ", "),
       "). Add the JLN rows to fund_data.xlsx and rerun Phases I and J.")

core <- c("flow", "R_LOW", "R_MID", "R_HIGH", CONTROLS)
# One common sample: all state variables and all three horizons non-missing,
# so every column of every table uses identical observations
samp <- panel_reg %>%
  filter(!is_december) %>%
  filter(if_all(all_of(c(core, "D_SENT", "SENT_ORTH", "D_AAII", "D_MD_DETREND",
                         unc_cols)), ~ !is.na(.))) %>%
  mutate(across(c(D_SENT, D_AAII, D_MD_DETREND, all_of(paste0("D_UNC", HORIZONS))),
                as.numeric),
         SENT_ORTH_z = as.numeric(scale(SENT_ORTH)),
         across(all_of(paste0("JLN_UNC_", HORIZONS, "M")),
                ~ as.numeric(scale(.x)), .names = "{.col}_z"),
         Ticker = as.factor(Ticker), yearmo = as.factor(yearmo))
cat(sprintf("Uncertainty horse-race sample: %d fund-months | %d funds | %d months (%s to %s)\n",
            nrow(samp), nlevels(samp$Ticker), nlevels(samp$yearmo),
            min(as.character(samp$yearmo)), max(as.character(samp$yearmo))))
cat(sprintf("Month-level correlation SENT_ORTH vs JLN h=1: %.2f | D_SENT vs D_UNC1 overlap: %.2f\n",
            with(distinct(samp, yearmo, SENT_ORTH, JLN_UNC_1M), cor(SENT_ORTH, JLN_UNC_1M)),
            with(distinct(samp, yearmo, D_SENT, D_UNC1), cor(D_SENT, D_UNC1))))

# --- 2. Estimation helpers ---------------------------------------------------
inter <- function(v) paste0(c("R_LOW:", "R_MID:", "R_HIGH:"), v)
fit <- function(states) {
  rhs <- c("R_LOW", "R_MID", "R_HIGH", CONTROLS, states, unlist(lapply(states, inter)))
  feols(as.formula(paste("flow ~", paste(rhs, collapse = " + "), "|", FE)),
        data = samp, cluster = ~ Ticker + yearmo)
}
# Coefficient index robust to fixest's ordering of interaction names
idx <- function(b, nm) {
  i <- which(names(b) == nm)
  if (!length(i)) i <- which(names(b) == paste(rev(strsplit(nm, ":")[[1]]), collapse = ":"))
  if (length(i) != 1L) stop("Coefficient not found: ", nm)
  i
}
joint <- function(m, v) {                     # Wald chi2(3) on R^q x v
  b <- coef(m); V <- vcov(m); k <- sapply(inter(v), function(n) idx(b, n))
  chi <- as.numeric(t(b[k]) %*% solve(V[k, k]) %*% b[k])
  c(chi2 = chi, p = pchisq(chi, length(k), lower.tail = FALSE))
}
asym <- function(m, v) {                      # H1 headline: d_HIGH - d_LOW, 1-sided +
  b <- coef(m); V <- vcov(m)
  h <- idx(b, paste0("R_HIGH:", v)); l <- idx(b, paste0("R_LOW:", v))
  z <- (b[h] - b[l]) / sqrt(V[h, h] + V[l, l] - 2 * V[h, l])
  c(z = unname(z), p = pnorm(unname(z), lower.tail = FALSE))
}
d1 <- function(m, v) {                        # H2 headline: d_LOW, 1-sided -
  b <- coef(m); V <- vcov(m); l <- idx(b, paste0("R_LOW:", v))
  z <- b[l] / sqrt(V[l, l])
  c(z = unname(z), p = pnorm(unname(z)))
}

# State variable pairs: behavioral S, matched uncertainty form, headline test
pairs <- list(
  list(id = "H1_DSENT", hyp = "H1", S = "D_SENT",      form = "dummy",
       lbl = "$D^{\\text{SENT}}$",     head = asym),
  list(id = "H1_SENT",  hyp = "H1", S = "SENT_ORTH_z", form = "cont",
       lbl = "$\\text{SENT}^\\perp$",  head = asym),
  list(id = "H1_AAII",  hyp = "H1", S = "D_AAII",      form = "dummy",
       lbl = "$D^{\\text{AAII}}$",     head = asym),
  list(id = "H2_MD",    hyp = "H2", S = "D_MD_DETREND", form = "dummy",
       lbl = "$D^{\\text{MD,Det}}$",   head = d1)
)
U_of <- function(form, h) if (form == "dummy") paste0("D_UNC", h) else paste0("JLN_UNC_", h, "M_z")

# --- 3. Estimate -------------------------------------------------------------
res <- list(); models <- list()
for (p in pairs) {
  m_a <- fit(p$S)
  for (h in HORIZONS) {
    U <- U_of(p$form, h)
    m_b <- fit(U); m_c <- fit(c(p$S, U))
    key <- paste(p$id, h, sep = "_h")
    res[[key]] <- data.frame(
      id = p$id, hyp = p$hyp, label = p$lbl, S = p$S, U = U, h = as.integer(h),
      S_chi_a = joint(m_a, p$S)[1], S_p_a = joint(m_a, p$S)[2],
      S_chi_c = joint(m_c, p$S)[1], S_p_c = joint(m_c, p$S)[2],
      U_chi_b = joint(m_b, U)[1],   U_p_b = joint(m_b, U)[2],
      U_chi_c = joint(m_c, U)[1],   U_p_c = joint(m_c, U)[2],
      head_z_a = p$head(m_a, p$S)[1], head_p_a = p$head(m_a, p$S)[2],
      head_z_c = p$head(m_c, p$S)[1], head_p_c = p$head(m_c, p$S)[2],
      n = nobs(m_c), row.names = NULL)
    if (h == MAIN_H) models[[p$id]] <- list(a = m_a, b = m_b, c = m_c, S = p$S, U = U)
  }
  cat(sprintf("  %-9s S terms: chi2 %6.2f (p=%.3f) alone -> %6.2f (p=%.3f) with U(h=%s) | U terms alone %6.2f (p=%.3f)\n",
              p$id, res[[paste0(p$id, "_h", MAIN_H)]]$S_chi_a, res[[paste0(p$id, "_h", MAIN_H)]]$S_p_a,
              res[[paste0(p$id, "_h", MAIN_H)]]$S_chi_c, res[[paste0(p$id, "_h", MAIN_H)]]$S_p_c,
              MAIN_H, res[[paste0(p$id, "_h", MAIN_H)]]$U_chi_b, res[[paste0(p$id, "_h", MAIN_H)]]$U_p_b))
}
res <- bind_rows(res)

# --- 4. Formatting helpers ---------------------------------------------------
f2 <- function(x) formatC(x, format = "f", digits = 2)
fp <- function(p) if (p < 0.001) "$<$0.001" else formatC(p, format = "f", digits = 3)
chi_cell <- function(chi, p) sprintf("%s (%s)", f2(chi), fp(p))
z_cell   <- function(z, p)   sprintf("%s%s (%s)", if (z >= 0) "+" else "$-$", f2(abs(z)), fp(p))
note_block <- function(txt) paste0("\\begin{singlespace}\\footnotesize\\noindent\n", txt,
                                   "\n\\end{singlespace}\n")
table_wrap <- function(caption, label, colspec, body, note, resize = FALSE) {
  tab <- c(paste0("\\begin{tabular}{", colspec, "}"), "\\toprule", body,
           "\\bottomrule", "\\end{tabular}")
  if (resize) tab <- c("\\resizebox{\\linewidth}{!}{%", tab, "}")
  c("\\begin{table}[H]", "\\centering", "\\footnotesize",
    paste0("\\caption{", caption, "}"), paste0("\\label{tab:", label, "}"),
    tab, "\\end{table}", note_block(note))
}
COMMON_NOTE <- paste0(
  "Sample: active funds, \\textcite{Evans2010}-corrected panel, December excluded, ",
  "lagged TNA of at least USD 5 million; one common sample of ",
  formatC(nrow(samp), format = "d", big.mark = ","), " fund-months (",
  formatC(nlevels(samp$Ticker), format = "d", big.mark = ","), " funds, ",
  nlevels(samp$yearmo), " months) in which all ",
  "state variables and all uncertainty horizons are available. Uncertainty: ",
  "\\textcite{JuradoLudvigsonNg2015} macroeconomic uncertainty (FRED JLNUM1M, ",
  "JLNUM3M, JLNUM12M); $D^{\\text{UNC}}$ is 1 when the index is in its top 34\\%, ",
  "and the continuous index is standardised within the sample. Specification as ",
  "the H1/H2 primary tables: contemporaneous states, fund fixed effects, controls ",
  "as in Table~\\ref{tab:H1_regression}, standard errors two-way clustered on fund ",
  "and calendar month.")

# --- 5. Main-text summary table (h = MAIN_H) ---------------------------------
mh <- res %>% filter(h == as.integer(MAIN_H))
body <- c(
  paste0(" & & \\multicolumn{2}{c}{Behavioral terms: joint $\\chi^2_3$ ($p$)} & ",
         "Uncertainty terms & \\multicolumn{2}{c}{Headline $z$ ($p$)} \\\\"),
  "\\cmidrule(lr){3-4}\\cmidrule(lr){5-5}\\cmidrule(lr){6-7}",
  "Hyp. & State & Without $U$ & With $U$ & $\\chi^2_3$ ($p$), with $S$ & Without $U$ & With $U$ \\\\",
  "\\midrule",
  vapply(seq_len(nrow(mh)), function(i) with(mh[i, ], paste0(
    hyp, " & ", label, " & ", chi_cell(S_chi_a, S_p_a), " & ", chi_cell(S_chi_c, S_p_c),
    " & ", chi_cell(U_chi_c, U_p_c), " & ", z_cell(head_z_a, head_p_a), " & ",
    z_cell(head_z_c, head_p_c), " \\\\")), character(1)))
note_main <- paste0(
  "Each row re-estimates the primary H1 or H2 specification for state variable $S$ ",
  "without and with the interactions of the rank segments $R^{\\text{LOW}}$, ",
  "$R^{\\text{MID}}$, $R^{\\text{HIGH}}$ with macroeconomic uncertainty $U$ ",
  sprintf("(%s-month horizon; dummy states are paired with $D^{\\text{UNC}}$, ", MAIN_H),
  "the continuous sentiment index with the continuous uncertainty index). ",
  "Behavioral terms: Wald test that the three $R^q\\times S$ interactions are zero. ",
  "Uncertainty terms: the same test for $R^q\\times U$ in the joint model. ",
  "Headline $z$: H1, $\\delta_{\\text{HIGH}}-\\delta_{\\text{LOW}}$ with the ",
  "one-sided $p$-value for a positive sign; H2, $\\delta_{\\text{LOW}}$ with the ",
  "one-sided $p$-value for a negative (disposition) sign. Horizons of 3 and 12 months ",
  "and full coefficient tables are in Appendix~\\ref{app:uncertainty}. ", COMMON_NOTE)
writeLines(table_wrap("Sentiment or Uncertainty? Behavioral State Effects Controlling for Macroeconomic Uncertainty",
                      "unc_summary", "llccccc", body, note_main, resize = TRUE),
           file.path(WORKING_DIR, "table_unc_summary.tex"))

# --- 6. Appendix: horizons ----------------------------------------------------
hz <- res %>% arrange(match(id, sapply(pairs, `[[`, "id")), h)
body_h <- c(
  "Hyp. & State & $h$ & $S$ terms, with $U$ & $U$ terms, with $S$ & $U$ terms, alone & Headline $z$, with $U$ \\\\",
  "\\midrule",
  vapply(seq_len(nrow(hz)), function(i) with(hz[i, ], paste0(
    if (h == as.integer(HORIZONS[1])) paste0(hyp, " & ", label) else " & ",
    " & ", h, " & ", chi_cell(S_chi_c, S_p_c), " & ", chi_cell(U_chi_c, U_p_c),
    " & ", chi_cell(U_chi_b, U_p_b), " & ", z_cell(head_z_c, head_p_c), " \\\\",
    if (h == as.integer(tail(HORIZONS, 1)) && i < nrow(hz)) " \\midrule" else "")),
    character(1)))
writeLines(table_wrap("Uncertainty Horse Race Across Forecast Horizons", "unc_horizons",
                      "llccccc", body_h, paste0(
  "$h$: forecast horizon (months) of the uncertainty index. Columns report joint ",
  "$\\chi^2_3$ tests ($p$-values in parentheses) of the behavioral interactions ",
  "$R^q\\times S$ and of the uncertainty interactions $R^q\\times U$ in the joint ",
  "model, of $R^q\\times U$ in a model without $S$, and the headline statistic in ",
  "the joint model (see Table~\\ref{tab:unc_summary}). ", COMMON_NOTE), resize = TRUE),
  file.path(WORKING_DIR, "table_unc_horizons.tex"))

# --- 7. Appendix: coefficient tables -----------------------------------------
cell <- function(m, nm) {
  b <- coef(m); se <- sqrt(diag(vcov(m)))
  i <- which(names(b) == nm)
  if (!length(i) && grepl(":", nm))
    i <- which(names(b) == paste(rev(strsplit(nm, ":")[[1]]), collapse = ":"))
  if (!length(i)) return(c("", ""))
  t <- b[i] / se[i]; st <- names(STAR_THR)[which(abs(t) >= STAR_THR)[1]]
  neg <- function(x) sub("^-", "$-$", x)                # typeset minus sign
  c(paste0(neg(formatC(b[i], format = "f", digits = 4)), if (is.na(st)) "" else paste0("$^{", st, "}$")),
    paste0("(", neg(formatC(t, format = "f", digits = 2)), ")"))
}
coef_table <- function(ids, caption, label, s_lbls) {
  mods <- unlist(lapply(ids, function(k) models[[k]][c("a", "b", "c")]), recursive = FALSE)
  Ss <- rep(sapply(ids, function(k) models[[k]]$S), each = 3)
  Us <- rep(sapply(ids, function(k) models[[k]]$U), each = 3)
  rows <- list(c("R_LOW", "$R^{\\text{LOW}}$"), c("R_MID", "$R^{\\text{MID}}$"),
               c("R_HIGH", "$R^{\\text{HIGH}}$"), c("S", "$S$"),
               c("R_LOW:S", "$R^{\\text{LOW}}\\times S$"), c("R_MID:S", "$R^{\\text{MID}}\\times S$"),
               c("R_HIGH:S", "$R^{\\text{HIGH}}\\times S$"), c("U", "$U$"),
               c("R_LOW:U", "$R^{\\text{LOW}}\\times U$"), c("R_MID:U", "$R^{\\text{MID}}\\times U$"),
               c("R_HIGH:U", "$R^{\\text{HIGH}}\\times U$"))
  out <- c()
  for (r in rows) {
    cs <- lapply(seq_along(mods), function(j)
      cell(mods[[j]], gsub("U", Us[j], gsub("S", Ss[j], r[1], fixed = TRUE), fixed = TRUE)))
    out <- c(out, paste0(r[2], " & ", paste(sapply(cs, `[`, 1), collapse = " & "), " \\\\"),
             paste0(" & ", paste(sapply(cs, `[`, 2), collapse = " & "), " \\\\[2pt]"))
  }
  hdr1 <- paste0(" & ", paste(sprintf("\\multicolumn{3}{c}{$S=$ %s}", s_lbls), collapse = " & "), " \\\\")
  cmid <- paste(sprintf("\\cmidrule(lr){%d-%d}", 2 + 3 * (seq_along(ids) - 1),
                        4 + 3 * (seq_along(ids) - 1)), collapse = "")
  hdr2 <- paste0(" & ", paste(rep(c("$S$", "$U$", "$S$ and $U$"), length(ids)), collapse = " & "), " \\\\")
  foot <- c("\\midrule",
            paste0("Controls & ", paste(rep("Yes", length(mods)), collapse = " & "), " \\\\"),
            paste0("Fund FE & ", paste(rep("Yes", length(mods)), collapse = " & "), " \\\\"),
            paste0("$N$ & ", paste(sapply(mods, function(m) formatC(nobs(m), format = "d", big.mark = ",")),
                                   collapse = " & "), " \\\\"),
            paste0("Within $R^2$ & ", paste(sapply(mods, function(m) formatC(fitstat(m, "wr2")[[1]],
                                   format = "f", digits = 4)), collapse = " & "), " \\\\"))
  table_wrap(caption, label, paste0("l", strrep("c", length(mods))),
             c(hdr1, cmid, hdr2, "\\midrule", out, foot), paste0(
    sprintf("Uncertainty horizon: %s month%s. $S$: behavioral state variable; $U$: matched ", MAIN_H, if (MAIN_H == "1") "" else "s"),
    "uncertainty measure ($D^{\\text{UNC}}$ for dummy states, standardised index for ",
    "the continuous sentiment index). $t$-statistics in parentheses; $^{*}$, $^{**}$, ",
    "$^{***}$: significant at 10\\%, 5\\%, 1\\%. ", COMMON_NOTE), resize = length(ids) > 1)  # narrow tables keep natural size
}
writeLines(coef_table(c("H1_DSENT", "H1_SENT"),
                      "H1 With Macroeconomic Uncertainty: Coefficient Estimates", "unc_H1",
                      c("$D^{\\text{SENT}}$", "$\\text{SENT}^\\perp$")),
           file.path(WORKING_DIR, "table_unc_H1.tex"))
writeLines(coef_table(c("H1_AAII"),
                      "H1 (AAII) With Macroeconomic Uncertainty: Coefficient Estimates", "unc_AAII",
                      c("$D^{\\text{AAII}}$")),
           file.path(WORKING_DIR, "table_unc_AAII.tex"))
writeLines(coef_table(c("H2_MD"),
                      "H2 With Macroeconomic Uncertainty: Coefficient Estimates", "unc_H2",
                      c("$D^{\\text{MD,Det}}$")),
           file.path(WORKING_DIR, "table_unc_H2.tex"))

saveRDS(list(results = res, n = nrow(samp), main_h = MAIN_H),
        file.path(WORKING_DIR, "uncertainty_robustness.rds"))
cat("Wrote table_unc_summary.tex, table_unc_horizons.tex, table_unc_H1.tex, table_unc_AAII.tex, table_unc_H2.tex\n")
print(res %>% transmute(id, h, S_p_a = round(S_p_a, 3), S_p_c = round(S_p_c, 3),
                        U_p_c = round(U_p_c, 3), head_z_a = round(head_z_a, 2),
                        head_z_c = round(head_z_c, 2)), row.names = FALSE)
