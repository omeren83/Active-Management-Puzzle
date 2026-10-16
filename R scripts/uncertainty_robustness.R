# uncertainty_robustness.R                                               v1.3
# =============================================================================
# v1.3 (Oct 2026): basis points use the native rank-space calibration in
#   rank_calibration.xlsx (psychological_premium.R v1.5), not the momentum
#   spread.
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
#   table_unc_H3.tex        appendix: H3 lottery demand and PPF (v1.2, Part 2)
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

# =============================================================================
# PART 2 (v1.2) -- H3 lottery demand and the Psychological Premium Framework
# with the uncertainty control. Added after the H1/H2 horse race (not
# pre-committed): H3 and the PPF condition on the same D_SENT regime that
# loses significance above. Same pairing (D_SENT with D_UNC) and horizons.
# =============================================================================
LOTTERY <- c(ActR2 = "ActR2", ActSkew = "ActSkew", MAX12 = "MAX12")

# H3 / PPF sample: identical filters to H3_lottery_demand.R and
# psychological_premium.R, plus the uncertainty dummies
samp3 <- panel_reg %>%
  filter(!excluded_h3, !is_december) %>%
  filter(if_all(all_of(c(core, LOTTERY, "D_SENT", "D_SENT_lag",
                         paste0("D_UNC", HORIZONS))), ~ !is.na(.))) %>%
  mutate(across(c(D_SENT, all_of(paste0("D_UNC", HORIZONS))), as.numeric),
         Ticker = as.factor(Ticker), yearmo = as.factor(yearmo))
for (v in c("ActSkew", "MAX12")) {               # 1/99 winsorisation, as H3
  q <- quantile(samp3[[v]], c(0.01, 0.99), names = FALSE)
  samp3[[v]] <- pmin(pmax(samp3[[v]], q[1]), q[2])
}
for (v in LOTTERY) samp3[[v]] <- as.numeric(scale(samp3[[v]]))   # z-scores
cat(sprintf("\nH3/PPF uncertainty sample: %d fund-months | %d funds | %d months\n",
            nrow(samp3), nlevels(droplevels(samp3$Ticker)), nlevels(droplevels(samp3$yearmo))))

fit3 <- function(L, states, seg_states = character(0)) {  # seg_states: R^q x state (PPF)
  rhs <- c("R_LOW", "R_MID", "R_HIGH", CONTROLS, L, states,
           paste0(L, ":", states), unlist(lapply(seg_states, inter)))
  feols(as.formula(paste("flow ~", paste(rhs, collapse = " + "), "|", FE)),
        data = samp3, cluster = ~ Ticker + yearmo)
}
zone <- function(m, nm) {                        # z and 1-sided p (positive) for one coefficient
  b <- coef(m); V <- vcov(m); i <- idx(b, nm); z <- unname(b[i] / sqrt(V[i, i]))
  c(b = unname(b[i]), z = z, p = pnorm(z, lower.tail = FALSE))
}
joint2 <- function(m, L, S) {                    # H3 joint chi2(2): lottery main + lottery x S
  b <- coef(m); V <- vcov(m); k <- c(idx(b, L), idx(b, paste0(L, ":", S)))
  chi <- as.numeric(t(b[k]) %*% solve(V[k, k]) %*% b[k])
  c(chi2 = chi, p = pchisq(chi, 2, lower.tail = FALSE))
}

# --- 8. H3 horse race ----------------------------------------------------------
res3 <- list()
for (L in LOTTERY) {
  m_a <- fit3(L, "D_SENT")
  for (h in HORIZONS) {
    U <- paste0("D_UNC", h)
    m_b <- fit3(L, U); m_c <- fit3(L, c("D_SENT", U))
    res3[[paste(L, h)]] <- data.frame(
      L = L, h = as.integer(h),
      J_chi_a = joint2(m_a, L, "D_SENT")[1], J_p_a = joint2(m_a, L, "D_SENT")[2],
      J_chi_c = joint2(m_c, L, "D_SENT")[1], J_p_c = joint2(m_c, L, "D_SENT")[2],
      amp_b_a = zone(m_a, paste0(L, ":D_SENT"))[1],
      amp_z_a = zone(m_a, paste0(L, ":D_SENT"))[2], amp_p_a = zone(m_a, paste0(L, ":D_SENT"))[3],
      amp_b_c = zone(m_c, paste0(L, ":D_SENT"))[1],
      amp_z_c = zone(m_c, paste0(L, ":D_SENT"))[2], amp_p_c = zone(m_c, paste0(L, ":D_SENT"))[3],
      uamp_z_b = zone(m_b, paste0(L, ":", U))[2], uamp_p_b = zone(m_b, paste0(L, ":", U))[3],
      uamp_z_c = zone(m_c, paste0(L, ":", U))[2], uamp_p_c = zone(m_c, paste0(L, ":", U))[3],
      n = nobs(m_c), row.names = NULL)
  }
  r <- res3[[paste(L, MAIN_H)]]
  cat(sprintf("  H3 %-7s amp z %+.2f (p=%.3f) alone -> %+.2f (p=%.3f) with U | U amp z alone %+.2f\n",
              L, r$amp_z_a, r$amp_p_a, r$amp_z_c, r$amp_p_c, r$uamp_z_b))
}
res3 <- bind_rows(res3)

# --- 9. PPF (MAX12) with uncertainty ------------------------------------------
# SP(S,U) = -(eta + phi*S + psi*U) / (beta_q + delta_q*S + gamma_q*U).
# Sentiment contrast at low uncertainty (U = 0): Diff_S = SP(1,0) - SP(0,0),
# the PPF Diff holding uncertainty fixed. Uncertainty contrast at low
# sentiment: Diff_U = SP(0,1) - SP(0,0). Delta-method SEs (numerical gradient).
sp_fun <- function(b, q, S, U, Uv) {
  g <- function(nm) if (is.null(nm)) 0 else b[[nm]]
  eta <- b[["MAX12"]]; beta <- b[[paste0("R_", q)]]
  num <- eta + S * g(.nm(b, "MAX12:D_SENT")) + U * if (is.null(Uv)) 0 else g(.nm(b, paste0("MAX12:", Uv)))
  den <- beta + S * g(.nm(b, paste0("R_", q, ":D_SENT"))) +
         U * if (is.null(Uv)) 0 else g(.nm(b, paste0("R_", q, ":", Uv)))
  -num / den
}
.nm <- function(b, nm) {                          # name as stored by fixest
  if (nm %in% names(b)) return(nm)
  r <- paste(rev(strsplit(nm, ":")[[1]]), collapse = ":")
  if (r %in% names(b)) return(r)
  stop("Coefficient not found: ", nm)
}
dm <- function(m, f) {                            # delta method for scalar f(b)
  b <- coef(m); V <- vcov(m); est <- f(b)
  gr <- vapply(seq_along(b), function(j) {
    e <- 1e-6 * max(1, abs(b[j])); bp <- b; bm <- b; bp[j] <- b[j] + e; bm[j] <- b[j] - e
    (f(bp) - f(bm)) / (2 * e) }, numeric(1))
  c(est = est, se = sqrt(as.numeric(t(gr) %*% V %*% gr)))
}
# Native rank-space calibration written by psychological_premium.R (v1.5)
RANK_CAL_FILE <- file.path(WORKING_DIR, "rank_calibration.xlsx")
BPS_PER_RANK <- NA_real_
if (file.exists(RANK_CAL_FILE)) {
  .cal <- readxl::read_excel(RANK_CAL_FILE, sheet = "native")
  BPS_PER_RANK <- round(.cal$alpha[.cal$portfolio == "Q5-Q1"] / 0.8 * 100)
  rm(.cal)
}
if (is.na(BPS_PER_RANK)) warning("rank_calibration.xlsx not found: basis points not reported.")

ppf <- list()
m_ppf_a <- fit3("MAX12", "D_SENT", seg_states = "D_SENT")
for (h in HORIZONS) {
  U <- paste0("D_UNC", h)
  m_ppf_c <- fit3("MAX12", c("D_SENT", U), seg_states = c("D_SENT", U))
  for (q in c("LOW", "MID", "HIGH")) {
    lo_a <- dm(m_ppf_a, function(b) sp_fun(b, q, 0, 0, NULL))
    hi_a <- dm(m_ppf_a, function(b) sp_fun(b, q, 1, 0, NULL))
    df_a <- dm(m_ppf_a, function(b) sp_fun(b, q, 1, 0, NULL) - sp_fun(b, q, 0, 0, NULL))
    lo_c <- dm(m_ppf_c, function(b) sp_fun(b, q, 0, 0, U))
    hi_c <- dm(m_ppf_c, function(b) sp_fun(b, q, 1, 0, U))
    df_c <- dm(m_ppf_c, function(b) sp_fun(b, q, 1, 0, U) - sp_fun(b, q, 0, 0, U))
    du_c <- dm(m_ppf_c, function(b) sp_fun(b, q, 0, 1, U) - sp_fun(b, q, 0, 0, U))
    ppf[[paste(h, q)]] <- data.frame(
      h = as.integer(h), seg = q,
      lo_a = lo_a[1], hi_a = hi_a[1], diff_a = df_a[1], diff_se_a = df_a[2],
      diff_p_a = pnorm(df_a[1] / df_a[2]),            # 1-sided, Diff < 0
      lo_c = lo_c[1], hi_c = hi_c[1], diff_c = df_c[1], diff_se_c = df_c[2],
      diff_p_c = pnorm(df_c[1] / df_c[2]),
      du_c = du_c[1], du_se_c = du_c[2], du_p_c = pnorm(du_c[1] / du_c[2]),
      row.names = NULL)
  }
}
ppf <- bind_rows(ppf)
pm <- ppf %>% filter(h == as.integer(MAIN_H), seg == "MID")
cat(sprintf("  PPF MAX12 MID: Diff %.3f (p=%.3f) alone -> %.3f (p=%.3f) with U; uncertainty Diff %.3f (p=%.3f)\n",
            pm$diff_a, pm$diff_p_a, pm$diff_c, pm$diff_p_c, pm$du_c, pm$du_p_c))

# --- 10. Appendix table: H3 and PPF with uncertainty --------------------------
f3 <- function(x) { s <- formatC(x, format = "f", digits = 3); sub("^-", "$-$", s) }
pse <- function(est, se) sprintf("%s [%s]", f3(est), formatC(se, format = "f", digits = 3))
bp  <- function(x) if (is.na(BPS_PER_RANK)) "--" else as.character(round(abs(x) * BPS_PER_RANK))
r1 <- res3 %>% filter(h == as.integer(MAIN_H))
body3 <- c(
  " & \\multicolumn{2}{c}{Joint $\\chi^2_2$ ($p$)} & \\multicolumn{2}{c}{Amplification $z$ ($p$)} & \\multicolumn{2}{c}{$\\text{AS}\\times U$ $z$ ($p$)} \\\\",
  "\\cmidrule(lr){2-3}\\cmidrule(lr){4-5}\\cmidrule(lr){6-7}",
  "Panel A: H3 & Without $U$ & With $U$ & Without $U$ & With $U$ & $U$ alone & With $S$ \\\\",
  "\\midrule",
  vapply(seq_len(nrow(r1)), function(i) with(r1[i, ], paste0(
    L, " & ", chi_cell(J_chi_a, J_p_a), " & ", chi_cell(J_chi_c, J_p_c), " & ",
    z_cell(amp_z_a, amp_p_a), " & ", z_cell(amp_z_c, amp_p_c), " & ",
    z_cell(uamp_z_b, uamp_p_b), " & ", z_cell(uamp_z_c, uamp_p_c), " \\\\")), character(1)),
  "\\midrule",
  " & \\multicolumn{3}{c}{Without $U$} & \\multicolumn{3}{c}{With $U$ (at $D^{\\text{UNC}}=0$)} \\\\",
  "\\cmidrule(lr){2-4}\\cmidrule(lr){5-7}",
  "Panel B: PPF, MAX12 & SP$_{\\text{low}}$ & SP$_{\\text{high}}$ & Diff [SE] ($p$) & SP$_{\\text{low}}$ & SP$_{\\text{high}}$ & Diff [SE] ($p$) \\\\",
  "\\midrule",
  vapply(c("LOW", "MID", "HIGH"), function(q) with(ppf[ppf$h == as.integer(MAIN_H) & ppf$seg == q, ],
    paste0("$R^{\\text{", q, "}}$ & ", f3(lo_a), " & ", f3(hi_a), " & ",
           pse(diff_a, diff_se_a), " (", fp(diff_p_a), ") & ", f3(lo_c), " & ", f3(hi_c), " & ",
           pse(diff_c, diff_se_c), " (", fp(diff_p_c), ") \\\\")), character(1)),
  with(pm, paste0("\\multicolumn{7}{l}{$R^{\\text{MID}}$, basis points per SD of MAX12: ",
                  bp(lo_a), " / ", bp(hi_a), " without $U$; ", bp(lo_c), " / ", bp(hi_c),
                  " with $U$. Uncertainty contrast at low sentiment: ",
                  pse(du_c, du_se_c), " (", fp(du_p_c), ").} \\\\")))
r3h <- res3 %>% filter(L == "MAX12"); p3h <- ppf %>% filter(seg == "MID")
note3 <- paste0(
  "$U=D^{\\text{UNC}}$ at the ", MAIN_H, "-month horizon. Panel A re-estimates the H3 ",
  "specification of Table~\\ref{tab:H3_regression} (lottery proxy AS, $D^{\\text{SENT}}$, ",
  "AS$\\times D^{\\text{SENT}}$, rank segments, controls, fund fixed effects) without and with ",
  "$U$ and AS$\\times U$. Joint $\\chi^2_2$: AS and AS$\\times D^{\\text{SENT}}$. Amplification: ",
  "AS$\\times D^{\\text{SENT}}$, one-sided $p$ for a positive sign; the last two columns test ",
  "AS$\\times U$ in a model with $U$ in place of $D^{\\text{SENT}}$ and in the joint model. ",
  "Panel B re-estimates the Psychological Premium regression for MAX12 ",
  "(Table~\\ref{tab:psychological_premium}) adding $U$, $R^q\\times U$ and MAX12$\\times U$; ",
  "shadow prices with $U$ are evaluated at low uncertainty, so Diff is the sentiment contrast ",
  "holding uncertainty fixed. Diff standard errors by the delta method; one-sided $p$ for ",
  "Diff $<0$. Uncertainty contrast: SP at $D^{\\text{UNC}}=1$ minus SP at $D^{\\text{UNC}}=0$, ",
  "both at $D^{\\text{SENT}}=0$ and $R^{\\text{MID}}$. ",
  if (!is.na(BPS_PER_RANK)) sprintf("Basis points use the calibration of Table~\\ref{tab:psychological_premium} (%d bp per rank point). ", as.integer(BPS_PER_RANK)) else "",
  sprintf(paste0("At the 3- and 12-month horizons the MAX12 amplification $z$ with $U$ is %s and %s, ",
                 "and the $R^{\\text{MID}}$ Diff is %s ($p$ = %s) and %s ($p$ = %s). "),
          z_cell(r3h$amp_z_c[r3h$h == 3], r3h$amp_p_c[r3h$h == 3]),
          z_cell(r3h$amp_z_c[r3h$h == 12], r3h$amp_p_c[r3h$h == 12]),
          f3(p3h$diff_c[p3h$h == 3]), fp(p3h$diff_p_c[p3h$h == 3]),
          f3(p3h$diff_c[p3h$h == 12]), fp(p3h$diff_p_c[p3h$h == 12])),
  "Sample: the H3 sample (active funds, H3 / activeness subsample, December excluded) ",
  sprintf("with the uncertainty indicators available: %s fund-months (%s funds). ",
          formatC(nrow(samp3), format = "d", big.mark = ","),
          formatC(nlevels(droplevels(samp3$Ticker)), format = "d", big.mark = ",")),
  "Lottery proxies winsorised at 1/99\\% (ActSkew, MAX12) and standardised in the sample. ",
  "Standard errors two-way clustered on fund and calendar month.")
writeLines(table_wrap("H3 and the Psychological Premium With Macroeconomic Uncertainty",
                      "unc_H3", "lcccccc", body3, note3, resize = TRUE),
           file.path(WORKING_DIR, "table_unc_H3.tex"))


saveRDS(list(results = res, h3 = res3, ppf = ppf, n = nrow(samp), n3 = nrow(samp3), main_h = MAIN_H),
        file.path(WORKING_DIR, "uncertainty_robustness.rds"))
cat("Wrote table_unc_summary.tex, table_unc_horizons.tex, table_unc_H1.tex, table_unc_AAII.tex, table_unc_H2.tex, table_unc_H3.tex\n")
print(res %>% transmute(id, h, S_p_a = round(S_p_a, 3), S_p_c = round(S_p_c, 3),
                        U_p_c = round(U_p_c, 3), head_z_a = round(head_z_a, 2),
                        head_z_c = round(head_z_c, 2)), row.names = FALSE)
