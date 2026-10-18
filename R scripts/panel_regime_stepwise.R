# =============================================================================
# panel_regime_stepwise.R                                                  v1.0
# =============================================================================
# Defense extensions to the H1-H4 panel tests (pre-committed 15 Oct 2026):
#   (3) Regime dependence: pooled primary H1-H3 with Post2010 interactions
#       (main text) and separate P1/P2/P3 regressions (Appendix J).
#   (4) Stepwise specifications in a fixed order (Appendix J).
#   (7) TNA-floor sensitivity: headline H1-H4 tests with floors of USD 5M
#       (baseline), 1M and none (Appendix J).
# Samples, controls, fixed effects and clustering are those of the primary
# H1-H4 tables; the floor-5 rows reproduce their headline statistics.
#
# Requires: panel_reg (session) or panel_reg.rds; panel_reg_below_floor.rds
#   (panel_regressions_setup.R v1.7); breaktest_results.xlsx.
# Outputs: panel_regime_results.xlsx, table_regime_pooled.tex (Chapter 5),
#   table_regime_separate.tex, table_regime_pooled_fe.tex, table_stepwise.tex,
#   table_tna_floor.tex (Appendix J).
# =============================================================================

suppressPackageStartupMessages({
  library(dplyr)
  library(fixest)
  library(readxl)
  library(writexl)
})

if (!exists("WORKING_DIR")) WORKING_DIR <- getwd()
setwd(WORKING_DIR)

SUBPERIOD_BREAKS <- c("2006-03", "2010-08")   # last month of P1 and P2
BREAKTEST_FILE   <- "breaktest_results.xlsx"
LOW_POWER_MONTHS <- 12L
FLOORS           <- c(5, 1, 0)                # USD millions; 5 = baseline

.bd <- format(as.Date(read_excel(BREAKTEST_FILE, sheet = "Break Dates")[["Point Estimate"]]), "%Y-%m")
if (!all(SUBPERIOD_BREAKS %in% .bd))
  stop("SUBPERIOD_BREAKS are not all break dates in ", BREAKTEST_FILE, call. = FALSE)

if (!exists("panel_reg")) panel_reg <- readRDS(file.path(WORKING_DIR, "panel_reg.rds"))
below_floor <- readRDS(file.path(WORKING_DIR, "panel_reg_below_floor.rds"))

# --- 1. Estimation samples (identical to the H1-H4 scripts) ------------------
core_rhs <- c("flow", "R_LOW", "R_MID", "R_HIGH", "log_TNA", "log_Age", "ExpRatio",
              "LoadDummy", "ret_vol", "Turnover", "style_flow_lag")
CTRL     <- c("log_TNA", "log_Age", "ExpRatio", "LoadDummy", "ret_vol", "Turnover",
              "style_flow_lag")
CTRL_H4  <- setdiff(CTRL, "ExpRatio")   # ExpRatio enters H4 as a regressor

fac <- function(d) d %>% mutate(Ticker = as.factor(Ticker), yearmo = as.factor(yearmo),
                                Lipper_Category = as.factor(Lipper_Category))
need <- function(d, v) d %>% filter(if_all(all_of(v), ~ !is.na(.)))
mk_h1 <- function(p) need(p %>% filter(!is_december),
                          c(core_rhs, "D_SENT", "D_AAII", "SENT_ORTH", "D_SENT_lag",
                            "D_AAII_lag", "SENT_ORTH_lag")) %>%
  mutate(SENT_ORTH_z = as.numeric(scale(SENT_ORTH))) %>% fac()
mk_md <- function(p) need(p %>% filter(!is_december),
                          c(core_rhs, "D_MD_DETREND", "D_SENT", "D_MD_DETREND_lag",
                            "D_SENT_lag")) %>%
  mutate(D_MD_DETREND = as.numeric(D_MD_DETREND), D_SENT = as.numeric(D_SENT)) %>% fac()
mk_h3 <- function(p) {
  s <- need(p %>% filter(!excluded_h3, !is_december),
            c(core_rhs, "ActR2", "ActSkew", "MAX12", "D_SENT", "D_SENT_lag"))
  for (v in c("ActSkew", "MAX12")) {
    q <- quantile(s[[v]], c(0.01, 0.99), na.rm = TRUE); s[[v]] <- pmin(pmax(s[[v]], q[1]), q[2])
  }
  for (v in c("ActR2", "ActSkew", "MAX12")) s[[v]] <- as.numeric(scale(s[[v]]))
  fac(s)
}
mk_h4 <- function(p) mk_h1(p) %>% filter(ExpRatio > 0)

# Hypothesis specifications: kind "rank" = R^q x S (chi2_3); "lottery" =
# MAX12 and MAX12 x D_SENT (chi2_2); "fee" = H4 (chi2_4, Lipper x month FE)
SPECS <- list(
  list(id = "H1_DSENT", hyp = "H1", lbl = "$D^{\\text{SENT}}$", S = "D_SENT",      kind = "rank",    mk = mk_h1),
  list(id = "H1_SENT",  hyp = "H1", lbl = "$\\text{SENT}^\\perp$", S = "SENT_ORTH_z", kind = "rank", mk = mk_h1),
  list(id = "H1_AAII",  hyp = "H1", lbl = "$D^{\\text{AAII}}$", S = "D_AAII",      kind = "rank",    mk = mk_h1),
  list(id = "H2_MD",    hyp = "H2", lbl = "$D^{\\text{MD,Det}}$", S = "D_MD_DETREND", kind = "rank", mk = mk_md),
  list(id = "H3_MAX12", hyp = "H3", lbl = "MAX12 $\\times D^{\\text{SENT}}$", S = "D_SENT", kind = "lottery", mk = mk_h3),
  list(id = "H4_DSENT", hyp = "H4", lbl = "$D^{\\text{SENT}}$", S = "D_SENT",      kind = "fee",     mk = mk_h4),
  list(id = "H4_SENT",  hyp = "H4", lbl = "$\\text{SENT}^\\perp$", S = "SENT_ORTH_z", kind = "fee",  mk = mk_h4),
  list(id = "H4_AAII",  hyp = "H4", lbl = "$D^{\\text{AAII}}$", S = "D_AAII",      kind = "fee",     mk = mk_h4))
REGIME_SPECS <- Filter(function(s) s$hyp %in% c("H1", "H2", "H3"), SPECS)

# Tested terms (as ":"-separated parts) and primary fixed effects per kind
RQ <- c("R_LOW", "R_MID", "R_HIGH")
test_terms <- function(sp) switch(sp$kind,
  rank    = paste(RQ, sp$S, sep = ":"),
  lottery = c("MAX12", paste("MAX12", sp$S, sep = ":")),
  fee     = c(paste("ExpRatio", sp$S, sep = ":"), paste(RQ, "ExpRatio", sp$S, sep = ":")))
primary_fe <- function(sp) if (sp$kind == "fee") "Lipper_Category^yearmo" else "Ticker"
rhs_main <- function(sp, ctrl = TRUE) {
  S <- sp$S
  x <- switch(sp$kind,
    rank    = c(RQ, S, paste(RQ, S, sep = ":")),
    lottery = c(RQ, "MAX12", S, paste("MAX12", S, sep = ":")),
    fee     = c(RQ, "ExpRatio", S, paste(RQ, S, sep = ":"), paste(RQ, "ExpRatio", sep = ":"),
                paste("ExpRatio", S, sep = ":"), paste(RQ, "ExpRatio", S, sep = ":")))
  c(x, if (ctrl) (if (sp$kind == "fee") CTRL_H4 else CTRL))
}
# Headline (signed) statistic, as in the H scripts: H1 delta_HIGH - delta_LOW,
# H2 delta_LOW, H3 delta (MAX12 x D_SENT), H4 delta_F (ExpRatio x S)
headline_w <- function(sp, terms) {
  w <- setNames(rep(0, length(terms)), terms)
  if (sp$hyp == "H1") { w[paste0("R_HIGH:", sp$S)] <- 1; w[paste0("R_LOW:", sp$S)] <- -1 }
  if (sp$hyp == "H2") w[paste0("R_LOW:", sp$S)] <- 1
  if (sp$hyp == "H3") w[paste0("MAX12:", sp$S)] <- 1
  if (sp$hyp == "H4") w[paste0("ExpRatio:", sp$S)] <- 1
  w
}

# --- 2. Wald helpers -----------------------------------------------------------
# Locate a coefficient whatever the order fixest gave the interaction parts
find_term <- function(bn, term) {
  key <- function(x) paste(sort(strsplit(x, ":", fixed = TRUE)[[1]]), collapse = ":")
  i <- which(vapply(bn, key, "") == key(term))
  if (length(i)) i[1] else NA_integer_
}
# Wald test of L b = 0; L rows given as named weight vectors over terms
wald <- function(m, Lrows) {
  b <- coef(m); V <- vcov(m); bn <- names(b)
  L <- t(vapply(Lrows, function(w) {
    r <- numeric(length(b)); idx <- vapply(names(w), find_term, 1L, bn = bn)
    if (anyNA(idx)) return(rep(NA_real_, length(b)))
    r[idx] <- w; r }, numeric(length(b))))
  if (anyNA(L)) return(c(chi2 = NA, df = NA, p = NA))
  est <- L %*% b; VL <- L %*% V %*% t(L)
  if (qr(VL)$rank < nrow(VL)) return(c(chi2 = NA, df = NA, p = NA))
  chi <- as.numeric(t(est) %*% solve(VL, est))
  c(chi2 = chi, df = nrow(L), p = pchisq(chi, nrow(L), lower.tail = FALSE))
}
unit_w <- function(term) setNames(1, term)   # not "unit": would mask grid::unit
joint <- function(m, terms) wald(m, lapply(terms, unit_w))
lin <- function(m, w) {    # single linear combination: estimate and z
  b <- coef(m); V <- vcov(m); bn <- names(b)
  idx <- vapply(names(w), find_term, 1L, bn = bn)
  if (anyNA(idx)) return(c(est = NA, z = NA, p = NA))
  r <- numeric(length(b)); r[idx] <- w
  est <- sum(r * b); se <- sqrt(as.numeric(t(r) %*% V %*% r))
  c(est = est, z = est / se, p = 2 * pnorm(-abs(est / se)))
}
fit <- function(rhs, fe, d) {
  f <- as.formula(paste("flow ~", paste(rhs, collapse = " + "),
                        if (!is.null(fe)) paste("|", fe) else ""))
  feols(f, data = d, cluster = ~ Ticker + yearmo, notes = FALSE, warn = FALSE)
}
r2_of <- function(m, fe) if (is.null(fe)) r2(m, "r2") else r2(m, "wr2")

regime_of <- function(date) {
  ym <- format(date, "%Y-%m")
  ifelse(ym <= SUBPERIOD_BREAKS[1], "P1", ifelse(ym <= SUBPERIOD_BREAKS[2], "P2", "P3"))
}

# Samples built once from the baseline panel
samples <- list(h1 = mk_h1(panel_reg), md = mk_md(panel_reg), h3 = mk_h3(panel_reg))
samp_of <- function(sp) switch(sp$id, H2_MD = samples$md, H3_MAX12 = samples$h3, samples$h1)
for (k in names(samples)) {
  samples[[k]]$POST   <- as.numeric(format(samples[[k]]$date, "%Y-%m") > SUBPERIOD_BREAKS[2])
  samples[[k]]$regime <- regime_of(samples[[k]]$date)
}

# --- 3. Regime: pooled model with Post2010 interactions (item 3) -------------
pooled_one <- function(sp, fe) {
  d <- samp_of(sp); base <- rhs_main(sp); tt <- test_terms(sp)
  # Every non-control regressor also interacted with POST
  inter <- setdiff(base, CTRL)
  rhs <- c(base, "POST", paste(inter, "POST", sep = ":"))
  m <- fit(rhs, fe, d)
  tt_post <- paste(tt, "POST", sep = ":")
  post_rows <- lapply(seq_along(tt), function(i) setNames(c(1, 1), c(tt[i], tt_post[i])))
  hw <- headline_w(sp, tt); hw <- hw[hw != 0]
  hw_post <- c(hw, setNames(hw, paste(names(hw), "POST", sep = ":")))
  pre <- joint(m, tt); post <- wald(m, post_rows); dpo <- joint(m, tt_post)
  h_pre <- lin(m, hw); h_post <- lin(m, hw_post)
  data.frame(id = sp$id, hyp = sp$hyp, fe = fe,
             pre_chi2 = pre[["chi2"]], pre_p = pre[["p"]],
             post_chi2 = post[["chi2"]], post_p = post[["p"]],
             dpost_chi2 = dpo[["chi2"]], dpost_df = dpo[["df"]], dpost_p = dpo[["p"]],
             head_pre = h_pre[["est"]], head_pre_z = h_pre[["z"]],
             head_post = h_post[["est"]], head_post_z = h_post[["z"]],
             n = nobs(m), months_pre = n_distinct(d$yearmo[d$POST == 0]),
             months_post = n_distinct(d$yearmo[d$POST == 1]))
}
cat("\n=== Pooled regime models ===\n")
pooled <- bind_rows(lapply(REGIME_SPECS, function(sp)
  bind_rows(pooled_one(sp, "Ticker"), pooled_one(sp, "Lipper_Category^yearmo"))))
print(pooled %>% select(id, fe, pre_p, post_p, dpost_chi2, dpost_p, head_pre_z, head_post_z), digits = 3)

# --- 4. Regime: separate P1/P2/P3 regressions (Appendix J) -------------------
state_months <- function(d, S) {
  if (!all(d[[S]] %in% c(0, 1))) return(NA_integer_)   # continuous state
  n_distinct(d$yearmo[d[[S]] == 1])
}
separate <- bind_rows(lapply(REGIME_SPECS, function(sp) {
  bind_rows(lapply(c("P1", "P2", "P3"), function(r) {
    d <- samp_of(sp) %>% filter(regime == r)
    sm <- state_months(d, sp$S); months <- n_distinct(d$yearmo)
    out <- data.frame(id = sp$id, hyp = sp$hyp, regime = r, n = nrow(d), months = months,
                      state_months = sm, chi2 = NA, df = NA, p = NA, head = NA, head_z = NA,
                      status = "ok")
    if (!is.na(sm) && (sm == 0L || sm == months)) { out$status <- "n.e."; return(out) }
    m <- tryCatch(fit(rhs_main(sp), primary_fe(sp), d), error = function(e) NULL)
    if (is.null(m)) { out$status <- "n.e."; return(out) }
    tt <- test_terms(sp); j <- joint(m, tt); h <- lin(m, { w <- headline_w(sp, tt); w[w != 0] })
    if (is.na(j[["chi2"]])) { out$status <- "n.e."; return(out) }
    out$chi2 <- j[["chi2"]]; out$df <- j[["df"]]; out$p <- j[["p"]]
    out$head <- h[["est"]]; out$head_z <- h[["z"]]; out$n <- nobs(m)
    if (!is.na(sm) && sm < LOW_POWER_MONTHS) out$status <- "low power"
    out
  }))
}))
print(separate, digits = 3)

# --- 5. Stepwise specifications (item 4) ---------------------------------------
STEP_LBL <- c("Rank segments", "+ State main effect", "+ Interactions", "+ Controls", "+ Fund FE")
stepwise <- bind_rows(lapply(REGIME_SPECS, function(sp) {
  d <- samp_of(sp); S <- sp$S; tt <- test_terms(sp)
  mains <- if (sp$kind == "lottery") c("MAX12", S) else S
  ints  <- if (sp$kind == "lottery") paste("MAX12", S, sep = ":") else paste(RQ, S, sep = ":")
  steps <- list(list(rhs = RQ, fe = NULL),
                list(rhs = c(RQ, mains), fe = NULL),
                list(rhs = c(RQ, mains, ints), fe = NULL),
                list(rhs = c(RQ, mains, ints, CTRL), fe = NULL),
                list(rhs = c(RQ, mains, ints, CTRL), fe = "Ticker"))
  bind_rows(lapply(seq_along(steps), function(k) {
    m <- fit(steps[[k]]$rhs, steps[[k]]$fe, d)
    ok <- all(!is.na(vapply(tt, find_term, 1L, bn = names(coef(m)))))
    j <- if (ok) joint(m, tt) else c(chi2 = NA, df = NA, p = NA)
    h <- if (ok) lin(m, { w <- headline_w(sp, tt); w[w != 0] }) else c(est = NA, z = NA, p = NA)
    data.frame(id = sp$id, hyp = sp$hyp, step = k, chi2 = j[["chi2"]], p = j[["p"]],
               head = h[["est"]], head_z = h[["z"]], r2 = r2_of(m, steps[[k]]$fe), n = nobs(m))
  }))
}))
print(stepwise, digits = 3)

# --- 6. TNA-floor sensitivity (item 7) ---------------------------------------
floor_res <- bind_rows(lapply(FLOORS, function(fl) {
  p <- bind_rows(panel_reg, below_floor %>% filter(log_TNA >= if (fl > 0) log(fl) else -Inf))
  ss <- list(h1 = mk_h1(p), md = mk_md(p), h3 = mk_h3(p), h4 = mk_h4(p))
  bind_rows(lapply(SPECS, function(sp) {
    d <- switch(sp$id, H2_MD = ss$md, H3_MAX12 = ss$h3,
                H4_DSENT = , H4_SENT = , H4_AAII = ss$h4, ss$h1)
    m <- fit(rhs_main(sp), primary_fe(sp), d); tt <- test_terms(sp)
    j <- joint(m, tt); h <- lin(m, { w <- headline_w(sp, tt); w[w != 0] })
    data.frame(floor = fl, id = sp$id, hyp = sp$hyp, chi2 = j[["chi2"]], df = j[["df"]],
               p = j[["p"]], head_z = h[["z"]], n = nobs(m))
  }))
}))
print(floor_res, digits = 3)

write_xlsx(list(Pooled = pooled, Separate = separate, Stepwise = stepwise,
                TNA_floor = floor_res), "panel_regime_results.xlsx")

# --- 7. LaTeX tables -------------------------------------------------------------
f2 <- function(x) ifelse(is.na(x), "--", ifelse(x < 0, paste0("$-$", formatC(abs(x), format = "f", digits = 2)),
                                                 formatC(x, format = "f", digits = 2)))
f4 <- function(x) ifelse(is.na(x), "--", ifelse(x < 0, paste0("$-$", formatC(abs(x), format = "f", digits = 4)),
                                                 formatC(x, format = "f", digits = 4)))
fp <- function(p) ifelse(is.na(p), "--", ifelse(p < 0.001, "$<$0.001", formatC(p, format = "f", digits = 3)))
chi_cell <- function(c, p) ifelse(is.na(c), "--", paste0(f2(c), " (", fp(p), ")"))
nfmt <- function(n) formatC(n, format = "d", big.mark = ",")
note_block <- function(txt) paste0("\\begin{singlespace}\\footnotesize\\noindent\n", txt,
                                   "\n\\end{singlespace}\n")
table_wrap <- function(caption, label, colspec, body, note) {
  c("\\begin{table}[H]", "\\centering", "\\footnotesize",
    paste0("\\caption{", caption, "}"), paste0("\\label{tab:", label, "}"),
    "\\resizebox{\\linewidth}{!}{%", paste0("\\begin{tabular}{", colspec, "}"),
    "\\toprule", body, "\\bottomrule", "\\end{tabular}", "}", "\\end{table}",
    note_block(note))
}
LBL <- setNames(vapply(SPECS, `[[`, "", "lbl"), vapply(SPECS, `[[`, "", "id"))
HEAD_TXT <- paste0(
  "Headline: H1, $\\delta_{\\text{HIGH}}-\\delta_{\\text{LOW}}$; H2, $\\delta_{\\text{LOW}}$; ",
  "H3, the MAX12 $\\times D^{\\text{SENT}}$ coefficient; ",
  "$z$-statistics in parentheses. ")
COMMON <- paste0(
  "Samples, controls and clustering as in the primary H1--H4 tables (fund fixed effects for ",
  "H1--H3, Lipper category $\\times$ month fixed effects for H4; standard errors two-way ",
  "clustered on fund and calendar month); December excluded; active funds, ",
  "\\textcite{Evans2010}-corrected panel.")
# H1--H3 only (regime and stepwise tables have no H4 rows)
COMMON_H13 <- paste0(
  "Samples, controls and clustering as in the primary H1--H3 tables (fund fixed effects; ",
  "standard errors two-way clustered on fund and calendar month); December excluded; active funds, ",
  "\\textcite{Evans2010}-corrected panel.")
COMMON_NOFE <- paste0(
  "Samples, controls and clustering as in the primary H1--H3 tables (standard errors two-way ",
  "clustered on fund and calendar month); December excluded; active funds, ",
  "\\textcite{Evans2010}-corrected panel.")
brk <- format(as.Date(paste0(SUBPERIOD_BREAKS[2], "-01")), "%B %Y")

pooled_table <- function(fe, label, caption, extra) {
  x <- pooled %>% filter(.data$fe == !!fe)
  body <- c(paste0("Hyp. & State & To ", format(as.Date(paste0(SUBPERIOD_BREAKS[2], "-01")), "%b %Y"),
                   " & After & Change & Headline, before & Headline, after & $N$ \\\\"),
            "\\midrule",
            vapply(seq_len(nrow(x)), function(i) with(x[i, ], paste0(
              hyp, " & ", LBL[[id]], " & ", chi_cell(pre_chi2, pre_p), " & ",
              chi_cell(post_chi2, post_p), " & ", chi_cell(dpost_chi2, dpost_p), " & ",
              f4(head_pre), " (", f2(head_pre_z), ") & ", f4(head_post), " (", f2(head_post_z),
              ") & ", nfmt(n), " \\\\")), character(1)))
  note <- paste0(
    "Each row re-estimates the primary specification with every behavioral regressor (rank ",
    "segments, state, lottery measure and their interactions) also interacted with ",
    "$\\text{Post}$, a dummy for months after ", brk, " (the P2/P3 boundary of ",
    "Table~\\ref{tab:breaktest}). Columns 3--5: Wald tests ($\\chi^2$, $p$) that the tested ",
    "terms (H1, H2: the three $R^q\\times S$ interactions; H3: MAX12 and MAX12 $\\times ",
    "D^{\\text{SENT}}$) are zero before ", brk, ", zero after (sum of the base and ",
    "$\\times\\text{Post}$ coefficients), and unchanged (the $\\times\\text{Post}$ terms, ",
    "$\\delta_{\\text{post}}$). ", HEAD_TXT, extra,
    if (fe == "Ticker") COMMON_H13 else COMMON_NOFE)
  writeLines(table_wrap(caption, label, "llcccccc", body, note), paste0("table_", label, ".tex"))
}
pooled_table("Ticker", "regime_pooled",
             "Behavioral Flow Effects Before and After the 2010 Regime Change", "")
pooled_table("Lipper_Category^yearmo", "regime_pooled_fe",
             "Behavioral Flow Effects Before and After 2010: Category $\\times$ Month Fixed Effects",
             "Fixed effects: Lipper category $\\times$ month instead of fund; the state and Post main effects are absorbed. ")

sep_body <- c("Hyp. & State & Regime & Months & State months & Joint $\\chi^2$ ($p$) & Headline ($z$) & $N$ \\\\",
              "\\midrule")
for (i in seq_len(nrow(separate))) {
  x <- separate[i, ]
  stat <- if (x$status == "n.e.") "n.e." else paste0(chi_cell(x$chi2, x$p), if (x$status == "low power") "$^{\\dagger}$" else "")
  hd <- if (x$status == "n.e.") "n.e." else paste0(f4(x$head), " (", f2(x$head_z), ")")
  sep_body <- c(sep_body, paste0(
    if (x$regime == "P1") paste0(x$hyp, " & ", LBL[[x$id]]) else " & ", " & ", x$regime, " & ",
    x$months, " & ", if (is.na(x$state_months)) "cont." else x$state_months, " & ", stat, " & ",
    hd, " & ", nfmt(x$n), " \\\\", if (x$regime == "P3" && i < nrow(separate)) " \\midrule" else ""))
}
writeLines(table_wrap("Behavioral Flow Tests Estimated Separately by Regime", "regime_separate",
  "lllcccccc", sep_body, paste0(
  "Primary specification estimated within each regime (P1 to ",
  format(as.Date(paste0(SUBPERIOD_BREAKS[1], "-01")), "%b %Y"), ", P2 to ",
  format(as.Date(paste0(SUBPERIOD_BREAKS[2], "-01")), "%b %Y"), ", P3 after). ",
  "\\textit{Months}: sample months in the regime; \\textit{State months}: months in which the ",
  "state dummy equals one (cont.: continuous index). n.e.: not estimable because the state ",
  "does not vary within the regime. $^{\\dagger}$: fewer than ", LOW_POWER_MONTHS,
  " state months (low power). ", HEAD_TXT, COMMON_H13)), "table_regime_separate.tex")

st_body <- c(paste0("Hyp. & State & & ", paste(sprintf("(%d) %s", 1:5, STEP_LBL), collapse = " & "), " \\\\"),
             "\\midrule")
for (sp in REGIME_SPECS) {
  x <- stepwise %>% filter(id == sp$id) %>% arrange(step)
  st_body <- c(st_body,
    paste0(sp$hyp, " & ", sp$lbl, " & Joint $\\chi^2$ ($p$) & ",
           paste(chi_cell(x$chi2, x$p), collapse = " & "), " \\\\"),
    paste0(" & & Headline ($z$) & ", paste(ifelse(is.na(x$head), "--",
           paste0(f4(x$head), " (", f2(x$head_z), ")")), collapse = " & "), " \\\\"),
    paste0(" & & $R^2$ & ", paste(formatC(x$r2, format = "f", digits = 3), collapse = " & "), " \\\\"),
    paste0(" & & $N$ & ", paste(nfmt(x$n), collapse = " & "), " \\\\",
           if (sp$id != tail(REGIME_SPECS, 1)[[1]]$id) " \\midrule" else ""))
}
writeLines(table_wrap("Stepwise Specifications of the Behavioral Flow Tests", "stepwise",
  "lllccccc", st_body, paste0(
  "Regressors added in a fixed order: (1) rank segments $R^{\\text{LOW}}$, $R^{\\text{MID}}$, ",
  "$R^{\\text{HIGH}}$; (2) state main effect (H3: also MAX12); (3) tested interactions; ",
  "(4) controls; (5) fund fixed effects (= primary specification). Same estimation sample in ",
  "all columns. $R^2$: within $R^2$ in column (5), $R^2$ otherwise. ", HEAD_TXT, COMMON_H13)),
  "table_stepwise.tex")

fl_body <- c(paste0("Hyp. & State & ", paste(sprintf("\\multicolumn{2}{c}{%s}",
             c("USD 5M (baseline)", "USD 1M", "No floor")), collapse = " & "), " \\\\"),
             "\\cmidrule(lr){3-4}\\cmidrule(lr){5-6}\\cmidrule(lr){7-8}",
             " & & Joint $\\chi^2$ ($p$) & $N$ & Joint $\\chi^2$ ($p$) & $N$ & Joint $\\chi^2$ ($p$) & $N$ \\\\",
             "\\midrule")
for (sp in SPECS) {
  x <- floor_res %>% filter(id == sp$id) %>% arrange(desc(floor))
  fl_body <- c(fl_body, paste0(sp$hyp, " & ", sp$lbl, " & ", paste(
    paste0(chi_cell(x$chi2, x$p), " & ", nfmt(x$n)), collapse = " & "), " \\\\"))
}
writeLines(table_wrap("Sensitivity of the Behavioral Flow Tests to the TNA Floor", "tna_floor",
  "llcccccc", fl_body, paste0(
  "Headline joint tests of the primary H1--H4 specifications when fund-months are kept if ",
  "lagged TNA is at least USD 5 million (baseline, as in the main tables), at least USD 1 ",
  "million, or positive. Tested terms: H1, H2 the three $R^q\\times S$ interactions ",
  "($\\chi^2_3$); H3 MAX12 and MAX12 $\\times D^{\\text{SENT}}$ ($\\chi^2_2$); H4 ",
  "ExpRatio $\\times S$ and the three $R^q\\times$ExpRatio$\\times S$ terms ($\\chi^2_4$). ",
  "Standardisation and winsorisation are redone within each sample. ", COMMON)),
  "table_tna_floor.tex")
cat("Written: panel_regime_results.xlsx and five table_*.tex files\n")
