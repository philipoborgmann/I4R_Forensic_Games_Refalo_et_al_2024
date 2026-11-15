## ============================================================================================
## CLAIMS SCRIPT — verify every claim in the AI report, one numbered block at a time.
## Claim numbers match the [claim N] tags in the report, the Appendix-A findings table, and the
## rows of the verdict survey.
##
## THE RULE: if you cannot verify a claim marked RETRACTION or CORRIGENDUM MATERIAL after
## honest study, RETURN THE PAPER and take another. Do not skip it. Do not let AI decide.
##
## NEVER contact the paper's authors or the journal. I4R handles all correspondence.
##
## 16 claims: 0 retraction material, 12 corrigendum material, 4 comment material. File language: .R (the deposit's own).
## ============================================================================================

## ---- standalone check harness: the verification notebook's setup chunk -------------------------
## How to run it: Rscript claims_script.R from any folder, with the deposit's folder in I4R_PKG_DIR (or a pkg/
## folder beside the working directory). Checks that read the article find it in paper/ beside pkg/ (or in
## I4R_AUDIT_DIR/paper/): the PDF as paper/paper.pdf and its text as paper/paper.txt, made with
##     pdftotext -layout paper/paper.pdf paper/paper.txt
## Run it in a UTF-8 locale. Every check passed, with these helpers, in the audit's verification notebook.
if (!isTRUE(l10n_info()[["UTF-8"]]))
  message("This R session is not in a UTF-8 locale; checks on text with non-ASCII characters may FAIL. ",
          "Run it with LANG=en_US.UTF-8 (or C.UTF-8).")

## The packages these checks use. A missing one is named here; the claims that need it record a FAIL.
.I4R_NEED <- c("readxl")
.I4R_MISSING <- .I4R_NEED[!vapply(.I4R_NEED, requireNamespace, logical(1), quietly = TRUE)]
if (length(.I4R_MISSING)) message(
  "These R packages are used below but not installed: ", paste(.I4R_MISSING, collapse = ", "),
  "\nInstall them, then run this file again: install.packages(", deparse(.I4R_MISSING), ")",
  "\n(a Bioconductor package installs with BiocManager::install()). Until then the claims that need them FAIL.")
suppressPackageStartupMessages(for (.p in c("knitr", "readxl")) if (requireNamespace(.p, quietly = TRUE)) library(.p, character.only = TRUE))

## Resolve the deposit WITHOUT hardcoding the auditor's machine: I4R_PKG_DIR, else a pkg/ folder beside the
## working directory (or the audit layout's), else stop.
PKG_DIR <- local({
  e <- Sys.getenv("I4R_PKG_DIR", "")
  if (nzchar(e) && dir.exists(e)) return(normalizePath(e))
  for (cand in c(file.path(getwd(), "pkg"), file.path(dirname(getwd()), "audit", "pkg"), "pkg"))
    if (dir.exists(cand)) return(normalizePath(cand))
  stop("cannot locate the deposit: set I4R_PKG_DIR to the extracted package directory (the pkg/ folder)")
})
AUDIT_DIR <- local({
  e <- Sys.getenv("I4R_AUDIT_DIR", "")
  if (nzchar(e) && dir.exists(e)) return(normalizePath(e))
  normalizePath(dirname(PKG_DIR))
})
SNIPS_DIR <- local({
  e <- Sys.getenv("I4R_SNIPS_DIR", "")
  if (nzchar(e) && dir.exists(e)) return(e)
  cand <- file.path(AUDIT_DIR, "verification_snippets")
  if (dir.exists(cand)) normalizePath(cand) else cand
})
## The notebook's chunks run from the audit folder; so do these checks.
setwd(AUDIT_DIR)
if (!file.exists(file.path(AUDIT_DIR, "paper", "paper.txt"))) message(
  "Some checks read the article at ", file.path(AUDIT_DIR, "paper", "paper.txt"), ": put the PDF at ",
  "paper/paper.pdf beside the deposit folder and run pdftotext -layout paper/paper.pdf paper/paper.txt ",
  "(or set I4R_AUDIT_DIR to the folder holding paper/). Until then those claims FAIL.")

## ---- read-only guard: pkg/ is the authors' deposit. Fingerprint on entry, re-check in the summary; scratch goes
## to tempdir(), never beside the deposit.
.pkg_manifest <- function() {
  fs <- list.files(PKG_DIR, recursive = TRUE, all.files = TRUE, no.. = TRUE, full.names = TRUE)
  data.frame(file = fs, size = file.size(fs), md5 = unname(tools::md5sum(fs)), stringsAsFactors = FALSE)
}
.PKG_ON_ENTRY <- .pkg_manifest()
SCRATCH <- tempfile("verif_"); dir.create(SCRATCH)

## ---- deposit loader: always R, whatever the package was written in.
read_deposit <- function(file, ...) {
  path <- if (file.exists(file.path(PKG_DIR, file))) file.path(PKG_DIR, file) else file
  if (!file.exists(path)) stop("not in the deposit: ", file)
  ext <- tolower(tools::file_ext(path))
  switch(ext,
    xlsx = , xls  = readxl::read_excel(path, ...),
    dta          = haven::read_dta(path, ...),
    sav = , por  = haven::read_sav(path, ...),
    sas7bdat     = haven::read_sas(path, ...),
    omv          = read_omv_deposit(path, ...),
    csv          = utils::read.csv(path, ...),
    tsv = , txt  = utils::read.delim(path, ...),
    rds          = readRDS(path),
    rdata = , rda = { e <- new.env(); load(path, envir = e); as.list(e) },
    parquet      = arrow::read_parquet(path, ...),
    stop("no loader for .", ext, " -- add one to read_deposit()")
  )
}
## A jamovi .omv is a zip: prefer the package; fall back to the zip so a missing jmvReadWrite does not block a check.
read_omv_deposit <- function(path, ...) {
  if (requireNamespace("jmvReadWrite", quietly = TRUE))
    return(jmvReadWrite::read_omv(path, ...))
  message("jmvReadWrite unavailable; reading the .omv as a zip archive")
  ex <- file.path(SCRATCH, paste0("omv_", basename(path)))
  dir.create(ex, showWarnings = FALSE); utils::unzip(path, exdir = ex)
  list(parts = list.files(ex, recursive = TRUE, full.names = TRUE))
}

## ---- expectation / check machinery: the notebook's verdict rules, one printed line per value.
.V <- new.env(parent = emptyenv())
.V$exp <- list(); .V$obs <- list(); .V$state <- list(); .V$order <- character(0); .V$current <- NULL
## A claim's block may record outcomes only under its own claim id.
.bind_claim <- function(id) {
  if (!is.null(.V$current) && !identical(id, .V$current))
    stop(sprintf("the %s block recorded an outcome under %s", .V$current, id))
}
.fmt <- function(x) {
  v <- if (is.numeric(x)) {
    ifelse(is.na(x), "NA",
      ifelse(x == round(x) & abs(x) < 1e15, format(x, big.mark = ",", scientific = FALSE),
             format(round(x, 6), nsmall = 0, scientific = FALSE)))
  } else as.character(x)
  paste(v, collapse = ", ")
}
.show <- function(x) if (is.null(x)) "NULL" else paste(format(x), collapse = ", ")
## expect() runs BEFORE anything is computed and prints the prediction.
expect <- function(id, ..., tol = 0, note = NULL) {
  .bind_claim(id)
  vals <- list(...); if (is.null(names(vals)) || any(names(vals) == "")) stop("expect(): every value needs a name")
  for (k in names(vals)) {
    key <- paste(id, k, sep = "|")
    .V$exp[[key]] <- list(id = id, key = k, expected = vals[[k]], tol = tol, note = note)
    if (!key %in% .V$order) .V$order <- c(.V$order, key)
    cat(sprintf("[%-6s %-34s] EXPECTED  %s\n", id, k, .show(vals[[k]])))
  }
  invisible(TRUE)
}
## check() prints the observation and the verdict, and never stops the run.
check <- function(id, ...) {
  .bind_claim(id)
  vals <- list(...); if (is.null(names(vals)) || any(names(vals) == "")) stop("check(): every value needs a name")
  for (k in names(vals)) {
    key <- paste(id, k, sep = "|"); e <- .V$exp[[key]]; ob <- vals[[k]]
    if (is.null(e)) {
      .V$obs[[key]] <- list(id = id, key = k, observed = vals[[k]], verdict = "FAIL", detail = "never declared with expect()")
      if (!key %in% .V$order) .V$order <- c(.V$order, key)
      cat(sprintf("[%-6s %-34s] FAIL  undeclared\n", id, k)); next
    }
    ex <- e$expected; tol <- e$tol
    if (is.numeric(ex) && is.numeric(ob) && length(ex) == length(ob)) {
      d <- max(abs(ex - ob)); ok <- isTRUE(all(!is.na(d)) && d <= tol + 1e-12)
    } else ok <- identical(as.character(ex), as.character(ob))
    .V$obs[[key]] <- list(id = id, key = k, observed = ob, verdict = if (ok) "PASS" else "FAIL")
    cat(sprintf("[%-6s %-34s] OBSERVED  %s  %s\n", id, k, .show(ob), if (ok) "PASS" else "FAIL"))
  }
  invisible(TRUE)
}
## Reach back into an earlier claim's observation.
observed_of <- function(claim, key) {
  o <- .V$obs[[paste(claim, key, sep = "|")]]
  if (is.null(o)) NA else o$observed
}
## The claim rests on an assessment, stated so a replicator can reject it on the merits.
judgement <- function(id, statement, basis = "") {
  .bind_claim(id)
  .V$state[[id]] <- list(id = id, verdict = "JUDGEMENT")
  cat(sprintf("[%-6s %-34s] JUDGEMENT  %s\n", id, "-", statement))
  if (nzchar(basis)) cat(sprintf("[%-6s %-34s] basis      %s\n", id, "-", basis))
  invisible(TRUE)
}
## The deposit cannot settle it; the reason is given.
unverifiable <- function(id, reason, observed = NULL) {
  .bind_claim(id)
  .V$state[[id]] <- list(id = id, verdict = "UNVERIFIABLE")
  cat(sprintf("[%-6s %-34s] UNVERIFIABLE  %s\n", id, "-", reason))
  invisible(TRUE)
}
## A runtime error inside a claim's block is that claim's FAIL, never the end of the run.
runtime_failure <- function(id, error) {
  .V$state[[id]] <- list(id = id, verdict = "FAIL")
  cat(sprintf("[%-6s %-34s] FAIL  runtime error: %s\n", id, "-", conditionMessage(error)))
  invisible(FALSE)
}
claims_summary <- function(claims = character(0)) {
  for (key in .V$order) if (is.null(.V$obs[[key]]))
    .V$obs[[key]] <- list(id = .V$exp[[key]]$id, key = .V$exp[[key]]$key, verdict = "FAIL")
  verdicts <- vapply(.V$obs, function(o) o$verdict, character(1))
  states <- vapply(.V$state, function(s) s$verdict, character(1))
  recorded <- unique(c(vapply(.V$obs, function(o) o$id, character(1)), names(.V$state)))
  silent <- setdiff(claims, recorded)
  foreign <- if (length(claims)) setdiff(recorded, claims) else character(0)
  after <- .pkg_manifest()
  sorted <- function(m) { m <- m[order(enc2utf8(m$file), method = "radix"), c("file", "size", "md5")]; rownames(m) <- NULL; m }
  same <- identical(sorted(.PKG_ON_ENTRY), sorted(after))
  prelude_ok <- isTRUE(get0(".I4R_PRELUDE_OK", ifnotfound = TRUE))
  n_pass <- sum(verdicts == "PASS")
  n_fail <- sum(verdicts != "PASS") + sum(states == "FAIL") + length(silent) + length(foreign) + (!same) + (!prelude_ok)
  cat(sprintf("\n%d checks, %d PASS, %d FAIL\n", n_pass + n_fail, n_pass, n_fail))
  cat(sprintf("claims marked JUDGEMENT: %d; UNVERIFIABLE: %d\n", sum(states == "JUDGEMENT"), sum(states == "UNVERIFIABLE")))
  if (!prelude_ok) cat("the shared helpers (_prelude.R) stopped with an error; claims that use them FAIL\n")
  if (any(states == "FAIL")) cat("claims that stopped with an error:", paste(names(states)[states == "FAIL"], collapse = ", "), "\n")
  if (length(silent)) cat("claims that recorded no outcome:", paste(silent, collapse = ", "), "\n")
  if (length(foreign)) cat("outcomes recorded under ids that are not claims here:", paste(foreign, collapse = ", "), "\n")
  cat(sprintf("deposit unmodified: %s\n", if (same) "yes" else "NO -- INVESTIGATE"))
  invisible(n_fail)
}
## ---- audit-specific helpers (verification_snippets/_prelude.R) ----
.I4R_PRELUDE_OK <- tryCatch({
## Audit-specific helpers: Refalo et al. (2024), seven Excel workbooks and five Quarto R scripts.
## Every object below is read from the deposit; nothing is written beside it.
suppressMessages(library(readxl))
rd <- function(f, sheet = 1) as.data.frame(read_deposit(f, sheet = sheet))
LW <- "Data/Volume/modified.longitudinal.models.xlsx"
FINAL_RL  <- rd(LW, "Final % Rep Loss")
PRE_RL    <- rd(LW, "% Rep Loss (Pre Data Audit)")
LOADCH_RL <- rd(LW, "% Rep Loss (With Load Changes)")
VOL       <- rd(LW, "Final Volume Load + Repetitions")
RFVL      <- rd("Data/Muscle Thickness/rf_vl_master.xlsx", 1)
MT        <- rd("Data/Muscle Thickness/mt_master_avg.xlsx", 1)
MT_MEANS  <- rd("Data/Muscle Thickness/mt_master_avg.xlsx", 2)
MTDIFF    <- rd("Data/Muscle Thickness/mt_diff_analysis_avg.xlsx", 1)
VLOSS     <- rd("Data/Velocity Loss/vl_master.xlsx", 1)
KEYS <- c("id", "week", "session", "exercise", "protocol")
QMD_DIR <- file.path(PKG_DIR, "R Code (QMD scripts)")
qmd <- function(rel) readLines(file.path(QMD_DIR, rel), warn = FALSE, encoding = "UTF-8")
QMD_FILES <- list.files(QMD_DIR, pattern = "\\.qmd$", recursive = TRUE)
## The article text (two-column extraction); minus signs normalised to ASCII.
PAPER <- local({
  f <- file.path(AUDIT_DIR, "paper", "paper.txt")
  if (file.exists(f)) gsub("−", "-", readLines(f, warn = FALSE, encoding = "UTF-8")) else character(0)
})
paper_has <- function(pattern, fixed = TRUE) any(grepl(pattern, PAPER, fixed = fixed))
## pooled mean of a repetition-loss column, in percent, by exercise and protocol
pooled_rl <- function(d) {
  a <- aggregate(rep.loss ~ exercise + protocol, data = d[!is.na(d$rep.loss), ], FUN = mean)
  setNames(100 * a$rep.loss, paste(a$exercise, a$protocol))
}
TRUE }, error = function(e) { cat(sprintf("[%-6s %-34s] FAIL  runtime error: %s\n", "prelude", "-", conditionMessage(e))); FALSE })
## ============================================================================================
## ============================================================================================
## CLAIM 1 [CORRIGENDUM MATERIAL]  (finding F1, Section 3)
##   The deposited volume.load variable, which feeds Table 4, Figure 2a, the Results 3.7
##   estimates and the Table 5 volume-load slopes, is not sets x repetitions x load as Methods
##   2.4.2 defines it: its ratio to repetitions x starting load has median 0.142 and is below 1
##   in 1,093 of 1,093 rows, it barely changes with repetitions at fixed load, and Table 4's own
##   printed volume loads are 0.11 to 0.14 of load lifted x total repetitions.
## Check written by the audit (no author code covers this claim):

.V$current <- "C-1"
tryCatch({
## F1: the deposited volume.load column is not sets x repetitions x load.
expect("C-1", n_rows = 1093, rows_ratio_below_1 = 1093, series_with_constant_load = 72, tol = 0,
       note = "exact counts in the Final Volume Load + Repetitions sheet (rows with volume.load, repetitions, sets and starting.load)")
expect("C-1", median_ratio = 0.142, tol = 0.0005,
       note = "median of volume.load / (repetitions x starting.load), quoted to three decimals")
expect("C-1", table4_wk1_lp_fail_ratio = 0.118, tol = 0.0005,
       note = "Table 4, week 1, leg press FAIL: printed volume load 546 / (load lifted 101 x total reps 46); inside the quoted 0.11 to 0.14")
d <- VOL[complete.cases(VOL[, c("volume.load", "repetitions", "sets", "starting.load")]), ]
r <- d$volume.load / (d$repetitions * d$starting.load)
k <- interaction(VOL$id, VOL$exercise, VOL$protocol, drop = TRUE)
n_load_values <- tapply(VOL$starting.load, k, function(x) length(unique(x[!is.na(x)])))
check("C-1", n_rows = nrow(d), rows_ratio_below_1 = sum(r < 1),
      series_with_constant_load = sum(n_load_values == 1),
      median_ratio = round(median(r), 3),
      table4_wk1_lp_fail_ratio = round(546 / (101 * 46), 3))
}, error = function(e) runtime_failure("C-1", e))
.V$current <- NULL

## ============================================================================================
## CLAIM 2 [CORRIGENDUM MATERIAL]  (finding F2, Section 4)
##   The primary muscle-thickness model has no participant-by-muscle term although a
##   participant's change in a muscle correlates strongly across the two limbs (r = 0.92 for RF
##   and 0.83 for VL) and only weakly between muscles (r = 0.24 to 0.27). With such a term added
##   the FAIL-minus-RIR contrasts keep their point estimates, the quadriceps HDI narrows from
##   -0.063 to 0.058 cm to -0.033 to 0.029 cm, and the probabilities that RF favours RIR and VL
##   favours FAIL rise from 81% and 79% to 96% and 93%.
## Check written by the audit (no author code covers this claim):

.V$current <- "C-2"
tryCatch({
## F2: model-free substrate of the participant-by-muscle dependence (brms refits are in report 04).
expect("C-2", r_rf_fail_vs_rir_limb = 0.92, r_vl_fail_vs_rir_limb = 0.83, tol = 0.005,
       note = "Pearson r of per-participant change between the FAIL and RIR limbs, two decimals")
expect("C-2", r_rf_vs_vl_min = 0.24, r_rf_vs_vl_max = 0.27, tol = 0.005,
       note = "r between RF and VL change within each protocol, two decimals")
expect("C-2", paired_se_quad = 0.0125, paired_se_rf = 0.0193, paired_se_vl = 0.0191, tol = 0.00005,
       note = "model-free paired standard error of the FAIL-minus-RIR change (n = 18), four decimals")
x <- RFVL; x$chg <- x$Post - x$Pre
w <- reshape(x[, c("id", "muscle", "protocol", "chg")], idvar = c("id", "muscle"), timevar = "protocol", direction = "wide")
w$diff <- w$chg.FAIL - w$chg.RIR
rf <- w[w$muscle == "Rectus Femoris", ]; vl <- w[w$muscle == "Vastus Lateralis", ]
rf <- rf[order(rf$id), ]; vl <- vl[order(vl$id), ]
se <- function(v) sd(v) / sqrt(length(v))
quad <- (rf$diff + vl$diff) / 2
rr <- c(cor(rf$chg.FAIL, vl$chg.FAIL), cor(rf$chg.RIR, vl$chg.RIR))
check("C-2", r_rf_fail_vs_rir_limb = round(cor(rf$chg.FAIL, rf$chg.RIR), 2),
      r_vl_fail_vs_rir_limb = round(cor(vl$chg.FAIL, vl$chg.RIR), 2),
      r_rf_vs_vl_min = round(min(rr), 2), r_rf_vs_vl_max = round(max(rr), 2),
      paired_se_quad = round(se(quad), 4), paired_se_rf = round(se(rf$diff), 4), paired_se_vl = round(se(vl$diff), 4))
}, error = function(e) runtime_failure("C-2", e))
.V$current <- NULL

## ============================================================================================
## CLAIM 3 [CORRIGENDUM MATERIAL]  (finding F3, Section 5)
##   Between the deposited Pre Data Audit and Final repetition-loss sheets 24 repetition-loss
##   values were blanked, all in rows where repetitions rose from the first to the final set, 22
##   of them on the FAIL limb and 23 on the leg press, by a rule the article and deposit do not
##   state. The leg-press difference in pooled mean repetition loss between FAIL and RIR is 3.6
##   percentage points in the Pre Data Audit sheet and 10.0 in the Final sheet.
## Check written by the audit (no author code covers this claim):

.V$current <- "C-3"
tryCatch({
## F3: 24 repetition-loss values present in the Pre Data Audit sheet are blank in the Final sheet.
expect("C-3", n_blanked = 24, blanked_with_gain = 24, blanked_fail = 22, blanked_leg_press = 23, tol = 0,
       note = "exact counts, rows matched on participant, week, session, exercise and protocol")
expect("C-3", lp_gap_pre_audit_pp = 3.6, lp_gap_final_pp = 10.0, tol = 0.05,
       note = "leg-press RIR minus FAIL pooled mean repetition loss, percentage points, one decimal")
m <- merge(PRE_RL[, c(KEYS, "rep.change", "rep.loss")], FINAL_RL[, c(KEYS, "rep.loss")],
           by = KEYS, suffixes = c(".pre", ".final"))
b <- m[!is.na(m$rep.loss.pre) & is.na(m$rep.loss.final), ]
gap <- function(d) { p <- pooled_rl(d); unname(p["Leg Press RIR"] - p["Leg Press FAIL"]) }
check("C-3", n_blanked = nrow(b), blanked_with_gain = sum(b$rep.change > 0),
      blanked_fail = sum(b$protocol == "FAIL"), blanked_leg_press = sum(b$exercise == "Leg Press"),
      lp_gap_pre_audit_pp = round(gap(PRE_RL), 1), lp_gap_final_pp = round(gap(FINAL_RL), 1))
}, error = function(e) runtime_failure("C-3", e))
.V$current <- NULL

## ============================================================================================
## CLAIM 4 [CORRIGENDUM MATERIAL]  (finding F4, Section 6)
##   Two printed 95% HDIs are incompatible with the pd values computed for the same estimand:
##   the week-4 FAIL-minus-RIR velocity contrast (-6.8%, HDI -12.4% to 1%, pd 99%) and the RIR
##   leg-extension repetition-loss slope (0.3%, HDI 0.1% to 0.7%, pd 94%); current-platform
##   refits of the authors' models give -13.0% to -1.2% and -0.08% to 0.61%. A third, the RIR
##   week-8 velocity change (-6.4%, HDI -12% to 0.6%, pd 99%), pairs an equal-weight interval
##   with a proportional-weight pd, and its equal-weight refit interval is -12.3% to -0.8%.
## Check written by the audit (no author code covers this claim):

.V$current <- "C-4"
tryCatch({
## F4: the three printed intervals, as the article prints them beside their pd values. A 95% HDI whose
## upper bound is positive cannot sit beside pd = 99% for a near-symmetric posterior, nor one whose lower
## bound is positive beside pd = 94%. The refit intervals are report 01's brms refits (not repeated here).
expect("C-4", wk4_contrast_hdi_crosses_zero_pd99 = "TRUE", rir_wk8_hdi_crosses_zero_pd99 = "TRUE",
       rir_le_slope_hdi_excludes_zero_pd94 = "TRUE",
       note = "each printed interval string occurs in the article text with the pd quoted in the claim")
wk4 <- grep("^\\s*Week 4\\s", PAPER, value = TRUE)
check("C-4",
      wk4_contrast_hdi_crosses_zero_pd99 = as.character(any(grepl("-6.8%\\s+-12.4% to 1%\\s+99%", wk4))),
      rir_wk8_hdi_crosses_zero_pd99 = as.character(paper_has("RIR [-6.4% (HDI: -12% to 0.6%); pd = 99%]")),
      rir_le_slope_hdi_excludes_zero_pd94 = as.character(paper_has("Leg Extension = 0.3% (HDI: 0.1%") &&
                                                         paper_has("to 0.7%); pd = 94%]")))
}, error = function(e) runtime_failure("C-4", e))
.V$current <- NULL

## ============================================================================================
## CLAIM 5 [CORRIGENDUM MATERIAL]  (finding F5, Section 7)
##   The lifting-velocity estimates and HDIs are emmeans averages that weight the two sexes
##   equally, while the pd values use sample-proportion weights (12 males to 6 females), so one
##   cell can print an HDI that includes zero beside a pd of 98%, as for RIR in week 1 (-4.4%,
##   HDI -9.1% to 0.7%). With proportional weights that cell is -5.16% (HDI -10.28% to -0.59%).
## Check written by the audit (no author code covers this claim):

.V$current <- "C-5"
tryCatch({
## F5: the analysis script takes velocity estimates from emmeans with its default (equal) weights and
## the pd values from emmeans with weights = "prop". The refit values are report 04's.
expect("C-5", lv_estimates_equal_weights = "TRUE", lv_pd_prop_weights = "TRUE",
       note = "code inspection of the deposited Data Analysis script")
expect("C-5", males = 12, females = 6, tol = 0, note = "sex mix in the velocity sheet, one row per participant")
a <- qmd("Initial Analysis/Refalo et al Data Analysis.qmd")
est <- grep("^LV_effects <- emmeans\\(LV_model", a, value = TRUE)
pdl <- grep("emmeans\\(LV_model, ~ ?protocol\\*time, weights = \"prop\"\\)", a, value = TRUE)
s <- unique(VLOSS[, c("id", "sex")])
check("C-5", lv_estimates_equal_weights = as.character(length(est) == 1 && !grepl("weights", est)),
      lv_pd_prop_weights = as.character(length(pdl) >= 1),
      males = sum(s$sex == "male"), females = sum(s$sex == "female"))
}, error = function(e) runtime_failure("C-5", e))
.V$current <- NULL

## ============================================================================================
## CLAIM 6 [CORRIGENDUM MATERIAL]  (finding F6, Section 8)
##   Table 4's Total Reps and Volume Load rows are two-session weekly sums, not the average of
##   both sessions that its caption states: per-session averages reproduce 0 of 32 cells in each
##   row, weekly sums 18 and 25 of 32 within half a printed unit. Its Load Lifted row cannot be
##   derived from the deposit, and Figure 2 shows the same weekly totals while Results 3.7
##   reports per-session model estimates without saying which unit applies.
## Check written by the audit (no author code covers this claim):

.V$current <- "C-6"
tryCatch({
## F6: Table 4's Total Reps and Volume Load are two-session weekly sums, not per-session averages.
## Week 1 has no missing session, so its cells test the aggregation rule directly.
expect("C-6", wk1_lp_fail_reps_weekly_sum = 46, wk1_lp_fail_vl_weekly_sum = 546, tol = 0.5,
       note = "Table 4 week 1 leg press FAIL prints Total Reps 46 and Volume Load 546; half a printed unit")
expect("C-6", wk1_lp_fail_reps_session_mean = 23, wk1_lp_fail_vl_session_mean = 273, tol = 0.5,
       note = "the per-session average is half the printed value, so the caption's average of both sessions does not describe the printed 46 and 546")
expect("C-6", series_with_constant_load = 72, tol = 0,
       note = "the deposit's only load column is constant within every participant-exercise-protocol series, so Load Lifted (101 to 121 kg on the leg press) cannot be derived")
d <- VOL[VOL$week == 1 & VOL$exercise == "Leg Press" & VOL$protocol == "FAIL", ]
ws <- aggregate(cbind(repetitions, volume.load) ~ id, data = d, FUN = sum)
k <- interaction(VOL$id, VOL$exercise, VOL$protocol, drop = TRUE)
nl <- tapply(VOL$starting.load, k, function(x) length(unique(x[!is.na(x)])))
check("C-6", wk1_lp_fail_reps_weekly_sum = round(mean(ws$repetitions), 1),
      wk1_lp_fail_vl_weekly_sum = round(mean(ws$volume.load), 1),
      wk1_lp_fail_reps_session_mean = round(mean(d$repetitions, na.rm = TRUE), 1),
      wk1_lp_fail_vl_session_mean = round(mean(d$volume.load, na.rm = TRUE), 1),
      series_with_constant_load = sum(nl == 1))
}, error = function(e) runtime_failure("C-6", e))
.V$current <- NULL

## ============================================================================================
## CLAIM 7 [CORRIGENDUM MATERIAL]  (finding F7, Section 9)
##   Table 5 prints pd values of 48% (quadriceps contrast) and 49% (leg-press repetition-volume
##   slope) because the code computes the share of draws in a direction fixed per row, and the
##   Discussion reads the 48% as the probability that a difference exists. It is the probability
##   that FAIL exceeds RIR; the probability of direction for that contrast is about 51 to 52%.
## Check written by the audit (no author code covers this claim):

.V$current <- "C-7"
tryCatch({
## F7: Table 5 prints pd = 48% and 49%, and the analysis code computes a direction fixed per row
## (> 0 for the quadriceps and VL contrasts, < 0 for RF). The refit shares are reports 01, 03 and 04.
expect("C-7", table5_quad_pd_48 = "TRUE", table5_lp_rv_slope_pd_49 = "TRUE", discussion_48_reading = "TRUE",
       code_quad_contrast_share_above_zero = "TRUE", code_rf_contrast_share_below_zero = "TRUE",
       note = "article text and deposited Data Analysis script")
a <- qmd("Initial Analysis/Refalo et al Data Analysis.qmd")
i_quad <- grep("^pairs\\(emmeans\\(MT_model, ~protocol, weights = \"prop\"\\)\\)", a)
i_rf <- grep("# Contrast for protocol by RF \\(<0\\)", a)
check("C-7",
      table5_quad_pd_48 = as.character(any(grepl("Quadriceps Thickness\\s+-0.001 cm\\s+-0.063 to 0.058\\s+48%", PAPER))),
      table5_lp_rv_slope_pd_49 = as.character(any(grepl("Leg Press\\s+0 repetitions\\s+-0.17 to 0.19\\s+49%", PAPER))),
      discussion_48_reading = as.character(paper_has("with a 48% probability (pd > TE = 3%) that any poten")),
      code_quad_contrast_share_above_zero = as.character(length(i_quad) >= 1 && any(grepl("mean\\(.value>0", a[i_quad[1] + 0:3]))),
      code_rf_contrast_share_below_zero = as.character(length(i_rf) == 1 && any(grepl("mean\\(.value<0", a[i_rf + 0:3]))))
}, error = function(e) runtime_failure("C-7", e))
.V$current <- NULL

## ============================================================================================
## CLAIM 8 [COMMENT MATERIAL]  (finding F8, Section 10)
##   In the copy audited the deposit does not support its statement that all data and code are
##   openly available: 17 of 27 data-read calls fail as written, 8 named input files are absent
##   (including the n = 19 sensitivity data and a second muscle-thickness file), and 2 calls
##   request sheets that do not exist. Sex, cohort counts and set volumes are deposited, but the
##   age, body-mass and training-history inputs of Table 1, the nutrition data of Table 2, the
##   RIR-accuracy data of Table 3 and the ultrasound reliability data behind the typical error
##   are not, the supplementary tables are unavailable, and the deposit has no README, licence,
##   codebook or record of package versions.
## Check written by the audit (no author code covers this claim):

.V$current <- "C-8"
tryCatch({
## F8: data-read calls in the five scripts, resolved against the deposit's Data folder.
expect("C-8", read_calls = 27, resolve_as_written = 10, resolve_after_folder_rename = 19,
       absent_files = 8, calls_to_missing_sheets = 2, tol = 0,
       note = "exact counts; paths resolved relative to the deposit's Data folder")
expect("C-8", readme_or_licence_files = 0, tol = 0, note = "no README, LICENSE, codebook or session-information file")
calls <- unlist(lapply(QMD_FILES, function(f) {
  L <- qmd(f)
  m <- regmatches(L, gregexpr("read_(excel|xlsx)\\(\"[^\"]+\"(,[0-9]+)?", L))
  unlist(m)
}))
path_of <- sub("^read_(excel|xlsx)\\(\"([^\"]+)\".*$", "\\2", calls)
sheet_of <- suppressWarnings(as.integer(sub("^.*\",([0-9]+)$", "\\1", ifelse(grepl("\",[0-9]+$", calls), calls, "x\",1"))))
data_dir <- file.path(PKG_DIR, "Data")
as_written <- file.exists(file.path(data_dir, path_of))
renamed <- ifelse(grepl("^(Muscle|Velocity)_", path_of), sub("^([A-Za-z]+)_([A-Za-z]+)/", "\\1 \\2/", path_of), path_of)
after <- file.exists(file.path(data_dir, renamed))
n_sheets <- vapply(seq_along(renamed), function(i) if (after[i]) length(readxl::excel_sheets(file.path(data_dir, renamed[i]))) else NA_integer_, 1L)
docs <- list.files(PKG_DIR, recursive = TRUE, ignore.case = TRUE, pattern = "readme|licen[cs]e|codebook|dictionary|sessioninfo|renv\\.lock")
check("C-8", read_calls = length(calls), resolve_as_written = sum(as_written), resolve_after_folder_rename = sum(after),
      absent_files = length(unique(basename(path_of[!after]))),
      calls_to_missing_sheets = sum(after & !is.na(n_sheets) & sheet_of > n_sheets),
      readme_or_licence_files = length(docs))
}, error = function(e) runtime_failure("C-8", e))
.V$current <- NULL

## ============================================================================================
## CLAIM 9 [CORRIGENDUM MATERIAL]  (finding F9, Section 11)
##   The Discussion's statement that the models included sex as a population-level effect
##   follows a passage on fatigue and volume and holds for those four models, but the primary
##   muscle-thickness model has no sex term (adding one moves the contrast from -0.001 to -0.002
##   cm) and its baseline covariate is not described in the Methods. Four of the five
##   16,000-iteration re-runs in the Diagnostics script fit random-effects structures that
##   differ from the published models, so the convergence evidence they produce does not
##   describe those models.
## Check written by the audit (no author code covers this claim):

.V$current <- "C-9"
tryCatch({
## F9: the primary muscle-thickness formula has no sex term, and four of the five 16,000-iteration
## re-runs in the Diagnostics script use formulas that differ from the published ones.
expect("C-9", mt_formula_has_sex = "FALSE", discussion_says_sex_included = "TRUE",
       note = "deposited Data Analysis script and article text")
expect("C-9", diag_reruns_16000 = 5, diag_reruns_differing = 4, tol = 0,
       note = "brm calls with iter = 16000 in the Diagnostics script, compared by formula with the Data Analysis script")
a <- qmd("Initial Analysis/Refalo et al Data Analysis.qmd")
g <- qmd("Initial Analysis/Refalo et al Diagnostics.qmd")
formula_of <- function(line) gsub("\\s+", "", sub("^.*brm\\(([^,]*),.*$", "\\1", line))
fa <- grep("<- brm\\(", a, value = TRUE)
ig <- grep("<- brm\\(", g)
is16 <- vapply(ig, function(i) any(grepl("iter = 16000", g[i:min(length(g), i + 6)])), TRUE)
lhs <- function(x) sub("~.*$", "", formula_of(x))
pub <- setNames(vapply(fa, formula_of, ""), vapply(fa, lhs, ""))
reruns <- g[ig[is16]]
same <- vapply(reruns, function(x) identical(unname(pub[lhs(x)]), formula_of(x)), TRUE)
mt <- grep("^MT_model <- brm\\(", a, value = TRUE)
check("C-9", mt_formula_has_sex = as.character(grepl("sex", formula_of(mt))),
      discussion_says_sex_included = as.character(paper_has("as a population-level effect; however, we didn")),
      diag_reruns_16000 = length(reruns), diag_reruns_differing = sum(!same))
}, error = function(e) runtime_failure("C-9", e))
.V$current <- NULL

## ============================================================================================
## CLAIM 10 [CORRIGENDUM MATERIAL]  (finding F10, Section 11)
##   The package holds a sex-differences analysis of the same cohort (a preparation script and
##   six brms models of sex by time by protocol) although the article says sex differences were
##   not analysed. The scripts cannot run on the deposited data, because they code sex as Male
##   and Female where the workbooks use lowercase labels, read an absent workbook and request
##   missing sheets, and 4 of 899 rows of their response variable rep.change disagree with final
##   minus first repetitions.
## Check written by the audit (no author code covers this claim):

.V$current <- "C-10"
tryCatch({
## F10: the sex-differences scripts: six brms models, capitalised sex levels in the code against
## lowercase labels in the workbooks, and four rep.change rows that disagree with final minus first.
expect("C-10", sexdiff_brm_calls = 6, workbook_sex_labels_lowercase = "TRUE", code_uses_capitalised_levels = "TRUE",
       rep_change_rows = 899, rep_change_mismatches = 4, tol = 0,
       note = "deposited Sex Differences scripts and the Final % Rep Loss sheet")
s <- qmd("Sex Differences Analysis/Sex Diff_Data Analysis_Final.qmd")
n_brm <- length(grep("<- brm\\(", s))
labs <- unique(c(FINAL_RL$sex, VOL$sex, VLOSS$sex, RFVL$sex, MT$sex))
d <- FINAL_RL[!is.na(FINAL_RL$reps.first) & !is.na(FINAL_RL$reps.final) & !is.na(FINAL_RL$rep.change), ]
check("C-10", sexdiff_brm_calls = n_brm,
      workbook_sex_labels_lowercase = as.character(setequal(labs, c("male", "female"))),
      code_uses_capitalised_levels = as.character(any(grepl("sexMale|\"Female\"|\"Male\"", s))),
      rep_change_rows = nrow(d), rep_change_mismatches = sum(abs(d$rep.change - (d$reps.final - d$reps.first)) > 1e-9))
}, error = function(e) runtime_failure("C-10", e))
.V$current <- NULL

## ============================================================================================
## CLAIM 11 [CORRIGENDUM MATERIAL]  (finding F11, Section 11)
##   In mt_master_avg.xlsx the post-intervention SD for RIR is computed from the change column
##   (cached 0.125 cm against 0.452 cm for the Post column). In mt_diff_analysis_avg.xlsx one
##   participant is labelled female who is male in the other six workbooks (11 and 7 against 12
##   and 6), and neither cell enters a published model.
## Check written by the audit (no author code covers this claim):

.V$current <- "C-11"
tryCatch({
## F11: the RIR Post SD cell in mt_master_avg.xlsx and the sex labels in mt_diff_analysis_avg.xlsx.
expect("C-11", cached_rir_post_sd = 0.125, sd_of_rir_post_column = 0.452, tol = 0.0005,
       note = "cached value in the MT Means sheet against the SD of the RIR Post values, three decimals")
expect("C-11", diff_file_males = 11, diff_file_females = 7, master_males = 12, master_females = 6,
       participants_relabelled = 1, tol = 0, note = "one row per participant")
cached <- MT_MEANS$sd[MT_MEANS$protocol == "RIR" & MT_MEANS$time == "Post"]
sm <- unique(MT[, c("id", "sex")]); sd2 <- unique(MTDIFF[, c("id", "sex")])
j <- merge(sm, sd2, by = "id", suffixes = c(".master", ".diff"))
check("C-11", cached_rir_post_sd = round(cached, 3), sd_of_rir_post_column = round(sd(MT$Post[MT$protocol == "RIR"]), 3),
      diff_file_males = sum(sd2$sex == "male"), diff_file_females = sum(sd2$sex == "female"),
      master_males = sum(sm$sex == "male"), master_females = sum(sm$sex == "female"),
      participants_relabelled = sum(j$sex.master != j$sex.diff))
}, error = function(e) runtime_failure("C-11", e))
.V$current <- NULL

## ============================================================================================
## CLAIM 12 [CORRIGENDUM MATERIAL]  (finding F12, Section 11)
##   The article reports no per-model sample size or missing-data handling: the velocity model
##   uses 102 of 108 rows, the repetition-loss model 874 of 1,152 and the volume models 1,093 of
##   1,152. Leg-press records are missing in 13 attended sessions of three participants (29
##   protocol rows across four participants) and the deposit does not record why; the article
##   describes two participants unable to perform the leg press because of discomfort. The
##   inclusion rule is stated as 90% of sessions (14 of 16), which is 87.5%.
## Check written by the audit (no author code covers this claim):

.V$current <- "C-12"
tryCatch({
## F12: rows the published models can use (brms drops incomplete rows), leg-press gaps inside attended
## sessions in the Final Volume Load + Repetitions sheet, and the inclusion arithmetic.
expect("C-12", velocity_rows_used = 102, velocity_rows = 108, rep_loss_rows_used = 874, volume_rows_used = 1093,
       long_rows = 1152, tol = 0, note = "complete rows for each model's response and covariates")
expect("C-12", lp_rows_missing_in_attended = 29, lp_rows_missing_participants = 4,
       lp_sessions_both_missing = 13, lp_sessions_both_missing_participants = 3, tol = 0,
       note = "an attended session has a set count on at least one of its four limb-exercise rows")
expect("C-12", fourteen_of_sixteen_pct = 87.5, tol = 0, note = "14 / 16 x 100")
v_ok <- complete.cases(VLOSS[, c("percent_diff", "first_set", "sets")])
rl_ok <- complete.cases(FINAL_RL[, c("rep.loss", "reps.first", "sets")])
vol_ok <- complete.cases(VOL[, c("volume.load", "repetitions", "sets")])
V <- VOL; V$has <- !is.na(V$sets)
att <- aggregate(has ~ id + week + session, data = V, FUN = any); names(att)[4] <- "attended"
lp <- merge(V[V$exercise == "Leg Press", ], att, by = c("id", "week", "session"))
lp <- lp[lp$attended, ]
miss <- lp[!lp$has, ]
both <- aggregate(has ~ id + week + session, data = lp, FUN = function(x) sum(!x))
check("C-12", velocity_rows_used = sum(v_ok), velocity_rows = nrow(VLOSS), rep_loss_rows_used = sum(rl_ok),
      volume_rows_used = sum(vol_ok), long_rows = nrow(FINAL_RL),
      lp_rows_missing_in_attended = nrow(miss), lp_rows_missing_participants = length(unique(miss$id)),
      lp_sessions_both_missing = sum(both$has == 2),
      lp_sessions_both_missing_participants = length(unique(both$id[both$has == 2])),
      fourteen_of_sixteen_pct = 100 * 14 / 16)
}, error = function(e) runtime_failure("C-12", e))
.V$current <- NULL

## ============================================================================================
## CLAIM 13 [COMMENT MATERIAL]  (finding F13, Section 11)
##   In the four primary analysis sheets with a limb column, in three workbooks, limb 1 is
##   always the FAIL limb and limb 2 always the RIR limb, so the deposit holds no anatomical
##   side or dominance variable. The stated randomisation of protocols to limbs and balance of
##   dominant limbs therefore cannot be checked, and the limb-level random effect in the models
##   duplicates the protocol term.
## Check written by the audit (no author code covers this claim):

.V$current <- "C-13"
tryCatch({
## F13: limb code against protocol in the four primary analysis sheets that carry a limb column
## (rf_vl_master, vl_master and the two Final sheets of the longitudinal workbook: three workbooks).
expect("C-13", primary_sheets_with_limb = 4, workbooks = 3, rows_limb1_not_fail = 0, rows_limb2_not_rir = 0, tol = 0,
       note = "exact counts over all rows of the four sheets")
L <- list(RFVL = RFVL, VLOSS = VLOSS, FINAL_RL = FINAL_RL, VOL = VOL)
bad1 <- sum(vapply(L, function(d) sum(d$limb == 1 & d$protocol != "FAIL"), 0))
bad2 <- sum(vapply(L, function(d) sum(d$limb == 2 & d$protocol != "RIR"), 0))
check("C-13", primary_sheets_with_limb = sum(vapply(L, function(d) "limb" %in% names(d), TRUE)),
      workbooks = length(unique(c("Data/Muscle Thickness/rf_vl_master.xlsx", "Data/Velocity Loss/vl_master.xlsx", LW, LW))),
      rows_limb1_not_fail = bad1, rows_limb2_not_rir = bad2)
}, error = function(e) runtime_failure("C-13", e))
.V$current <- NULL

## ============================================================================================
## CLAIM 14 [CORRIGENDUM MATERIAL]  (finding F14, Section 11)
##   Several printed details do not match the deposit or their own arithmetic: the printed
##   percentage changes are changes in group means while the deposited code computes means of
##   individual percentages (7.01 and 7.03 against 6.96 and 6.98), and the Figure 5 caption
##   formula has the opposite sign to the data. The HDI level is not stated, the sample-size and
##   dropout arithmetic does not give 20, and d = 0.59 matches a paired t-test at alpha 0.05 and
##   80% power for n = 19 one-sided (0.593), but the article states neither the test, its
##   sidedness, its power nor the n used.
## Check written by the audit (no author code covers this claim):

.V$current <- "C-14"
tryCatch({
## F14: printed percentage changes are changes in group means; the deposited code's mean of individual
## percentage changes gives 7.01 and 7.03.
expect("C-14", group_mean_pct_fail = 6.96, group_mean_pct_rir = 6.98, mean_individual_pct_fail = 7.01,
       mean_individual_pct_rir = 7.03, tol = 0.005, note = "quadriceps, mt_master_avg.xlsx, two decimals")
expect("C-14", d_reproduces_n19_one_sided = 0.593, tol = 0.0005,
       note = "paired-design critical effect size, n = 19, alpha 0.05 one-sided, power 0.80 (reports 03, 04)")
pc <- function(p) { d <- MT[MT$protocol == p, ]; c(g = 100 * (mean(d$Post) / mean(d$Pre) - 1), i = 100 * mean(d$Post / d$Pre - 1)) }
f <- pc("FAIL"); r <- pc("RIR")
dcrit <- function(n) uniroot(function(d) power.t.test(n = n, delta = d, sd = 1, type = "one.sample",
                                                      alternative = "one.sided")$power - 0.8, c(0.05, 3))$root
check("C-14", group_mean_pct_fail = round(f[["g"]], 2), group_mean_pct_rir = round(r[["g"]], 2),
      mean_individual_pct_fail = round(f[["i"]], 2), mean_individual_pct_rir = round(r[["i"]], 2),
      d_reproduces_n19_one_sided = round(dcrit(19), 3))
}, error = function(e) runtime_failure("C-14", e))
.V$current <- NULL

## ============================================================================================
## CLAIM 15 [COMMENT MATERIAL]  (finding F15, Section 11)
##   The deposit records no achieved repetitions-in-reserve during training, and Table 3's
##   accuracy test covered 1 and 3 RIR targets while the leg press was trained to 2 RIR. On
##   first sets the FAIL limb completed 0.89 more leg-press repetitions (paired p = 0.001) and
##   0.22 more leg-extension repetitions (p = 0.27) than the RIR limb, against nominal gaps of
##   about 2 and 1.
## Check written by the audit (no author code covers this claim):

.V$current <- "C-15"
tryCatch({
## F15: first-set repetitions in the Final % Rep Loss sheet, FAIL limb minus RIR limb, per participant (mean over sessions).
expect("C-15", lp_first_set_gap = 0.89, le_first_set_gap = 0.22, tol = 0.005,
       note = "paired mean difference in first-set repetitions, two decimals (report 04)")
expect("C-15", lp_p_below_0_01 = "TRUE", le_p = 0.27, tol = 0.005, note = "paired t test")
g <- function(ex) {
  d <- FINAL_RL[FINAL_RL$exercise == ex & !is.na(FINAL_RL$reps.first), ]
  m <- aggregate(reps.first ~ id + protocol, data = d, FUN = mean)
  w <- reshape(m, idvar = "id", timevar = "protocol", direction = "wide")
  t.test(w$reps.first.FAIL, w$reps.first.RIR, paired = TRUE)
}
lp <- g("Leg Press"); le <- g("Leg Extension")
check("C-15", lp_first_set_gap = round(unname(lp$estimate), 2), le_first_set_gap = round(unname(le$estimate), 2),
      lp_p_below_0_01 = as.character(lp$p.value < 0.01), le_p = round(le$p.value, 2))
}, error = function(e) runtime_failure("C-15", e))
.V$current <- NULL

## ============================================================================================
## CLAIM 16 [COMMENT MATERIAL]  (finding F16, Section 11)
##   In the copy audited all seven workbooks under pkg/Data hold participant-level records for
##   18 participants, keyed by a plain integer participant number (column id, present in 12 of
##   16 sheets) with a two-level sex label (column sex) and no direct identifier. The article
##   states consent to publication and makes no anonymisation claim, no README says the numbers
##   are pseudonymous codes, and all seven workbooks carry person-name creator and
##   last-modified-by fields in their document properties, which hold no participant data.
## Check written by the audit (no author code covers this claim):

.V$current <- "C-16"
tryCatch({
## F16: participant-level records, counts only (no value is printed).
expect("C-16", participants = 18, males = 12, females = 6, sheets_with_id = 12, sheets_total = 16, tol = 0,
       note = "distinct participant numbers and sex classes; sheets with an id column across the seven workbooks")
expect("C-16", personal_data_headers = 0, tol = 0,
       note = "headers naming a name, e-mail, birth date, date, age, address, phone or free-text field")
files <- list.files(file.path(PKG_DIR, "Data"), pattern = "\\.xlsx$", recursive = TRUE, full.names = TRUE)
hdr <- unlist(lapply(files, function(f) lapply(readxl::excel_sheets(f), function(s) names(readxl::read_excel(f, sheet = s, n_max = 0)))), recursive = FALSE)
has_id <- vapply(hdr, function(h) "id" %in% h, TRUE)
pd_hdr <- sum(grepl("name|mail|birth|dob|date|age$|^age|address|phone|comment|note", tolower(unlist(hdr))))
s <- unique(MT[, c("id", "sex")])
check("C-16", participants = length(unique(MT$id)), males = sum(s$sex == "male"), females = sum(s$sex == "female"),
      sheets_with_id = sum(has_id), sheets_total = length(hdr), personal_data_headers = pd_hdr)
}, error = function(e) runtime_failure("C-16", e))
.V$current <- NULL

## ---- summary of every audit-written check above ----
claims_summary(c("C-1", "C-2", "C-3", "C-4", "C-5", "C-6", "C-7", "C-8", "C-9", "C-10", "C-11", "C-12", "C-13", "C-14", "C-15", "C-16"))
