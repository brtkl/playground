################################################################################
# Program:  demog_rtables.R
# Purpose:  Demographic & baseline characteristics table (Safety Population)
#           Implementation: {rtables} layout + {tern} analysis functions
# Input:    statprog_environment/data/adam/cdisc_pilot_project/adsl.xpt
# Output:   statprog_environment/outputs/tlf_demog_rtables.rtf
# Log:      statprog_environment/logs/demog_rtables.log
# Author:   Bartosz Szymecki
# Date:     2026-05-14
# Note:     Run from the project root (RStudio project working directory).
################################################################################


# 0. Logging -------------------------------------------------------------------

log_path <- file.path("statprog_environment", "logs", "demog_rtables.log")
if (file.exists(log_path)) file.remove(log_path)
log_con <- file(log_path, open = "wt")
sink(log_con, split = TRUE)
sink(log_con, type = "message")
on.exit({
  sink(type = "message"); sink(); close(log_con)
}, add = TRUE)

cat("demog_rtables.R - start:", format(Sys.time()), "\n",
    "Working directory:", getwd(), "\n\n", sep = "")


# 1. Packages ------------------------------------------------------------------

suppressPackageStartupMessages({
  library(haven)
  library(dplyr)
  library(rtables)
  library(tern)
  library(flextable)         # for RTF export via tt_to_flextable()
})


# 2. Read ADSL -----------------------------------------------------------------

adsl <- read_xpt(file.path("statprog_environment", "data", "adam",
                           "cdisc_pilot_project", "adsl.xpt"))
cat("ADSL: ", nrow(adsl), " subjects, ", ncol(adsl), " variables\n", sep = "")


# 3. Population (Safety) and variable prep -------------------------------------

trt_levels <- c("Placebo", "Xanomeline Low Dose", "Xanomeline High Dose")

adsl_saf <- adsl |>
  filter(SAFFL == "Y") |>
  mutate(
    TRT01P  = factor(TRT01P, levels = trt_levels),
    AGEGR1  = factor(AGEGR1, levels = c("<65", "65-80", ">80")),
    SEX     = factor(SEX,    levels = c("F", "M"),
                              labels = c("Female", "Male")),
    RACE    = factor(RACE)
  )

# Variable labels picked up by tern via var_labels()
formatters::var_labels(adsl_saf) <- c(
  formatters::var_labels(adsl_saf)[!names(adsl_saf) %in%
                                     c("AGE", "AGEGR1", "SEX", "RACE")],
  AGE    = "Age (years)",
  AGEGR1 = "Age group (years)",
  SEX    = "Sex",
  RACE   = "Race"
)

cat("Safety population N=", nrow(adsl_saf), "\n", sep = "")
cat("By TRT01P:\n"); print(table(adsl_saf$TRT01P))


# 4. Build rtables layout ------------------------------------------------------

vars       <- c("AGE", "AGEGR1", "SEX", "RACE")
var_labels <- formatters::var_labels(adsl_saf)[vars]

lyt <- basic_table(
    title       = "Table 14.1.1",
    subtitles   = "Demographic and Baseline Characteristics - Safety Population",
    main_footer = c(
      "Percentages are based on the number of subjects in the safety population within each treatment group.",
      paste("Source: ADSL  |  Program: demog_rtables.R  |  Generated:",
            format(Sys.time(), "%Y-%m-%d %H:%M"))
    ),
    show_colcounts = TRUE
  ) |>
  split_cols_by("TRT01P") |>
  add_overall_col("Total") |>
  analyze_vars(
    vars      = vars,
    var_labels = var_labels,
    .stats    = c("n", "mean_sd", "median", "range",
                  "count_fraction"),
    .formats  = c(n              = "xx",
                  mean_sd        = "xx.x (xx.xx)",
                  median         = "xx.x",
                  range          = "xx, xx",
                  count_fraction = "xx (xx.x%)"),
    .labels   = c(n              = "N",
                  mean_sd        = "Mean (SD)",
                  median         = "Median",
                  range          = "Min, Max")
  )


# 5. Build & print table -------------------------------------------------------

tbl <- build_table(lyt, adsl_saf)

cat("\n--- Table preview ---\n")
print(tbl)


# 6. RTF output ----------------------------------------------------------------

out_dir  <- file.path("statprog_environment", "outputs")
if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)
out_path <- file.path(out_dir, "tlf_demog_rtables.rtf")
if (file.exists(out_path)) file.remove(out_path)

# Convert rtables -> flextable -> RTF
ft <- tt_to_flextable(tbl) |>
  flextable::fontsize(size = 9, part = "all") |>
  flextable::font(fontname = "Calibri", part = "all") |>
  flextable::padding(padding = 2, part = "all")

flextable::save_as_rtf("Table 14.1.1" = ft, path = out_path)

# Confirm the file was actually written
if (file.exists(out_path)) {
  cat("\nRTF written: ", normalizePath(out_path), "  (",
      file.info(out_path)$size, " bytes)\n", sep = "")
} else {
  stop("RTF file was NOT written: ", out_path)
}


# 7. Session info -------------------------------------------------------------

cat("\n--- sessionInfo ---\n")
print(sessionInfo())

cat("\ndemog_rtables.R - end:", format(Sys.time()), "\n")
