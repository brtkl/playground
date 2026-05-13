################################################################################
# Program:  demog_r2rtf.R
# Purpose:  Demographic & baseline characteristics table (Safety Population)
#           Implementation: {r2rtf} with manual dplyr summaries
# Input:    statprog_environment/data/adam/cdisc_pilot_project/adsl.xpt
# Output:   statprog_environment/outputs/tlf_demog_r2rtf.rtf
# Log:      statprog_environment/logs/demog_r2rtf.log
# Author:   Bartosz Szymecki
# Date:     2026-05-14
# Note:     Run from the project root (RStudio project working directory).
################################################################################


# 0. Logging -------------------------------------------------------------------

log_path <- file.path("statprog_environment", "logs", "demog_r2rtf.log")
if (file.exists(log_path)) file.remove(log_path)
log_con <- file(log_path, open = "wt")
sink(log_con, split = TRUE)              # stdout -> console + log
sink(log_con, type = "message")          # messages/warnings -> log
on.exit({
  sink(type = "message"); sink(); close(log_con)
}, add = TRUE)

cat("demog_r2rtf.R - start:", format(Sys.time()), "\n",
    "Working directory:", getwd(), "\n\n", sep = "")


# 1. Packages ------------------------------------------------------------------

suppressPackageStartupMessages({
  library(haven)
  library(dplyr)
  library(tidyr)
  library(r2rtf)
})


# 2. Read ADSL -----------------------------------------------------------------

adsl <- read_xpt(file.path("statprog_environment", "data", "adam",
                           "cdisc_pilot_project", "adsl.xpt"))
cat("ADSL: ", nrow(adsl), " subjects, ", ncol(adsl), " variables\n", sep = "")


# 3. Population (Safety) -------------------------------------------------------

trt_levels <- c("Placebo", "Xanomeline Low Dose", "Xanomeline High Dose")

pop <- adsl |>
  filter(SAFFL == "Y") |>
  mutate(TRT01P = factor(TRT01P, levels = trt_levels))

n_by_trt <- pop |> count(TRT01P, .drop = FALSE) |> pull(n)
n_total  <- nrow(pop)
cat("Safety population N=", n_total,
    "  (", paste(n_by_trt, collapse = " / "), ")\n", sep = "")


# 4. Summary helpers -----------------------------------------------------------

empty_row <- function(label) {
  tibble(label = label,
         Placebo = "",
         `Xanomeline Low Dose` = "",
         `Xanomeline High Dose` = "",
         Total = "")
}

fmt_n_pct <- function(n, denom) {
  ifelse(is.na(n) | n == 0, "0",
         sprintf("%d (%.1f)", n, 100 * n / denom))
}

# Continuous: returns header row + N / Mean (SD) / Median / Min, Max
summary_continuous <- function(data, var, var_label) {
  base <- data |>
    group_by(TRT01P, .drop = FALSE) |>
    summarise(
      N      = sum(!is.na(.data[[var]])),
      Mean   = mean(.data[[var]],   na.rm = TRUE),
      SD     = sd(.data[[var]],     na.rm = TRUE),
      Median = median(.data[[var]], na.rm = TRUE),
      Min    = min(.data[[var]],    na.rm = TRUE),
      Max    = max(.data[[var]],    na.rm = TRUE),
      .groups = "drop"
    ) |>
    mutate(TRT01P = as.character(TRT01P))

  total <- data |>
    summarise(
      TRT01P = "Total",
      N      = sum(!is.na(.data[[var]])),
      Mean   = mean(.data[[var]],   na.rm = TRUE),
      SD     = sd(.data[[var]],     na.rm = TRUE),
      Median = median(.data[[var]], na.rm = TRUE),
      Min    = min(.data[[var]],    na.rm = TRUE),
      Max    = max(.data[[var]],    na.rm = TRUE)
    )

  stats <- bind_rows(base, total) |>
    transmute(
      TRT01P,
      `  N`         = sprintf("%d", N),
      `  Mean (SD)` = sprintf("%.1f (%.2f)", Mean, SD),
      `  Median`    = sprintf("%.1f", Median),
      `  Min, Max`  = sprintf("%g, %g", Min, Max)
    ) |>
    pivot_longer(-TRT01P, names_to = "label", values_to = "value") |>
    pivot_wider(names_from = TRT01P, values_from = value) |>
    select(label, Placebo, `Xanomeline Low Dose`, `Xanomeline High Dose`, Total)

  bind_rows(empty_row(var_label), stats)
}

# Categorical: returns header row + n (%) per level
summary_categorical <- function(data, var, var_label, levels = NULL,
                                level_labels = NULL) {
  if (is.null(levels)) {
    levels <- sort(unique(stats::na.omit(data[[var]])))
  }
  if (is.null(level_labels)) level_labels <- levels

  by_trt <- data |>
    mutate(.cat = factor(.data[[var]], levels = levels)) |>
    count(TRT01P, .cat, .drop = FALSE) |>
    pivot_wider(names_from = TRT01P, values_from = n, values_fill = 0)

  total_n <- data |>
    mutate(.cat = factor(.data[[var]], levels = levels)) |>
    count(.cat, .drop = FALSE) |>
    rename(Total = n)

  combined <- by_trt |> left_join(total_n, by = ".cat")

  out <- combined |>
    mutate(
      label = paste0("  ", level_labels[match(.cat, levels)]),
      Placebo                = fmt_n_pct(Placebo,                n_by_trt[1]),
      `Xanomeline Low Dose`  = fmt_n_pct(`Xanomeline Low Dose`,  n_by_trt[2]),
      `Xanomeline High Dose` = fmt_n_pct(`Xanomeline High Dose`, n_by_trt[3]),
      Total                  = fmt_n_pct(Total,                  n_total)
    ) |>
    select(label, Placebo, `Xanomeline Low Dose`,
           `Xanomeline High Dose`, Total)

  bind_rows(empty_row(var_label), out)
}


# 5. Build demographics table -------------------------------------------------

tbl <- bind_rows(
  summary_continuous (pop, "AGE",    "Age (years)"),
  summary_categorical(pop, "AGEGR1", "Age group (years)",
                      levels = c("<65", "65-80", ">80")),
  summary_categorical(pop, "SEX",    "Sex",
                      levels       = c("F", "M"),
                      level_labels = c("Female", "Male")),
  summary_categorical(pop, "RACE",   "Race")
)

cat("\n--- Table preview ---\n")
print(tbl, n = Inf)


# 6. RTF output ---------------------------------------------------------------

col_header <- paste(
  " ",
  sprintf("Placebo\\line (N=%d)",              n_by_trt[1]),
  sprintf("Xanomeline Low Dose\\line (N=%d)",  n_by_trt[2]),
  sprintf("Xanomeline High Dose\\line (N=%d)", n_by_trt[3]),
  sprintf("Total\\line (N=%d)",                n_total),
  sep = " | "
)

col_widths <- c(3.0, 1.7, 1.7, 1.7, 1.7)
col_align  <- c("l", "c", "c", "c", "c")

out_path <- file.path("statprog_environment", "outputs", "tlf_demog_r2rtf.rtf")

tbl |>
  rtf_page(orientation = "landscape") |>
  rtf_title(
    title    = "Table 14.1.1",
    subtitle = "Demographic and Baseline Characteristics - Safety Population"
  ) |>
  rtf_colheader(
    colheader          = col_header,
    col_rel_width      = col_widths,
    text_justification = col_align,
    text_format        = "b"
  ) |>
  rtf_body(
    col_rel_width      = col_widths,
    text_justification = col_align
  ) |>
  rtf_footnote(c(
    "Percentages are based on the number of subjects in the safety population within each treatment group.",
    paste("Source: ADSL  |  Program: demog_r2rtf.R  |  Generated:",
          format(Sys.time(), "%Y-%m-%d %H:%M"))
  )) |>
  rtf_encode() |>
  write_rtf(file = out_path)

cat("\nRTF written to: ", normalizePath(out_path, mustWork = FALSE), "\n", sep = "")


# 7. Session info -------------------------------------------------------------

cat("\n--- sessionInfo ---\n")
print(sessionInfo())

cat("\ndemog_r2rtf.R - end:", format(Sys.time()), "\n")
