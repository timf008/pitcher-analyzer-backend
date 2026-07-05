#!/usr/bin/env Rscript

library(dplyr)
library(jsonlite)
library(stringr)
library(stringi)
library(readr)   # <-- new

args <- commandArgs(trailingOnly = TRUE)
season <- args[1]

file_path <- file.path(getwd(), sprintf("stathead_pitching_%s.csv", season))

# ============================================================
# Read CSV robustly with readr (handles encoding + parsing)
# ============================================================
df <- read_csv(
  file_path,
  locale = locale(encoding = "UTF-8"),
  show_col_types = FALSE
)

# ============================================================
# Normalize column names safely (NO janitor)
# ============================================================
names(df) <- names(df) |>
  str_replace_all("%", "pct") |>
  str_replace_all("/", "_") |>
  str_replace_all("\\.", "") |>
  str_replace_all(" ", "_")

names(df) <- make.unique(names(df), sep = "_")

# ============================================================
# Detect SO_BB column (SO/BB ratio)
# ============================================================
ratio_col <- names(df)[str_detect(names(df), regex("^so_?bb$", ignore_case = TRUE))]

if (length(ratio_col) == 1) {
    names(df)[names(df) == ratio_col] <- "SO_BB"
} else {
    stop("Could not identify SO/BB column after cleaning.")
}

# ============================================================
# Compute K%, BB%, K/BB
# ============================================================
df <- df %>%
  mutate(
    Kpct  = round((SO / BF) * 100, 1),
    BBpct = round((BB / BF) * 100, 1),
    KBB   = round(SO_BB, 2),
    ERA   = round(ERA, 2),
    WHIP  = round(WHIP, 3)
  )

# ============================================================
# Output JSON as UTF-8
# ============================================================
cat(enc2utf8(toJSON(df, pretty = FALSE, auto_unbox = TRUE)))



