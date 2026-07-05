#!/usr/bin/env Rscript

library(dplyr)
library(jsonlite)
library(stringr)
library(stringi)

args <- commandArgs(trailingOnly = TRUE)
season <- args[1]

file_path <- file.path(getwd(), sprintf("stathead_pitching_%s.csv", season))

# ============================================================
# Read raw file safely (no parsing yet)
# ============================================================
raw <- readLines(file_path, warn = FALSE)

# Remove BOM if present
raw <- sub("\ufeff", "", raw)

# Fix malformed quotes (Stathead sometimes emits broken CSV)
raw <- gsub('""', '"', raw)

# ============================================================
# Parse using read.csv(text=...) which bypasses fileEncoding issues
# ============================================================
df <- read.csv(
  text = raw,
  stringsAsFactors = FALSE,
  check.names = FALSE
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


