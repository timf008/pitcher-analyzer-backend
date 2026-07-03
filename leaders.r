#!/usr/bin/env Rscript

library(dplyr)
library(jsonlite)

args <- commandArgs(trailingOnly = TRUE)
season <- args[1]

file_path <- file.path(getwd(), sprintf("stathead_pitching_%s.csv", season))

df <- read.csv(file_path, stringsAsFactors = FALSE)

# ============================================================
# Normalize column names (same pattern as batting leaders)
# ============================================================
clean_names <- names(df) |>
  (\(x) gsub("%", "pct", x))() |>
  (\(x) gsub("/", "_", x))() |>
  (\(x) gsub("\\.", "", x))() |>
  (\(x) gsub(" ", "_", x))()

# Fix duplicates created by cleaning
names(df) <- make.unique(clean_names, sep = "_")

# ============================================================
# Compute K%, BB%, K/BB
# Your CSV already has SO, BB, BF, SO_BB, ERA, WHIP
# ============================================================
df <- df %>%
  mutate(
    Kpct  = round((SO / BF) * 100, 1),
    BBpct = round((BB / BF) * 100, 1),
    KBB   = round(SO_BB, 2),
    ERA   = round(ERA, 2),
    WHIP  = round(WHIP, 3)
  )

cat(toJSON(df, pretty = FALSE, auto_unbox = TRUE))

