#!/usr/bin/env Rscript

library(dplyr)
library(jsonlite)
library(stringr)
library(stringi)

args <- commandArgs(trailingOnly = TRUE)
season <- args[1]

file_path <- file.path(getwd(), sprintf("stathead_pitching_%s.csv", season))

# ============================================================
# Detect encoding and read CSV safely
# ============================================================
# Read a chunk of the file as raw text
raw_lines <- readLines(file_path, n = 100, warn = FALSE)

enc_guess <- stri_enc_detect(raw_lines)[[1]]$Encoding[1]

# Fallback if detection fails
if (is.na(enc_guess)) {
  enc_guess <- "UTF-8"
}

# Read using detected encoding
df <- read.csv(
  file_path,
  stringsAsFactors = FALSE,
  fileEncoding = enc_guess
)

# ============================================================
# Normalize column names safely (NO janitor)
# ============================================================
names(df) <- names(df) |>
  str_replace_all("%", "pct") |>
  str_replace_all("/", "_") |>
  str_replace_all("\\.", "") |>
  str_replace_all(" ", "_")

# Fix duplicates created by cleaning
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
# Emit JSON as UTF-8 so accents survive
# ============================================================
json_out <- toJSON(df, pretty = FALSE, auto_unbox = TRUE)
cat(enc2utf8(json_out))

