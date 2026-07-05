#!/usr/bin/env Rscript

library(dplyr)
library(jsonlite)
library(stringr)
library(stringi)

args <- commandArgs(trailingOnly = TRUE)
season <- args[1]

file_path <- file.path(getwd(), sprintf("stathead_pitching_%s.csv", season))

df <- read.csv(file_path, stringsAsFactors = FALSE)

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
# UTF‑8 SAFE Name Normalization (for matching only)
# ============================================================
normalize_name <- function(x) {

    # Remove accents ONLY for matching (keeps base letters intact)
    x <- stringi::stri_trans_general(x, "NFD; [:Nonspacing Mark:] Remove; NFC")

    x <- gsub("[,*#†+]", "", x)
    x <- gsub("\\.", "", x)
    x <- gsub("\\s+", " ", x)
    x <- trimws(x)

    if (grepl(",", x)) {
        parts <- unlist(strsplit(x, ","))
        last  <- trimws(parts[1])
        first <- trimws(parts[2])
        return(toupper(paste(first, last)))
    }

    parts <- unlist(strsplit(x, " "))
    if (length(parts) == 2) {
        first <- parts[1]
        last  <- parts[2]
        return(toupper(paste(first, last)))
    }

    return(toupper(x))
}

# ============================================================
# Add BOTH name fields
# ============================================================
df <- df %>%
  mutate(
    PlayerDisplay = player_name,                 # KEEP original UTF‑8 (Sánchez)
    PlayerClean   = normalize_name(player_name)  # For matching only
  )

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
# Output JSON (UTF‑8 SAFE)
# ============================================================
cat(toJSON(df, pretty = FALSE, auto_unbox = TRUE))
