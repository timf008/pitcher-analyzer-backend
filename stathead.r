#!/usr/bin/env Rscript

library(readr)
library(dplyr)
library(jsonlite)
library(stringr)

args <- commandArgs(trailingOnly = TRUE)
player_name <- args[1]
season <- args[2]

# -------------------------------
# Name Normalization Helpers
# -------------------------------

# Convert "LAST, FIRST" → "FIRST LAST"
fix_last_first <- function(x) {
    if (grepl(",", x)) {
        parts <- unlist(strsplit(x, ","))
        first <- trimws(parts[2])
        last  <- trimws(parts[1])
        return(paste(first, last))
    }
    return(x)
}

# Clean and normalize names
clean <- function(x) {
    x <- fix_last_first(x)
    x <- trimws(x)
    x <- gsub("[,*#†+]", "", x)
    x <- gsub("\\.", "", x)
    x <- gsub("\\s+", " ", x)
    toupper(x)
}

player_name_clean <- clean(player_name)

# -------------------------------
# Load correct CSV for season
# -------------------------------
file_path <- sprintf("stathead_pitching_%s.csv", season)

if (!file.exists(file_path)) {
    cat(toJSON(list(error = paste("CSV not found:", file_path)), auto_unbox = TRUE))
    quit(status = 0)
}

df <- read_csv(file_path, show_col_types = FALSE)

# -------------------------------
# Normalize column names
# -------------------------------
names(df) <- names(df) |>
  str_replace_all("%", "pct") |>
  str_replace_all("/", "_") |>
  str_replace_all("\\.", "") |>
  str_replace_all(" ", "_")

# -------------------------------
# Detect player name column
# -------------------------------
name_col <- names(df)[str_detect(names(df), regex("^Player$", ignore_case = TRUE))][1]

if (is.na(name_col)) {
    cat(toJSON(list(error = "No Player column found"), auto_unbox = TRUE))
    quit(status = 0)
}

# -------------------------------
# Normalize names in CSV
# -------------------------------
df$NameClean <- clean(df[[name_col]])

# -------------------------------
# Season must be numeric (clean weird Stathead formats)
# -------------------------------
df$Season <- df$Season |>
    as.character() |>
    gsub("[^0-9]", "", _) |>
    as.numeric()

# -------------------------------
# FIX: Duplicate SO column issue
# -------------------------------
# Your CSV has SO twice — the LAST one is the real strikeout column
so_cols <- names(df)[names(df) == "SO"]
so_col <- so_cols[length(so_cols)]  # pick last SO

# BB column is correct
bb_col <- "BB"

# -------------------------------
# Filter for player + season
# -------------------------------
p <- df %>%
  filter(
    NameClean == player_name_clean,
    Season == as.numeric(season)
  )

if (nrow(p) == 0) {
    cat(toJSON(list(error = "Player not found"), auto_unbox = TRUE))
    quit(status = 0)
}

# -------------------------------
# Bulletproof K% and BB% logic
# -------------------------------
if ("BF" %in% names(p) && !is.na(p$BF)) {

    p$Kpct <- (p[[so_col]] / p$BF) * 100
    p$BBpct <- (p[[bb_col]] / p$BF) * 100

} else if (all(c("IP", "H", "BB", "HBP") %in% names(p))) {

    est_BF <- (p$IP * 3) + p$H + p$BB + p$HBP

    p$Kpct <- (p[[so_col]] / est_BF) * 100
    p$BBpct <- (p[[bb_col]] / est_BF) * 100

} else if (all(c("SO9", "BB9") %in% names(p))) {

    p$Kpct <- (p$SO9 / 27) * 100
    p$BBpct <- (p$BB9 / 27) * 100

} else {
    p$Kpct <- NA
    p$BBpct <- NA
}

# -------------------------------
# Compute K/BB if missing
# -------------------------------
if (!"SO_BB" %in% names(p)) {
    p$SO_BB <- p[[so_col]] / p[[bb_col]]
}

# -------------------------------
# Build JSON output
# -------------------------------
result <- p %>%
  transmute(
    ERA  = as.numeric(ERA),
    WHIP = as.numeric(WHIP),
    Kpct = as.numeric(Kpct),
    BBpct = as.numeric(BBpct),
    KBB = as.numeric(SO_BB),
    IP = as.numeric(IP),
    HR9 = as.numeric(HR9),
    FIP = as.numeric(FIP),
    W = as.numeric(W),
    L = as.numeric(L),
    GS = as.numeric(GS)   # REQUIRED FOR RANK
  )

cat(toJSON(result, pretty = TRUE, auto_unbox = TRUE))
