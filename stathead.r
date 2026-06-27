#!/usr/bin/env Rscript

library(readr)
library(dplyr)
library(jsonlite)
library(stringr)

args <- commandArgs(trailingOnly = TRUE)
player_name <- args[1]
season <- args[2]

# ============================================================
# Clean names (FIRST LAST only — no reordering)
# ============================================================
clean_name <- function(x) {
    x <- iconv(x, from = "", to = "ASCII//TRANSLIT")
    x <- gsub("[,*#†+]", "", x)
    x <- gsub("\\.", "", x)
    x <- gsub("\\s+", " ", x)
    x <- trimws(x)
    toupper(x)
}

player_name_clean <- clean_name(player_name)

# ============================================================
# Load CSV
# ============================================================
file_path <- sprintf("stathead_pitching_%s.csv", season)

if (!file.exists(file_path)) {
    cat(toJSON(list(error = paste("CSV not found:", file_path)), auto_unbox = TRUE))
    quit(status = 0)
}

df <- read_csv(file_path, show_col_types = FALSE)

# ============================================================
# Normalize column names
# ============================================================
names(df) <- names(df) |>
  str_replace_all("%", "pct") |>
  str_replace_all("/", "_") |>
  str_replace_all("\\.", "") |>
  str_replace_all(" ", "_")

# ============================================================
# Detect Player column
# ============================================================
name_col <- names(df)[str_detect(names(df), regex("^Player$", ignore_case = TRUE))][1]

if (is.na(name_col)) {
    cat(toJSON(list(error = "No Player column found"), auto_unbox = TRUE))
    quit(status = 0)
}

# ============================================================
# Normalize CSV names (FIRST LAST only)
# ============================================================
df$NameClean <- clean_name(df[[name_col]])

# ============================================================
# Clean Season column
# ============================================================
df$Season <- as.numeric(gsub("[^0-9]", "", as.character(df$Season)))

# ============================================================
# Detect SO and BB columns (patched for duplicates)
# ============================================================
# SO: choose the LAST "SO" column (real strikeouts)
so_cols <- names(df)[str_detect(names(df), "^SO$")]
so_col <- so_cols[length(so_cols)]

# BB: choose the LAST "BB" column (not IBB)
bb_cols <- names(df)[str_detect(names(df), "^BB$")]
bb_col <- bb_cols[length(bb_cols)]

# ============================================================
# Filter for player + season
# ============================================================
p <- df %>%
  filter(
    NameClean == player_name_clean,
    Season == as.numeric(season)
  )

if (nrow(p) == 0) {
    cat(toJSON(list(error = "Player not found"), auto_unbox = TRUE))
    quit(status = 0)
}

# ============================================================
# Bulletproof K% and BB%
# ============================================================
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

# ============================================================
# Compute K/BB if missing
# ============================================================
if (!"SO_BB" %in% names(p)) {
    p$SO_BB <- p[[so_col]] / p[[bb_col]]
}

# ============================================================
# Build JSON output
# ============================================================
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
    GS = as.numeric(GS)
  )

cat(toJSON(result, pretty = TRUE, auto_unbox = TRUE))
