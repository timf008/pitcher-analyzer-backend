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

# Convert "First Last" → "Last, First"
to_last_first <- function(x) {
    x <- trimws(x)
    parts <- unlist(strsplit(x, " "))
    if (length(parts) == 2) {
        return(paste(parts[2], parts[1], sep = ", "))
    }
    return(x)
}

# Clean punctuation, accents, spacing
clean <- function(x) {
    x <- iconv(x, from = "", to = "ASCII//TRANSLIT")  # remove accents
    x <- gsub("[,*#†+]", "", x)
    x <- gsub("\\.", "", x)
    x <- gsub("\\s+", " ", x)
    trimws(x)
}

# Apply full normalization
player_name_clean <- clean(player_name)
player_name_clean <- to_last_first(player_name_clean)
player_name_clean <- toupper(player_name_clean)

# -------------------------------
# Load CSV
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
df$NameClean <- df[[name_col]] |>
    clean() |>
    to_last_first() |>
    toupper()

# -------------------------------
# Clean season column
# -------------------------------
df$Season <- as.numeric(gsub("[^0-9]", "", as.character(df$Season)))

# -------------------------------
# Detect SO and BB columns
# -------------------------------
so_cols <- names(df)[str_detect(names(df), "^SO")]
bb_cols <- names(df)[str_detect(names(df), "^BB$")]

so_col <- so_cols[1]
bb_col <- bb_cols[1]

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
# Bulletproof K% and BB%
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
    GS = as.numeric(GS)
  )

cat(toJSON(result, pretty = TRUE, auto_unbox = TRUE))

