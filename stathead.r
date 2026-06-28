#!/usr/bin/env Rscript

library(readr)
library(dplyr)
library(jsonlite)
library(stringr)

args <- commandArgs(trailingOnly = TRUE)
player_name <- args[1]
season <- args[2]

# ============================================================
# Name Normalization (UTF-8 SAFE)
# Converts ALL formats → "FIRST LAST"
# ============================================================
normalize_name <- function(x) {
    # Remove accents safely
    x <- iconv(x, from = "UTF-8", to = "ASCII//TRANSLIT", sub = "")

    # Remove punctuation
    x <- gsub("[,*#†+]", "", x)
    x <- gsub("\\.", "", x)
    x <- gsub("\\s+", " ", x)
    x <- trimws(x)

    # If "Last, First" → convert to "First Last"
    if (grepl(",", x)) {
        parts <- unlist(strsplit(x, ","))
        last  <- trimws(parts[1])
        first <- trimws(parts[2])
        return(toupper(paste(first, last)))
    }

    # If already "First Last"
    parts <- unlist(strsplit(x, " "))
    if (length(parts) == 2) {
        first <- parts[1]
        last  <- parts[2]
        return(toupper(paste(first, last)))
    }

    # Fallback
    return(toupper(x))
}

player_name_clean <- normalize_name(player_name)

# ============================================================
# Load CSV (ABSOLUTE PATH FIX)
# ============================================================
file_path <- file.path(getwd(), sprintf("stathead_pitching_%s.csv", season))

if (!file.exists(file_path)) {
    cat(toJSON(list(error = paste("CSV not found:", file_path)), auto_unbox = TRUE))
    quit(status = 1)
}

df <- read_csv(file_path, show_col_types = FALSE)
print(head(df))

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
    quit(status = 1)
}

# ============================================================
# Normalize CSV names (UTF-8 SAFE)
# ============================================================
df$NameClean <- sapply(df[[name_col]], normalize_name)

# ============================================================
# Clean Season column
# ============================================================
df$Season <- as.numeric(gsub("[^0-9]", "", as.character(df$Season)))

# ============================================================
# Detect SO and BB columns
# ============================================================
so_cols <- names(df)[str_detect(names(df), "^SO")]
bb_cols <- names(df)[str_detect(names(df), "^BB$")]

so_col <- so_cols[1]
bb_col <- bb_cols[1]

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
    quit(status = 1)
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
