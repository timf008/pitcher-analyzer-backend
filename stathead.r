#!/usr/bin/env Rscript

library(readr)
library(dplyr)
library(jsonlite)
library(stringr)
library(stringi)

args <- commandArgs(trailingOnly = TRUE)
player_name <- args[1]
season <- args[2]

# ============================================================
# Name Normalization (UTF-8 SAFE)
# Converts ALL formats → "FIRST LAST"
# ============================================================
normalize_name <- function(x) {
    x <- stri_trans_general(x, "Latin-ASCII")
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
get_col <- function(pattern) {
    cols <- names(df)[str_detect(names(df), pattern)]
    if (length(cols) == 0) return(NA_character_)
    cols[1]
}

so_cols <- names(df)[str_detect(names(df), "^SO")]
bb_cols <- names(df)[str_detect(names(df), "^BB$")]

so_col <- so_cols[1]
bb_col <- bb_cols[1]

team_col <- get_col("^Team$")   # ⭐ NEW
# ============================================================
# Bulletproof K% and BB%
# ============================================================
if ("BF" %in% names(df)) {
    df$Kpct <- (df[[so_col]] / df$BF) * 100
    df$BBpct <- (df[[bb_col]] / df$BF) * 100

} else if (all(c("IP", "H", "BB", "HBP") %in% names(df))) {
    est_BF <- (df$IP * 3) + df$H + df$BB + df$HBP
    df$Kpct <- (df[[so_col]] / est_BF) * 100
    df$BBpct <- (df[[bb_col]] / est_BF) * 100

} else if (all(c("SO9", "BB9") %in% names(df))) {
    df$Kpct <- (df$SO9 / 27) * 100
    df$BBpct <- (df$BB9 / 27) * 100

} else {
    df$Kpct <- NA
    df$BBpct <- NA
}

# ============================================================
# Compute K/BB if missing
# ============================================================
if (!"SO_BB" %in% names(df)) {
    df$SO_BB <- df[[so_col]] / df[[bb_col]]
}

# ============================================================
# Percentile helper
# ============================================================
percentile <- function(x, higher_is_better = TRUE) {
    valid <- !is.na(x)
    if (higher_is_better) {
        return(rank(x, na.last = "keep") / sum(valid) * 100)
    } else {
        return(rank(-x, na.last = "keep") / sum(valid) * 100)
    }
}

# ============================================================
# Backend Overall Score (same formula as frontend)
# ============================================================
score_era <- function(era) {
    pmin(pmax(10 * (5.00 - era) / (5.00 - 2.00), 0), 10)
}

score_whip <- function(whip) {
    pmin(pmax(10 * (1.40 - whip) / (1.40 - 0.90), 0), 10)
}

score_kpct <- function(kpct) {
    pmin(pmax(10 * (kpct - 15) / (35 - 15), 0), 10)
}

score_bbpct <- function(bbpct) {
    pmin(pmax(10 * (10 - bbpct) / (10 - 3), 0), 10)
}

score_kbb <- function(kbb) {
    pmin(pmax(10 * (kbb - 1.5) / (6.0 - 1.5), 0), 10)
}

compute_overall <- function(era, whip, kpct, bbpct, kbb) {
    score_era(era)  * 0.25 +
    score_whip(whip) * 0.25 +
    score_kpct(kpct) * 0.1875 +
    score_bbpct(bbpct) * 0.125 +
    score_kbb(kbb)   * 0.1875
}

df$OverallScore <- compute_overall(df$ERA, df$WHIP, df$Kpct, df$BBpct, df$SO_BB)

# ============================================================
# Compute Overall Percentile
# ============================================================
df$Overall_pct <- percentile(df$OverallScore, higher_is_better = TRUE)

# ============================================================
# Pitcher XP Score (updated formula)
# ============================================================
compute_pitcher_xp <- function(kpct, kbb, era, whip, bbpct) {
    xp <- (kpct * 2) +
          (kbb * 10) -
          (era * 15) -
          (whip * 40) -
          (bbpct * 10)

    return(xp + 1000)  # Pitchers get +1000 baseline
}

df$XP <- compute_pitcher_xp(df$Kpct, df$SO_BB, df$ERA, df$WHIP, df$BBpct)



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
# Build JSON output
# ============================================================
result <- p %>%
  transmute(
    ERA  = as.numeric(ERA),
    WHIP = as.numeric(WHIP),
    Kpct = as.numeric(Kpct),
    BBpct = as.numeric(BBpct),
    KBB = as.numeric(SO_BB),

    XP = as.numeric(XP),   # ⭐ NEW — Pitcher XP included

    Overall_pct = as.numeric(Overall_pct),

    Team = if (!is.na(team_col)) as.character(.data[[team_col]]) else NA_character_,   # ⭐ NEW

    IP = as.numeric(IP),
    HR9 = as.numeric(HR9),
    FIP = as.numeric(FIP),
    W = as.numeric(W),
    L = as.numeric(L),
    GS = as.numeric(GS)
  )

cat(toJSON(result, pretty = TRUE, auto_unbox = TRUE))

