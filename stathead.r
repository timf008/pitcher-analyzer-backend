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

df <- suppressWarnings(read_csv(file_path, show_col_types = FALSE))

# ============================================================
# Normalize column names (remove ALL hidden unicode + NBSP)
# ============================================================
clean_col <- function(x) {
    x <- iconv(x, from = "", to = "ASCII//TRANSLIT", sub = "")
    x <- gsub("[\u00A0\u200B\u200C\u200D\uFEFF]", "", x)  # NBSP, ZWSP, ZWNJ, ZWJ, BOM
    x <- gsub("[[:space:]]+", "", x)                     # remove ASCII whitespace
    x <- gsub("[^A-Za-z0-9_]", "", x)                    # remove punctuation
    x
}

names(df) <- names(df) |>
  str_replace_all("%", "pct") |>
  str_replace_all("/", "_") |>
  str_replace_all("\\.", "") |>
  str_replace_all(" ", "_") |>
  trimws() |>
  clean_col()

# ============================================================
# Detect Player column (robust)
# ============================================================
name_col <- names(df)[str_detect(names(df), regex("player", ignore_case = TRUE))][1]

if (is.na(name_col)) {
    cat(toJSON(list(error = "No Player column found"), auto_unbox = TRUE))
    quit(status = 0)
}

df$NameClean <- clean_name(df[[name_col]])

# ============================================================
# Clean Season column
# ============================================================
df$Season <- suppressWarnings(as.numeric(gsub("[^0-9]", "", as.character(df$Season))))

# ============================================================
# Detect SO and BB columns (NEW — works with SO3, SO25, etc.)
# ============================================================
# Any column that *starts* with SO is a strikeout column
so_cols <- names(df)[str_detect(names(df), "^SO")]

# BB is still BB
bb_cols <- names(df)[names(df) == "BB"]

if (length(so_cols) == 0 || length(bb_cols) == 0) {
    cat(toJSON(list(error = "Missing SO or BB column"), auto_unbox = TRUE))
    quit(status = 0)
}

# Use the LAST SO column (Stathead puts the real SO last)
so_col <- so_cols[length(so_cols)]
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

    p$Kpct <- (as.numeric(p[[so_col]]) / as.numeric(p$BF)) * 100
    p$BBpct <- (as.numeric(p[[bb_col]]) / as.numeric(p$BF)) * 100

} else if (all(c("IP", "H", "BB", "HBP") %in% names(p))) {

    est_BF <- (as.numeric(p$IP) * 3) + as.numeric(p$H) + as.numeric(p$BB) + as.numeric(p$HBP)

    p$Kpct <- (as.numeric(p[[so_col]]) / est_BF) * 100
    p$BBpct <- (as.numeric(p[[bb_col]]) / est_BF) * 100

} else if (all(c("SO9", "BB9") %in% names(p))) {

    p$Kpct <- (as.numeric(p$SO9) / 27) * 100
    p$BBpct <- (as.numeric(p$BB9) / 27) * 100

} else {
    p$Kpct <- NA
    p$BBpct <- NA
}

# ============================================================
# Compute K/BB if missing
# ============================================================
if (!"SO_BB" %in% names(p)) {
    p$SO_BB <- as.numeric(p[[so_col]]) / as.numeric(p[[bb_col]])
}

# ============================================================
# Build JSON output (suppress warnings)
# ============================================================
result <- suppressWarnings(
    p %>%
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
)

cat(toJSON(result, pretty = TRUE, auto_unbox = TRUE))

