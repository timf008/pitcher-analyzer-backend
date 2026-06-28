library(readr)
library(jsonlite)
library(stringr)
library(dplyr)

args <- commandArgs(trailingOnly = TRUE)
name <- args[1]
stat <- args[2]
season <- as.numeric(args[3])

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

nameClean <- clean_name(name)

# ============================================================
# Load CSV
# ============================================================
file_path <- sprintf("stathead_pitching_%s.csv", season)

if (!file.exists(file_path)) {
    cat(toJSON(list(value = NULL), auto_unbox = TRUE))
    quit()
}

df <- suppressWarnings(read_csv(file_path, show_col_types = FALSE))

# ============================================================
# Normalize column names (remove ALL hidden unicode + NBSP)
# ============================================================
clean_col <- function(x) {
    x <- iconv(x, from = "", to = "ASCII//TRANSLIT")
    x <- gsub("[[:space:]]+", "", x)      # remove ALL whitespace (space, NBSP, tabs)
    x <- gsub("[^A-Za-z0-9_]", "", x)     # remove punctuation, BOM, unicode
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
# Detect Player column
# ============================================================
name_col <- names(df)[str_detect(names(df), regex("player", ignore_case = TRUE))][1]

if (is.na(name_col)) {
    cat(toJSON(list(value = NULL), auto_unbox = TRUE))
    quit()
}

df$NameClean <- clean_name(df[[name_col]])

# ============================================================
# Clean Season column
# ============================================================
df$Season <- suppressWarnings(as.numeric(gsub("[^0-9]", "", as.character(df$Season))))

# ============================================================
# Detect SO and BB columns (bulletproof)
# ============================================================
so_cols <- names(df)[names(df) == "SO"]
bb_cols <- names(df)[names(df) == "BB"]

if (length(so_cols) == 0 || length(bb_cols) == 0) {
    cat(toJSON(list(value = NULL), auto_unbox = TRUE))
    quit()
}

so_col <- so_cols[length(so_cols)]
bb_col <- bb_cols[length(bb_cols)]

# ============================================================
# Match row
# ============================================================
row <- df %>% filter(NameClean == nameClean, Season == season)

if (nrow(row) == 0) {
    cat(toJSON(list(value = NULL), auto_unbox = TRUE))
    quit()
}

# ============================================================
# Safe numeric conversion
# ============================================================
safenum <- function(x) suppressWarnings(as.numeric(x))

# ============================================================
# Compute derived stats
# ============================================================
if (stat == "Kpct") {

    if ("BF" %in% names(row) && !is.na(row$BF)) {
        value <- (safenum(row[[so_col]]) / safenum(row$BF)) * 100
    } else {
        value <- NA
    }

} else if (stat == "BBpct") {

    if ("BF" %in% names(row) && !is.na(row$BF)) {
        value <- (safenum(row[[bb_col]]) / safenum(row$BF)) * 100
    } else {
        value <- NA
    }

} else if (stat == "KBB") {

    value <- safenum(row[[so_col]]) / safenum(row[[bb_col]])

} else {

    value <- safenum(row[[stat]])
}

# ============================================================
# Final normalization
# ============================================================
value <- safenum(value)

if (is.null(value) || length(value) == 0 || is.na(value)) {
    value <- NA
}

# ============================================================
# Output JSON
# ============================================================
cat(toJSON(list(value = value), auto_unbox = TRUE))
