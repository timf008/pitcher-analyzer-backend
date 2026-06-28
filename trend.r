#!/usr/bin/env Rscript

library(readr)
library(jsonlite)
library(stringr)
library(dplyr)

args <- commandArgs(trailingOnly = TRUE)
name <- args[1]
stat <- args[2]
season <- as.numeric(args[3])

# -------------------------------
# Clean names (NO reordering)
# -------------------------------
clean <- function(x) {
    x <- trimws(x)
    x <- gsub("[,*#†+]", "", x)
    x <- gsub("\\.", "", x)
    x <- gsub("\\s+", " ", x)
    toupper(x)
}

nameClean <- clean(name)

# -------------------------------
# Load CSV (ABSOLUTE PATH FIX)
# -------------------------------
file_path <- file.path(getwd(), sprintf("stathead_pitching_%s.csv", season))

if (!file.exists(file_path)) {
    cat(toJSON(list(value = NULL), auto_unbox = TRUE))
    quit(status = 1)
}

df <- suppressWarnings(read_csv(file_path, show_col_types = FALSE))

# -------------------------------
# Normalize names
# -------------------------------
name_col <- names(df)[str_detect(names(df), regex("^Player$", ignore_case = TRUE))][1]

if (is.na(name_col)) {
    cat(toJSON(list(value = NULL), auto_unbox = TRUE))
    quit(status = 1)
}

df$NameClean <- clean(df[[name_col]])

# -------------------------------
# Clean Season column
# -------------------------------
df$Season <- suppressWarnings(as.numeric(df$Season))

# -------------------------------
# Detect SO and BB columns (robust)
# -------------------------------
so_cols <- names(df)[str_detect(names(df), "^SO")]
bb_cols <- names(df)[str_detect(names(df), "^BB$")]

if (length(so_cols) == 0 || length(bb_cols) == 0) {
    cat(toJSON(list(value = NULL), auto_unbox = TRUE))
    quit(status = 1)
}

# Use LAST SO column (Stathead puts real SO last)
so_col <- so_cols[length(so_cols)]
bb_col <- bb_cols[length(bb_cols)]

# -------------------------------
# Match row
# -------------------------------
row <- df[df$NameClean == nameClean & df$Season == season, ]

if (nrow(row) == 0) {
    cat(toJSON(list(value = NULL), auto_unbox = TRUE))
    quit(status = 1)
}

# -------------------------------
# Safe numeric conversion
# -------------------------------
safenum <- function(x) suppressWarnings(as.numeric(x))

# -------------------------------
# Compute derived stats
# -------------------------------
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

# -------------------------------
# Final normalization
# -------------------------------
value <- safenum(value)

if (is.null(value) || length(value) == 0 || is.na(value)) {
    value <- NA
}

# -------------------------------
# Output JSON (clean)
# -------------------------------
cat(toJSON(list(value = value), auto_unbox = TRUE))
