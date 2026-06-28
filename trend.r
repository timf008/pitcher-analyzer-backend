#!/usr/bin/env Rscript

library(readr)
library(jsonlite)
library(stringr)
library(dplyr)

args <- commandArgs(trailingOnly = TRUE)
name <- args[1]
stat <- args[2]
season <- as.numeric(args[3])

# ============================================================
# Name Normalization (same as stathead.r)
# ============================================================
normalize_name <- function(x) {
    x <- iconv(x, from = "UTF-8", to = "ASCII//TRANSLIT", sub = "")
    x <- gsub("[,*#†+]", "", x)
    x <- gsub("\\.", "", x)
    x <- gsub("\\s+", " ", x)
    x <- trimws(x)

    if (grepl(",", x)) {
        parts <- unlist(strsplit(x, ","))
        last  <- trimws(parts[1])
        first <- trimws(parts[2])
        return(toupper(paste(last, first)))
    }

    parts <- unlist(strsplit(x, " "))
    if (length(parts) == 2) {
        first <- parts[1]
        last  <- parts[2]
        return(toupper(paste(last, first)))
    }

    return(toupper(x))
}

nameClean <- normalize_name(name)

# ============================================================
# Load CSV (ABSOLUTE PATH)
# ============================================================
file_path <- file.path(getwd(), sprintf("stathead_pitching_%s.csv", season))

if (!file.exists(file_path)) {
    cat(toJSON(list(value = NULL), auto_unbox = TRUE))
    quit(status = 1)
}

df <- suppressWarnings(read_csv(file_path, show_col_types = FALSE))

# Normalize column names like stathead.r
names(df) <- names(df) |>
  str_replace_all("%", "pct") |>
  str_replace_all("/", "_") |>
  str_replace_all("\\.", "") |>
  str_replace_all(" ", "_")

# Player column
name_col <- names(df)[str_detect(names(df), regex("^Player$", ignore_case = TRUE))][1]

if (is.na(name_col)) {
    cat(toJSON(list(value = NULL), auto_unbox = TRUE))
    quit(status = 1)
}

df$NameClean <- sapply(df[[name_col]], normalize_name)

# Season numeric
df$Season <- as.numeric(gsub("[^0-9]", "", as.character(df$Season)))

# SO / BB
so_cols <- names(df)[str_detect(names(df), "^SO")]
bb_cols <- names(df)[str_detect(names(df), "^BB$")]

if (length(so_cols) == 0 || length(bb_cols) == 0) {
    cat(toJSON(list(value = NULL), auto_unbox = TRUE))
    quit(status = 1)
}

so_col <- so_cols[length(so_cols)]
bb_col <- bb_cols[length(bb_cols)]

# Match row
row <- df[df$NameClean == nameClean & df$Season == season, ]

if (nrow(row) == 0) {
    cat(toJSON(list(value = NULL), auto_unbox = TRUE))
    quit(status = 1)
}

safenum <- function(x) suppressWarnings(as.numeric(x))

# Derived stats
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

value <- safenum(value)
if (is.null(value) || length(value) == 0 || is.na(value)) value <- NA

cat(toJSON(list(value = value), auto_unbox = TRUE))

