#!/usr/bin/env Rscript

library(readr)
library(dplyr)
library(stringr)

# ============================================================
# Normalize to FIRST LAST (matches your restored backend)
# ============================================================
normalize_first_last <- function(x) {
    # Remove accents
    x <- iconv(x, from = "", to = "ASCII//TRANSLIT")

    # Remove punctuation
    x <- gsub("[,*#†+]", "", x)
    x <- gsub("\\.", "", x)
    x <- gsub("\\s+", " ", x)
    x <- trimws(x)

    # If "LAST FIRST" (all caps, two words)
    if (grepl("^[A-Z]+ [A-Z]+$", x)) {
        parts <- unlist(strsplit(x, " "))
        last  <- parts[1]
        first <- parts[2]
        return(paste(first, last))
    }

    # If "Last, First"
    if (grepl(",", x)) {
        parts <- unlist(strsplit(x, ","))
        last  <- trimws(parts[1])
        first <- trimws(parts[2])
        return(paste(first, last))
    }

    # Already "First Last"
    return(x)
}

# ============================================================
# Process all stathead_pitching_*.csv files
# ============================================================
files <- list.files(pattern = "^stathead_pitching_.*\\.csv$")

cat("Found", length(files), "CSV files\n")

for (f in files) {
    cat("Processing:", f, "\n")

    df <- read_csv(f, show_col_types = FALSE)

    # Detect Player column
    name_col <- names(df)[str_detect(names(df), regex("^Player$", ignore_case = TRUE))][1]

    if (is.na(name_col)) {
        cat("  ❌ No Player column found, skipping\n")
        next
    }

    # Normalize names to FIRST LAST
    df$NameClean <- sapply(df[[name_col]], normalize_first_last)

    # Write back to CSV
    write_csv(df, f)
    cat("  ✔ Updated:", f, "\n")
}

cat("\nAll CSVs converted to FIRST LAST.\n")
