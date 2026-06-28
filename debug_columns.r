#!/usr/bin/env Rscript

library(readr)
library(stringr)

args <- commandArgs(trailingOnly = TRUE)
season <- args[1]

file_path <- sprintf("stathead_pitching_%s.csv", season)

df <- read_csv(file_path, show_col_types = FALSE)

clean_col <- function(x) {
    x <- iconv(x, from = "", to = "ASCII//TRANSLIT", sub = "")
    x <- gsub("[\u00A0\u200B\u200C\u200D\uFEFF]", "", x)  # NBSP, ZWSP, ZWNJ, ZWJ, BOM
    x <- gsub("[[:space:]]+", "", x)
    x <- gsub("[^A-Za-z0-9_]", "", x)
    x
}

cat("RAW COLUMN NAMES:\n")
for (n in names(df)) {
    cat(sprintf("[%s]  (hex: %s)\n",
        n,
        paste(sprintf("%02X", utf8ToInt(n)), collapse=" ")
    ))
}

cat("\nNORMALIZED COLUMN NAMES:\n")
norm <- clean_col(names(df))
for (n in norm) {
    cat(sprintf("[%s]  (hex: %s)\n",
        n,
        paste(sprintf("%02X", utf8ToInt(n)), collapse=" ")
    ))
}
