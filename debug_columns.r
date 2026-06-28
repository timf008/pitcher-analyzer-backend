#!/usr/bin/env Rscript

library(readr)
library(jsonlite)

args <- commandArgs(trailingOnly = TRUE)
season <- args[1]

file_path <- sprintf("stathead_pitching_%s.csv", season)

df <- read_csv(file_path, show_col_types = FALSE)

raw <- names(df)

clean_col <- function(x) {
    x <- iconv(x, from = "", to = "ASCII//TRANSLIT", sub = "")
    x <- gsub("[\u00A0\u200B\u200C\u200D\uFEFF]", "", x)
    x <- gsub("[[:space:]]+", "", x)
    x <- gsub("[^A-Za-z0-9_]", "", x)
    x
}

norm <- clean_col(raw)

cat(toJSON(list(raw = raw, normalized = norm), auto_unbox = TRUE))
