library(readr)
library(jsonlite)
library(stringr)

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

# -------------------------------
# Load CSV
# -------------------------------
file_path <- sprintf("stathead_pitching_%s.csv", season)
df <- read_csv(file_path, show_col_types = FALSE)

# Normalize names
name_col <- names(df)[str_detect(names(df), regex("^Player$", ignore_case = TRUE))][1]
df$NameClean <- clean(df[[name_col]])
nameClean <- clean(name)

# Ensure Season numeric
df$Season <- suppressWarnings(as.numeric(df$Season))

# -------------------------------
# Detect SO and BB columns (patched)
# -------------------------------
# SO: choose the LAST "SO" column (real strikeouts)
so_cols <- names(df)[str_detect(names(df), "^SO$")]
so_col <- so_cols[length(so_cols)]

# BB: choose the LAST "BB" column (not IBB)
bb_cols <- names(df)[str_detect(names(df), "^BB$")]
bb_col <- bb_cols[length(bb_cols)]

# -------------------------------
# Match row
# -------------------------------
row <- df[df$NameClean == nameClean & df$Season == season, ]

if (nrow(row) == 0) {
    cat(toJSON(list(value = NULL), auto_unbox = TRUE))
    quit()
}

# -------------------------------
# Compute derived stats
# -------------------------------
if (stat == "Kpct") {

    if ("BF" %in% names(row) && !is.na(row$BF)) {
        value <- (row[[so_col]] / row$BF) * 100
    } else {
        value <- NA
    }

} else if (stat == "BBpct") {

    if ("BF" %in% names(row) && !is.na(row$BF)) {
        value <- (row[[bb_col]] / row$BF) * 100
    } else {
        value <- NA
    }

} else if (stat == "KBB") {

    value <- row[[so_col]] / row[[bb_col]]

} else {

    # Normal stat from CSV
    value <- row[[stat]]
}

# -------------------------------
# Normalize value
# -------------------------------
value <- unlist(value)

if (is.null(value) || length(value) == 0) {
    value <- NA
}

value <- suppressWarnings(as.numeric(value))

if (is.null(value) || length(value) == 0 || is.na(value)) {
    value <- NA
}

# -------------------------------
# Output JSON
# -------------------------------
cat(toJSON(list(value = value), auto_unbox = TRUE))


