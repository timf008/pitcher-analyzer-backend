#!/usr/bin/env Rscript

library(dplyr)
library(jsonlite)
library(stringr)
library(stringi)

args <- commandArgs(trailingOnly = TRUE)
season <- args[1]

file_path <- file.path(getwd(), sprintf("stathead_pitching_%s.csv", season))

df <- read.csv(file_path, stringsAsFactors = FALSE)

# ============================================================
# Normalize column names safely
# ============================================================
names(df) <- names(df) |>
  str_replace_all("%", "pct") |>
  str_replace_all("/", "_") |>
  str_replace_all("\\.", "") |>
  str_replace_all(" ", "_")

names(df) <- make.unique(names(df), sep = "_")

# Detect SO/BB column
ratio_col <- names(df)[str_detect(names(df), regex("^so_?bb$", ignore_case = TRUE))]
if (length(ratio_col) == 1) {
    names(df)[names(df) == ratio_col] <- "SO_BB"
} else {
    stop("Could not identify SO/BB column after cleaning.")
}

# ============================================================
# Clamp helper
# ============================================================
clamp <- function(x, min_val = 0, max_val = 10) {
  pmax(min_val, pmin(max_val, x))
}

# ============================================================
# Scoring functions (Pitcher 5‑metric model)
# ============================================================
scoreERA <- function(era) clamp(10 * (5.00 - era) / (5.00 - 2.00))
scoreWHIP <- function(whip) clamp(10 * (1.40 - whip) / (1.40 - 0.90))
scoreKpct <- function(kpct) clamp(10 * (kpct - 15) / (35 - 15))
scoreBBpct <- function(bbpct) clamp(10 * (10 - bbpct) / (10 - 3))
scoreKBB <- function(kbb) clamp(10 * (kbb - 1.5) / (6.0 - 1.5))

# ============================================================
# Weighted Overall Score
# ============================================================
computeWeightedOverallPitcher <- function(eraScore, whipScore, kpctScore, bbpctScore, kbbScore) {
  (eraScore  * 0.25 +
   whipScore * 0.25 +
   kpctScore * 0.1875 +
   bbpctScore* 0.125 +
   kbbScore  * 0.1875)
}

# ============================================================
# Compute pitcher metrics
# ============================================================
df <- df %>%
  mutate(
    Kpct  = round((SO / BF) * 100, 1),
    BBpct = round((BB / BF) * 100, 1),
    KBB   = round(SO_BB, 2),
    ERA   = round(ERA, 2),
    WHIP  = round(WHIP, 3),

    eraScore   = scoreERA(ERA),
    whipScore  = scoreWHIP(WHIP),
    kpctScore  = scoreKpct(Kpct),
    bbpctScore = scoreBBpct(BBpct),
    kbbScore   = scoreKBB(KBB),

    overall = round(
      computeWeightedOverallPitcher(
        eraScore, whipScore, kpctScore, bbpctScore, kbbScore
      ),
      1
    ),

    XP = round(
          (Kpct * 2) +
          (KBB  * 10) -
          (ERA  * 15) -
          (WHIP * 40) -
          (BBpct * 10) +
          1000
        )
  )

# ============================================================
# League Averages (GS > 5)
# ============================================================
league_avgs <- df %>%
  filter(GS > 5) %>%
  summarise(
    league_avg_XP      = mean(XP, na.rm = TRUE),
    league_avg_overall = mean(overall, na.rm = TRUE)
  )

# Output JSON
cat(toJSON(as.list(league_avgs), pretty = FALSE, auto_unbox = TRUE))
