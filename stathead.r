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
# Load CSV
# ============================================================
file_path <- file.path(
    getwd(),
    sprintf("stathead_pitching_%s.csv", season)
)

if (!file.exists(file_path)) {
    cat(
        toJSON(
            list(
                error = paste(
                    "CSV not found:",
                    file_path
                )
            ),
            auto_unbox = TRUE
        )
    )

    quit(status = 1)
}

df <- read_csv(
    file_path,
    show_col_types = FALSE
)

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
name_col <- names(df)[
    str_detect(
        names(df),
        regex(
            "^Player$",
            ignore_case = TRUE
        )
    )
][1]

if (is.na(name_col)) {
    cat(
        toJSON(
            list(
                error = "No Player column found"
            ),
            auto_unbox = TRUE
        )
    )

    quit(status = 1)
}

# ============================================================
# Normalize CSV names
# ============================================================
df$NameClean <- sapply(
    df[[name_col]],
    normalize_name
)

# ============================================================
# Clean Season column
# ============================================================
df$Season <- as.numeric(
    gsub(
        "[^0-9]",
        "",
        as.character(df$Season)
    )
)

# ============================================================
# Safe column helper
# ============================================================
get_col <- function(pattern) {
    cols <- names(df)[
        str_detect(
            names(df),
            pattern
        )
    ]

    if (length(cols) == 0) {
        return(NA_character_)
    }

    cols[1]
}

# ============================================================
# Detect SO / BB / Team columns
#
# Stathead may create duplicate strikeout columns such as:
# SO3, SO25, SO_pitch, etc.
#
# Prefer SO_pitch when available because it is explicitly
# the pitching strikeout total.
# ============================================================

if ("SO_pitch" %in% names(df)) {

    so_col <- "SO_pitch"

} else {

    so_candidates <- names(df)[
        str_detect(
            names(df),
            "^SO[0-9]*$"
        )
    ]

    if (length(so_candidates) > 0) {
        so_col <- so_candidates[1]
    } else {
        so_col <- NA_character_
    }
}

bb_col   <- get_col("^BB$")
team_col <- get_col("^Team$")

# ============================================================
# Player Browser Mode
# ============================================================

if (player_name == "__LIST__") {

    format_browser_name <- function(x) {

    name <- str_to_title(x)

    # Restore two-letter initials: JT, CJ, AJ, etc.
    name <- str_replace_all(
        name,
        "\\b([A-Za-z])([A-Za-z])\\b",
        function(m) toupper(m)
    )

    # Restore name suffixes
    name <- str_replace_all(name, "\\bJR\\b", "Jr")
    name <- str_replace_all(name, "\\bSR\\b", "Sr")

    return(name)
}

    players <- df %>%
        transmute(
            Player = sapply(
                NameClean,
                format_browser_name,
                USE.NAMES = FALSE
            ),
            Team = if (!is.na(team_col))
                as.character(.data[[team_col]])
            else
                NA_character_
        ) %>%
        filter(!is.na(Player), Player != "") %>%
        arrange(Player)

    cat(
        toJSON(
            players,
            pretty = TRUE,
            auto_unbox = TRUE
        )
    )

    quit(status = 0)
}

# Season Production columns
h_col  <- get_col("^H$")
r_col  <- get_col("^R$")
er_col <- get_col("^ER$")
hr_col <- get_col("^HR$")
ip_col <- get_col("^IP$")

if (is.na(so_col) || is.na(bb_col)) {
    cat(
        toJSON(
            list(
                error = "SO or BB column not found"
            ),
            auto_unbox = TRUE
        )
    )

    quit(status = 1)
}

# ============================================================
# K% and BB%
# ============================================================
if ("BF" %in% names(df)) {

    df$Kpct <-
        (df[[so_col]] / df$BF) * 100

    df$BBpct <-
        (df[[bb_col]] / df$BF) * 100

} else if (
    all(
        c(
            "IP",
            "H",
            "BB",
            "HBP"
        ) %in% names(df)
    )
) {

    est_BF <-
        (df$IP * 3) +
        df$H +
        df$BB +
        df$HBP

    df$Kpct <-
        (df[[so_col]] / est_BF) * 100

    df$BBpct <-
        (df[[bb_col]] / est_BF) * 100

} else if (
    all(
        c(
            "SO9",
            "BB9"
        ) %in% names(df)
    )
) {

    df$Kpct <-
        (df$SO9 / 27) * 100

    df$BBpct <-
        (df$BB9 / 27) * 100

} else {

    df$Kpct <- NA_real_
    df$BBpct <- NA_real_
}

# ============================================================
# Compute K/BB if missing
# ============================================================
if (!"SO_BB" %in% names(df)) {

    df$SO_BB <-
        df[[so_col]] /
        df[[bb_col]]
}

# ============================================================
# Percentile helper
# ============================================================
percentile <- function(
    x,
    higher_is_better = TRUE
) {

    valid <- !is.na(x)

    if (higher_is_better) {

        return(
            rank(
                x,
                na.last = "keep"
            ) /
            sum(valid) *
            100
        )

    } else {

        return(
            rank(
                -x,
                na.last = "keep"
            ) /
            sum(valid) *
            100
        )
    }
}

# ============================================================
# Backend Overall Score
# ============================================================
score_era <- function(era) {

    pmin(
        pmax(
            10 *
            (5.00 - era) /
            (5.00 - 2.00),
            0
        ),
        10
    )
}

score_whip <- function(whip) {

    pmin(
        pmax(
            10 *
            (1.40 - whip) /
            (1.40 - 0.90),
            0
        ),
        10
    )
}

score_kpct <- function(kpct) {

    pmin(
        pmax(
            10 *
            (kpct - 15) /
            (35 - 15),
            0
        ),
        10
    )
}

score_bbpct <- function(bbpct) {

    pmin(
        pmax(
            10 *
            (10 - bbpct) /
            (10 - 3),
            0
        ),
        10
    )
}

score_kbb <- function(kbb) {

    pmin(
        pmax(
            10 *
            (kbb - 1.5) /
            (6.0 - 1.5),
            0
        ),
        10
    )
}

compute_overall <- function(
    era,
    whip,
    kpct,
    bbpct,
    kbb
) {

    score_era(era)      * 0.25 +
    score_whip(whip)    * 0.25 +
    score_kpct(kpct)    * 0.1875 +
    score_bbpct(bbpct)  * 0.125 +
    score_kbb(kbb)      * 0.1875
}

df$OverallScore <- compute_overall(
    df$ERA,
    df$WHIP,
    df$Kpct,
    df$BBpct,
    df$SO_BB
)

# ============================================================
# Component Scores
# ============================================================
df$ERA_score <-
    score_era(df$ERA)

df$WHIP_score <-
    score_whip(df$WHIP)

df$Kpct_score <-
    score_kpct(df$Kpct)

df$BBpct_score <-
    score_bbpct(df$BBpct)

df$KBB_score <-
    score_kbb(df$SO_BB)

# ============================================================
# Pitcher Archetype
#
# Classifies profile SHAPE rather than overall quality.
#
# Each pitcher's five component scores are centered around
# that pitcher's own mean score before measuring Euclidean
# distance to the fixed archetype landmarks.
# ============================================================

pitcher_archetype_landmarks <- list(

    "Strikeout / Variable" = c(
        ERA  = -0.60,
        WHIP = -0.87,
        Kpct =  2.58,
        BBpct = -1.17,
        KBB  =  0.05
    ),

    "Run Prevention / Balanced" = c(
        ERA  =  1.01,
        WHIP =  0.43,
        Kpct = -0.97,
        BBpct =  0.00,
        KBB  = -0.47
    ),

    "Power / Wildness" = c(
        ERA  =  3.06,
        WHIP =  0.43,
        Kpct =  1.26,
        BBpct = -3.16,
        KBB  = -1.59
    ),

    "Command / Contact" = c(
        ERA  = -1.57,
        WHIP = -0.99,
        Kpct = -0.74,
        BBpct =  2.26,
        KBB  =  1.04
    )
)


classify_pitcher_archetype <- function(
    era_score,
    whip_score,
    kpct_score,
    bbpct_score,
    kbb_score
) {

    scores <- c(
        ERA  = era_score,
        WHIP = whip_score,
        Kpct = kpct_score,
        BBpct = bbpct_score,
        KBB  = kbb_score
    )

    # Archetype requires a complete finite profile
    if (!all(is.finite(scores))) {

        return(c(
            Archetype = NA_character_,
            ArchetypeMatch = NA_character_,
            ArchetypeStrength = NA_character_
        ))
    }

    # --------------------------------------------------------
    # Remove overall level.
    #
    # This preserves profile SHAPE while preventing archetype
    # from becoming another measure of pitcher quality.
    # --------------------------------------------------------

    centered_scores <-
        scores - mean(scores)


    # --------------------------------------------------------
    # Euclidean distance from centered profile to each
    # fixed pitcher archetype landmark.
    # --------------------------------------------------------

    distances <- sapply(
        pitcher_archetype_landmarks,
        function(landmark) {

            sqrt(
                sum(
                    (centered_scores - landmark)^2
                )
            )
        }
    )


    ranked <- sort(distances)

    nearest_distance <-
        unname(ranked[1])

    second_distance <-
        unname(ranked[2])

    archetype <-
        names(ranked)[1]


    # --------------------------------------------------------
    # Match Strength
    #
    # 0 = essentially on a boundary between landmarks
    # Higher values = clearer separation from second choice
    # --------------------------------------------------------

    if (
        !is.finite(nearest_distance) ||
        !is.finite(second_distance) ||
        second_distance <= 0
    ) {

        match_strength <- NA_real_

    } else {

        match_strength <-
            1 -
            (
                nearest_distance /
                second_distance
            )
    }


    # --------------------------------------------------------
    # Match classification
    # --------------------------------------------------------

    if (is.na(match_strength)) {

        match_label <- NA_character_

    } else if (match_strength >= 0.50) {

        match_label <- "Strong Match"

    } else if (match_strength >= 0.30) {

        match_label <- "Moderate Match"

    } else {

        match_label <- "Weak Match"
    }


    return(
        c(
            Archetype =
                unname(archetype),

            ArchetypeMatch =
                unname(match_label),

            ArchetypeStrength =
                ifelse(
                    is.na(match_strength),
                    NA_character_,
                    sprintf(
                        "%.3f",
                        unname(match_strength)
                    )
                )
        )
    )
}


# ============================================================
# Classify all pitchers
# ============================================================

pitcher_archetype_results <- lapply(
    seq_len(nrow(df)),
    function(i) {

        classify_pitcher_archetype(
            df$ERA_score[i],
            df$WHIP_score[i],
            df$Kpct_score[i],
            df$BBpct_score[i],
            df$KBB_score[i]
        )
    }
)


df$Archetype <- vapply(
    pitcher_archetype_results,
    function(x) x[["Archetype"]],
    character(1)
)

df$ArchetypeMatch <- vapply(
    pitcher_archetype_results,
    function(x) x[["ArchetypeMatch"]],
    character(1)
)

df$ArchetypeStrength <- as.numeric(
    vapply(
        pitcher_archetype_results,
        function(x) x[["ArchetypeStrength"]],
        character(1)
    )
)

# ============================================================
# Cross-Sectional Expected Overall
#
# Uses the current season population.
# Each pitcher is compared with the 10 closest
# complete profiles using the five component scores.
# ============================================================

profile_cols <- c(
    "ERA_score",
    "WHIP_score",
    "Kpct_score",
    "BBpct_score",
    "KBB_score"
)

# ============================================================
# Convert profile data to a numeric matrix
# ============================================================

profile_data <- as.matrix(
    df[, profile_cols]
)

storage.mode(profile_data) <- "numeric"

# ============================================================
# Only use completely finite profiles
#
# complete.cases() alone does not reject Inf/-Inf.
# ============================================================

valid_profiles <- apply(
    profile_data,
    1,
    function(x) {
        all(is.finite(x))
    }
) & is.finite(df$OverallScore)

profile_indices <- which(
    valid_profiles
)

# Need at least two pitchers to create peers
if (length(profile_indices) < 2) {

    df$ExpectedOverall <- NA_real_

} else {

    # ========================================================
    # Standardize profiles
    # ========================================================

    profile_matrix <- scale(
        profile_data[
            valid_profiles,
            ,
            drop = FALSE
        ]
    )

    # ========================================================
    # Calculate all pairwise distances ONCE
    # ========================================================

    distance_matrix <- as.matrix(
        dist(profile_matrix)
    )

    # Prevent each pitcher from selecting himself
    diag(distance_matrix) <- Inf

# ============================================================
# Similar Profiles Distance Matrix
#
# Uses the raw 0-10 component scores rather than standardized
# scores. This preserves the original five-metric profile
# geometry for pitcher-to-pitcher similarity.
# ============================================================

similarity_matrix <- profile_data[
    valid_profiles,
    ,
    drop = FALSE
]

similarity_distance_matrix <- as.matrix(
    dist(similarity_matrix)
)

# Prevent each pitcher from matching himself
diag(similarity_distance_matrix) <- Inf

    # ========================================================
    # Expected Overall from 10 nearest neighbors
    # ========================================================

    k <- 10

    expected_overall_valid <- apply(
        distance_matrix,
        1,
        function(d) {

            finite <- which(
                is.finite(d)
            )

            if (length(finite) == 0) {
                return(NA_real_)
            }

            neighbors <- finite[
                order(
                    d[finite]
                )[
                    1:min(
                        k,
                        length(finite)
                    )
                ]
            ]

            neighbor_indices <-
                profile_indices[
                    neighbors
                ]

            mean(
                df$OverallScore[
                    neighbor_indices
                ],
                na.rm = TRUE
            )
        }
    )

    # ========================================================
    # Store result back into full dataframe
    # ========================================================

    df$ExpectedOverall <- NA_real_

    df$ExpectedOverall[
        profile_indices
    ] <- expected_overall_valid
}

# ============================================================
# Overall Divergence
#
# Positive = Actual Overall is above comparable expectation
# Negative = Actual Overall is below comparable expectation
# ============================================================

df$OverallDivergence <-
    df$OverallScore -
    df$ExpectedOverall

# ============================================================
# Overall Divergence Standard Deviation
# ============================================================

overall_divergence_sd <- sd(
    df$OverallDivergence,
    na.rm = TRUE
)

# ============================================================
# Compute Overall Percentile
# ============================================================

df$Overall_pct <- percentile(
    df$OverallScore,
    higher_is_better = TRUE
)

# ============================================================
# Pitcher XP Score
# Strikeout / Command Performance
# ============================================================

compute_pitcher_xp <- function(
    kpct,
    kbb,
    bbpct
) {

    xp <-
        (kpct * 4) +
        (kbb * 2) -
        (bbpct * 10)

    return(
        xp + 1000
    )
}

df$XP <- compute_pitcher_xp(
    df$Kpct,
    df$SO_BB,
    df$BBpct
)

# ============================================================
# Filter for player + season
# ============================================================

p <- df %>%
    filter(
        NameClean == player_name_clean,
        Season == as.numeric(season)
    )

if (nrow(p) == 0) {

    cat(
        toJSON(
            list(
                error = "Player not found"
            ),
            auto_unbox = TRUE
        )
    )

    quit(status = 1)
}

# ============================================================
# Find 3 Nearest Similar Profiles
# ============================================================

selected_df_index <- which(
    df$NameClean == player_name_clean &
    df$Season == as.numeric(season)
)[1]

selected_profile_position <- match(
    selected_df_index,
    profile_indices
)

similar_profiles <- list()

if (!is.na(selected_profile_position)) {

    distances <- similarity_distance_matrix[
        selected_profile_position,
    ]

    finite <- which(
        is.finite(distances)
    )

    if (length(finite) > 0) {

        nearest_positions <- finite[
            order(distances[finite])[
                1:min(3, length(finite))
            ]
        ]

        nearest_df_indices <-
            profile_indices[nearest_positions]

        similar_profiles <- lapply(
            seq_along(nearest_df_indices),
            function(i) {

                idx <- nearest_df_indices[i]

                list(
                    Player = str_to_title(
                             df$NameClean[idx]
                    ),

                    Team = if (!is.na(team_col))
                        as.character(df[[team_col]][idx])
                    else
                        NA_character_,

                    Overall = as.numeric(
                        df$OverallScore[idx]
                    ),

                    XP = as.numeric(
                        df$XP[idx]
                    ),

                    BF = if ("BF" %in% names(df))
                        as.numeric(df$BF[idx])
                    else
                        NA_real_,

                    Distance = as.numeric(
                        distances[nearest_positions[i]]
                    )
                )
            }
        )
    }
}

# ============================================================
# Build JSON output
# ============================================================

result <- p %>%
    transmute(

        ERA =
            as.numeric(ERA),

        WHIP =
            as.numeric(WHIP),

        Kpct =
            as.numeric(Kpct),

        BBpct =
            as.numeric(BBpct),

        KBB =
            as.numeric(SO_BB),

        ERA_score =
            as.numeric(ERA_score),

        WHIP_score =
            as.numeric(WHIP_score),

        Kpct_score =
            as.numeric(Kpct_score),

        BBpct_score =
            as.numeric(BBpct_score),

        KBB_score =
            as.numeric(KBB_score),

Archetype =
    as.character(Archetype),

ArchetypeMatch =
    as.character(ArchetypeMatch),

ArchetypeStrength =
    as.numeric(ArchetypeStrength),

        Overall =
            as.numeric(OverallScore),

        ExpectedOverall =
            as.numeric(ExpectedOverall),

        OverallDivergence =
            as.numeric(OverallDivergence),

        OverallDivergenceSD =
            as.numeric(overall_divergence_sd),

        Overall_pct =
            as.numeric(Overall_pct),

        XP =
            as.numeric(XP),

        Team =
            if (!is.na(team_col))
                as.character(
                    .data[[team_col]]
                )
            else
                NA_character_,

# ========================================================
# Season Production
# ========================================================

IP =
    if (!is.na(ip_col))
        as.numeric(.data[[ip_col]])
    else
        NA_real_,

H =
    if (!is.na(h_col))
        as.numeric(.data[[h_col]])
    else
        NA_real_,

R =
    if (!is.na(r_col))
        as.numeric(.data[[r_col]])
    else
        NA_real_,

ER =
    if (!is.na(er_col))
        as.numeric(.data[[er_col]])
    else
        NA_real_,

BB =
    if (!is.na(bb_col))
        as.numeric(.data[[bb_col]])
    else
        NA_real_,

SO =
    if (!is.na(so_col))
        as.numeric(.data[[so_col]])
    else
        NA_real_,

HR =
    if (!is.na(hr_col))
        as.numeric(.data[[hr_col]])
    else
        NA_real_,

HR9 =
    as.numeric(HR9),

        FIP =
            as.numeric(FIP),

        W =
            as.numeric(W),

        L =
            as.numeric(L),

        GS =
            as.numeric(GS)
    )

# ============================================================
# JSON output
# ============================================================

result$SimilarProfiles <- list(
    similar_profiles
)

cat(
    toJSON(
        result,
        pretty = TRUE,
        auto_unbox = TRUE,
        na = "null"
    )
)
