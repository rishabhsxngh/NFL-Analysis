# Precompute the small tables the app needs so it doesn't download
# six seasons of play-by-play every time it starts.
# Run once from the repo root:  Rscript scripts/build_data.R
# Source: nflverse (nflreadr::load_pbp / load_schedules), seasons 2018-2023.
suppressMessages({library(dplyr)})
seasons <- 2018:2023

get_rds <- function(url) {
  f <- tempfile(fileext = ".rds")
  download.file(url, f, mode = "wb", quiet = TRUE)
  readRDS(f)
}

if (requireNamespace("nflreadr", quietly = TRUE)) {
  pbp   <- nflreadr::load_pbp(seasons)
  games <- nflreadr::load_schedules(seasons)
} else {
  base  <- "https://github.com/nflverse/nflverse-data/releases/download"
  pbp   <- bind_rows(lapply(seasons, function(y) get_rds(sprintf("%s/pbp/play_by_play_%d.rds", base, y))))
  games <- get_rds(paste0(base, "/schedules/games.rds")) |> filter(season %in% seasons)
}

pen <- pbp |>
  filter(penalty == 1, !is.na(penalty_team)) |>
  group_by(game_id, penalty_team) |>
  summarise(penalties_against = n(),
            penalty_yards_against = sum(penalty_yards, na.rm = TRUE),
            .groups = "drop")

sched <- games |>
  select(game_id, season, game_type, home_team, away_team, home_score, away_score)

dir.create("data", showWarnings = FALSE)
write.csv(pen,   "data/penalties_by_game.csv", row.names = FALSE)
write.csv(sched, "data/schedules.csv",         row.names = FALSE)
cat("penalty rows:", nrow(pen), " games:", nrow(sched), "\n")
