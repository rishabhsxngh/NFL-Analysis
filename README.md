# NFL penalty margin and win analysis

R Shiny app comparing penalty margin and penalty-yard margin with win percentage, 2018 to 2023. It splits results by regular season and playoffs and by home and away games, shows trends by team, and flags outlier teams by z-score.

Real data from [nflverse](https://nflverse.com).

## Run it

```r
install.packages(c("shiny", "dplyr", "ggplot2", "tidyr", "tibble", "nflplotR"))
shiny::runApp()
```

The app reads two small precomputed tables in `data/`, so it starts quickly and needs no network access for the data.

## Rebuild the data

`scripts/build_data.R` regenerates `data/` from nflverse play-by-play and schedules (needs `nflreadr`, or downloads the same files from the nflverse GitHub releases):

```
Rscript scripts/build_data.R
```

- Penalty margin = penalties on the opponent minus penalties on your team.
- Yard margin = penalty yards on the opponent minus penalty yards against your team.
