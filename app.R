#########################################
# NFL Penalty Margin, Yard Margin,
# Home vs Away Analysis - Shiny App
#########################################

library(shiny)
library(dplyr)
library(ggplot2)
library(tidyr)
library(tibble)
library(nflreadr)
library(nflplotR)

# -----------------------------
# Safe data loading
# -----------------------------
safe_load <- function(expr, name) {
  tryCatch(expr, error = function(e) {
    message(sprintf("[WARN] Failed to load %s: %s", name, conditionMessage(e)))
    NULL
  })
}

empty_schedules <- tibble(
  game_id = character(), season = integer(), game_type = character(),
  home_team = character(), away_team = character(),
  home_score = numeric(), away_score = numeric()
)

empty_pbp <- tibble(
  game_id = character(), penalty = integer(),
  penalty_team = character(), penalty_yards = numeric()
)

seasons <- 2018:2023

schedules_raw <- safe_load(nflreadr::load_schedules(seasons), "schedules")
pbp_raw       <- safe_load(nflreadr::load_pbp(seasons), "pbp")

schedules <- if (is.null(schedules_raw)) empty_schedules else
  schedules_raw |>
  filter(!is.na(home_score), !is.na(away_score)) |>
  mutate(season = as.integer(season))

pbp <- if (is.null(pbp_raw)) empty_pbp else pbp_raw

# -----------------------------
# Team-game outcomes
# -----------------------------
games_long <- schedules |>
  mutate(orig_home = home_team, orig_away = away_team) |>
  select(game_id, season, game_type,
         home_team, away_team, home_score, away_score,
         orig_home, orig_away) |>
  pivot_longer(cols = c(home_team, away_team),
               names_to = "home_away", values_to = "team") |>
  mutate(
    opponent   = if_else(home_away == "home_team", orig_away, orig_home),
    is_home    = (home_away == "home_team"),
    team_score = if_else(is_home, home_score, away_score),
    opp_score  = if_else(is_home, away_score, home_score),
    win        = case_when(team_score > opp_score ~ 1,
                           team_score < opp_score ~ 0,
                           TRUE ~ NA_real_),
    phase      = if_else(game_type == "REG", "REG", "POST")
  ) |>
  select(game_id, season, game_type, phase, team, opponent, is_home,
         team_score, opp_score, win) |>
  mutate(season = as.integer(season),
         team   = as.character(team),
         opponent = as.character(opponent))

# -----------------------------
# Penalties (against each team)
# -----------------------------
penalties <- pbp |>
  filter(penalty == 1, !is.na(penalty_team)) |>
  select(game_id, penalty_team, penalty_yards)

penalties_against <- penalties |>
  group_by(game_id, penalty_team) |>
  summarise(
    penalties_against     = n(),
    penalty_yards_against = sum(penalty_yards, na.rm = TRUE),
    .groups = "drop"
  ) |>
  rename(team = penalty_team) |>
  mutate(team = as.character(team))

# -----------------------------
# Merge + margins
# -----------------------------
games_with_pens <- games_long |>
  left_join(penalties_against, by = c("game_id", "team")) |>
  left_join(
    penalties_against |>
      rename(opponent = team,
             penalties_on_opp     = penalties_against,
             penalty_yards_on_opp = penalty_yards_against),
    by = c("game_id", "opponent")
  ) |>
  mutate(
    penalties_against     = coalesce(penalties_against, 0L),
    penalties_on_opp      = coalesce(penalties_on_opp, 0L),
    penalty_yards_against = coalesce(penalty_yards_against, 0L),
    penalty_yards_on_opp  = coalesce(penalty_yards_on_opp, 0L),
    penalty_margin        = penalties_on_opp - penalties_against,
    yard_margin           = penalty_yards_on_opp - penalty_yards_against
  )

# -----------------------------
# Team-season aggregates
# -----------------------------
team_season_stats <- games_with_pens |>
  mutate(season = as.integer(season), team = as.character(team)) |>
  group_by(season, phase, team) |>
  summarise(
    games_played = sum(!is.na(win)),
    wins         = sum(win == 1, na.rm = TRUE),
    win_pct      = if_else(games_played > 0, wins / games_played, NA_real_),
    total_penalty_margin = sum(penalty_margin, na.rm = TRUE),
    avg_penalty_margin   = if_else(games_played > 0, total_penalty_margin / games_played, NA_real_),
    total_yard_margin = sum(yard_margin, na.rm = TRUE),
    avg_yard_margin   = if_else(games_played > 0, total_yard_margin / games_played, NA_real_),
    avg_penalty_margin_home = mean(penalty_margin[is_home], na.rm = TRUE),
    avg_penalty_margin_away = mean(penalty_margin[!is_home], na.rm = TRUE),
    avg_yard_margin_home    = mean(yard_margin[is_home], na.rm = TRUE),
    avg_yard_margin_away    = mean(yard_margin[!is_home], na.rm = TRUE),
    .groups = "drop"
  )

team_season_pen_rates <- games_with_pens |>
  mutate(season = as.integer(season), team = as.character(team)) |>
  group_by(season, phase, team) |>
  summarise(
    games_played  = sum(!is.na(win)),
    pens_against  = sum(penalties_against, na.rm = TRUE),
    yards_against = sum(penalty_yards_against, na.rm = TRUE),
    pens_pg       = if_else(games_played > 0, pens_against  / games_played, NA_real_),
    yards_pg      = if_else(games_played > 0, yards_against / games_played, NA_real_),
    .groups = "drop"
  )

# League average per season (for Trends overlay)
league_avg_pen <- team_season_pen_rates |>
  group_by(season, phase) |>
  summarise(league_pens_pg = mean(pens_pg, na.rm = TRUE), .groups = "drop")

season_choices <- sort(unique(team_season_stats$season))
if (length(season_choices) == 0) season_choices <- seasons

team_choices <- sort(unique(games_with_pens$team))

# -----------------------------
# UI
# -----------------------------
ui <- fluidPage(
  titlePanel("NFL Penalty and Yard Margin vs Winning Percentage"),
  sidebarLayout(
    sidebarPanel(
      selectInput("season", "Season:", choices = season_choices, selected = max(season_choices)),
      radioButtons("phase", "Game Type:",
                   choices = c("Regular Season" = "REG", "Playoffs" = "POST"),
                   selected = "REG"),
      radioButtons("homeaway", "Game Location:",
                   choices = c("Overall" = "overall", "Home Games Only" = "home", "Away Games Only" = "away"),
                   selected = "overall"),
      radioButtons("xmetric", "X-Axis Metric:",
                   choices = c("Penalty Margin" = "penalty", "Yard Margin" = "yard"),
                   selected = "penalty"),
      
      selectizeInput("team_filter", "Team (for Trends):",
                     choices = c("All teams" = "ALL", setNames(team_choices, team_choices)),
                     selected = "ALL",
                     options = list(placeholder = "Choose a team or keep All")),
      
      sliderInput("trend_seasons", "Trends seasons:",
                  min = min(season_choices), max = max(season_choices),
                  value = c(min(season_choices), max(season_choices)),
                  step = 1, sep = ""),
      
      checkboxInput("trend_show_reg", "Trends: show regression line", value = FALSE),
      checkboxInput("ttest_show_reg",  "T-Test: show regression line",  value = TRUE),
      
      helpText("Penalty margin = penalties on opponent - penalties on your team."),
      helpText("Yard margin = penalty yards on opponent - penalty yards against your team.")
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Plot", br(), plotOutput("scatter", height = "600px")),
        tabPanel("Summary Table", br(), tableOutput("summary_table")),
        tabPanel("Trends", br(),
                 plotOutput("trend_plot", height = "520px"),
                 br(),
                 tableOutput("trend_team_table")),
        tabPanel("Disproportionate",
                 br(),
                 numericInput("z_thresh", "Z-score threshold", value = 1.5, min = 0.5, max = 3, step = 0.1),
                 checkboxInput("only_flagged", "Show only flagged teams", value = TRUE),
                 br(),
                 tableOutput("z_table")),
        tabPanel("T-Test",
                 br(),
                 helpText("Tests whether better teams (higher win%) are flagged less often."),
                 plotOutput("ttest_plot", height = "450px"),
                 br(),
                 tableOutput("ttest_table"),
                 verbatimTextOutput("ttest_summary"))
      )
    )
  )
)

# -----------------------------
# SERVER
# -----------------------------
server <- function(input, output, session) {
  
  data_ok <- reactive({
    nrow(team_season_stats) > 0 && nrow(team_season_pen_rates) > 0
  })
  
  filtered_stats <- reactive({
    team_season_stats |>
      filter(season == input$season, phase == input$phase, games_played > 0)
  })
  
  # ---- Plot tab (scatter) ----
  output$scatter <- renderPlot({
    validate(need(data_ok(), "Data failed to load."))
    
    df <- filtered_stats()
    validate(need(nrow(df) > 0, "No games for this filter combination."))
    
    metric_overall <- if (input$xmetric == "penalty") df$avg_penalty_margin else df$avg_yard_margin
    metric_home    <- if (input$xmetric == "penalty") df$avg_penalty_margin_home else df$avg_yard_margin_home
    metric_away    <- if (input$xmetric == "penalty") df$avg_penalty_margin_away else df$avg_yard_margin_away
    
    df$x_plot <- case_when(
      input$homeaway == "overall" ~ metric_overall,
      input$homeaway == "home"    ~ metric_home,
      input$homeaway == "away"    ~ metric_away
    )
    
    ggplot(df, aes(x = x_plot, y = win_pct)) +
      nflplotR::geom_nfl_logos(aes(team_abbr = team), width = 0.065) +
      geom_hline(yintercept = 0.5, linetype = "dashed") +
      geom_vline(xintercept = 0,   linetype = "dashed") +
      labs(
        x = paste(ifelse(input$xmetric == "penalty", "Avg Penalty Margin", "Avg Yard Margin"),
                  " (", input$homeaway, ")", sep = ""),
        y = "Winning Percentage",
        title = paste0(ifelse(input$xmetric == "penalty", "Penalty Margin", "Yard Margin"),
                       " vs Win% - ", input$season, " (",
                       ifelse(input$phase == "REG", "Regular Season", "Playoffs"), ")")
      ) +
      theme_minimal(base_size = 16)
  })
  
  # ---- Summary table ----
  output$summary_table <- renderTable({
    validate(need(data_ok(), "Data failed to load."))
    filtered_stats() |>
      arrange(desc(win_pct)) |>
      select(team, games_played, wins, win_pct,
             avg_penalty_margin, avg_yard_margin,
             avg_penalty_margin_home, avg_penalty_margin_away,
             avg_yard_margin_home,    avg_yard_margin_away)
  }, digits = 3)
  
  # ---- Trends helpers ----
  trends_base <- reactive({
    validate(need(data_ok(), "Data failed to load."))
    
    team_season_pen_rates |>
      filter(
        phase == input$phase,
        between(as.integer(season),
                as.integer(input$trend_seasons[1]),
                as.integer(input$trend_seasons[2]))
      )
  })
  
  league_avg_slice <- reactive({
    league_avg_pen |>
      filter(
        phase == input$phase,
        between(as.integer(season),
                as.integer(input$trend_seasons[1]),
                as.integer(input$trend_seasons[2]))
      )
  })
  
  # ---- Trends plot (clean and readable) ----
  output$trend_plot <- renderPlot({
    df      <- trends_base()
    league  <- league_avg_slice()
    
    validate(need(nrow(df) > 0, "No penalty data available for this selection."))
    
    if (!is.null(input$team_filter) && input$team_filter != "ALL") {
      team_df <- df |> filter(team == as.character(input$team_filter))
      
      validate(need(nrow(team_df) > 0, "No rows for this team and season range."))
      
      p <- ggplot() +
        # league average
        geom_line(data = league,
                  aes(x = as.numeric(season), y = league_pens_pg),
                  size = 1.2, linetype = "dashed") +
        # selected team
        geom_line(data = team_df,
                  aes(x = as.numeric(season), y = pens_pg, group = team),
                  size = 1.3) +
        geom_point(data = team_df,
                   aes(x = as.numeric(season), y = pens_pg)) +
        nflplotR::geom_nfl_logos(data = team_df,
                                 aes(team_abbr = team, x = as.numeric(season), y = pens_pg),
                                 width = 0.07) +
        labs(
          title = paste0("Penalties per game over time - ", input$team_filter),
          x = "Season", y = "Avg penalties against per game",
          subtitle = "Solid line = selected team, dashed = league average"
        ) +
        scale_x_continuous(breaks = seq(input$trend_seasons[1], input$trend_seasons[2], by = 1)) +
        theme_minimal(base_size = 14)
      
      if (isTRUE(input$trend_show_reg)) {
        p <- p + geom_smooth(data = team_df,
                             aes(x = as.numeric(season), y = pens_pg),
                             method = "lm", se = FALSE, size = 1)
      }
      
      p
      
    } else {
      # All teams view: faint spaghetti + bold league average
      p <- ggplot() +
        geom_line(data = df,
                  aes(x = as.numeric(season), y = pens_pg, group = team),
                  size = 0.5, alpha = 0.25, color = "grey30") +
        geom_point(data = df,
                   aes(x = as.numeric(season), y = pens_pg, group = team),
                   size = 0.6, alpha = 0.25, color = "grey30") +
        geom_line(data = league,
                  aes(x = as.numeric(season), y = league_pens_pg),
                  size = 1.6) +
        geom_point(data = league,
                   aes(x = as.numeric(season), y = league_pens_pg),
                   size = 2) +
        labs(
          title = "Penalties per game over time (all teams)",
          x = "Season", y = "Avg penalties against per game",
          subtitle = "Bold line = league average. Team lines shown faintly for context."
        ) +
        scale_x_continuous(breaks = seq(input$trend_seasons[1], input$trend_seasons[2], by = 1)) +
        theme_minimal(base_size = 14)
      
      if (isTRUE(input$trend_show_reg)) {
        p <- p + geom_smooth(data = df,
                             aes(x = as.numeric(season), y = pens_pg, group = 1),
                             method = "lm", se = FALSE, size = 1,
                             inherit.aes = FALSE)
      }
      
      p
    }
  })
  
  # ---- Trends table ----
  output$trend_team_table <- renderTable({
    df <- trends_base()
    
    if (!is.null(input$team_filter) && input$team_filter != "ALL") {
      df <- df |> filter(team == as.character(input$team_filter))
    }
    
    validate(need(nrow(df) > 0, "No rows for this selection."))
    
    df |>
      arrange(season, team) |>
      select(season, team, pens_against, games_played, pens_pg, yards_against, yards_pg)
  }, digits = 3)
  
  # ---- Disproportionate (z-scores within season) ----
  z_data <- reactive({
    df <- team_season_pen_rates |>
      filter(season == input$season, phase == input$phase)
    
    mu  <- mean(df$pens_pg, na.rm = TRUE)
    sig <- sd(df$pens_pg, na.rm = TRUE)
    
    df <- df |>
      mutate(
        z_penalties = if (is.na(sig) || sig == 0) NA_real_ else (pens_pg - mu) / sig,
        flagged = !is.na(z_penalties) & z_penalties >= input$z_thresh
      )
    
    if (isTRUE(input$only_flagged)) df <- df |> filter(flagged)
    
    df |>
      arrange(desc(z_penalties)) |>
      select(team, games_played, pens_against, pens_pg, yards_against, yards_pg, z_penalties, flagged)
  })
  
  output$z_table <- renderTable({
    df <- z_data()
    validate(need(nrow(df) > 0, "No teams exceed the threshold for this selection."))
    df
  }, digits = 3)
  
  # ---- T-Test: better teams flagged less? ----
  tt_data <- reactive({
    left  <- team_season_stats     |> filter(season == input$season, phase == input$phase, games_played > 0)
    right <- team_season_pen_rates |> filter(season == input$season, phase == input$phase)
    inner_join(left, right, by = c("season", "phase", "team"))
  })
  
  output$ttest_table <- renderTable({
    df <- tt_data()
    validate(need(nrow(df) > 5, "Not enough data to run the test."))
    
    test <- cor.test(df$win_pct, df$pens_pg, alternative = "less", method = "pearson")
    
    tibble(
      `Correlation (r)` = round(unname(test$estimate), 3),
      `t statistic`     = round(unname(test$statistic), 3),
      `df`              = unname(test$parameter),
      `p-value`         = signif(test$p.value, 4),
      `Alternative`     = as.character(test$alternative)
    )
  })
  
  output$ttest_summary <- renderPrint({
    df <- tt_data()
    if (nrow(df) < 6) return(cat("Not enough data to run the test."))
    
    test <- cor.test(df$win_pct, df$pens_pg, alternative = "less", method = "pearson")
    
    cat("Interpretation:\n")
    if (test$p.value < 0.05) {
      cat("There is statistically significant evidence that higher win% is associated with fewer penalties per game.\n")
      cat("Correlation r =", round(unname(test$estimate), 3),
          " p =", signif(test$p.value, 4), "\n")
    } else {
      cat("No statistically significant relationship for this season and phase.\n")
      cat("Correlation r =", round(unname(test$estimate), 3),
          " p =", signif(test$p.value, 4), "\n")
    }
  })
  
  output$ttest_plot <- renderPlot({
    df <- tt_data()
    validate(need(nrow(df) > 5, "Not enough data to plot."))
    
    p <- ggplot(df, aes(x = pens_pg, y = win_pct)) +
      nflplotR::geom_nfl_logos(aes(team_abbr = team), width = 0.06) +
      labs(
        title = "Win percentage vs penalties per game",
        x = "Penalties against per game",
        y = "Winning percentage"
      ) +
      theme_minimal(base_size = 14)
    
    if (isTRUE(input$ttest_show_reg)) {
      p <- p + geom_smooth(method = "lm", se = TRUE)
    }
    
    p
  })
}

# -----------------------------
# Run app
# -----------------------------
shinyApp(ui = ui, server = server)
