library(tidyverse)
library(nbastatR)
library(nbaTools)
library(ggimage)
library(gt)
library(dplyr)
library(ggplot2)
library(ggrepel)
library(png)
library(paletteer)
install.packages("webshot2")
library(webshot2)
library(fs)

Sys.setenv("VROOM_CONNECTION_SIZE"= 131072 * 10)
bref_players_stats(seasons = c(1980:2023), tables = "totals", widen = TRUE, assign_to_environment = TRUE)
basic_stats <- dataBREFPlayerTotals %>%
  mutate(player = namePlayer, season = yearSeason + 1) %>%
  filter(countGames >= 41) %>% 
  group_by(slugSeason, season, idPlayerNBA) %>%
  summarize(player = namePlayer, slugPlayerBREF, team = slugTeamsBREF, ppg = ptsTotals/countGames, apg = astTotals/countGames, rpg = trbTotals/countGames, spg = stlTotals/countGames, bpg = blkTotals/countGames)

pbp <- game_logs(seasons = c(1980:2023))
wins <- pbp %>%
  mutate(outcome = ifelse(outcomeGame == "W", 1, 0)) %>%
  group_by(yearSeason, idPlayer) %>%
  filter(n() >= 41) %>% 
  summarize(win = sum(outcome))

basic_stats <- inner_join(basic_stats, wins, by=c('season'='yearSeason', 'idPlayerNBA'='idPlayer'))

seasons <- basic_stats %>%
  select(slugSeason, season, idPlayerNBA, slugPlayerBREF)

mvp_awards <- bref_awards(awards = "Most Valuable Player") 

mvp_awards[mvp_awards == "Nikola Jokić"] <- "Nikola Jokic"

mvp_awards <- left_join(seasons, mvp_awards, by = c("slugSeason", "slugPlayerBREF"))

mvp_awards <- mvp_awards %>%
  mutate(mvp = ifelse(is.na(slugAward), 0, 1)) 

basic_stats <- inner_join(basic_stats, mvp_awards, by = c("season", "idPlayerNBA"))

basic_stats <- basic_stats %>%
  select(player, season, team, ppg, apg, rpg, spg, bpg, win, mvp)

basic_stats_train <- basic_stats %>% filter(season != 2023)
basic_stats_test <- basic_stats %>% filter(season == 2023)
basic_stats_reg <- glm(mvp ~ ppg + apg + rpg + spg + bpg + win, data = basic_stats_train, family = binomial)

basic_stats_train <- basic_stats_train %>%
  ungroup() %>%
  mutate(prediction = predict(basic_stats_reg, basic_stats_train, type = "response")) %>%
  group_by(season) %>%
  mutate(mvp_prob = prediction/sum(prediction)) %>%
  mutate(mvp_won = ifelse(mvp == 1, "WON", "")) %>%
  ungroup() 

basic_stats_test <- basic_stats_test %>%
  ungroup() %>%
  mutate(prediction = predict(basic_stats_reg, basic_stats_test, type = "response")) %>%
  group_by(season) %>%
  mutate(mvp_prob = prediction/sum(prediction)) %>%
  mutate(mvp_won = ifelse(mvp == 1, "WON", "")) %>%
  ungroup() 
  
subfolder_path <- "basic/"
dir.create(subfolder_path, showWarnings = FALSE)

for (year in 1980:2022) {
  per_year <- basic_stats_train %>%
    filter(season == year) %>%
    mutate(mvp_prob = round(mvp_prob, 3)) %>%
    select(player, team, season, mvp_prob, mvp_won) %>%
    arrange(-mvp_prob) %>%
    filter(row_number() <= 6) %>%
    ungroup()
  table <- per_year %>% gt() %>% 
    cols_align(
      align = "center",
      columns = c(player, team, season, mvp_prob, mvp_won)
    ) %>%
    data_color(
      columns = mvp_prob,
      colors = scales::col_numeric(
        palette = paletteer::paletteer_d(
          palette = "ggsci::blue_material"
        ) %>% as.character(),
        domain = NULL
      )
    ) %>%
    cols_label(
      player = md("**Player**"),
      team = md("**Team**"),
      season = md("**Season**"),
      mvp_prob = md("**MVP Probability**"),
      mvp_won = md("**MVP Result**")
    ) 
  filename <- paste0(year, "basic.png")
  gtsave(table, file.path(subfolder_path, filename))
}


t_2023 <- basic_stats_test %>%
  mutate(mvp_prob = round(mvp_prob, 3)) %>%
  select(player, team, season, mvp_prob, mvp_won) %>%
  arrange(-mvp_prob) %>%
  filter(row_number() <= 6) %>%
  ungroup()
testtable <- t_2023 %>% gt() %>% 
  cols_align(
    align = "center",
    columns = c(player, team, season, mvp_prob, mvp_won)
  ) %>%
  data_color(
    columns = mvp_prob,
    colors = scales::col_numeric(
      palette = paletteer::paletteer_d(
        palette = "ggsci::blue_material"
      ) %>% as.character(),
      domain = NULL
    )
  ) %>%
  cols_label(
    player = md("**Player**"),
    team = md("**Team**"),
    season = md("**Season**"),
    mvp_prob = md("**MVP Probability**"),
    mvp_won = md("**MVP Result**")
  ) %>%
  tab_header(
    title = md("**2023 NBA MVP Probability**"),
    subtitle = "Based on NBA MVP Data from 1980 - 2022 Involving Basic Statistics"
  )
gtsave(testtable, "basic/2023basic.png")

  




