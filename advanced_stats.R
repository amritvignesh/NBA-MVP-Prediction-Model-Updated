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
library(webshot2)
library(fs)

Sys.setenv("VROOM_CONNECTION_SIZE"= 131072 * 10)
bref_players_stats(seasons = c(1980:2023), tables = "advanced", widen = TRUE, assign_to_environment = TRUE)
advanced_stats <- dataBREFPlayerAdvanced %>%
  mutate(player = namePlayer, season = yearSeason + 1) %>%
  filter(countGames >= 41) %>% 
  group_by(slugSeason, season, idPlayerNBA) %>%
  summarize(player = namePlayer, slugPlayerBREF, team = slugTeamsBREF, per = ratioPER, ws48 = ratioWSPer48, bpm = ratioBPM, vorp = ratioVORP)

advanced_stats <- advanced_stats %>%
  select(player, season, team, per, ws48, bpm, vorp)

advanced_stats <- inner_join(advanced_stats, mvp_awards, by = c("season", "idPlayerNBA"))

advanced_stats_train <- advanced_stats %>% filter(season != 2023)
advanced_stats_test <- advanced_stats %>% filter(season == 2023)
advanced_stats_reg <- glm(mvp ~ per + ws48 + bpm + vorp, data = advanced_stats_train, family = binomial)
summary(advanced_stats_reg)

advanced_stats_train <- advanced_stats_train %>%
  ungroup() %>%
  mutate(prediction = predict(advanced_stats_reg, advanced_stats_train, type = "response")) %>%
  group_by(season) %>%
  mutate(mvp_prob = prediction/sum(prediction)) %>%
  mutate(mvp_won = ifelse(mvp == 1, "WON", "")) %>%
  ungroup() 

advanced_stats_test <- advanced_stats_test %>%
  ungroup() %>%
  mutate(prediction = predict(advanced_stats_reg, advanced_stats_test, type = "response")) %>%
  group_by(season) %>%
  mutate(mvp_prob = prediction/sum(prediction)) %>%
  mutate(mvp_won = ifelse(mvp == 1, "WON", "")) %>%
  ungroup() 

subfolder_path_2 <- "advanced/"
dir.create(subfolder_path_2, showWarnings = FALSE)

for (year in 1980:2022) {
  per_year <- advanced_stats_train %>%
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
  filename <- paste0(year, "advanced.png")
  gtsave(table, file.path(subfolder_path_2, filename))
}


t_2023_2 <- advanced_stats_test %>%
  mutate(mvp_prob = round(mvp_prob, 3)) %>%
  select(player, team, season, mvp_prob, mvp_won) %>%
  arrange(-mvp_prob) %>%
  filter(row_number() <= 6) %>%
  ungroup()
testtable_2 <- t_2023_2 %>% gt() %>% 
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
    subtitle = "Based on NBA MVP Data from 1980 - 2022 Involving Advanced Statistics"
  )
gtsave(testtable_2, "advanced/2023advanced.png")




