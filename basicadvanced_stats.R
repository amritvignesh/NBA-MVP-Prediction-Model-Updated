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

basicadvanced_stats_train <- inner_join(basic_stats_train, advanced_stats_train, by = c("idPlayerNBA", "season")) %>%
  select(player = player.x, season, team = team.x, ppg, apg, rpg, spg, bpg, win, per, ws48, bpm, vorp, mvp = mvp.x)
basicadvanced_stats_test <- inner_join(basic_stats_test, advanced_stats_test, by = c("idPlayerNBA", "season")) %>%
  select(player = player.x, season, team = team.x, ppg, apg, rpg, spg, bpg, win, per, ws48, bpm, vorp, mvp = mvp.x)

basicadvanced_stats_reg <- glm(mvp ~ ppg + apg + rpg + spg + bpg + win + per + ws48 + bpm + vorp, data = basicadvanced_stats_train, family = "binomial")
summary(basicadvanced_stats_reg)

basicadvanced_stats_train <- basicadvanced_stats_train %>%
  ungroup() %>%
  mutate(prediction = predict(basicadvanced_stats_reg, basicadvanced_stats_train, type = "response")) %>%
  group_by(season) %>%
  mutate(mvp_prob = prediction/sum(prediction)) %>%
  mutate(mvp_won = ifelse(mvp == 1, "WON", "")) %>%
  ungroup() 

basicadvanced_stats_test <- basicadvanced_stats_test %>%
  ungroup() %>%
  mutate(prediction = predict(basicadvanced_stats_reg, basicadvanced_stats_test, type = "response")) %>%
  group_by(season) %>%
  mutate(mvp_prob = prediction/sum(prediction)) %>%
  mutate(mvp_won = ifelse(mvp == 1, "WON", "")) %>%
  ungroup() 

subfolder_path_3 <- "basicadvanced/"
dir.create(subfolder_path_3, showWarnings = FALSE)

for (year in 1980:2022) {
  per_year <- basicadvanced_stats_train %>%
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
  filename <- paste0(year, "basicadvanced.png")
  gtsave(table, file.path(subfolder_path_3, filename))
}


t_2023_3 <- basicadvanced_stats_test %>%
  mutate(mvp_prob = round(mvp_prob, 3)) %>%
  select(player, team, season, mvp_prob, mvp_won) %>%
  arrange(-mvp_prob) %>%
  filter(row_number() <= 6) %>%
  ungroup()
testtable_3 <- t_2023_3 %>% gt() %>% 
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
    subtitle = "Based on NBA MVP Data from 1980 - 2022 Involving Basic and Advanced Statistics"
  )
gtsave(testtable_3, "basicadvanced/2023basicadvanced.png")





