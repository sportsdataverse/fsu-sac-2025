


library(cfbfastR)
library(ggalluvial)
library(ggtext)
library(ggplot2)
library(dplyr)
library(tidyr)
library(purrr)
library(glue)
library(forcats)

years <- 2020:cfbfastR:::most_recent_cfb_season()

team_info <- cfbfastR::cfbd_team_info(year = cfbfastR:::most_recent_cfb_season())

recruiting_raw <- purrr::map(years, cfbd_recruiting_player) %>%
  purrr::list_rbind()

recruiting_raw <- recruiting_raw %>%
  mutate(
    blue_chip = ifelse(stars >= 4, TRUE, FALSE),
    position_2 = case_when(
      position == "DUAL" ~ "QB",
      position == "PRO" ~ "QB",
      position == "OT" ~ "OL",
      position == "OG" ~ "OL",
      position == "OC" ~ "OL",
      position == "LS" ~ "OL",
      position == "APB" ~ "RB",
      position == "FB" ~ "RB",
      position == "OLB" ~ "LB",
      position == "ILB" ~ "LB",
      position == "WDE" ~ "DL",
      position == "SDE" ~ "DL",
      position == "DT" ~ "DL",
      position == "P" ~ "P/K",
      position == "K" ~ "P/K",
      TRUE ~ position)
  )

recruiting_df <- recruiting_raw %>%
  filter(year >= 2016, !is.na(state_province), blue_chip) %>%
  filter(committed_to %in% (team_info %>% filter(conference == "ACC") %>% pull("school"))) %>%
  left_join(team_info %>%
              select("school", "abbreviation", "color"), by = c("committed_to" = "school")) %>%
  mutate(school = committed_to,
         committed_to = abbreviation) %>%
  #count(state_province) %>% arrange(desc(n)) %>%
  group_by(state_province) %>%
  mutate(n_state = n()) %>%
  ungroup() %>%
  group_by(committed_to) %>%
  mutate(n_school = n()) %>%
  ungroup() %>%
  mutate(
    state = fct_rev(fct_reorder(state_province, n_state)),
    committed_to = fct_rev(fct_reorder(committed_to, n_school)),
    state = fct_lump_n(state, 10)) %>%
  mutate(
    position = factor(position, levels = c(
      "DUAL","PRO","OT","OG","OC","LS","APB","RB","FB","WR","TE",
      "ATH","OLB","ILB","S","CB","WDE","SDE","DT","P","K")),
    position_2 = factor(position_2, levels = c(
      "QB","OL","RB","WR","TE","ATH","LB","S","CB","DL","P/K")),
    stars = as.factor(stars)) %>%
  group_by(position_2) %>%
  mutate(n_pos = n()) %>%
  ungroup() %>%
  mutate(position_2 = fct_rev(fct_reorder(position_2, n_pos))) %>%
  group_by(position_2, committed_to, state, school, color) %>%
  #group_by(position_2,position,committed_to,stars,state) %>% #_province) %>%
  summarize(freq = n(), .groups = 'drop') %>%
  ungroup() %>%
  arrange(state, committed_to, position_2) %>%
  select("state", "committed_to", "position_2", "freq", "school", "color")

recruiting_df %>%
  ggplot(aes(y = freq,
             axis1 = state,
             axis2 = committed_to,
             axis3 = position_2)) +
  geom_alluvium(aes(fill = school), width = 1/4) +
  geom_stratum(width = 1/4, fill = "black", color = "grey", alpha = .7) +
  geom_label(stat = "stratum", aes(label = after_stat(stratum)), alpha = .7) +
  scale_x_discrete(breaks = c("state","committed_to","position_2"),
                   labels = c("State","School","Position")) +
  labs(y = "Number of Recruits",
       title = glue::glue("ACC <span style='color:#1A52D1'>Blue Chip</span> Recruits {min(years)}-{max(years)}"),
       subtitle = "Where they come from, where they go, and what they are",
       caption = "@Saiem Gilani | Data: @CFB_Data via @cfbfastR") +
  theme_minimal() +
  theme(legend.position = "none",
        plot.title = element_markdown(size = 26, face = "bold", hjust = .5),
        plot.subtitle = element_text(size = 16, face = "italic", hjust = .5))

ggsave("media/acc_bc_sankey.png", height = 14, width = 8, unit = "in")
