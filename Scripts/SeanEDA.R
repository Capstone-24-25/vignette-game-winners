library(tidyverse)
library(ggrepel)
library(nflreadr)
library(nflplotR)
library(nflfastR)

options(scipen = 9999) # prefer not to display in scientific notation

nfl_pbp <- load_pbp(2021:2024)

View(nfl_pbp)

# ALL RESULTS ARE BASED ON THE 2023 SEASON

# CHART 1 - Boxplot of win percentage by point difference

# things that effect winning (stadium, weather, win percentage, home or away team)

data %>%
  select(home_team, away_team, weather)

games <- nflreadr::load_schedules()
str(games)

home <- games %>%
  filter(game_type == 'REG') %>%
  select(season, week, home_team, result) %>%
  rename(team = home_team)
home %>% head(5)

away <- games %>%
  filter(game_type == 'REG') %>%
  select(season, week, away_team, result) %>%
  rename(team = away_team) %>%
  mutate(result = -result)
away %>% head(5)

results <- bind_rows(home, away) %>%
  arrange(week) %>%
  mutate(
    win = case_when(
      result > 0 ~ 1,
      result < 0 ~ 0,
      result == 0 ~ 0.5
    )
  )

results %>% filter(season == 2023 & team == 'GB')

team_wins <- results %>%
  group_by(team, season) %>%
  summarize(
    wins = sum(win),
    point_diff = sum(result)) %>%
  ungroup()
#> `summarise()` has grouped output by 'team'. You can override using the
#> `.groups` argument.

team_wins %>%
  arrange(-wins) %>%
  head(5)

pt_diff_per_win = team_wins %>%
                    select(point_diff, wins) %>%
                    group_by(wins) %>%
                    summarize(avg_pt_diff = mean(point_diff))

median_colors <- team_wins %>%
  group_by(wins) %>%
  summarize(median_diff = median(point_diff, na.rm = TRUE)) %>%
  mutate(fill_color = ifelse(median_diff < 0, "red", "green"))

# Join back to the original data to map colors
team_wins <- team_wins %>%
  left_join(median_colors, by = "wins")

# Plot with fill aesthetic
pt_diff_chart = ggplot(team_wins, aes(x = wins, y = point_diff, group = wins, fill = fill_color)) +
  geom_boxplot() +
  scale_fill_identity() +  # Use the fill colors directly
  theme_minimal() +
  labs(title = "Boxplot of Point Difference by Wins",
       x = "Wins",
       y = "Point Difference")
pt_diff_chart
# Saved file of the finished boxplot

saveRDS(pt_diff_chart, file = "pt_diff.rds")

# CHART 2 - 

# finding if home team wins depend on stadium

roof_effect = nfl_pbp %>%
                select(game_id, roof, home_wp, quarter_seconds_remaining, qtr) %>%
                filter(home_wp == 1 | home_wp == 0) %>%
  group_by(roof) %>%
  summarize(avg_home_wp = mean(home_wp))

roof_effect %>%
  ggplot(aes(x = roof, y = avg_home_wp)) +
  geom_col()

# Every single average of home teams win percentage is above 50%. However, there is a clear advantage for the home team when playing in a dome roof stadium. This particular roof format gets at least a 4% win percentage boost for any home team.

# Now lets check to see how this compares to the original odds given by vegas

roof_effect_vegas = nfl_pbp %>%
  select(game_id, roof, vegas_home_wp, quarter_seconds_remaining, qtr) %>%
  filter(qtr == 1 & quarter_seconds_remaining == 900) %>%
  group_by(roof) %>%
  summarize(avg_vegas_wp = mean(vegas_home_wp))

roof_effect_vegas %>%
  pivot_wider(
    names_from = 'roof',
    values_from = 'avg_vegas_wp'
  )

roof_effect_vegas %>%
  ggplot(aes(x = roof, y = avg_vegas_wp)) +
  geom_col()

# interestingly enough, vegas does not pay much mind to the roof conditions on kickoff. This is excluding the open roof which drops the win percentage of the home team to 44% from the ~55% of every other roof condition

total_home_wp = left_join(
  roof_effect,
  roof_effect_vegas,
  by = 'roof')

long_data <- total_home_wp %>%
  pivot_longer(
    cols = c(avg_home_wp, avg_vegas_wp), # Columns to pivot
    names_to = "Result",                # New column for variable names
    values_to = "value"                 # New column for values
  )

LV_vs_Act = ggplot(long_data, aes(x = roof, y = value, fill = Result)) +
  geom_bar(stat = "identity", position = "dodge", color = "black") +
  geom_text(
    aes(label = scales::percent(value, accuracy = 0.1)),           
    position = position_dodge(width = 0.9),                      
    vjust = -0.25,                                                  
    size = 3                                                         
  ) +
  labs(
    title = "Average Home WP and Vegas WP by Roof Type",
    x = "Roof Type",
    y = "Win Percentage",
    fill = "Result"
  ) +
  theme_minimal()
LV_vs_Act
# The Cowboys, Colts, Falcons, Texans and Cardinals all have the opportunity to open and close the roofs to their stadiums.


saveRDS(LV_vs_Act, file = "LV_vs_Act.rds")

# do high scoring games result in the home or away team winning

ten_years <- nflfastR::load_pbp(2014:2024)
wp_pts_scored = ten_years %>%
                select(total, home_wp, away_wp) %>%
                filter(home_wp == 1 | away_wp == 1) %>%
                mutate(
                  total_range = case_when(
                    total >= 0 & total < 38 ~ "0-38",
                    total >= 38 & total < 49 ~ "38-49",
                    total >= 49 ~ "49+"
                 )
                ) %>%
  group_by(total_range) %>%
  summarize(mean_home = mean(home_wp),
            mean_away = mean(away_wp))

wp_pts_scored_long <- wp_pts_scored %>%
  pivot_longer(
    cols = c(mean_home, mean_away),
    names_to = "WP_Type",
    values_to = "Average"
  )

stacked_total = ggplot(wp_pts_scored_long, aes(x = total_range, y = Average, fill = WP_Type)) +
  geom_bar(stat = "identity", position = "stack", color = "black") +
  labs(
    title = "Stacked Bar Chart of Average Home and Away WP by Total Points Past 10 Years",
    x = "Point Totals",
    y = "Average WP",
    fill = "WP Type"
  ) +
  theme_minimal()

stacked_total

saveRDS(stacked_total, file = 'Stacked_Total.rds')

# Again, the home team is always favored to win, but low and high scoring games have no effect. the middle ground games between 38 and 49 combined points contain the most significant difference when looking at the mean of the home vs away win percentage


qbs <- nfl_pbp %>%
  filter(season_type == "REG", !is.na(epa)) %>%
  group_by(id, name) %>%
  summarize(
    epa = mean(qb_epa),
    cpoe = mean(cpoe, na.rm = T),
    n_dropbacks = sum(pass),
    n_plays = n(),
    team = last(posteam)
  ) %>%
  ungroup() %>%
  filter(n_dropbacks > 100 & n_plays > 1000)

qbs <- qbs %>%
  left_join(load_teams(), by = c('team' = 'team_abbr'))

# CHART 3
efficiency = qbs %>%
  ggplot(aes(x = cpoe, y = epa)) +
  #horizontal line with mean EPA
  geom_hline(yintercept = mean(qbs$epa), color = "red", linetype = "dashed", alpha=0.5) +
  #vertical line with mean CPOE
  geom_vline(xintercept =  mean(qbs$cpoe), color = "red", linetype = "dashed", alpha=0.5) +
  #add points for the QBs with the logos (this uses nflplotR package)
  geom_nfl_logos(aes(team_abbr = team), width = qbs$n_plays / 45000, alpha = 0.75) +
  #add names using ggrepel, which tries to make them not overlap
  geom_text_repel(aes(label=name)) +
  #add a smooth line fitting cpoe + epa
  stat_smooth(geom='line', alpha=0.5, se=FALSE, method='lm')+
  #titles and caption
  labs(x = "Completion % above expected (CPOE)",
       y = "EPA per play (passes, rushes, and penalties)",
       title = "Quarterback Efficiency, 2021 - 2024",
       caption = "Data: @nflfastR") +
  theme_bw() +
  #center title
  theme(
    plot.title = element_text(size = 14, hjust = 0.5, face = "bold")
  ) +
  #make ticks look nice
  scale_y_continuous(breaks = scales::pretty_breaks(n = 10)) +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 10))

efficiency

saveRDS(efficiency, file = "QB_efficiency.rds")

# CHART 4

library(nflplotR)
# get pbp and filter to regular season rush and pass plays
pbp <- nfl_pbp %>%
  filter(season_type == "REG") %>%
  filter(!is.na(posteam) & (rush == 1 | pass == 1))
# offense epa
offense <- pbp %>%
  group_by(team = posteam) %>%
  summarise(off_epa = mean(epa, na.rm = TRUE))
# defense epa
defense <- pbp %>%
  group_by(team = defteam) %>%
  summarise(def_epa = mean(epa, na.rm = TRUE))
# make figure
EPA = offense %>%
  inner_join(defense, by = "team") %>%
  ggplot(aes(x = off_epa, y = def_epa)) +
  # tier lines
  geom_abline(slope = -1.5, intercept = (4:-3)/10, alpha = .2) +
  # nflplotR magic
  geom_mean_lines(aes(y0 = off_epa, x0 = def_epa)) +
  geom_nfl_logos(aes(team_abbr = team), width = 0.07, alpha = 0.7) +
  labs(
    x = "Offense EPA/play",
    y = "Defense EPA/play",
    caption = "Data: @nflfastR",
    title = "2021-2024 NFL Offensive and Defensive EPA per Play"
  ) +
  theme_bw() +
  theme(
    plot.title = element_text(size = 12, hjust = 0.5, face = "bold")
  ) +
  scale_y_reverse()
EPA

saveRDS(EPA, file = 'EPA_2024.rds')
