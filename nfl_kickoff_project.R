# NFL kickoff rule project from 2021-2025

library(nflreadr)
library(dplyr)
library(tidyr)
library(ggplot2)
library(stringr)
library(readr)

seasons <- 2021:2025
pbp <- nflreadr::load_pbp(seasons)

# first scrimmage play for each drive
first_drive_plays <- pbp %>%
  filter(
    season_type == "REG",
    !is.na(game_id),
    !is.na(drive),
    !is.na(posteam),
    !is.na(yardline_100),
    play_type %in% c("run", "pass", "sack", "qb_kneel", "qb_spike")
  ) %>%
  arrange(game_id, drive, play_id) %>%
  group_by(game_id, drive) %>%
  summarise(
    drive_posteam = first(posteam),
    drive_first_play_id = first(play_id),
    start_own_yardline = 100 - as.numeric(first(yardline_100)),
    .groups = "drop"
  )

# regular-season kickoffs and labeling the two rule periods
kickoffs_raw <- pbp %>%
  filter(
    season_type == "REG",
    play_type == "kickoff"
  ) %>%
  mutate(
    kickoff_id = row_number(),

    period_2 = if_else(
      season <= 2023,
      "Pre-Rule (2021-2023)",
      "Post-Rule (2024-2025)"
    ),

    period_3 = case_when(
      season <= 2023 ~ "Pre-Rule (2021-2023)",
      season == 2024 ~ "Dynamic v1 (2024)",
      season == 2025 ~ "Dynamic v2 (2025)"
    ),

    period_3 = factor(
      period_3,
      levels = c(
        "Pre-Rule (2021-2023)",
        "Dynamic v1 (2024)",
        "Dynamic v2 (2025)"
      )
    ),

    is_touchback = coalesce(as.integer(touchback), 0L),
    is_oob = coalesce(as.integer(kickoff_out_of_bounds), 0L),
    is_fair_catch = coalesce(as.integer(kickoff_fair_catch), 0L),
    is_return_td = coalesce(as.integer(return_touchdown), 0L),

    # return has to be more than a touchback, fair catch, or kick out of bounds
    is_returned = if_else(
      is_touchback == 0L &
        is_oob == 0L &
        is_fair_catch == 0L &
        (
          !is.na(kickoff_returner_player_id) |
            coalesce(return_yards, 0) > 0 |
            is_return_td == 1L
        ),
      1L,
      0L
    ),

    ret_yards = if_else(
      is_returned == 1L,
      as.numeric(return_yards),
      NA_real_
    ),

    is_30_plus = if_else(
      is_returned == 1L &
        !is.na(ret_yards) &
        ret_yards >= 30,
      1L,
      0L
    ),

    kickoff_outcome = case_when(
      is_touchback == 1L ~ "Touchback",
      is_returned == 1L ~ "Returned",
      is_fair_catch == 1L ~ "Fair Catch",
      is_oob == 1L ~ "Out of Bounds",
      TRUE ~ "Other"
    )
  )

# match each kickoff to the receiving teams first play
kickoff_matches <- kickoffs_raw %>%
  select(kickoff_id, game_id, play_id, defteam) %>%
  left_join(
    first_drive_plays,
    by = "game_id",
    relationship = "many-to-many"
  ) %>%
  filter(
    !is.na(defteam),
    !is.na(drive_first_play_id),
    drive_first_play_id > play_id,
    drive_posteam == defteam
  ) %>%
  group_by(kickoff_id) %>%
  slice_min(
    order_by = drive_first_play_id,
    n = 1,
    with_ties = FALSE
  ) %>%
  ungroup() %>%
  select(kickoff_id, start_own_yardline)

# keep kickoffs even if a receiving drive could not be matched
kickoffs <- kickoffs_raw %>%
  left_join(kickoff_matches, by = "kickoff_id")

# how many kickoffs were matched to a starting field position
cat("\nDRIVE MATCHING CHECK\n")
print(
  kickoffs %>%
    group_by(season) %>%
    summarise(
      total_kickoffs = n(),
      matched_drives = sum(!is.na(start_own_yardline)),
      unmatched_drives = sum(is.na(start_own_yardline)),
      match_percentage = mean(!is.na(start_own_yardline)) * 100,
      .groups = "drop"
    )
)

cat("\nSTARTING FIELD POSITION SUMMARY\n")
print(summary(kickoffs$start_own_yardline))

cat("\nINVALID FIELD POSITION VALUES (SHOULD BE ZERO)\n")
print(
  sum(
    kickoffs$start_own_yardline < 0 |
      kickoffs$start_own_yardline > 100,
    na.rm = TRUE
  )
)

# check the number of returns, touchbacks, fair catches, and out-of-bounds kicks.
cat("\nKICKOFF OUTCOMES BY SEASON\n")
print(
  kickoffs %>%
    group_by(season) %>%
    summarise(
      kickoffs = n(),
      returns = sum(is_returned),
      fair_catches = sum(is_fair_catch),
      touchbacks = sum(is_touchback),
      out_of_bounds = sum(is_oob),
      .groups = "drop"
    )
)

# calculate the main kickoff statistics for each season
by_season <- kickoffs %>%
  group_by(season, period_3) %>%
  summarise(
    total_kickoffs = n(),
    returns = sum(is_returned),
    touchbacks = sum(is_touchback),
    fair_catches = sum(is_fair_catch),
    out_of_bounds = sum(is_oob),

    return_rate = mean(is_returned),
    touchback_rate = mean(is_touchback),

    returns_30_plus = sum(is_30_plus),
    big_play_rate_ko = mean(is_30_plus),
    big_play_rate_ret = sum(is_30_plus) / pmax(sum(is_returned), 1),

    return_tds = sum(is_return_td),
    return_td_rate_ko = mean(is_return_td),
    return_td_rate_ret = sum(is_return_td) / pmax(sum(is_returned), 1),

    mean_ret_yards = mean(ret_yards, na.rm = TRUE),
    median_ret_yards = median(ret_yards, na.rm = TRUE),

    mean_start_own_yd = mean(start_own_yardline, na.rm = TRUE),
    median_start_own_yd = median(start_own_yardline, na.rm = TRUE),
    matched_positions = sum(!is.na(start_own_yardline)),

    .groups = "drop"
  )

cat("\nKICKOFF STATISTICS BY SEASON\n")
print(by_season)

# comparing the seasons before the rule change and seasons after it
by_period_2 <- kickoffs %>%
  group_by(period_2) %>%
  summarise(
    total_kickoffs = n(),
    returns = sum(is_returned),
    touchbacks = sum(is_touchback),
    fair_catches = sum(is_fair_catch),

    return_rate = mean(is_returned),
    touchback_rate = mean(is_touchback),

    big_play_rate_ko = mean(is_30_plus),
    big_play_rate_ret = sum(is_30_plus) / pmax(sum(is_returned), 1),

    return_tds = sum(is_return_td),
    return_td_rate_ko = mean(is_return_td),
    return_td_rate_ret = sum(is_return_td) / pmax(sum(is_returned), 1),

    mean_ret_yards = mean(ret_yards, na.rm = TRUE),
    median_ret_yards = median(ret_yards, na.rm = TRUE),

    mean_start_own_yd = mean(start_own_yardline, na.rm = TRUE),
    median_start_own_yd = median(start_own_yardline, na.rm = TRUE),
    matched_positions = sum(!is.na(start_own_yardline)),

    .groups = "drop"
  )

cat("\nPRE-RULE VS POST-RULE\n")
print(by_period_2)

# Average and median starting position by season and kickoff outcome.
by_outcome <- kickoffs %>%
  filter(!is.na(start_own_yardline)) %>%
  group_by(season, period_3, kickoff_outcome) %>%
  summarise(
    total = n(),
    mean_start = mean(start_own_yardline),
    median_start = median(start_own_yardline),
    .groups = "drop"
  )

cat("\nSTARTING POSITION BY KICKOFF OUTCOME\n")
print(by_outcome)

# return and touchback rates before and after the rule change
cat("\nRETURN RATE: PRE VS POST\n")
print(chisq.test(table(kickoffs$period_2, kickoffs$is_returned)))

cat("\nTOUCHBACK RATE: PRE VS POST\n")
print(chisq.test(table(kickoffs$period_2, kickoffs$is_touchback)))

# asks how often a kickoff produced a 30+ yard return
cat("\n30+ YARD RETURNS PER KICKOFF: PRE VS POST\n")
print(chisq.test(table(kickoffs$period_2, kickoffs$is_30_plus)))

cat("\nRETURN TOUCHDOWNS: PRE VS POST\n")
print(fisher.test(table(kickoffs$period_2, kickoffs$is_return_td)))

# compare return and touchback rates across all three rule periods
cat("\nRETURN RATE: THREE PERIODS\n")
print(chisq.test(table(kickoffs$period_3, kickoffs$is_returned)))

cat("\nTOUCHBACK RATE: THREE PERIODS\n")
print(chisq.test(table(kickoffs$period_3, kickoffs$is_touchback)))

cat("\n30+ YARD RETURNS PER KICKOFF: THREE PERIODS\n")
print(chisq.test(table(kickoffs$period_3, kickoffs$is_30_plus)))

returns_only <- kickoffs %>%
  filter(is_returned == 1)

cat("\n30+ YARD RETURNS PER RETURN: PRE VS POST\n")
print(chisq.test(table(returns_only$period_2, returns_only$is_30_plus)))

cat("\n30+ YARD RETURNS PER RETURN: THREE PERIODS\n")
print(chisq.test(table(returns_only$period_3, returns_only$is_30_plus)))

cat("\nRETURN TOUCHDOWNS PER RETURN: PRE VS POST\n")
print(fisher.test(table(returns_only$period_2, returns_only$is_return_td)))

# compare average starting field position before and after the rule change
pre_yd <- kickoffs %>%
  filter(
    period_2 == "Pre-Rule (2021-2023)",
    !is.na(start_own_yardline)
  ) %>%
  pull(start_own_yardline)

post_yd <- kickoffs %>%
  filter(
    period_2 == "Post-Rule (2024-2025)",
    !is.na(start_own_yardline)
  ) %>%
  pull(start_own_yardline)

if (length(pre_yd) > 1 && length(post_yd) > 1) {

  cat("\nSTARTING FIELD POSITION: WELCH T-TEST\n")
  print(t.test(post_yd, pre_yd, var.equal = FALSE))

  cat("\nSTARTING FIELD POSITION: WILCOXON TEST\n")
  print(wilcox.test(post_yd, pre_yd))
}

# look at starting position after returns and touchbacks separately
cat("\nSTARTING POSITION AFTER RETURNS\n")
print(
  kickoffs %>%
    filter(is_returned == 1, !is.na(start_own_yardline)) %>%
    group_by(period_2) %>%
    summarise(
      mean_start = mean(start_own_yardline),
      median_start = median(start_own_yardline),
      n = n(),
      .groups = "drop"
    )
)

cat("\nSTARTING POSITION AFTER TOUCHBACKS\n")
print(
  kickoffs %>%
    filter(is_touchback == 1, !is.na(start_own_yardline)) %>%
    group_by(period_2) %>%
    summarise(
      mean_start = mean(start_own_yardline),
      median_start = median(start_own_yardline),
      n = n(),
      .groups = "drop"
    )
)

# Graph 1: Return, touchback, and big-play rates by season
p1 <- by_season %>%
  select(season, return_rate, touchback_rate, big_play_rate_ko) %>%
  pivot_longer(
    -season,
    names_to = "metric",
    values_to = "rate"
  ) %>%
  ggplot(
    aes(
      x = factor(season),
      y = rate * 100,
      group = metric,
      color = metric
    )
  ) +
  geom_line(linewidth = 1) +
  geom_point(size = 2.5) +
  labs(
    title = "NFL Kickoff Rates by Season",
    subtitle = "Regular Season, 2021-2025 (Fair Catches Excluded from Returns)",
    x = "Season",
    y = "Percentage of Kickoffs (%)",
    color = "Metric"
  ) +
  theme_minimal()

print(p1)

# Graph 2: Number of returns compared with touchbacks
p2 <- by_season %>%
  select(season, returns, touchbacks) %>%
  pivot_longer(
    -season,
    names_to = "outcome",
    values_to = "count"
  ) %>%
  ggplot(aes(x = factor(season), y = count, fill = outcome)) +
  geom_col(position = "dodge") +
  labs(
    title = "Kickoff Returns vs. Touchbacks",
    x = "Season",
    y = "Number of Kickoffs",
    fill = "Outcome"
  ) +
  theme_minimal()

print(p2)

# Graph 3: Return yardage by season
p3 <- kickoffs %>%
  filter(is_returned == 1, !is.na(ret_yards)) %>%
  ggplot(aes(x = factor(season), y = ret_yards)) +
  geom_boxplot() +
  labs(
    title = "Kickoff Return Yardage by Season",
    x = "Season",
    y = "Return Yards"
  ) +
  theme_minimal()

print(p3)

# Graph 4: 30+ yard returns and touchdowns as a percentage of returns
p4 <- by_season %>%
  select(season, big_play_rate_ret, return_td_rate_ret) %>%
  pivot_longer(
    -season,
    names_to = "metric",
    values_to = "rate"
  ) %>%
  ggplot(
    aes(
      x = factor(season),
      y = rate * 100,
      group = metric,
      color = metric
    )
  ) +
  geom_line(linewidth = 1) +
  geom_point(size = 2.5) +
  labs(
    title = "Big Returns and Return Touchdowns",
    subtitle = "Rates calculated per kickoff return",
    x = "Season",
    y = "Percentage of Returns (%)",
    color = "Metric"
  ) +
  theme_minimal()

print(p4)

# Graph 5: Average starting field position after returns and touchbacks
p5 <- kickoffs %>%
  filter(
    !is.na(start_own_yardline),
    kickoff_outcome %in% c("Returned", "Touchback")
  ) %>%
  group_by(season, kickoff_outcome) %>%
  summarise(
    mean_start = mean(start_own_yardline),
    .groups = "drop"
  ) %>%
  ggplot(
    aes(
      x = factor(season),
      y = mean_start,
      color = kickoff_outcome,
      group = kickoff_outcome
    )
  ) +
  geom_line(linewidth = 1) +
  geom_point(size = 2.5) +
  labs(
    title = "Average Starting Field Position by Kickoff Outcome",
    x = "Season",
    y = "Receiving Team's Own Yard Line",
    color = "Kickoff Outcome"
  ) +
  theme_minimal()

print(p5)

# Graph 6: Starting field position across the three rule periods
p6 <- kickoffs %>%
  filter(
    !is.na(start_own_yardline),
    is_returned == 1 | is_touchback == 1
  ) %>%
  mutate(
    outcome = if_else(is_returned == 1, "Returned", "Touchback")
  ) %>%
  ggplot(
    aes(
      x = period_3,
      y = start_own_yardline,
      fill = outcome
    )
  ) +
  geom_boxplot(position = position_dodge(width = 0.8)) +
  labs(
    title = "Starting Field Position by Kickoff Outcome",
    x = "Rule Period",
    y = "Receiving Team's Own Yard Line",
    fill = "Outcome"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(
      angle = 15,
      hjust = 1
    )
  )

print(p6)

injury_safety <- tibble::tibble(
  season = c(2022, 2023, 2024),
  kickoffs = c(2698, 2698, 2803),
  returns = c(1011, 587, 920),
  total_injuries = c(29, 20, 33),
  concussions = c(5, 1, 4),
  upper_extremity = c(0, 3, 7),
  lower_extremity = c(7, 6, 16)
) %>%
  mutate(
    total_injury_rate_ko = total_injuries / kickoffs * 1000,
    concussion_rate_ko = concussions / kickoffs * 1000,

    total_injury_rate_ret = total_injuries / returns * 1000,
    concussion_rate_ret = concussions / returns * 1000
  )

cat("\nINJURY DATA\n")
print(injury_safety)

# Graph 7: Injuries and concussions per 1,000 kickoffs
injury_long <- injury_safety %>%
  select(
    season,
    total_injury_rate_ko,
    concussion_rate_ko
  ) %>%
  pivot_longer(
    -season,
    names_to = "injury_type",
    values_to = "rate"
  )

p7 <- ggplot(
  injury_long,
  aes(
    x = factor(season),
    y = rate,
    group = injury_type,
    color = injury_type
  )
) +
  geom_line(linewidth = 1) +
  geom_point(size = 2.5) +
  labs(
    title = "NFL Kickoff Injury Rates (Published Study)",
    subtitle = "Puga et al. (2022-2024); injuries per 1,000 kickoffs",
    x = "Season",
    y = "Injuries per 1,000 Kickoffs",
    color = "Injury Type"
  ) +
  theme_minimal()

print(p7)

# Graph 8: Injuries and concussions per 1,000 returns
injury_return_long <- injury_safety %>%
  select(
    season,
    total_injury_rate_ret,
    concussion_rate_ret
  ) %>%
  pivot_longer(
    -season,
    names_to = "injury_type",
    values_to = "rate"
  )

p8 <- ggplot(
  injury_return_long,
  aes(
    x = factor(season),
    y = rate,
    group = injury_type,
    color = injury_type
  )
) +
  geom_line(linewidth = 1) +
  geom_point(size = 2.5) +
  labs(
    title = "NFL Kickoff Injury Rates per Return (Published Study)",
    subtitle = "Puga et al. (2022-2024); injuries per 1,000 returns",
    x = "Season",
    y = "Injuries per 1,000 Returns",
    color = "Injury Type"
  ) +
  theme_minimal()

print(p8)


