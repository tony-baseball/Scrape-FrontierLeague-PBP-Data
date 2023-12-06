library(tidyverse)

# Read data
pbp_ <- read.csv('C:/Users/tdmed/OneDrive/2023 FL PBP.csv') %>%
  filter(!is.na(Batter) | !grepl('na', Batter)) %>%
  rename(RunsonPlay = Runs) %>%
  group_by(Date, GameID, Inning, Top.Bottom, Team) %>%
  ungroup() %>%
  mutate(Event = str_replace_all(Event, c(
    'homered' = 'HR', 'tripled' = '3B', 'doubled' = '2B', 'singled' = '1B',
    'intentionally walked' = 'IBB', 'walked' = 'BB', 'hit by pitch' = 'HBP',
    'fielders choice' = 'FC', "fielder's choice" = 'FC', 'catchers interference' = 'CI',
    "catcher's interference" = 'CI',
    'reached first on a wild pitch' = 'K-WP',
    'struck out, reached first on a passed ball' = 'K-PB',
    'struck out swinging, reached first on a passed ball' = 'K-PB',
    'struck out lookingDropped' = 'K-WP',
    'struck out, reached first' = 'K-WP',
    'BB, grounded out to 1b unassisted\\. \\(1 out\\)' = 'BB',
    'Dropped foul ball, E5, flied out to\\. \\(1 out\\)' = 'reached on E',
    'Dropped foul ball, E3struck out looking' = 'K-WP', 'on an error by' = 'on E'
  )),
  RunsonPlay = ifelse(str_detect(Event, 'advanced to home'), RunsonPlay + 1, RunsonPlay),
  Event = ifelse(grepl('stole|advanced to second on a wild pitch|advanced to third on a wild pitch|caught stealing', Event),
                 paste(Batter, Event), Event),
  Batter = ifelse(grepl('stole|advanced to second on a wild pitch|advanced to third on a wild pitch|caught stealing|advanced to second on E c|advanced to third on E c', Event) &
                    !grepl('K-WP|K-PB', Event), lead(Batter), Batter),
  GamePA = rleid(GameID, Date, Inning, Team, Top.Bottom, Batter),
  InningPA = rleid(GameID, Date, Inning, Team, Top.Bottom, Batter, .after = GamePA),
  R1 = NA, R2 = NA, R3 = NA,
  OutsonPlay = ifelse(grepl(' out', Event) & !grepl('K-WP|K-PB', Event), 1,
                      ifelse(grepl('double play', Event), 2,
                             ifelse(grepl('triple play', Event), 3, 0))),
  across(c('R1', 'R2', 'R3'), ~ ifelse(InningPA == 1, 0, .)),
  Outs = ifelse(InningPA == 1, 0, NA), .after = Event) %>%
  ungroup() %>%
  mutate(R1 = ifelse(lag(grepl("1B|BB|IBB|HBP|FC|CI|K-WP|K-PB|reached first on E|reached second on E|Dropped foul ball, E3struck out looking",
                               Event), default = FALSE), 1, R1),
         R2 = ifelse(lag(grepl("2B|advanced to second|stole second", Event), default = FALSE), 2, R2),
         R3 = ifelse(lag(grepl("3B|advanced to third|stole third", Event), default = FALSE), 3, R3),
         across(c('R1', 'R2', 'R3'), ~ ifelse(lag(grepl('HR', Event), default = FALSE), 0, .)),
  ) %>%
  mutate(across(c('R1', 'R2', 'R3'), ~ ifelse(is.na(.) & lag(!grepl('advanced|stole', Event), default = FALSE), lag(., default = FALSE), .)),
         R1 = ifelse(lag(grepl("2B|3B", Event), default = FALSE), 0, R1),
         R2 = ifelse(lag(grepl("3B", Event), default = FALSE), 0, R2),
         R1 = ifelse(lag(grepl("out at second c to", Event), default = FALSE) &
                       lag(grepl("caught stealing", Event), default = FALSE), 0, R1),
         R2 = ifelse(lag(grepl("out at third c to", Event), default = FALSE) &
                       lag(grepl("caught stealing", Event), default = FALSE), 0, R2),
  ) %>%
  group_by(Date, GameID, Inning, Top.Bottom) %>%
  mutate(TotalRunsInning = sum(RunsonPlay),
         Runs_upto = lag(cumsum(RunsonPlay)),
         Runs_upto = ifelse(is.na(Runs_upto), 0, Runs_upto),
         Outs = lag(cumsum(OutsonPlay)),
         Outs = ifelse(is.na(Outs), 0, Outs),
         across(c('R1', 'R2', 'R3'), ~ ifelse(is.na(.), 0, .))
  ) %>%
  ungroup() %>%
  mutate(Runners = paste0(R1, R2, R3)) %>%
  select(Date, GameID, Top.Bottom, Inning, Team, Batter, Event, Runners, OutsonPlay, RunsonPlay, Outs, Runs_upto, TotalRunsInning)

# Create r_matrix
r_matrix <- pbp_ %>%
  group_by(Outs, Runners) %>%
  summarise(RE = round(mean(TotalRunsInning - RunsonPlay), 3)) %>%
  mutate(Runners = factor(Runners, levels = c('000', '100', '020', '120', '003', '103', '023', '123'))) %>%
  arrange(Runners) %>%
  pivot_wider(names_from = Outs, values_from = RE)

# Create r_matrix_long
r_matrix_long <- pbp_ %>%
  group_by(Outs, Runners) %>%
  summarise(RE = round(mean(TotalRunsInning - RunsonPlay), 3)) %>%
  mutate(Runners = factor(Runners, levels = c('000', '100', '020', '120', '003', '103', '023', '123')),
         State = paste(Outs, Runners)) %>%
  arrange(State) %>%
  ungroup() %>%
  select(-Outs, -Runners)
%>%
  pivot_wider(names_from = State, values_from = RE)

# Additional processing
pbp_w <- pbp_ %>%
  mutate(State = paste(Outs, Runners),
         NewState = lead(State),
         Result = case_when(
           grepl('HR', Event) ~ 'HR',
           grepl('3B', Event) ~ '3B',
           grepl('2B', Event) ~ '2B',
           grepl('1B', Event) ~ '1B',
           grepl('BB', Event) ~ 'BB',
           grepl('HBP', Event) ~ 'HBP',
           grepl('E|error', Event) ~ 'E',
           grepl('FC', Event) ~ 'FC',
           grepl('IBB', Event) ~ 'IBB',
           grepl('CI', Event) ~ 'CI',
           grepl('stole', Event) ~ 'SB',
           grepl('caught|picked off', Event) ~ 'CS',
           grepl('wild pitch', Event) ~ 'WP',
           grepl('passed ball', Event) ~ 'PB',
           grepl('balk|illegal pitch', Event) ~ 'BK',
           grepl('K-WP|K-PB|struck out|grounded out|flied out|popped up|lined out|popped out|double play|triple play|fouled out|out at first|infield fly', Event) ~ 'Out',
           TRUE ~ NA_character_
         ),
         RE = r_matrix_long$RE[match(State, r_matrix_long$State)],
         RE_New = r_matrix_long$RE[match(NewState, r_matrix_long$State)],
         .after = Event) %>%
  select(-Runners, -Outs) %>%
  group_by(Date, GameID, Top.Bottom, Inning, Team) %>%
  mutate(RE_diff = RE_New - RE + RunsonPlay)

# Create weights
weights <- pbp_ %>%
  group_by(Result) %>%
  summarise(sum = sum(RE_diff, na.rm = TRUE),
            n = n()) %>%
  filter(Result %in% c('BB', 'HBP', '1B', '2B', '3B', 'HR', 'Out')) %>%
  mutate(
    Result = factor(Result, levels = c('BB', 'HBP', '1B', '2B', '3B', 'HR', 'Out')),
    weight = sum / n,
    new_weight = weight + abs(weight[Result == 'Out']),
    scaled_weights = new_weight * 1.36) %>%
  arrange(Result) %>%
  select(Result, scaled_weights) %>%
  pivot_wider(names_from = Result, values_from = scaled_weights) %>%
  select(-Out) %>%
  rename_all(~ paste0("w", .)) %>%
  mutate(season = 2023,
         lg_woba = 0.354271,
         woba_scale = 1.35726535, .before = wBB) %>%
  mutate(runSB = 0.2,
         runCS = -0.343935353,
         lg_r_pa = 0.139830794,
         lg_r_w = 11.48344089,
         cFIP = 3.60333886)

# Write CSV files
# write.csv(pbp_, "C:/Users/tdmed/OneDrive/pbp.csv", row.names = FALSE)
# write.csv(weights, 'C:/Users/tdmed/OneDrive/frontier_league_weights.csv', row.names = FALSE)
