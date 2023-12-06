{
  library(rvest)
  library(plyr)
  library(dplyr)
  library(xml2)
  library(XML)
  library(magrittr)
  library(lubridate)
  library(data.table)
  library(stringr)
  library(discordr)
  library(emojifont)
  library(tidyverse)

}

# define start date of season to scrape.
start_date <- '2023-05-11'


# url of webpage where boxscores are located
url <- "https://www.frontierleague.com/composite?d="

# url of frontier league website
url2<- "https://www.frontierleague.com"

# start date of boxscores / calendar
webpage <- read_html(paste0(url,start_date))

# get all links on webpage, will result in finding game dates for seasons
links <- html_nodes(webpage, "a") %>% html_attr("href")

# filter the links to only have 2023 games
game_dates <- unique(links[grepl('/composite\\?d=2023',links)])

# generating range of dates from the game_dates urls, removes everything before the = 
range <- gsub(".*=","",game_dates)

# empty create empty df for daily log, gamelog, and pbp
dailylog <- data.frame()
dailylog2 <- data.frame()
pbp <- data.frame()
game_data <- data.frame(Date = character(),
                        HomeTeam = character(),
                        Umpire = character(),
                        stringsAsFactors = FALSE)

{
  # s variable is just the time the scrape begins. I like to see how long all my loops/scrapes take so i also have an e at the end to compare
s <- now()
  # for each date in the range of dates
for(date in range) {

  # read the date's webpage to find the boxscores urls on that date
  webpage_ <- read_html(paste0(url,date)) %>%
    html_nodes("a") %>% 
    html_attr("href") 
  
  # filter the webpage to get the list of links for the boxscores
  boxscores <- webpage_[grepl('boxscores',webpage_)]

  # the number of boxscores needs to be > 0. I usualy run this code as part of a script every morning during the season, and it'll use the night before's date. Since games are not played every day, i need to make sure that the code will run smoothly.
  if (length(boxscores > 0)) {
    
    # for future use: might filter out these strings
    positions_ <- c('to p','to c', 'to 1b','to 2b','to 3b','to ss','to lf','to cf','to rf','to dh', 'to pr', 'pinch run', 'pinch ran', 'pinch hit')
    positions <- paste(positions_, collapse = "|")
    
    # for each box score on that date
    for (bs in boxscores) {
     # bs <- boxscores[1]
      
      # get the link for each boxscore on that date 
      box<- read_html(paste0(url2,bs)) %>% 
        html_table(fill = TRUE)
      
      # - the 9th table is where the play by play data starts, and it'll go until the end of the game. The logs are split into half innings
      start <- 9
      end <- length(box)
      
      # index of each of the tables where half innings are found
      list_of_half_innings <-start:end
    
      # create empty df
      gamelog <- data.frame()
      
      if(length(box) >8 ){
        
        # for each half inning in entire game....
        for (half in list_of_half_innings) {
          
          # half <- list_of_half_innings[1]
          
          # create blank df
          innings <- as.data.frame(box[[half]]) 
          
          if(nrow(innings) > 0 & 
             length(list_of_half_innings) > 5
             
          ){
            inning <- innings  %>%
              rename(Event = X1)%>%
              mutate(Event = str_squish(Event)) %>%
              filter(!grepl('Inning Summary:', Event)) %>%
              separate(Event, into = c("Team", "Inning"), sep = 'Top|Bottom', remove = FALSE, extra = 'merge') %>%
              mutate(`Top.Bottom` = ifelse(grepl("Top", Event, ignore.case = TRUE), "Top",
                                           ifelse(grepl("Bottom", Event, ignore.case = TRUE), "Bottom",
                                                  ""))[1],
                     Date = as.Date(date) ,
                     # extract the inning number from the scrape
                     Inning = as.numeric(str_squish(gsub('Inning|of|st|nd|rd|th','',Inning)))[1],
                     Team = str_squish(Team)[1],
                     Batter = str_squish(str_to_lower(word(sapply(str_extract_all(Event, "\\b\\w+\\b"),
                                                                 function(x) paste(x[1:2], collapse = " ")), 2))),
                     Event = str_replace(Event, "^[^ ]* ", ""),
                     Runs =  str_count(Event, "homered") + str_count(Event, "scored"),
                     Game = bs)  %>%
              # filter(!grepl('Inning|to p for|pinch hit', Event)) %>%
              # filter(!grepl('to p for', Event)) %>%
              # filter(!grepl('pinch hit', Event)) %>%
              
              # filter(!grepl('stole', Event)) %>%
              # filter(!grepl('wild pitch', Event)) %>%
              # filter(!grepl('passed ball', Event)) %>%
              # filter(!grepl(positions, Event)) %>%
              filter(!grepl('Inning', Event)) %>%
              mutate(PAofInning = row_number()) %>%
              select(5,4,3,2,6,1,7,8)  %>%
              distinct(.keep_all = TRUE) %>%
              mutate(PAofGame = cumsum(Batter != lag(Batter, default = first(Batter))) + 1) %>%
              group_by(Inning,Top.Bottom) %>%
              mutate(PAofInning = cumsum(Batter != lag(Batter, default = first(Batter))) + 1) %>%
              ungroup()
            
            if (nrow(gamelog) == 0) {
              
              gamelog <- inning  %>%
                distinct(.keep_all = TRUE) %>%
                mutate(PAofGame = cumsum(Batter != lag(Batter, default = first(Batter))) + 1) %>%
                group_by(Inning,Top.Bottom) %>%
                mutate(PAofInning = cumsum(Batter != lag(Batter, default = first(Batter))) + 1) %>%
                ungroup()
              
            } else {
              
              # inning <- inning %>%
              #   mutate(PAofGame = row_number() + nrow(gamelog))
              
              gamelog <- rbind(gamelog, inning) %>%
                distinct(.keep_all = TRUE) %>%
                mutate(PAofGame = cumsum(Batter != lag(Batter, default = first(Batter))) + 1) %>%
                group_by(Inning,Top.Bottom) %>%
                mutate(PAofInning = cumsum(Batter != lag(Batter, default = first(Batter))) + 1) %>%
                ungroup()
            }
          }
        }
      }
      
      if (nrow(dailylog) == 0) {
        dailylog <- gamelog
      } else {
        dailylog <- rbind(dailylog, gamelog)
      }
    }
    
    
    dailylog2 <- dailylog %>%
      distinct(Game) %>%
      mutate(GameID = row_number())%>%
      left_join(dailylog, by = "Game") %>%
      select(-Game) %>% 
      mutate(Team = str_squish(Team),
             Batter = str_to_lower(Batter),
             Team = str_to_title(Team)) %>%
      rename(GamePA = 'PAofGame')
    
    # 
    # boxs<-read.csv("C:/Users/tdmed/OneDrive/_Shiny/szn box up to 6-7.csv")
    # boxs2 <- boxs%>%
    # #  filter(Date !='2023-06-07') %>%
    #   distinct()%>%
    #   distinct(Game) %>%
    #   mutate(GameID = row_number()) %>%
    #   left_join(boxs, by = "Game") %>%
    #   select(-Game) %>% 
    #   mutate(Team = str_squish(Team),
    #          Batter = str_to_lower(Batter),
    #          Team = str_to_title(Team)) %>%
    #   rename(GamePA = 'PAofGame') %>%
    #   arrange(GameID, GamePA)
    
    pbp <-  rbind(pbp, dailylog2) %>% distinct()
  } else {"No games yesterday"}
  #rm(boxs2)
  
  #write.csv(boxs2,"C:/Users/tdmed/OneDrive/_Shiny/Daily boxscore update.csv", row.names = FALSE, na='')
  
  #send_webhook_message(paste(emoji('white_check_mark'),"Boxscores successfully scraped!"))
} 
  
  paste(positions_,"for")
  grepl(paste(paste(positions_,"for"), collapse = "|"),Event)
  
  
  
  pbp_ <- pbp %>% distinct()  %>%
    filter(Batter != 'na') %>%
    # filter(!grepl(paste( paste(positions_,"for"), collapse = "|"),Event),
    #        !grepl(paste0(positions_,"\\.$", collapse = "|"), Event)) 
    rename(RunsonPlay = 'Runs',
           InningPA = 'PAofInning') %>%
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
      'Dropped foul ball, E3struck out looking' = 'K-WP', 'on an error by' = 'on E',
      "reached first on a dropped fly by ss" = 'E'
    )),
    RunsonPlay = ifelse(str_detect(Event, 'advanced to home'), RunsonPlay + 1, RunsonPlay),
    Event = ifelse(grepl('stole|advanced to second on a wild pitch|advanced to third on a wild pitch|caught stealing', Event),
                   paste(Batter, Event), Event),
    Batter = ifelse(grepl('stole|advanced to second on a wild pitch|advanced to third on a wild pitch|caught stealing|advanced to second on E c|advanced to third on E c', Event) &
                      !grepl('K-WP|K-PB', Event), lead(Batter), Batter),
    # GamePA = rleid(GameID, Date, Inning, Team, Top.Bottom, Batter),
    # InningPA = rleid(GameID, Date, Inning, Team, Top.Bottom, Batter, .after = GamePA),
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
      grepl("K-WP|K-PB|struck out|grounded out|flied out|popped up|lined out|popped out|double play|triple play|fouled out|out at first|infield fly|batter's interference", Event) ~ 'Out',
      grepl(paste( paste(positions_,"for"), collapse = "|"),Event) ~ "SUB",
      grepl(paste0(positions_,"\\.$", collapse = "|"), Event) ~ "PosChange",
      grepl("place on second|placed on first|placed on second|place on first", Event) ~ "XInn_Runner",
      grepl('Failed pickoff attempt',Event ) ~ "POA",
            TRUE ~ NA_character_
            )
      ) %>%
    mutate(Result = case_when(
      is.na(Result) & grepl('out at', Event) ~ 'Out_on_bases', 
      is.na(Result) & grepl('scored', Event) ~ 'Run_on_bases', 
      is.na(Result) & str_count(Event, "\\S+") == 3 & word(Event, 1) == 'for' ~ "SUB" ,
      is.na(Result) & grepl('advanced to second|advanced to third', Event) ~ 'Runner_adv',
      is.na(Result) & grepl("for", Event) ~ "SUB",
      TRUE ~ Result) # CHANGE THIS 
         ) %>% 
    mutate(R1 = NA, 
           R2 = NA, 
           R3 = NA,
           OutsonPlay = ifelse(grepl(' out', Event) & !grepl('K-WP|K-PB', Event), 1,
                               ifelse(grepl('double play', Event), 2,
                                      ifelse(grepl('triple play', Event), 3, 0))) ) %>%
    filter(!Result %in% c('XInn_Runner', 'SUB'))  %>%
    group_by(Date, GameID, Top.Bottom, Inning, Team) %>%
    mutate(InningPA = cumsum(Batter != lag(Batter, default = first(Batter))) + 1) %>%
    ungroup() %>% group_by(Date, GameID) %>%
    mutate(GamePA = cumsum(Batter != lag(Batter, default = first(Batter))) + 1)  %>%
    ungroup() %>%
    mutate(across(c('R1', 'R2', 'R3'), ~ ifelse(InningPA == 1, 0, .)),
         #  across(c('R1_end', 'R2_end', 'R3_end'), ~ ifelse(InningPA == 1, 0, .)),
           Outs = ifelse(InningPA == 1, 0, NA), .after = Event) %>%
    mutate(R1 = ifelse(lag(grepl("1B|BB|IBB|HBP|FC|CI|K-WP|K-PB|reached first on E|reached second on E|Dropped foul ball, E3struck out looking|placed on first|place on first",
                                 Event), default = FALSE), 1, R1),
           R2 = ifelse(lag(grepl("2B|advanced to second|stole second|placed on second|place on second", Event), default = FALSE), 2, R2),
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
    ungroup()  %>%
  
  # TO EXCEL FOR FUTHER REVIEW
 # write.csv(pbp_, "C:/Users/tdmed/OneDrive/pbp_review balks.csv", row.names = F)
  
    mutate(Runners = paste0(R1, R2, R3)) %>%
    dplyr::select(Date, GameID, GamePA, InningPA, Top.Bottom, Inning, Team, Batter, Event, Result, Runners, OutsonPlay, RunsonPlay, Outs, Runs_upto, TotalRunsInning, )
  
  
  # Create run_matrix
  run_matrix <- pbp_ %>%
    group_by(Outs, Runners) %>%
    summarise(RE = round(mean(TotalRunsInning - RunsonPlay), 3)) %>%
    mutate(Runners = factor(Runners, levels = c('000', '100', '020', '120', '003', '103', '023', '123'))) %>%
    arrange(Runners) %>%
    pivot_wider(names_from = Outs, values_from = RE)
  
  # Create run_matrix_long
  run_matrix_long <- pbp_ %>%
    group_by(Outs, Runners) %>%
    summarise(RE = round(mean(TotalRunsInning - RunsonPlay), 3)) %>%
    mutate(Runners = factor(Runners, levels = c('000', '100', '020', '120', '003', '103', '023', '123')),
           State = paste(Outs, Runners)) %>%
    arrange(State) %>%
    ungroup() %>%
    select(-Outs, -Runners) 
  
  pbp_2 <- pbp_ %>%
    mutate(State = paste(Outs, Runners),
           NewState = lead(State),
           RE = run_matrix_long$RE[match(State, run_matrix_long$State)],
           RE_New = run_matrix_long$RE[match(NewState, run_matrix_long$State)],
           .after = Event)%>%
    select(-Runners, -Outs) %>%
    group_by(Date, GameID, Top.Bottom, Inning, Team) %>%
    mutate(RE_diff = RE_New - RE + RunsonPlay)
  
  weights <- pbp_2 %>%
    group_by(Result) %>%
    summarise(sum = sum(RE_diff, na.rm = TRUE),
              n = sum(RE_diff != 0, na.rm = TRUE)) %>% ######
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
  e <- now()
  print(e-s)
}

  write.csv(weights, "C:/Users/tdmed/OneDrive/_Shiny/_Coop2/weights.csv", row.names = F)
  write.csv(run_matrix, "C:/Users/tdmed/OneDrive/_Shiny/_Coop2/run_matrix.csv", row.names = F)

  wb <- createWorkbook()
  addWorksheet(wb, 1)
  writeData(wb, 1, weights)
  addWorksheet(wb, 2)
  writeData(wb, 2, run_matrix)
  
  saveWorkbook(wb, "C:/Users/tdmed/OneDrive/_Shiny/_Coop2/FL_weights_run_matrix.xlsx")
  

