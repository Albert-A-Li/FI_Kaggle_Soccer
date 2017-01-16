# prepare solution data
# <Clean solution Data>------------------------------------------------------------------------------------------------
col_position <- colnames(raw_solution) %>%
    grep("player_X|player_Y", x = ., ignore.case = T, value = T)

solution <- raw_solution %>%
    select(-one_of(col_position)) %>%
    select(-(GBH:BSA)) %>%
    select(-(PSH:PSA)) %>%
    select(-(SJH:SJA)) %>%
    .[complete.cases(.),]

solution$date <- solution$date %>% strptime(format="%m/%d/%Y") %>% as.Date()
# </Clean Player Data>-----------------------------------------------------------------------------------------------

# <Join Team data to solution>------------------------------------------------------------------------------------------
# merge by closest dates
# home team
chk_solution <- solution %>% 
    select(home_team_api_id, date)

chk_diff <- left_join(chk_solution, Team, by = c("home_team_api_id" = "team_api_id"))

chk_diff[,2:3] <- lapply(chk_diff[,2:3], as.Date)
chk_diff$diff <- abs(chk_diff$date.y - chk_diff$date.x)

chk_diff <- chk_diff %>% 
    group_by(home_team_api_id, date.x) %>%
    mutate(min_diff = min(diff))

chk_diff <- chk_diff %>%
    filter(diff == min_diff)

home_join_df <- chk_diff %>%
    select(-diff, -min_diff)

# away team
chk_solution <- solution %>% 
    select(away_team_api_id, date)

chk_diff <- full_join(chk_solution, Team, by = c("away_team_api_id" = "team_api_id"))

chk_diff[,2:3] <- lapply(chk_diff[,2:3], as.Date)
chk_diff$diff <- abs(chk_diff$date.y - chk_diff$date.x)

chk_diff <- chk_diff %>% 
    group_by(away_team_api_id, date.x) %>%
    mutate(min_diff = min(diff))

chk_diff <- chk_diff %>%
    filter(diff == min_diff)

away_join_df <- chk_diff %>%
    select(-diff, -min_diff)

# join with solution table
solution <- solution %>%
    inner_join(home_join_df, by = c("home_team_api_id" = "home_team_api_id", "date" = "date.x"), suffix = c("_home","_away"))%>%
    inner_join(away_join_df, by = c("away_team_api_id" = "away_team_api_id", "date" = "date.x"), suffix = c("_home","_away")) %>%
    select(-date.y_home, -date.y_away)

# <Join Team data to solution>------------------------------------------------------------------------------------------

solution <- solution %>% 
    select(-(id:away_player_11)) %>%
    select(-team_short_name_home,
           -team_short_name_away)
