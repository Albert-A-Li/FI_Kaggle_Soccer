# <Data Loading>================================================================================================
# load packages
pkg <- c("dplyr",
         "RSQLite",
         "tidyr",
         "DBI",
         "e1071",
         "rpart",
         "glmnet",
         "plotly")

pkg_load <- lapply(pkg, require, character.only = T, quietly = TRUE)

# connect to the db
conn <- dbConnect(drv = RSQLite::SQLite(),
                  dbname = "data/database.sqlite")
#list tables
tbl_list <- db_list_tables(conn)

# load tables
for (i in seq_along(tbl_list)){
    assign(paste0('raw_',tbl_list[i]), dbGetQuery(conn, paste("select * from", tbl_list[i])))
}

rm(list = c("raw_League", "raw_Country", "raw_sqlite_sequence"))

raw_solution <- read.csv("data/solution_set.csv")
# </Data Loading>================================================================================================

# <Data Cleaning>================================================================================================

# <Clean Team Data>----------------------------------------------------------------------------------------------
# Combine Team and Team_Attributes
# Use team_api_id for all team id, drop fifa id
Team <- raw_Team %>% 
    select(-id,
           -team_fifa_api_id, 
           -team_long_name)

Team <- raw_Team_Attributes %>% 
    select(-id, 
           -team_fifa_api_id) %>%
    left_join(Team, by = 'team_api_id')


# Convert data types
col_class <- colnames(Team) %>% 
    grep(pattern = "class$", x = ., ignore.case = T, value = T)

i <- 2
Team[,col_class[i]] %>% table
Team[,col_class[i]] <- ifelse(Team[,col_class[i]] == "Normal", 0, 
                              ifelse(Team[,col_class[i]] == "Lots", 1, -1))

i <- 4
Team[,col_class[i]] %>% table
Team[,col_class[i]] <- ifelse(Team[,col_class[i]] == "Free Form", 0, 1)

i <- 8
Team[,col_class[i]] %>% table
Team[,col_class[i]] <- ifelse(Team[,col_class[i]] == "Free Form", 0, 1)

Team <- Team %>% 
    select(-one_of(col_class[-c(2, 4, 8, 12)])) %>%
    select(-buildUpPlayDribbling)

Team$defenceDefenderLineClass %>% table
Team$defenceDefenderLineClass <- ifelse(Team$defenceDefenderLineClass == "Cover", 0, 1)
# </Clean Team Data>-----------------------------------------------------------------------------------------------

# <Clean Player Data>----------------------------------------------------------------------------------------------
Player <- raw_Player %>%
    select(-id,
           -player_fifa_api_id)









# </Clean Player Data>----------------------------------------------------------------------------------------------

# <Clean Match Data>------------------------------------------------------------------------------------------------
col_position <- colnames(raw_Match) %>%
    grep("player_X|player_Y", x = ., ignore.case = T, value = T)

Match <- raw_Match %>%
    select(-(home_team_goal:away_team_goal)) %>%
    select(-(goal:possession)) %>%
    select(-one_of(col_position)) %>%
    select(-(GBH:BSA)) %>%
    select(-(PSH:PSA)) %>%
    select(-(SJH:SJA)) %>%
    .[complete.cases(.),]

Match$date <- Match$date %>% as.Date()
# </Clean Player Data>-----------------------------------------------------------------------------------------------

# <Join Team data to Match>------------------------------------------------------------------------------------------
# merge by closest dates
# home team
chk_match <- Match %>% 
    select(home_team_api_id, date)

chk_diff <- full_join(chk_match, Team, by = c("home_team_api_id" = "team_api_id"))

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
chk_match <- Match %>% 
    select(away_team_api_id, date)

chk_diff <- full_join(chk_match, Team, by = c("away_team_api_id" = "team_api_id"))

chk_diff[,2:3] <- lapply(chk_diff[,2:3], as.Date)
chk_diff$diff <- abs(chk_diff$date.y - chk_diff$date.x)

chk_diff <- chk_diff %>% 
    group_by(away_team_api_id, date.x) %>%
    mutate(min_diff = min(diff))

chk_diff <- chk_diff %>%
    filter(diff == min_diff)

away_join_df <- chk_diff %>%
    select(-diff, -min_diff)

# join with match table
Match <- Match %>%
    inner_join(home_join_df, by = c("home_team_api_id" = "home_team_api_id", "date" = "date.x"), suffix = c("_home","_away"))%>%
    inner_join(away_join_df, by = c("away_team_api_id" = "away_team_api_id", "date" = "date.x"), suffix = c("_home","_away")) %>%
    select(-date.y_home, -date.y_away)

# <Join Team data to Match>------------------------------------------------------------------------------------------


