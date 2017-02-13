# prepare solution data
# <Clean solution Data>------------------------------------------------------------------------------------------------
col_position <- colnames(raw_solution) %>%
    grep("player_", x = ., ignore.case = T, value = T)

solution <- raw_solution %>%
    select(-one_of(col_position)) %>%
    select(-(GBH:BSA)) %>%
    select(-(PSH:PSA)) %>%
    select(-(SJH:SJA)) 
    

solution$date <- solution$date %>% strptime(format="%m/%d/%Y") %>% as.Date()
# </Clean Player Data>-----------------------------------------------------------------------------------------------

# <Join Team data to solution>------------------------------------------------------------------------------------------
# merge by closest dates
# home team
home_chk_match <- solution %>% 
    select(match_api_id, home_team_api_id, date)

home_chk_diff <- left_join(home_chk_match, Team, by = c("home_team_api_id" = "team_api_id"))

home_chk_diff[,3:4] <- lapply(home_chk_diff[,3:4], as.Date)
home_chk_diff$diff <- abs(home_chk_diff$date.y - home_chk_diff$date.x)

home_chk_diff <- home_chk_diff %>% 
    group_by(home_team_api_id, date.x) %>%
    mutate(min_diff = min(diff)) %>%
    filter(diff == min_diff) %>%
    ungroup

home_join_df <- home_chk_diff %>%
    select(-diff, -min_diff) %>%
    select(-date.x, -date.y)

# away team
away_chk_match <- solution %>% 
    select(match_api_id, away_team_api_id, date)

away_chk_diff <- left_join(away_chk_match, Team, by = c("away_team_api_id" = "team_api_id"))

away_chk_diff[,3:4] <- lapply(away_chk_diff[,3:4], as.Date)
away_chk_diff$diff <- abs(away_chk_diff$date.y - away_chk_diff$date.x)

away_chk_diff <- away_chk_diff %>% 
    group_by(away_team_api_id, date.x) %>%
    mutate(min_diff = min(diff)) %>%
    filter(diff == min_diff) %>%
    ungroup

away_join_df <- away_chk_diff %>%
    select(-diff, -min_diff) %>%
    select(-date.x, -date.y)

# join away and home
team_join <- inner_join(home_join_df, away_join_df, by = c("match_api_id"),suffix = c("_home","_away"))


# join with match table
solution_joint <- solution %>%
    left_join(team_join, by = c("match_api_id", "home_team_api_id", "away_team_api_id")) %>%
    select(-team_short_name_home, -team_short_name_away)

solution_complete <- solution_joint %>% .[complete.cases(.),]
solution_miss <- solution_joint %>% .[!complete.cases(.),]

col_ratios <- solution_miss %>% select(B365H:VCA) %>% colnames()
col_teams <- solution_miss %>% select(-(B365H:VCA)) %>% colnames()

row_miss_3_ratios <- (rowSums(is.na(solution_miss %>% select(one_of(col_ratios)))) <= 3) & (rowSums(is.na(solution_miss %>% select(one_of(col_ratios)))) >0)
row_miss_ratios <- (rowSums(is.na(solution_miss %>% select(one_of(col_ratios)))) > 3) & (rowSums(is.na(solution_miss %>% select(one_of(col_teams)))) == 0)
row_miss_teams <- (rowSums(is.na(solution_miss %>% select(one_of(col_ratios)))) == 0) & (rowSums(is.na(solution_miss %>% select(one_of(col_teams)))) > 0)
row_miss_both <- (rowSums(is.na(solution_miss %>% select(one_of(col_ratios)))) > 3) & (rowSums(is.na(solution_miss %>% select(one_of(col_teams)))) > 0)

# SLR for the 3 missing values
solution_miss_3_ratios <- solution_miss[row_miss_3_ratios, ] # imputation, then joint model
solution_miss_ratios <- solution_miss[row_miss_ratios, ] %>% select(-one_of(col_ratios)) # team model
solution_miss_teams <- solution_miss[row_miss_teams, ] %>% select(-(buildUpPlaySpeed_home:defenceDefenderLineClass_away))# ratio model
solution_miss_both <- solution_miss[row_miss_both, ] # guess

# predict
pred_complete <- predict(svm_c_poly, solution_complete %>% select(-(id:away_team_api_id)))
pred_complete_df <- solution_complete %>% select(id) %>% cbind(pred_complete) %>% setNames(c("id", "result"))

pred_team <- predict(svm_t_poly, solution_miss_ratios %>% select(-(id:away_team_api_id)))
pred_team_df <- solution_miss_ratios %>% select(id) %>% cbind(pred_team) %>% setNames(c("id", "result"))

pred_ratio <- predict(svm_r_linear, solution_miss_teams)
pred_ratio_df <- solution_miss_teams %>% select(id) %>% cbind(pred_ratio) %>% setNames(c("id", "result"))

# go to imputation.R
pred_imp <- predict(svm_c_poly, solution_miss_3_ratios %>% select(-(id:away_team_api_id)))
pred_imp_df <- solution_miss_3_ratios %>% select(id) %>% cbind(pred_imp) %>% setNames(c("id", "result"))

# after imputation, guess remaining 7
pred_guess_df <- solution_miss_both %>% select(id) %>% mutate(result = "home_win")

# combine
result <- pred_complete_df %>% rbind(pred_team_df) %>% rbind(pred_ratio_df) %>% rbind(pred_imp_df) %>% rbind(pred_guess_df) %>%
    arrange(id)

write.csv(result, "attempt_1.csv")
