# Model Fitting
# fit 3 models
# 1: complete cases: from match_cln
# 2: team info only: from match_cln
# 3: ratio info only: from Match

# <SVM>
train_complete <- match_cln %>% 
    select(-(id:away_team_api_id)) %>%
    select(-team_short_name_home,
           -team_short_name_away) %>%
    .[complete.cases(.),]

train_team <- match_cln %>% 
    select(-(id:away_team_api_id)) %>%
    select(-(B365H:VCA)) %>%
    select(-team_short_name_home,
           -team_short_name_away) %>%
    .[complete.cases(.),]

train_ratios <- Match %>% 
    select(-(id:away_team_api_id)) %>%
    .[complete.cases(.),]


train_complete$outcome <- train_complete$outcome %>% factor()
train_team$outcome <- train_team$outcome %>% factor()
train_ratios$outcome <- train_ratios$outcome %>% factor()

colSums(train_complete %>% is.na()) %>% sum()
colSums(train_team %>% is.na()) %>% sum()
colSums(train_ratios %>% is.na()) %>% sum()

svm_c_linear <- svm(outcome ~ . , data = train_complete, type = "C-classification", kernel = "linear")
svm_c_poly <- svm(outcome ~ . , data = train_complete, type = "C-classification", kernel = "polynomial")
svm_c_sigmoid <- svm(outcome ~ . , data = train_complete, type = "C-classification", kernel = "sigmoid")

svm_t_linear <- svm(outcome ~ . , data = train_team, type = "C-classification", kernel = "linear")
svm_t_poly <- svm(outcome ~ . , data = train_team, type = "C-classification", kernel = "polynomial")
svm_t_sigmoid <- svm(outcome ~ . , data = train_team, type = "C-classification", kernel = "sigmoid")

svm_r_linear <- svm(outcome ~ . , data = train_ratios, type = "C-classification", kernel = "linear")
svm_r_poly <- svm(outcome ~ . , data = train_ratios, type = "C-classification", kernel = "polynomial")
svm_r_sigmoid <- svm(outcome ~ . , data = train_ratios, type = "C-classification", kernel = "sigmoid")


save(svm_c_linear, file = "model/svm_c_linear.rda")
save(svm_c_poly, file = "model/svm_c_poly.rda")
save(svm_c_sigmoid, file = "model/svm_c_sigmoid.rda")

save(svm_t_linear, file = "model/svm_t_linear.rda")
save(svm_t_poly, file = "model/svm_t_poly.rda")
save(svm_t_sigmoid, file = "model/svm_t_sigmoid.rda")

save(svm_r_linear, file = "model/svm_r_linear.rda")
save(svm_r_poly, file = "model/svm_r_poly.rda")
save(svm_r_sigmoid, file = "model/svm_r_sigmoid.rda")

# sample test data
set.seed(123)
random_sample <- sample(1:21000)[1:1000]

test <- train_complete[random_sample,] %>% select(-outcome)
test_outcome <- train_complete[random_sample,] %>% select(outcome) %>% unlist

# test complete
pred_linear <- predict(svm_c_linear, test)
sum(pred_linear == test_outcome)/1000

pred_poly <- predict(svm_c_poly, test)
sum(pred_poly == test_outcome)/1000

pred_sigmoid <- predict(svm_c_sigmoid, test)
sum(pred_sigmoid == test_outcome)/1000

# test team
pred_linear <- predict(svm_t_linear, test)
sum(pred_linear == test_outcome)/1000

pred_poly <- predict(svm_t_poly, test)
sum(pred_poly == test_outcome)/1000

pred_sigmoid <- predict(svm_t_sigmoid, test)
sum(pred_sigmoid == test_outcome)/1000

# test ratio
pred_linear <- predict(svm_r_linear, test)
sum(pred_linear == test_outcome)/1000

pred_poly <- predict(svm_r_poly, test)
sum(pred_poly == test_outcome)/1000

pred_sigmoid <- predict(svm_r_sigmoid, test)
sum(pred_sigmoid == test_outcome)/1000

# pred
sample_solution <- read.csv(file = "sample_solution.csv")

pred_poly <- predict(model_poly, solution)
sub_file <- raw_solution %>% cbind(pred_poly %>% as.vector())


