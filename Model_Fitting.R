# Model Fitting



# <SVM>
train_svm <- Match %>% 
    select(-(id:away_player_11)) %>%
    select(-team_short_name_home,
           -team_short_name_away)

train_svm$outcome <- train_svm$outcome %>% factor()

colSums(train_svm %>% is.na()) %>% sum()

model_linear <- svm(outcome ~ . , data = train_svm, type = "C-classification", kernel = "linear")
model_poly <- svm(outcome ~ . , data = train_svm, type = "C-classification", kernel = "polynomial")
model_sigmoid <- svm(outcome ~ . , data = train_svm, type = "C-classification", kernel = "sigmoid")

# test
set.seed(111)
random_sample <- sample(1:200)[1:200]

test <- train_svm[random_sample,] %>% select(-outcome)
test_outcome <- train_svm[random_sample,] %>% select(outcome) %>% unlist

pred_linear <- predict(model_linear, test)
sum(pred_linear == test_outcome)/200

pred_sigmoid <- predict(model_sigmoid, test)
sum(pred_sigmoid == test_outcome)/200

pred_poly <- predict(model_poly, test)
sum(pred_poly == test_outcome)/200

# predict
pred_poly <- predict(model_poly, solution)

sub_file <- raw_solution %>% cbind(pred_poly %>% as.vector())
