# <slice solution set>
solution_miss_3_ratios

# miss IW-X
lm_IWH <- lm(IWH ~ B365H + BWH + LBH + WHH + VCH, data = train_ratios)
lm_IWD <- lm(IWH ~ B365D + BWD + LBD + WHD + VCD, data = train_ratios)
lm_IWA <- lm(IWH ~ B365A + BWA + LBA + WHA + VCA, data = train_ratios)

# miss LB-X
lm_LBH <- lm(IWH ~ B365H + BWH + IWH + WHH + VCH, data = train_ratios)
lm_LBD <- lm(IWH ~ B365D + BWD + IWD + WHD + VCD, data = train_ratios)
lm_LBA <- lm(IWH ~ B365A + BWA + IWA + WHA + VCA, data = train_ratios)


# impute
solution_miss_3_ratios$IWH[1] <- predict(lm_IWH, solution_miss_3_ratios %>% select(B365H, BWH, LBH, WHH, VCH) %>% slice(1))
solution_miss_3_ratios$IWD[1] <- predict(lm_IWD, solution_miss_3_ratios %>% select(B365D, BWD, LBD, WHD, VCD) %>% slice(1))
solution_miss_3_ratios$IWA[1] <- predict(lm_IWA, solution_miss_3_ratios %>% select(B365A, BWA, LBA, WHA, VCA) %>% slice(1))

# impute
solution_miss_3_ratios$LBH[2] <- predict(lm_LBH, solution_miss_3_ratios %>% select(B365H, BWH, IWH, WHH, VCH) %>% slice(2))
solution_miss_3_ratios$LBD[2] <- predict(lm_LBD, solution_miss_3_ratios %>% select(B365D, BWD, IWD, WHD, VCD) %>% slice(2))
solution_miss_3_ratios$LBA[2] <- predict(lm_LBA, solution_miss_3_ratios %>% select(B365A, BWA, IWA, WHA, VCA) %>% slice(2))