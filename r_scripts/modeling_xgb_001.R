


# very first initial xgboost -- this is just using string distances to get a good mapping 
# between leader board and local results

library(rstudioapi)
setwd(dirname(dirname(rstudioapi::getActiveDocumentContext()$path)))
source("r_scripts/r_script_config.R")

    # vignette("xgboost")  # opens local file

    # ?vignette
    # vignette()
    # demo()
    # demo("cross_validation")


# starting point
train3 <- readRDS(file = 'processed_data/modeling_rf_001_train3.rds')
test2 <- readRDS(file = 'processed_data/modeling_rf_001_test2.rds')
holdout <- readRDS(file = 'processed_data/modeling_rf_001_holdout.rds')

train3_no_factor <- read.csv(file = 'processed_data/modeling_rf_001_train3_no_factor.csv', stringsAsFactors = F)
test = read.csv('processed_data/modeling_rf_001_test2.csv', stringsAsFactors = F)


# model specific stuff
    train3_y <- train3$is_duplicate
    train3_y <- as.integer(train3_y) - 1  # converts from character factor to [0, 1] integer
    
    
    
    train3$is_duplicate <- NULL
    
    train3_mat <- as.matrix(train3)
    class(train3_mat[1,1])
    
    
    # holdout is massive, lets split it into two pieces and optimize the combo
    
    # can also store params in a list
    params <- list(
        eval_metric = "logloss",
        max_depth = 3,
        objective = "binary:logistic",
        nthread = 6
    )
    
    # quick xval
    xgb_cv1 <- xgb.cv(data = train3_mat, label = train3_y, params = params, nfold = 5,
                   nrounds = 10000, early_stopping_rounds = 30)
    #xgb_cv1 <- xgb1
    xgb_cv1$best_iteration  # 509
    
    # single model
    xgb1 <- xgboost(data = train3_mat, label = train3_y, params = params, nrounds = 509)
    saveRDS(xgb1, 'model_snapshots/modeling_xgb_001_xgb1.rds')
    
    
    # set up test
    
    test_mat <- as.matrix(test2)
    
    preds <- predict(xgb1, test_mat)                
    
    this_sub <- data.frame(test_id = test_raw$test_id, is_duplicate = preds)
    this_sub2 <- data.frame(test_id = test_raw$test_id, is_duplicate = (1 - preds))
    
    write.csv(this_sub, file = 'submissions/xgb1_this_sub.csv', row.names = F)
    write.csv(this_sub2, file = 'submissions/xgb1_this_sub2.csv', row.names = F)
    
    test_raw <- read.csv('input/test.csv', stringsAsFactors = F)
    ss <- read.csv('input/sample_submission.csv')
    
    
    ?xgb.cv


