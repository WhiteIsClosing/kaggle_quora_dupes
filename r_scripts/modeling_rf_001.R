


# modeling_rf_001 -- this is to get a feel for the public leaderboard

# standard script start
    library(rstudioapi)
    
    # set wd to root of project
    setwd(dirname(dirname(rstudioapi::getActiveDocumentContext()$path)))
    source('r_scripts/r_script_config.R')



# load data:    
    list.files('processed_data')
    train <- readRDS('processed_data/train_02.rds')
    test <- readRDS('processed_data/test_02.rds')
    test$is_duplicate <- NA
    
    test_id <- test$test_id
    train_qid1 <- train$qid1    
    train_qid2 <- train$qid2
    
            
# remove cols that aren't common
    not_in_test <- names(train)[!names(train) %in% names(test)]
    not_in_train <- names(test)[!names(test) %in% names(train)]
    
    train <- train[ , setdiff(names(train), not_in_test)]
    test <- test[ , setdiff(names(test), not_in_train)]
    
    
    
# add dataset type and combine
    train$dataset <- 'train'
    test$dataset <- 'test'
    
    all <- dplyr::bind_rows(train, test)
    
    
    
# keep only features and the target var
    feat_cols <- c( names(all)[grepl('feature', names(all))])
    tar_cols <- c('is_duplicate', 'dataset')
    
    all_feat <- all[, feat_cols]
    all_tar <- all[, tar_cols]
    
    
# scale numeric features -- remove NA and NaNs (replace with zeros)
    
    
    # filter to just numeric/integer cols (all should be numeric or integer)
    all_numb <- all_feat[ ,   (sapply(all_feat, class) %in% c('numeric', 'integer'))  ]
    sum(!sapply(all_numb, class) %in% c('numeric', 'integer')) # should be zero 
    
    
    # scale and center numeric features
    all_numb <- sapply(all_numb, scale)  # takes 1.15 minutes
    all_numb <- data.frame(all_numb, stringsAsFactors = F)
    
    
    # set NaN and NA to zero
    is.nan.data.frame <- function(x) {do.call(cbind, lapply(x, is.nan))}
    all_numb[is.nan(all_numb)] <- 0
    all_numb[is.na(all_numb)] <- 0
    
    
    # combine target var, dataset origin, and numeric features
    all2 <- dplyr::bind_cols(all_tar, all_numb)
    
    
    comment(all2) <- "This is all string distances between the two questions for train and 
     test with the exception of the hamming distance. All string distance values have been  
     scaled and centered"
    
    
    attr(all2, 'comment')  # extracting comment
    
    
    saveRDS(all2, file = 'processed_data/all2_numeric_feature_matrix_001.rds')
    write.csv(all2, file = 'processed_data/all2_numeric_feature_matrix_001.csv', row.names = F)
    
# split back into train and test:
    
    train2 <- subset(all2, dataset == "train")
    test2 <- subset(all2, dataset == "test")    
    
    # remove dataset and target variable from test; remove dataset from train
    test2$is_duplicate <- NULL 
    test2$dataset <- NULL
    train2$dataset <- NULL
    
    
# convert target to factor and change levels to variable-friendly values (this is for randomForest)
    train2_no_factor <- train2
    
    train2$is_duplicate <- as.factor(train2$is_duplicate)
    levels(train2$is_duplicate)[levels(train2$is_duplicate) == 0] <- 'zero'
    levels(train2$is_duplicate)[levels(train2$is_duplicate) == 1] <- 'one'
    
    set.seed(1776)
    holdout_index <- caret::createDataPartition(y = train$is_duplicate, times = 1, p = 0.3, list = F)
    holdout <- train2[ holdout_index, ]    
    train3 <- train2[ -holdout_index, ]
    
    train3_no_factor <- train2_no_factor[ -holdout_index, ]
    
    nrow(train3) / nrow(train2)  # should be 0.7
    
    
# now build cross ref model using train3, test against holdout, then check LB with test2
    
    # Save RDS
    saveRDS(train3, file = 'processed_data/modeling_rf_001_train3.rds')
    saveRDS(test2, file = 'processed_data/modeling_rf_001_test2.rds')
    saveRDS(holdout, file = 'processed_data/modeling_rf_001_holdout.rds')
    
    # Also save as CSV
    write.csv(train3, file = 'processed_data/modeling_rf_001_train3.csv')
    write.csv(train3_no_factor, file = 'processed_data/modeling_rf_001_train3_no_factor.csv')
    write.csv(test2, file = 'processed_data/modeling_rf_001_test2.csv')
    write.csv(holdout, file = 'processed_data/modeling_rf_001_holdout.csv')
    
    # Read in RDS (can start here if we've already run script before)
    train3 <- readRDS(file = 'processed_data/modeling_rf_001_train3.rds')
    test2 <- readRDS(file = 'processed_data/modeling_rf_001_test2.rds')
    holdout <- readRDS(file = 'processed_data/modeling_rf_001_holdout.rds')
    
    
    # clean environment
    keep_objs <- c('train3', 'test2', 'holdout')
    rm(list = setdiff(ls(), keep_objs))
    source('r_scripts/r_script_config.R')
    gc()
    
    
    library(randomForest)
    library(ranger)
    library(caret)
    
    ?ranger
    
    set.seed(1776)
    sample_indx <- caret::createDataPartition(y = train3$is_duplicate, times = 1, p = 0.5, list = F)
    train4 <- train3[sample_indx,] 
    
    
    
    t_rgr <- Sys.time()
    rgr1 <- ranger::ranger(
        formula = is_duplicate ~.,
        data = train3,
        num.trees = 200,
        importance = 'impurity',
        write.forest = T,
        probability = T,
        min.node.size = 3,
        num.threads = 6,
        seed = 1776,
        verbose = T
    )
    t_rgr_elapsed <- Sys.time() - t_rgr
    
    
    rgr1
    
    
    
    preds <- predict(rgr1, holdout)
    
    head(preds$predictions)
    
    results <- cbind(holdout[, 'is_duplicate'], preds$predictions)
    
    library(ggplot2)
    library(reshape2)
    res2 <- reshape2::melt(results, id)
    
    ggplot(data = results, aes(x = one, fill = is_duplicate)) +
        geom_histogram(alpha = 0.3, position = 'identity') +
        xlab("Probability of 'is_duplicate' == 1") + 
        ggtitle("Local Holdout: Predicted Probability of Duplicate")
    
    
    test_pred <- predict(rgr1, test2)
    test_results <- data.frame(test_id = test$test_id, is_duplicate = test_pred$predictions[, 2])
    write.csv(test_results, file = 'submissions/modeling_rf_001_non_cv_rgr.csv', row.names = F)
    
    class(test_pred$predictions)
    
    
    
    
    # closest to how kaggle does it
    LogLossBinary = function(actual, predicted, eps = 1e-15) {
        predicted = pmin(pmax(predicted, eps), 1-eps)
        - (sum(actual * log(predicted) + (1 - actual) * log(1 - predicted))) / length(actual)
        }
    
    LogLossBinary(as.integer(results$is_duplicate), results$one)
    LogLossBinary(as.integer(results$is_duplicate), results$zero)
    
    
    
    
    
    
    # 
    # t_rf_model <- Sys.time()
    # rf_model <- caret::train(is_duplicate ~ .,
    #                              trControl = trainControl(method = 'repeatedcv', repeats = 5, 
    #                                 classProbs = T, summaryFunction = twoClassSummary),
    #                              data = train3,
    #                              method = 'rf',
    #                              ntrees = 50)
    # elapsed_t_rf_model <- Sys.time() - t_rf_model
    # 
    # 
    
    list.files('input')
    ss <- readr::read_csv(file = 'input/sample_submission.csv')
    
    
        