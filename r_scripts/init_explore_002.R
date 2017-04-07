

# init explore 002 -- Quora Competition:


# Quora dupes initial explore script:
library(rstudioapi)


# set wd to root of project
setwd(dirname(dirname(rstudioapi::getActiveDocumentContext()$path)))
source('r_scripts/r_script_config.R')


train <- readr::read_csv(file = 'input/train.csv')
test <- readr::read_csv(file = 'input/test.csv')
sampsub <- read.csv(file = 'input/sample_submission.csv', stringsAsFactors = F)


# transformations

#' var name legend:
#'     qtp: "quick text processing"
#'     reo: "reordering (sorting) of tokens"
#'     unq: "unique tokens"


# quick pre process for feature generation:
    
    t_preproc <- Sys.time()

    train$q1_qtp <- sapply(train$question1, quick_text_proc)
    train$q2_qtp <- sapply(train$question2, quick_text_proc)
    test$q1_qtp <- sapply(test$question1, quick_text_proc)
    test$q2_qtp <- sapply(test$question2, quick_text_proc)
    
    train$q1_qtp_reo <- sapply(train$question1, function(x) reorder_tokens(quick_text_proc(x)))
    train$q2_qtp_reo <- sapply(train$question2, function(x) reorder_tokens(quick_text_proc(x)))
    test$q1_qtp_reo <- sapply(test$question1, function(x) reorder_tokens(quick_text_proc(x)))
    test$q2_qtp_reo <- sapply(test$question2, function(x) reorder_tokens(quick_text_proc(x)))
    
    train$q1_qtp_reo_unq <- sapply(train$q1_qtp_reo, unique_tokens)
    train$q2_qtp_reo_unq <- sapply(train$q2_qtp_reo, unique_tokens)
    test$q1_qtp_reo_unq <- sapply(test$q1_qtp_reo, unique_tokens)
    test$q2_qtp_reo_unq <- sapply(test$q2_qtp_reo, unique_tokens)
    
    elapsed_preproc <- Sys.time() - t_preproc  # 5.56 hours (i think i made a mistake on timing this one)
    
    saveRDS(train, file = 'processed_data/train_01.rds')
    saveRDS(test, file = 'processed_data/test_01.rds')


# calculating string distance features:

    t_strdist <- Sys.time()
    
    # raw questions train
    train$feature_raw_osa <- str_dist_wrapper(train$question1, train$question2, 'osa')
    train$feature_raw_lv <- str_dist_wrapper(train$question1, train$question2, 'lv')
    train$feature_raw_dl <- str_dist_wrapper(train$question1, train$question2, 'dl')
    train$feature_raw_lcs <- str_dist_wrapper(train$question1, train$question2, 'lcs')
    train$feature_raw_cosine <- str_dist_wrapper(train$question1, train$question2, 'cosine')
    train$feature_raw_jaccard <- str_dist_wrapper(train$question1, train$question2, 'jaccard')
    train$feature_raw_jw <- str_dist_wrapper(train$question1, train$question2, 'jw')
    # raw questions test
    test$feature_raw_osa <- str_dist_wrapper(test$question1, test$question2, 'osa')
    test$feature_raw_lv <- str_dist_wrapper(test$question1, test$question2, 'lv')
    test$feature_raw_dl <- str_dist_wrapper(test$question1, test$question2, 'dl')
    test$feature_raw_lcs <- str_dist_wrapper(test$question1, test$question2, 'lcs')
    test$feature_raw_cosine <- str_dist_wrapper(test$question1, test$question2, 'cosine')
    test$feature_raw_jaccard <- str_dist_wrapper(test$question1, test$question2, 'jaccard')
    test$feature_raw_jw <- str_dist_wrapper(test$question1, test$question2, 'jw')
    
    
    # qtp train
    train$feature_qtp_osa <- str_dist_wrapper(train$q1_qtp, train$q2_qtp, 'osa')
    train$feature_qtp_lv <- str_dist_wrapper(train$q1_qtp, train$q2_qtp, 'lv')
    train$feature_qtp_dl <- str_dist_wrapper(train$q1_qtp, train$q2_qtp, 'dl')
    train$feature_qtp_lcs <- str_dist_wrapper(train$q1_qtp, train$q2_qtp, 'lcs')
    train$feature_qtp_cosine <- str_dist_wrapper(train$q1_qtp, train$q2_qtp, 'cosine')
    train$feature_qtp_jaccard <- str_dist_wrapper(train$q1_qtp, train$q2_qtp, 'jaccard')
    train$feature_qtp_jw <- str_dist_wrapper(train$q1_qtp, train$q2_qtp, 'jw')
    # qtp test
    test$feature_qtp_osa <- str_dist_wrapper(test$q1_qtp, test$q2_qtp, 'osa')
    test$feature_qtp_lv <- str_dist_wrapper(test$q1_qtp, test$q2_qtp, 'lv')
    test$feature_qtp_dl <- str_dist_wrapper(test$q1_qtp, test$q2_qtp, 'dl')
    test$feature_qtp_lcs <- str_dist_wrapper(test$q1_qtp, test$q2_qtp, 'lcs')
    test$feature_qtp_cosine <- str_dist_wrapper(test$q1_qtp, test$q2_qtp, 'cosine')
    test$feature_qtp_jaccard <- str_dist_wrapper(test$q1_qtp, test$q2_qtp, 'jaccard')
    test$feature_qtp_jw <- str_dist_wrapper(test$q1_qtp, test$q2_qtp, 'jw')
    
    
    # qtp_reo train
    train$feature_qtp_reo_osa <- str_dist_wrapper(train$q1_qtp_reo, train$q2_qtp_reo, 'osa')
    train$feature_qtp_reo_lv <- str_dist_wrapper(train$q1_qtp_reo, train$q2_qtp_reo, 'lv')
    train$feature_qtp_reo_dl <- str_dist_wrapper(train$q1_qtp_reo, train$q2_qtp_reo, 'dl')
    train$feature_qtp_reo_lcs <- str_dist_wrapper(train$q1_qtp_reo, train$q2_qtp_reo, 'lcs')
    train$feature_qtp_reo_cosine <- str_dist_wrapper(train$q1_qtp_reo, train$q2_qtp_reo, 'cosine')
    train$feature_qtp_reo_jaccard <- str_dist_wrapper(train$q1_qtp_reo, train$q2_qtp_reo, 'jaccard')
    train$feature_qtp_reo_jw <- str_dist_wrapper(train$q1_qtp_reo, train$q2_qtp_reo, 'jw')
    # qtp_reo test
    test$feature_qtp_reo_osa <- str_dist_wrapper(test$q1_qtp_reo, test$q2_qtp_reo, 'osa')
    test$feature_qtp_reo_lv <- str_dist_wrapper(test$q1_qtp_reo, test$q2_qtp_reo, 'lv')
    test$feature_qtp_reo_dl <- str_dist_wrapper(test$q1_qtp_reo, test$q2_qtp_reo, 'dl')
    test$feature_qtp_reo_lcs <- str_dist_wrapper(test$q1_qtp_reo, test$q2_qtp_reo, 'lcs')
    test$feature_qtp_reo_cosine <- str_dist_wrapper(test$q1_qtp_reo, test$q2_qtp_reo, 'cosine')
    test$feature_qtp_reo_jaccard <- str_dist_wrapper(test$q1_qtp_reo, test$q2_qtp_reo, 'jaccard')
    test$feature_qtp_reo_jw <- str_dist_wrapper(test$q1_qtp_reo, test$q2_qtp_reo, 'jw')
    
    
    # qtp_reo_unq train
    train$feature_q1_qtp_reo_unq_osa <- str_dist_wrapper(train$q1_qtp_reo_unq, train$q2_qtp_reo_unq, 'osa')
    train$feature_q1_qtp_reo_unq_lv <- str_dist_wrapper(train$q1_qtp_reo_unq, train$q2_qtp_reo_unq, 'lv')
    train$feature_q1_qtp_reo_unq_dl <- str_dist_wrapper(train$q1_qtp_reo_unq, train$q2_qtp_reo_unq, 'dl')
    train$feature_q1_qtp_reo_unq_lcs <- str_dist_wrapper(train$q1_qtp_reo_unq, train$q2_qtp_reo_unq, 'lcs')
    train$feature_q1_qtp_reo_unq_cosine <- str_dist_wrapper(train$q1_qtp_reo_unq, train$q2_qtp_reo_unq, 'cosine')
    train$feature_q1_qtp_reo_unq_jaccard <- str_dist_wrapper(train$q1_qtp_reo_unq, train$q2_qtp_reo_unq, 'jaccard')
    train$feature_q1_qtp_reo_unq_jw <- str_dist_wrapper(train$q1_qtp_reo_unq, train$q2_qtp_reo_unq, 'jw')
    # qtp_reo_unq test
    test$feature_q1_qtp_reo_unq_osa <- str_dist_wrapper(test$q1_qtp_reo_unq, test$q2_qtp_reo_unq, 'osa')
    test$feature_q1_qtp_reo_unq_lv <- str_dist_wrapper(test$q1_qtp_reo_unq, test$q2_qtp_reo_unq, 'lv')
    test$feature_q1_qtp_reo_unq_dl <- str_dist_wrapper(test$q1_qtp_reo_unq, test$q2_qtp_reo_unq, 'dl')
    test$feature_q1_qtp_reo_unq_lcs <- str_dist_wrapper(test$q1_qtp_reo_unq, test$q2_qtp_reo_unq, 'lcs')
    test$feature_q1_qtp_reo_unq_cosine <- str_dist_wrapper(test$q1_qtp_reo_unq, test$q2_qtp_reo_unq, 'cosine')
    test$feature_q1_qtp_reo_unq_jaccard <- str_dist_wrapper(test$q1_qtp_reo_unq, test$q2_qtp_reo_unq, 'jaccard')
    test$feature_q1_qtp_reo_unq_jw <- str_dist_wrapper(test$q1_qtp_reo_unq, test$q2_qtp_reo_unq, 'jw')
    
    elapsed_strdist <- Sys.time() - t_strdist # 1.07 hours
    
    
    t_write02 <- Sys.time()
    saveRDS(train, file = 'processed_data/train_02.rds')
    saveRDS(test, file = 'processed_data/test_02.rds')
    elapsed_write02 <- Sys.time() - t_write02

    
# going back to calculate hamming string dist (utilizing my trailing zeros function):
    
    stringdist::stringdist(trailing_zeros(x1, x2)[[1]], trailing_zeros(x1, x2)[[2]], 'hamming')
    
    # adding these as features while I'm thinking about it
    train$feature_q1_nchar <- nchar(train$question1)
    train$feature_q2_nchar <- nchar(train$question2)
    test$feature_q1_nchar <- nchar(test$question1)
    test$feature_q2_nchar <- nchar(test$question2)
    
    
    # not a feature, this is an intermediary step for hamming
    train$question1_trail0 <- dplyr::if_else(train$feature_q1_nchar < train$feature_q2_nchar, 
                                trailing_zeros(train$question1, train$question2)[[1]], train$question1)
    
    
    train$feature_raw_hamming <- str_dist_wrapper(trailing_zeros(train$question1, train$question2)[[1]],
                                        trailing_zeros(train$question1, train$question2)[[2]], 'hamming')
    
    
    
