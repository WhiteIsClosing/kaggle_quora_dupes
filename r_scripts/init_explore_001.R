

# Quora dupes initial explore script:
library(rstudioapi)


# set wd to root of project
setwd(dirname(dirname(rstudioapi::getActiveDocumentContext()$path)))
source('r_scripts/r_script_config.R')


train <- readr::read_csv(file = 'input/train.csv')
test <- readr::read_csv(file = 'input/test.csv')
sampsub <- read.csv(file = 'input/sample_submission.csv', stringsAsFactors = F)






# quick train inspect:
    
    sapply(train, function(x) sum(duplicated(x)))

    train$question1 <- sapply(train$question1, trimws)
    train$question2 <- sapply(train$question2, trimws)
    
    # two observations have a blank for "question2" field (turns into NA after trimws)
    sapply(train, function(x) sum(x == ''))
    sapply(train, function(x) sum(is.na(x)))
    
    
    # can join question content to these if interested 
    train_qid1_freq <- train$qid1 %>% table() %>% data.frame() %>% arrange(desc(Freq))
    train_qid2_freq <- train$qid2 %>% table() %>% data.frame() %>% arrange(desc(Freq))
    
    # no duplicate combinations of questions in train
    train_qid1_qid2_freq <- paste0(train$qid1, '-', train$qid2) %>% 
        table() %>% data.frame() %>% arrange(desc(Freq))
    

# quick test inspect:
    
    sapply(test, function(x) sum(duplicated(x)))
    
    test$question1 <- sapply(test$question1, trim)
    test$question2 <- sapply(test$question2, trim)
    
    sapply(test, function(x) sum(x == ''))
    sapply(test, function(x) sum(is.na(x)))
    
    head(test)
    
    
    
    # several duplicates
    test_q1_q2_freq <- paste0(test$question1, '|', test$question2) %>% table() %>% data.frame() %>%
        arrange(desc(Freq))
        
    
    test_q1_ids <- as.numeric(as.factor(test$question1))
    # t <- Sys.time()
    test_q2_ids <- as.numeric(as.factor(test$question2))
    test$test_q1_ids <- test_q1_ids
    test$test_q2_ids <- test_q2_ids
    test$combined_key <- paste0(test$test_q1_ids, '-', test$test_q2_ids)
    # elapsed <- Sys.time() - t
    # print(elapsed)
    
    test_dupe_id_combos <-  test$combined_key %>% table() %>% data.frame() %>% 
        arrange(desc(Freq)) %>% filter(Freq > 1)
    
    table(test_dupe_id_combos$Freq)
    
    paste0(round(nrow(test_dupe_id_combos) / (sum(test_dupe_id_combos$Freq) + nrow(test)) * 100, digits = 2), '%')
