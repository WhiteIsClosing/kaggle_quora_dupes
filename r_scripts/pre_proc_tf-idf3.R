

#' pre_proc_tf-idf3
#' 
#' So one thing I'm thinking about right now is how much I have neglected "test"... 
#' should those have been combined before creating features such as tf_idf? My vote is yes.



# load libs
    library(rstudioapi)
    library(tidyr)
    library(data.table)  # just for faster dplyr verbs
    library(dplyr)
    library(purrr); library(digest)  # i think digest has to be loaded or something?
    library(text2vec)

    
    
    
    
# set working dir
    setwd(dirname(dirname(rstudioapi::getActiveDocumentContext()$path)))

    

    
    
# load data:
    train <- read.csv(file="input/train.csv", stringsAsFactors = F)
    #train_docvec <- readRDS(file="processed_data/docvecs_train.rds")  # this is the huge one
    train_docvec <- readRDS(file="processed_data/docvecs_train_small.rds")
    # need to combine train and train_docvec somehow
    
        # # creating this for memory efficiency... one-off creation should be moved upstream
        # train_docvec_small <- train_docvec[, setdiff(names(train_docvec), 'data')]
        # setDF(train_docvec_small)
        # saveRDS(train_docvec_small, file='processed_data/docvecs_train_small.rds')
        
    
# mapping doc vecs to qid1 in train
    # make copy to be mapped into the qid1's in train
    qid1_docvecs <- train_docvec
    
    # rename data fields
    names(qid1_docvecs)[!grepl("^qid$", names(qid1_docvecs))] <- 
        paste0('qid1_', names(qid1_docvecs)[!grepl("^qid$", names(qid1_docvecs))])
    
    # rename the id
    names(qid1_docvecs)[grepl("^qid$", names(qid1_docvecs))] <- "qid1"
    
    # join to train
    setDT(train)
    setDT(qid1_docvecs)  # hmm... not sure how this will go with nested data being converted to data.table
    train2 <- merge(x=train, y=qid1_docvecs, by='qid1', all.x=T, all.y=F)
    
# mapping doc vecs to qid2
    # make copy
    qid2_docvecs <- train_docvec
    
    # rename data fields
    names(qid2_docvecs)[!grepl("^qid$", names(qid2_docvecs))] <- 
        paste0('qid2_', names(qid2_docvecs)[!grepl("^qid$", names(qid2_docvecs))])
    
    # rename the id
    names(qid2_docvecs)[grepl("^qid$", names(qid2_docvecs))] <- "qid2"
    setDT(qid2_docvecs)
    names(qid2_docvecs)
    train3 <- merge(x=train2, y=qid2_docvecs, by='qid2', all.x=T, all.y=F)    
    
    rm(qid1_docvecs, qid2_docvecs, train_docvec, train2)
    gc()
    
    head(train3)
    
    
# define a function to be used for mapping in the scores
    score_docvec_pair <- function(dv1, dv2) {
        
        # ok so this is returning as a matrix right now but I don't think it should?, just be numeric!
        # changing this below to be numeric -- need to do some regression testing here
        as.numeric(text2vec::sim2(x=t(matrix(dv1)), y=t(matrix(dv2)), method='cosine', norm='l2'))
    }
    
            # # testing it as a solo function call:
            # (test1 <- score_docvec_pair(train3$qid1_doc_vec_exp1[[1]], train3$qid2_doc_vec_exp1[[1]]))
            # class(test1)  # should this really be a matrix?
            # 
            # # test it as a map2 purrr function call
            # train3_small <- train3[1:5,]
            # (test2 <- purrr::map2(train3_small$qid1_doc_vec_exp1, train3_small$qid2_doc_vec_exp1, score_docvec_pair))
            # class(test2)                # now this is a list
            # class(test2 %>% unlist())   # and this is "numeric" (would have been a matrix I believe)
            
    
# we can't just use "map" and the function we defined above because some of the docvectors have "NULL" value
    sum(vapply(train3$qid1_doc_vec_exp1, is.null, TRUE))
    sum(vapply(train3$qid1_doc_vec_exp1, is.null, TRUE))
    
    
    
# safely will return a $results and $error for each result coming from the function
    # t_exp1_scores <- Sys.time()
    # exp1s <- purrr::map2(train3$qid1_doc_vec_exp1, train3$qid2_doc_vec_exp1, safely(score_docvec_pair))
    # (t_elap_exp1_scores <- Sys.time() - t_exp1_scores)  # should be less than 2 minutes on train
    # exp1.5s <- purrr::map2(train3$qid1_doc_vec_exp1.5, train3$qid2_doc_vec_exp1.5, safely(score_docvec_pair))
    # exp2s <- purrr::map2(train3$qid1_doc_vec_exp2, train3$qid2_doc_vec_exp2, safely(score_docvec_pair))
    
    setDT(train3)
    
    train3_small <- train3[1:1000,]
    
    # bingo.. this is what we needed to do all along -- this also takes ~ 1.5 min per each map2
    gc()
    t_test1 <- Sys.time()
    train4 <- train3 %>%
    # train4 <- train3_small %>%    # if I want to test on a smaller data.table
        
        # instead of "safely(score_docvec_pair)" lets try "possibly(score_docvec_pair, NA_real_)"
        mutate(tfidf1_dv_sim = map2(qid1_doc_vec_exp1, qid2_doc_vec_exp1, possibly(score_docvec_pair, NA_real_)))
    (t_elap_test1 <- Sys.time() - t_test1)
      
      
          
    # do the remaining ones
    t_test2 <- Sys.time() 
    train4 <- train4 %>%
               mutate(tfidf1.5_dv_sim = map2(qid1_doc_vec_exp1.5, qid2_doc_vec_exp1.5, possibly(score_docvec_pair, NA_real_)),
               tfidf2_dv_sim = map2(qid1_doc_vec_exp2, qid2_doc_vec_exp2, possibly(score_docvec_pair, NA_real_)),
               tfidf3_dv_sim = map2(qid1_doc_vec_exp3, qid2_doc_vec_exp3, possibly(score_docvec_pair, NA_real_)),
               tfidf4_dv_sim = map2(qid1_doc_vec_exp4, qid2_doc_vec_exp4, possibly(score_docvec_pair, NA_real_)),
               tfidf5_dv_sim = map2(qid1_doc_vec_exp5, qid2_doc_vec_exp5, possibly(score_docvec_pair, NA_real_)),
               tfidf9_dv_sim = map2(qid1_doc_vec_exp9, qid2_doc_vec_exp9, possibly(score_docvec_pair, NA_real_))
        )
    (t_elap_test2 <- Sys.time() - t_test2)
    
    
    # added these to the one above it so they are all three together
    # # doing two at a time here, eventually we will be able to do them all at the same time
    # train4 <- train4 %>%
    #     mutate(tfidf1.5_dv_sim = map2(qid1_doc_vec_exp1.5, qid2_doc_vec_exp1.5, safely(score_docvec_pair)),
    #            tfidf2_dv_sim = map2(qid1_doc_vec_exp2, qid2_doc_vec_exp2, safely(score_docvec_pair)))
    
    
    # need to determine if this step is still necessary
    train4 <- train4 %>%
        mutate(tfidf1_dv_sim = tfidf1_dv_sim %>% flatten_dbl,
               tfidf1.5_dv_sim = tfidf1.5_dv_sim %>% flatten_dbl,
               tfidf2_dv_sim = tfidf2_dv_sim %>% flatten_dbl,
               tfidf3_dv_sim = tfidf3_dv_sim %>% flatten_dbl,
               tfidf4_dv_sim = tfidf4_dv_sim %>% flatten_dbl,
               tfidf5_dv_sim = tfidf5_dv_sim %>% flatten_dbl,
               tfidf9_dv_sim = tfidf9_dv_sim %>% flatten_dbl)
    
    
    
    train4$tfidf1_dv_sim %>% head()
    train4$tdvs_1 %>% head()
    
    
    
# quick exploratory plots to see how the scores are doing:
    library(ggplot2)
    
    train4$is_dup_fac <- as.factor(train4$is_duplicate)
    g1 <- ggplot(data=train4, aes(x=tfidf1_dv_sim, fill=is_dup_fac)) +
        geom_histogram(alpha=0.4, position='identity')
    plot(g1)
    
    train4$tfidf1_dv_sim %>% unlist(recursive=T) %>% head() 
    
    train4_sub <- train4[, c("id", "qid1", "qid2",  names(train4)[!grepl("^qid", names(train4))]   )  ] 
    
    saveRDS(train4, file='processed_data/train_docvector_sim_01.rds')
    saveRDS(train4_sub, file='processed_data/train_docvector_sim_01_sub.rds')
    
    
    
    
    
# ================  inspecting results and baselining ==================================================================
    
    # load libs
    library(rstudioapi)
    library(tidyr)
    library(data.table)  # just for faster dplyr verbs
    library(dplyr)
    library(purrr); library(digest)  # i think digest has to be loaded or something?
    library(text2vec)
    setwd(dirname(dirname(rstudioapi::getActiveDocumentContext()$path)))
    results <- readRDS(file='processed_data/train_docvector_sim_01_sub.rds')
    
    
    
    over_pred <- results[results$tfidf1_dv_sim > 0.999 & results$is_duplicate == 0,]
    
    
        # super quick model benchmarking:
        library(caret)
        results_preds_tar <- results[, c("is_duplicate", names(results)[grepl("^tfidf", names(results))])]
        set.seed(1)
        train_indx <- caret::createDataPartition(y=results_preds_tar$is_duplicate, times=1, p=0.7, list=F)
        results_preds_tar1 <- results_preds_tar[train_indx,]
        results_preds_tar2 <- results_preds_tar[-train_indx,]
        names(results_preds_tar)    
        
        # here, "is_duplicate" is an integer
        quicklm <- lm(formula = is_duplicate ~ ., data=results_preds_tar1)      
        summary(quicklm)    
        results_preds_tar2$preds_lm <- predict(quicklm, newdata=results_preds_tar2)
        head(results_preds_tar2$preds_lm)
        
        ggplot(results_preds_tar2, aes(x=preds_lm, fill=as.factor(is_duplicate))) +
            geom_histogram(alpha=0.3, position='identity')
            
        
        
        
        # quick glm
        quickglm <- glm(formula = as.factor(is_duplicate) ~ tfidf1_dv_sim, data=results_preds_tar1, family = binomial)
        summary(quickglm)    
        results_preds_tar2$preds_glm <- predict(quickglm, newdata=results_preds_tar2, type='response')
        head(results_preds_tar2$preds_glm)    
        range((results_preds_tar2$preds_glm), na.rm=T)    
        
        ggplot(results_preds_tar2, aes(x=preds_glm, fill=as.factor(is_duplicate))) +
            geom_histogram(alpha=0.3, position='identity')
        
        
        
        LogLoss<-function(act, pred)
        {
            eps = 1e-15;
            nr = length(pred)
            pred = matrix(sapply( pred, function(x) max(eps,x)), nrow = nr) 
            pred = matrix(sapply( pred, function(x) min(1-eps,x)), nrow = nr)
            ll = sum(act*log(pred) + (1-act)*log(1-pred))
            ll = ll * -1/(length(act)) 
            return(ll);
        }
        
    
        LogLoss(results_preds_tar2$is_duplicate[!is.na(results_preds_tar2$preds_glm)], 
                results_preds_tar2$preds_glm[!is.na(results_preds_tar2$preds_glm)] )
        
        