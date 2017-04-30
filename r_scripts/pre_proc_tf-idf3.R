

#' pre_proc_tf-idf3
#' 
#' So one thing I'm thinking about right now is how much I have neglected "test"... 
#' should those have been combined before creating features such as tf_idf? My vote is yes.



# load libs
    library(rstudioapi)
    library(tidyr)
    library(data.table)  # just for faster dplyr verbs
    library(dplyr)
    library(purrr)
    library(text2vec)

# set working dir
    setwd(dirname(dirname(rstudioapi::getActiveDocumentContext()$path)))


# load data:
    train <- read.csv(file="input/train.csv", stringsAsFactors = F)
    train_docvec <- readRDS(file="processed_data/docvecs_train.rds")
    # need to combine train and train_docvec somehow
    

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
    
    
# mapping doc vecs
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
    
    head(train3)
    
    class(train3$qid1_doc_vec_exp1[[1]])
    head(train3)
    
    score_docvec_pair <- function(dv1, dv2) {
        text2vec::sim2(x=t(matrix(dv1)), y=t(matrix(dv2)), method='cosine', norm='l2')
    }
    
    
    sum(is.null(train3$qid1_doc_vec_exp1))
    qid_indx <- grep("^qid", names(train3))
    sapply(train3[,qid_indx], function(x) sum(is.null(x)))
    
    names(train3)
    
    # this fails
    train3 <- 
        train3 %>%
        mutate(tdv1_score = map2(qid1_doc_vec_exp1, qid2_doc_vec_exp1, score_docvec_pair)) %>%
        mutate(tdv1.5_score = map2(qid1_doc_vec_exp1.5, qid2_doc_vec_exp1.5, score_docvec_pair)) %>%
        mutate(tdv2_score = map2(qid1_doc_vec_exp2, qid2_doc_vec_exp2, score_docvec_pair))
    
    
    # safely will return a $results and $error for each result coming from the function
    exp1s <- map2(train3$qid1_doc_vec_exp1, train3$qid2_doc_vec_exp1, safely(score_docvec_pair))
    
    # transpose will 
    exp1s_t <- purrr::transpose(exp1s)
    exp1s_t$error %>% unique() %>% is.null()
    exp1_errors <- which(!vapply(exp1s_t$error, is.null, TRUE))
    exp1s_t$result[exp1_errors]
    sum(sapply(train3$qid1_data, is.null))
    train3_exp1_errors <- train3[exp1_errors,]
    exp1s_t$result %>% head() %>% unique()
    
    exp1.5s_t <- purrr::transpose(exp1.5s)
    exp1.5s_t$error %>% head() %>% unique()
    exp1.5s_t$result %>% head() %>% unique()
    
    length(exp1s_t$result)
    
    class(exp1s_t$result[[1]])
    
    
            # procedural testing with randomness
            i <- floor(runif(1, 1, nrow(train3)))
            x1 <- train3$qid1_doc_vec_exp1[[i]]
            x2 <- train3$qid2_doc_vec_exp1[[i]]    
            text2vec::sim2(x=t(matrix(x1)), y=t(matrix(x2)), method="cosine", norm="l2") ==
                text2vec::sim2(x=t(matrix(x2)), y=t(matrix(x1)), method="cosine", norm="l2")
            score_docvec_pair(train3$qid1_doc_vec_exp1[[i]], train3$qid2_doc_vec_exp1[[i]])
            train3$is_duplicate[[i]]
            
    
    
    
    