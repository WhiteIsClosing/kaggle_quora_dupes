

# alright, this is where I want to start investigating plans for combining word_vectors and tf_idf scores

# I'm starting to understand why that research paper about tf-idf / wordvectors was concerned about
# fixed-number of words... maybe we can append zeros for ranks for docs with less than n number of words?

# great primer on purrr (and much of tidyverse):
# http://ijlyttle.github.io/isugg_purrr/presentation.html#

# deep dive into "double nesting" with purrr:
# http://stackoverflow.com/questions/39228502/double-nesting-in-the-tidyverse


t_all <- Sys.time()


library(rstudioapi)
setwd(dirname(dirname(rstudioapi::getActiveDocumentContext()$path)))


library(tidyr)
library(data.table)  # just for faster dplyr verbs
library(dplyr)



# read in raw data, word vector, tf-idf scores
train <- read.csv(file="input/train.csv", stringsAsFactors = F)
glove <- readRDS("LARGE_FILES_word_vectors/glove_6B_50D_processed.rds")
train_tfidf <- readRDS("processed_data/tf_idf_train_01.rds")




# limit train_tfidf to only what is found in glove and vice versa?
train_tfidf2 <- train_tfidf[train_tfidf$word %in% names(glove),]
glove_words_to_keep <- names(glove)[names(glove) %in% train_tfidf2$word]
glove2 <- glove[ ,  glove_words_to_keep]


# put glove in a format that is easier to work with (vector can be nested with tidyr)
glove3 <- data.frame(t(glove2))
glove3$word <- glove_words_to_keep
glove4 <- glove3 %>%
    nest(-word)


        # I can do mathematical operations on these vectors like so:
        unlist(glove4$data[[1]]) + 100
        glove4$data[[1]] + 100
        glove4[['data']][[1]] + 200
        
        # temp, I don't think we need these, but I might change my mind later:
        rm(glove, train_tfidf)


# might need to also re-do those ranks now?
train_tfidf2$tf_idf_rank_within_qid %>% head()
train_tfidf2 <- train_tfidf2 %>% 
    dplyr::group_by(qid) %>%
    mutate(tf_idf_rank_within_qid2=row_number())



# join our nested glove vectors to our original data
setDT(train_tfidf2)
setDT(glove4)
tfidf_vector <- merge(x=train_tfidf2, y=glove4, by="word", all.x=T, all.y=F)
setDF(tfidf_vector); setDF(glove4); setDF(train_tfidf2)



        # PSA: Drink your milk, eat your veggies, and DO YOUR TESTING:
        # sample random words from glove4
        glove4[['word']][sample(1:nrow(glove4), 1)]   # this is fun, my random word grabber
        # give me the "data" column from glove4, but then filter it down to just the
        # index value where the "word" column is equal to the value "tanzanian"
        this_g4 <- glove4[['data']][which(glove4[['word']] == "tanzanian")]
        
        
        # same thing as above: grab me the index of where the value "tanzanian" appears in the "word" 
        # field and use that index to grab the "data" field in "tfidf_vector"
        this_tfvec <- tfidf_vector[['data']][which(tfidf_vector[['word']] == 'tanzanian')[[1]]]
        
        class(this_tfvec)
        class(this_g4)
        
        # the [[1]] is because we were returned lists...    
        this_g4[[1]] == this_tfvec[[1]]                               # should be a lot of TRUEs
        sum(this_g4[[1]] == this_tfvec[[1]]) == length(this_g4[[1]])  # should be TRUE
    
        # always remember to clean up after yourself!
        rm(this_g4, this_tfvec)
    

# ok, now we're ready for some feature creation using our vectors
tfidf_vector <- arrange(tfidf_vector, qid, tf_idf_rank_within_qid2)

tfidf_vector2 <- tfidf_vector %>%
    nest(-qid)



names(tfidf_vector2$data[[1]])             # second level nest
names(tfidf_vector2$data[[1]]$data[[1]])   # first level nest




    
    # Compress all w2v for each word in a doc into single d2v vector (weighted by tf-idf) 
    my_d2v <- function(nestdata2, tfidf_exp_weight, wvdim=50) {
        
            # # for rapid dev (this will be a parameter passed in):
            # nestdata2 <- tfidf_vector2$data  # so this takes a min
            # # tfidf exponent weight (suggested values, 1 (no change from tfidf), 1.3, 1.5, 2)
            # tfidf_exp_weight <- 1.3   # this will be used to "boost" the weight of higher value words
        
        tfidf_exp_weight <- 1
        
        # from data, extract
        tfidf <- nestdata2[[1]]$tf_idf
        wv <- nestdata2[[1]]$data
        wv_mat <- matrix(data=unlist(wv), ncol=ncol(wv[[1]]))
    
        # weighted average using tfidf as the weights
        d2v <- apply(X=wv_mat, MARGIN=2, FUN=weighted.mean, (tfidf^2))
        return(d2v)
    }
    
    tfidf_vector3 <- tfidf_vector2 %>%
        mutate(d2v = map(data, my_dv2, 1))
    
    
        # test
        f2(tfidf_vector2$data)
        
    
    # so I have these tf_idf values for a given question:
    miex_tfidf <- tfidf_vector$tf_idf[tfidf_vector$qid == 1]
    print(miex_tfidf)
    
    # and these tf_idf rankings:
    miex_tfidf_ranks <- tfidf_vector$tf_idf_rank_within_qid2[tfidf_vector$qid == 1]
    print(miex_tfidf_ranks)
    
    # and these word vector values for each:
    miex_wv <- tfidf_vector[['data']][tfidf_vector$qid == 1]
    miex_wv  # this one is a bit more complex
    
    # pretend for a minute that these are our tfidf:
    (fake_tfidf <- seq(1, 120, 10))
    miex_wv_mat <- matrix(data=unlist(miex_wv), ncol=50)
    
    miex_wv_mat
    miex_tfidf
    
    miex_wv_tfidf_weighted <- (miex_wv_mat * miex_tfidf)
    (miex_q1_vec_median <- apply(miex_wv_tfidf_weighted, 2, median))
    (miex_q1_vec_mean <- apply(miex_wv_tfidf_weighted, 2, mean))
    
    head(tfidf_vector)
    
    
    # functionizing the micro example
    f1.mean <- function(wv_df, tfidf_df, wv_dim) {
        
        tfidf_df <- miex_tfidf
        
        
    }
    
    
    
    
    head(tfidf_vector)
    
    
    ?sweep
    ?apply
    
    apply(miex_wv_mat, 1, `+`, fake_tfidf)
    
    miex_wv_mat + fake_tfidf
    
    miex_wv * fake_tfidf
    
    class(tfidf_vector$data[[1]])
    
    
    # catchall 
    
    
    
    
    
    
    
    
    
    
    
# this should always be at the end to monitor speed
t_elap_all <- Sys.time() - t_all
print(t_elap_all)  # 32 seconds, that shit is quick man


