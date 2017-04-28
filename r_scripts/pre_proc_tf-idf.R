

# first attempt at some tf-idf type analysis in quora dupes -- will be beneficial in many
# text-analytics initiatives at work as well
# I'm seeing some potentially huge encoding issues here



# setup:
    library(rstudioapi)
    setwd(dirname(dirname(rstudioapi::getActiveDocumentContext()$path)))
    source("r_scripts/r_script_config.R")

    
    
    
# read data:
    list.files('input')
    train <- read.csv(file="input/train.csv", stringsAsFactors = F)
    
    # should tf-idf be calculated on train or the combination of train and test?
    
    
    
    
    
    
# set up for tf-idf
    library(dplyr)
    
    # tons of overlap
    sum(train$qid1 %in% train$qid2)
    sum(train$qid2 %in% train$qid1)

    
    # combine all questtions in train
    train_qs_df <- data.frame(qid=c(train$qid1, train$qid2), question=c(train$question1, train$question2), stringsAsFactors = F)
    train_qs_df <- arrange(train_qs_df, qid)
    train_qs_df <- train_qs_df[!duplicated(train_qs_df),]            # make sure it is unique
    train_qs_df$question <- tm::removeNumbers(train_qs_df$question)  # get the numbers out (for now)
    
    
    
    
    # for these two below, "q" does not represent "q1" and "q2" but rather the same data going through a transformation
    # I no longer have these by q1 / q2, I cbind'd those together into just a "q"
    
    train_words_by_q <- train_qs_df %>%
        # "word" is the name I choose, "question" is what I want split by "qid"
        tidytext::unnest_tokens(word, question)  
        head(train_words_by_q)
        
    
    train_words_by_q2 <- train_words_by_q %>%
        # find "tf" of "tf-idf" (term frequency within each qid)
        dplyr::count(word, qid, sort=T) %>%
        ungroup()
        
    
    
    # find total words per each qid -- standard dplyr
    train_total_words_by_qid <- train_words_by_q2 %>%
        group_by(qid) %>%
        summarise(total=sum(n))
    
    
    # join [total words per question] to the [total count of each word per question]
    #train_words_by_q3 <- merge(x=train_words_by_q2, y=train_total_words_by_qid, by='qid', all=T)  # how I normally do it
    train_words_by_q3 <- dplyr::left_join(train_words_by_q2, train_total_words_by_qid)             # tidy way -- uses all matching col names
    
        
    
    # going to try passing in train_words_by_q2 instead to see if it can do it -- it can...
    train_words_by_q4 <- train_words_by_q3 %>%
        # args are [word=name of word], [qid=what we're grouped by], [n=count of word per doc]
        # kind of weird we don't specify the total?
        tidytext::bind_tf_idf(word, qid, n)
    
    
    train_tf_idf <- arrange(train_words_by_q4, qid, desc(tf_idf))  %>%
        group_by(qid) %>%
        mutate(tf_idf_rank_within_qid = row_number())
    
    
    # ok so now I'm thinking the next step would be to cross reference this with whether it exists in a word vector or not
    # now we'll have many different word vectors, so this will be highly iterative
    
    # write it out now
    saveRDS(train_tf_idf, file='processed_data/tf_idf_train_01.rds')
            
    
    

    
# # example of tf-idf ------------------------------------------ 
#     # http://juliasilge.com/blog/Term-Frequency-tf-idf/
# 
#     library(dplyr)
#     library(janeaustenr)
#     library(tidytext)
#     abooks <- austen_books()
#     
#     # this is organized by sentence / book (in different columns)
#     head(abooks[30:60,], 30)
#     
#     
#     # going to break this out into it's multiple steps to see what its doing
#     # book_words <- abooks %>%
#     #     tidytext::unnest_tokens(word, text) %>%
#     #     dplyr::count(book, word, sort=T) %>%
#     #     ungroup()
#     
#     
#     # ok so this splits each and every word out according to book/category
#     book_words1 <- abooks %>%
#         # "text" are sentences and "book" is the category or document name
#         tidytext::unnest_tokens(word, text)  
#     
#     
#     book_words2 <- book_words1 %>%
#         # count of each word in each doc (I believe this is the "tf" in "tf-idf")
#         dplyr::count(book, word, sort=T)
#     
#     book_words3 <- book_words2 %>%
#         # Not sure how necessary this is, but I'm cool with it
#         ungroup()
# 
#     
#     # now find total words within each book
#     total_words <- book_words3 %>% 
#         group_by(book) %>% 
#         summarize(total = sum(n))
#     
        
        
    