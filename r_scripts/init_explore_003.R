

# init explore 3 -- feature generation for more basic word matching stuff


# Quora dupes initial explore script:
library(rstudioapi)


# set wd to root of project
setwd(dirname(dirname(rstudioapi::getActiveDocumentContext()$path)))
source('r_scripts/r_script_config.R')


train <- readr::read_csv(file = 'input/train.csv')
test <- readr::read_csv(file = 'input/test.csv')


x1 <- train$question1[2]
x2 <- train$question2[2]

x1; x2

# What do I want to do? Plan it before coding it....
#' I want to have both questions stripped to unique non-stop words
#' Then count how many of the unique non-stop words can be found in BOTH
#' divided by the total number of unique stop words

x1 <- tm::removeNumbers(x1)
x1 <- tm::removePunctuation(x1)
x1 <- tm::removeWords(x1, stopwords(kind = 'en'))
x1 <- tolower(x1)
x1 <-  gsub("\\s+", " ", x1)
x1 <- unlist(strsplit(x1, ' '))
x1


x2 <- tm::removeNumbers(x2)
x2 <- tm::removePunctuation(x2)
x2 <- tm::removeWords(x2, stopwords(kind = 'en'))
x2 <- tolower(x2)
x2 <-  gsub("\\s+", " ", x2)
x2 <- unlist(strsplit(x2, ' '))
x2

un <- dplyr::union(x1, x2)
inter <- dplyr::intersect(x1, x2)

length(inter) / length(un)  # ratio of unique words in common

un; inter

# much of the intention of the question should be based on which of these key question words exist
# example, there is a big difference between "what is a black hole" and "how does a black hole ____"
q_words <- c('who', 'what', 'when', 'where', 'how')
tm::removeWords(q_words, stopwords(kind = 'en'))

