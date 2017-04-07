

library(rstudioapi)
library(dplyr)
#library(data.table)
library(tm)
library(SnowballC)
library(readr)
#library(topicmodels)
library(stringdist)
#library(parallel)
#library(foreach)
library(caret)
library(xgboost)



# this might be a bit more efficient than "trimws()"
    trim <- function(x) {
        return(gsub("^\\s+|\\s+$", "", x))
    }


# quick and dirty text pre-processing for better calculation of string distances
    quick_text_proc <- function(p_string) {
        p_string <-  tm::removeNumbers(p_string)
        p_string <- tm::removePunctuation(p_string)
        p_string <- tm::removeWords(p_string, stopwords("english"))
        p_string <- tolower(p_string)
        p_string <- SnowballC::wordStem(p_string)
        p_string <-  gsub("\\s+", " ", p_string)
        return(p_string)
    }


# reorder tokens into alphabetical order -- this is more for string similarity than meaning
    reorder_tokens <- function(p_string) {
        
        # inside out:
        #  split string into tokens by white space -- unlist this
        #  sort it
        #  collapse it back into a single string separated by spaces
        return(paste0(sort(unlist(strsplit(p_string, ' '))), collapse=' '))
    }


# splits by white space, filters to unique tokens, then collapses with single space separator
    unique_tokens <- function(p_string) {
        return(paste0(unique(unlist(strsplit(p_string, ' '))), collapse = ' '))
    }


# string distance wrapper
    str_dist_wrapper <- function(p_vec1, p_vec2, p_method) {
        
        # fail if vectors aren't same length
        if(length(p_vec1) != length(p_vec2)) {stop("first two arguments should be character vectors of the same length")}
        results <- numeric(length(p_vec1))
        
        # loop, capturing results of each loop
        for(i in 1:nrow(train)) {
            results[i] <- stringdist::stringdist(a = p_vec1[i], b = p_vec2[i], method = p_method)
        }
        
        return(results)
    }


# add leading zeros so the strings are the same length
# make sure to do this before removing numbers (we add zeros)
    leading_zeros <- function(p_string1, p_string2) {
        
        nps1 <- nchar(p_string1)
        nps2 <- nchar(p_string2)
        
        if(nps1 < nps2) {
            # if first string is smaller than second string, concat zeros to first string
            p_string1 <- paste0(paste0(rep('0', nps2 - nps1), collapse = ''), p_string1, collapse = '')
        } else {
            # if second string is smaller than first string, concat zeros to second string
            p_string2 <- paste0(paste0(rep('0', nps1 - nps2), collapse = ''), p_string2, collapse = '')
        }
        
        return(c(p_string1, p_string2))
    }


# same as leading zeros, but for trailing zeros if we want to try that (should be the same)
    trailing_zeros <- function(p_string1, p_string2) {
        nps1 <- nchar(p_string1)
        nps2 <- nchar(p_string2)
        if(nps1 < nps2) {
            p_string1 <- paste0(p_string1, paste0(rep('0', nps2 - nps1), collapse = ''), collapse = '')
        } else {
            p_string2 <- paste0(p_string2, paste0(rep('0', nps1 - nps2), collapse = ''), collapse = '')
        }
        return(c(p_string1, p_string2))
    }






