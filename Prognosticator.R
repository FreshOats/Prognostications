library(quanteda)
library(data.table)
library(readtext)
library(stringr)
library(dplyr)

## Open the file that has the n-grams and their associated probabilites
nGrams <- fread("filtered.txt", col.names = c("Phrase", "Prediction", "Probability"), data.table = T, stringsAsFactors = F, encoding = "UTF-8")
    lambda <- 0.4
    setkey(nGrams, Phrase)


## This tokenizes the input string - the highest number of words that are searched is 4, since the largest n-gram sampled was a 5-gram, but the fifth word is stripped from the reference phrase, the largest is should sample is a 4-gram. The else condition lowers the number incrementally, removing the first word from the input string until there are only the last 4 words remaining.
input.Axe <- function(input.String, n = 4) {
    chops <- tokens(input.String, what = "word", remove_numbers = T, remove_punct = T, remove_symbols = T, 
                    remove_twitter = T, remove_hyphens = T, remove_url = T, remove_separators = T, ngrams = n) %>% 
        tokens_tolower() %>%
        unlist(use.names = F) %>%
        as.data.table()
    if (chops[, .N] > 0) return(chops[.N])
    else return(input.Axe(input.String, n - 1))
    }

  
# This now determines the number of words in the input string by counting the spaces between words. If the number of tokens is exactly 1, then the function looks for any characters from the back of each word to find the nearest match. Otherwise it searches for strings that do not start with an underscore but end with an underscore and subsequently remove the final underscore from the input phrase.     
nGram.Axe <- function(input.Phrase) {
    if (input.Phrase == " ") return(0)
    else {nTokens <- str_count(input.Phrase, "_") + 1} 
    
    if (nTokens == 1) return(sub(".", "",input.Phrase))
    else return(sub("^[^_]*_", "", input.Phrase))
    }


## The Stupid Backoff looks at the input string and scans the n-gram table for matching strings. If a result is returned, the model returns the indexed matching results and scales the probability based on the next lower n-gram. If there is no reult returned, the string is returned to the Axe, which then reduces the ngram by 1. This is based off the Stupid Backoff Model. 
Backoff <- function(input.Phrase, lambda = 0.4) {
    result <- nGrams[Phrase == input.Phrase]
    nTokens <- str_count(input.Phrase, "_") + 1
    ## Four-Grams
    if (nrow(result) > 0 & nTokens == 4) {
        nResult <- mutate(result, Probability = 100 * Probability)
        
        if (nrow(nResult) < 8) {
            input.Phrase <- nGram.Axe(input.Phrase)
            lessone.Result <- mutate(nGrams[Phrase == input.Phrase], Probability = 100 * lambda * Probability)
            lessone.Result <- lessone.Result[!(lessone.Result$Prediction %in% nResult$Prediction) ,]
            nResult <- rbind(nResult, lessone.Result)
        }
        
        if (nrow(nResult) < 8) {
            input.Phrase <- nGram.Axe(input.Phrase)
            lesstwo.Result <- mutate(nGrams[Phrase == input.Phrase], Probability = 100 * lambda^2 * Probability)
            lesstwo.Result <- lesstwo.Result[!(lesstwo.Result$Prediction %in% nResult$Prediction) ,]
            nResult <- rbind(nResult, lesstwo.Result)
        }
        
        if (nrow(nResult) < 8) {
            input.Phrase <- nGram.Axe(input.Phrase)
            lessthree.Result <- mutate(nGrams[Phrase == input.Phrase], Probability = 100 * lambda^3 * Probability)
            lessthree.Result <- lessthree.Result[!(lessthree.Result$Prediction %in% nResult$Prediction) ,]
            nResult <- rbind(nResult, lessthree.Result)
        }
        
        return(nResult)
    }
    
    ## Three Grams
    else if (nrow(result) > 0 & nTokens == 3) {
        nResult <- mutate(result, Probability = 100 * Probability)
        
        if (nrow(nResult) < 8) {
            input.Phrase <- nGram.Axe(input.Phrase)
            lessone.Result <- mutate(nGrams[Phrase == input.Phrase], Probability = 100 * lambda * Probability)
            lessone.Result <- lessone.Result[!(lessone.Result$Prediction %in% nResult$Prediction) ,]
            nResult <- rbind(nResult, lessone.Result)
        }
        
        if (nrow(nResult) < 8) {
            input.Phrase <- nGram.Axe(input.Phrase)
            lesstwo.Result <- mutate(nGrams[Phrase == input.Phrase], Probability = 100 * lambda^2 * Probability)
            lesstwo.Result <- lesstwo.Result[!(lesstwo.Result$Prediction %in% nResult$Prediction) ,]
            nResult <- rbind(nResult, lesstwo.Result)
        }
        
        return(nResult)
    }
    
    ## Two Grams
    else if (nrow(result) > 0 & nTokens == 2) {   
        nResult <- mutate(result, Probability = 100 * Probability)
        
        if (nrow(nResult) < 8) {
            input.Phrase <- nGram.Axe(input.Phrase)
            lessone.Result <- mutate(nGrams[Phrase == input.Phrase], Probability = 100 * lambda * Probability)
            lessone.Result <- lessone.Result[!(lessone.Result$Prediction %in% nResult$Prediction) ,]
            nResult <- rbind(nResult, lessone.Result)
        }
        return(nResult)
    }
    
    ## One Grams
    else if (nrow(result) > 0 & nTokens == 1) {   
        nResult <- mutate(result, Probability = 100 * Probability)
        return(nResult)
    } 
        
        
    else return(Backoff(nGram.Axe(input.Phrase)))
}


## This is the actual predictor for the front end. The string is entered as an input. This is then tokenized as a decreasing series of ngrams. After the tokenization, the ngram is passed to the Backoff function, which searches for matches and then incrementally backed off and returns predictions based on the highest ngram
Prognosticate <- function(input.String) {
    nGram <- input.Axe(input.String)
    prognostication <- Backoff(nGram)
    prognostication$Probability <- round(prognostication$Probability, digits = 3)
    prognostication$Probability <- paste0(prognostication$Probability, " %")
    return(prognostication)
}
