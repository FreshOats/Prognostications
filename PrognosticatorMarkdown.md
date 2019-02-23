---
title: "Prognosticator Functions"
author: "Justin Papreck"
date: "February 22, 2019"
output: 
    html_document:
        keep_md: true
---



# Pipeline
1. Read finalized ngram probability file
2. Tokenize the input string, cut the input string down to 4-grams or less
3. Determine if any of the words in the input string have unknown words
4. Process the input string, look for anything starting with 4-grams
5. Final Prognosticate Model is defined using previous nested functions

### 1. Read finalized ngram probability file

Open the file that has the n-grams and the associated probabilities. The phrase is the n - 1 gram, since the prediction term is subtracted. The associated probability is then calculated with the associated predictive terms. 
    

```r
    library(readtext)
    library(stringr)
    library(dplyr)
    library(quanteda)
    library(data.table)
    nGrams <- fread("filtered.txt", col.names = c("Phrase", "Prediction", "Probability"), data.table = T, stringsAsFactors = F, encoding = "UTF-8")
    lambda <- 0.4
    setkey(nGrams, Phrase)
```

### 2. Tokenize the input string, cut the input string down to 4-grams or less

This tokenizes the input string - the highest number of words that are searched is 4, since the largest n-gram sampled was a 5-gram, but the fifth word is stripped from the reference phrase, the largest is should sample is a 4-gram. The else condition lowers the number incrementally, removing the first word from the input string until there are only the last 4 words remaining.

```r
    input.Axe <- function(input.String, n = 4) {
    chops <- tokens(input.String, what = "word", remove_numbers = T, remove_punct = T, remove_symbols = T, 
                    remove_twitter = T, remove_hyphens = T, remove_url = T, remove_separators = T, ngrams = n) %>% 
        tokens_tolower() %>%
        unlist(use.names = F) %>%
        as.data.table()
    if (chops[, .N] > 0) return(chops[.N])
    else return(input.Axe(input.String, n - 1))
    }
```

### 3. Determine if any of the words in the input string have unknown words

This now determines the number of words in the input string by counting the spaces between words. If the number of tokens is exactly 1, then the function looks for any characters from the back of each word to find the nearest match. Otherwise it searches for strings that do not start with an underscore but end with an underscore and subsequently remove the final underscore from the input phrase. 

```r
    nGram.Axe <- function(input.Phrase) {
    if (input.Phrase == " ") return(0)
    else {nTokens <- str_count(input.Phrase, "_") + 1} 
    
    if (nTokens == 1) return(sub(".", "",input.Phrase))
    else return(sub("^[^_]*_", "", input.Phrase))
    }
```

### 4. Process the input string, look for anything starting with 4-grams

The Stupid Backoff looks at the tokenized input string and scans the ngram table for matching strings. If a result is returned, the model returns the index matching results and scales the probability based on the variable 'a'. Unless needed to reduce file size, the variable 'a' is set to 1. If no result is returned to the Axe, the ngram reduces the ngram by 1. This is based off of the Stupid Backoff Model as defined by the authors.
    The Stupid Backoff looks at the input string and scans the n-gram table for matching strings. If a result is returned, the model returns the indexed matching results and scales the probability based on the next lower n-gram. If there is no reult returned, the string is returned to the Axe, which then reduces the ngram by 1. This is based off the Stupid Backoff Model. Since the initial number of recognized words in the string is important for calculating the weighted probability, this was applied for 4, 3, 2 grams, and then single token input analysis. 

```r
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
```

### 5. Final Prognosticate Model is defined using previous nested functions

The Prognosticate.R function is the actual predictor function for the front end of the application. The string is entered as an input, and then tokenized as a decreasing series of ngrams. After the tokeinzation, the ngram is passed to the Backoff function, which searches for matches and then is incrementally backed off and returnd the predictions based off the highest order ngram. 

```r
    Prognosticate <- function(input.String) {
        nGram <- input.Axe(input.String)
        prognostication <- Backoff(nGram)
        prognostication$Probability <- round(prognostication$Probability, digits = 3)
        prognostication$Probability <- paste0(prognostication$Probability, " %")
        return(prognostication)
    }
```

Once the function is sourced, the user only needs to input a string within quotations, and the function will return the top 8 predictions and their associated probabilities.   


### References:
    See references at the end of PipelineMarkdown
