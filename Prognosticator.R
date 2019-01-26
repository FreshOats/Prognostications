library(quanteda)
library(data.table)
library(readtext)
library(stringr)
library(dplyr)

## Open the file that has the n-grams and their associated probabilites
nGrams <- fread(paste0(path, "filtered.txt"), col.names = c("phrase", "prediction", "probability"), data.table = T, stringsAsFactors = F, encoding = "UTF-8")
    lambda <- 0.4
    setkey(nGrams, phrase)

## This determines whether the input string exists within the file ngrams in the filtered text
nTokens <- function(input.Phrase) {
    if (input.Phrase == "") return(0)
    return(str_count(input.Phrase, "_") + 1)
}

## This cuts off the first word from the input string. If the string is of length 1, then it returns an NA
nGram.Axe <- function(input.Phrase) {
    if (nTokens(input.Phrase) == 1) return("<NA>")
    return(sub("^[^_]*_", "", input.Phrase))
}

## This tokenizes the input string - the highest number of words that are searched is 6, since there is nothing greater than 5-grams, this is more than ample. The else condition lowers the number incrementally
input.Axe <- function(input.String, n = 6) {
    if (trimws(input.String, "both") == "") return ("<NA>")
    chops <- tokens(input.String, what = "word", remove_numbers = T, remove_punct = T, remove_symbols = T, remove_twitter = T, remove_hyphens = T, remove_url = T, remove_separators = T, ngrams = n) %>% 
        tokens_tolower() %>%
        unlist(use.names = F) %>%
        as.data.table()
    if (chops[, .N] > 0) return(chops[.N])
    else return(input.Axe(input.String, n - 1))
}

## The Stupid Backoff looks at the input string and scans the n-gram table for matching strings. If a result is returned, the model returns the indexed matching results and scales the probability based on a. If there is no reult returned, the string is returned to the Axe, which then reduces the ngram by 1. This is based off the Stupid Backoff Model 
Backoff <- function(input.Phrase, a = 1) {
    result <- nGrams[phrase == input.Phrase]
    if (nrow(result) > 0) return(mutate(result, probability = a * probability))
    else return(Backoff(nGram.Axe(input.Phrase), a * lambda))
}

## This is the actual predictor for the front end. The string is entered as an input. This is then tokenized as a decreasing series of ngrams. After the tokenization, the ngram is passed to the Backoff function, which searches for matches and then incrementally backed off and returns predictions based on the highest ngram
Prognosticate <- function(input.String) {
    nGram <- input.Axe(input.String)
    prognostication <- Backoff(nGram)
    return(prognostication)
}