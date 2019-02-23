---
title: "Text Prognosticator"
author: "Justin Papreck"
date: "February 22, 2019"
output: 
    html_document:
        keep_md: true
---



Prognosicator is the back-end of the Coursera Capstone Project in collaboration with Swift-Key for the Data Science Specialization through Johns-Hopkins University. The Prognosicator Pipeline uses text data from English language corpora obtained from news sources, blogs, and twitter feeds. Following processing, these corpora are used to make predictions for the next word. All new word predictions have profanities removed, however the program still makes predictions based on profinities in the input phrases. 

This code was written for optimized use on a laptop computer with 8 GB of available RAM. The trade-offs considered due to the RAM available was the number of chunks each file was broken into, the number of n-grams to which the corpora were broken into, and the largest set of filtered data that can be opened and used after processing and filtration of data. 

This code broke the raw corpora into 20 smaller corpus files to process n-grams, which were then compiled into 5 data tables. This computer was able to run up to 5-grams before it produced memory allocation errors. Before the final steps, all 5 of the filtered data tables were concatenated and probabilites for predictions were calculated from that final data table. 

# Pipeline
### Preprocessing
    1. Set the directory, path, and open all of the libraries
    2. Open and read the corpora, count the number of lines in each corpus
    3. Split the corpora into different data sets and write to 20 smaller files
### Processing
    4. Read lines from the new split files, then save 4 files each to 5 data tables
    5. Filter each table using the profanity index
    6. Concatenate all of the above tables and save
    7. Process any aggregates, calculate probabilities, and save as text file
### Execute
    8. Source Prognosicator.R


## Preprocessing
### 1. Set the directory, path, and open all of the libraries 

```r
    working.directory <- "C:/Users/Caroline/Desktop/Coursera-SwiftKey/"
        setwd(working.directory)
        path <- paste0(working.directory, "data/final/en_US/")
        
        suppressMessages(library(quanteda))
        suppressMessages(library(data.table))
        suppressMessages(library(readtext))
        suppressMessages(library(dplyr))
        suppressMessages(library(R.utils))
        suppressMessages(library(rlang))
        suppressMessages(library(stringr))
```
   
### 2. Open, Read, Count Lines
    
Open the corpora

```r
    options(warn = -1)
    unzip("Coursera-Swiftkey.zip", exdir = "data", overwrite = FALSE, setTimes = FALSE)
    blog.con <- file(paste0(path, "en_US.blogs.txt"), "rb", encoding = "UTF-8")
    news.con <- file(paste0(path, "en_US.blogs.txt"), "rb", encoding = "UTF-8")
    twitter.con <- file(paste0(path, "en_US.twitter.txt"), "rb", encoding = "UTF-8")
```
    
Open the number of lines in each corpus

```r
    blog.lines <- countLines(paste0(path, "en_US.blogs.txt"))
    news.lines <- countLines(paste0(path, "en_US.news.txt"))
    twitter.lines <- countLines(paste0(path, "en_US.twitter.txt"))
```

### 3. Split the corpora into different data sets and write to 20 smaller files

Set seed and establish Test and Training Paths

```r
    set.seed(1117)
    training <- lapply(paste0(path, "training_", 1:20, ".txt"), file, "w")
    testing <- file(paste0(path, "testing.txt"), "w")
```

Define the Corpus Axe text splitting function, a similar function is used for the input text

```r
    corpus.Axe <- function(corpus.con, corpus.lines) {
            training.Number <- rbinom(corpus.lines, 1, 0.9) # Strip 90% of the text for the training files
            Number <- rep_along(training.Number, 1:20) # Split into 20 separate files
            for (i in 1:corpus.lines) {
                Lines <- readLines(corpus.con, n = 1, skipNul = T, encoding = "UTF-8") # n = 1 read one line at a time
                if (training.Number[i]) {
                    writeLines(Lines, con = training[[Number[i]]]) # Write the different files
                } else { 
                    writeLines(Lines, con = testing)} # 10% of the text is reserved for test files
            }
            close(corpus.con)
    }    
```

Read the corpora and actually split the files using corpus.Axe

```r
    corpus.Axe(blog.con, blog.lines)
    corpus.Axe(twitter.con, twitter.lines)
    corpus.Axe(news.con, news.lines)
    
    invisible(lapply(training, close))
    close(testing)
    rm(blog.con, blog.lines, news.con, news.lines, twitter.con, twitter.lines)
```

## Processing
    
Note: After each chunk was read, R had to be reset. Originally, the code was written to iteratively process all 5 chunks, but R crashed during the reading of the second chunk. After trying to process smaller sets numerous times, due to the constraints of this computer, the optimal size for the training set was 5 chunks of 4 files each. 
    
### 4. Read lines from the new split files, then save 4 files each to 5 data tables 

```r
    To <- Sys.time()
            for (file.path in training.Path) {
                print(file.path)
                corpus.Number <- readLines(file.path)
                corpus.Number <- sapply(corpus.Number, tolower, USE.NAMES = F) # removes all capital letters
                corpus.Number <- sapply(corpus.Number, function(x) str_replace_all(x, "[^a-z']", " "), USE.NAMES = F)
                # above line replaces all text that is not a lower case letter or an apostrophe [] = NOT
                corpus.Number <- corpus(corpus.Number) # converts type string to corpus
                corpus.Tokens <- tokens(corpus.Number, what = "word", ngrams = 2:5) 
                # above line tokenizes the corpus into n-grams from 2-grams to 5-grams by word
                rm(corpus.Number)
                corpus.Tokens <- unlist(corpus.Tokens, use.names = F) %>% as.data.table()
                # above line converts the tokens data.frame into a data.table, which is less intensive for processing
                names(corpus.Tokens) <- "nGram" # adds a heading called "nGram"
                ngram.Table <- rbind(ngram.Table, corpus.Tokens[, list(Count = .N), by = list(nGram)])
                # above line adds rows to the empty table ngram.Table sorted by n-gram, and counts the number of like n-grams
                rm(corpus.Tokens)
                gc()
            }
            print(Sys.time() - To)
            print("One down, 4 to go! If R takes twice as long to process the next chunk, consider restarting R")
            
            save(ngram.Table, file = paste0(path, "ngram.Table1.RData"))
            rm(ngram.Table)
    
        ## Tokenize Chunk 2 of 5
            training.Path <- paste0(path, "training_", 5:8, ".txt")
            ngram.Table <- data.table()
            
            To <- Sys.time()
            for (file.path in training.Path) {
                print(file.path)
                corpus.Number <- readLines(file.path)
                corpus.Number <- sapply(corpus.Number, tolower, USE.NAMES = F)
                corpus.Number <- sapply(corpus.Number, function(x) str_replace_all(x, "[^a-z']", " "), USE.NAMES = F)
                corpus.Number <- corpus(corpus.Number)
                corpus.Tokens <- tokens(corpus.Number, what = "word", ngrams = 2:5)
                rm(corpus.Number)
                corpus.Tokens <- unlist(corpus.Tokens, use.names = F) %>% as.data.table()
                names(corpus.Tokens) <- "nGram"
                ngram.Table <- rbind(ngram.Table, corpus.Tokens[, list(Count = .N), by = list(nGram)])
                # above line adds rows from this file to the end of ngram.Table, already containing ngrams from Chunk 1
                rm(corpus.Tokens)
                gc()
            }
            print(Sys.time() - To); print("Two down, Three to go. You may need to reset R")
            
            save(ngram.Table, file = paste0(path, "ngram.Table2.RData"))
            rm(ngram.Table)
            
        ## Tokenize Chunk 3 of 5
            training.Path <- paste0(path, "training_", 9:12, ".txt")
            ngram.Table <- data.table()
            
            To <- Sys.time()
            for (file.path in training.Path) {
                print(file.path)
                corpus.Number <- readLines(file.path)
                corpus.Number <- sapply(corpus.Number, tolower, USE.NAMES = F)
                corpus.Number <- sapply(corpus.Number, function(x) str_replace_all(x, "[^a-z']", " "), USE.NAMES = F)
                corpus.Number <- corpus(corpus.Number)
                corpus.Tokens <- tokens(corpus.Number, what = "word", ngrams = 2:5)
                rm(corpus.Number)
                corpus.Tokens <- unlist(corpus.Tokens, use.names = F) %>% as.data.table()
                names(corpus.Tokens) <- "nGram"
                ngram.Table <- rbind(ngram.Table, corpus.Tokens[, list(Count = .N), by = list(nGram)])
                rm(corpus.Tokens)
                gc()
            }
            print(Sys.time() - To)
            
            save(ngram.Table, file = paste0(path, "ngram.Table3.RData"))
            print(Sys.time() - To); print("Over half way done!")
            rm(ngram.Table)
            
        ## Tokenize Chunk 4 of 5
            training.Path <- paste0(path, "training_", 13:16, ".txt")
            ngram.Table <- data.table()
            
            To <- Sys.time()
            for (file.path in training.Path) {
                print(file.path)
                corpus.Number <- readLines(file.path)
                corpus.Number <- sapply(corpus.Number, tolower, USE.NAMES = F)
                corpus.Number <- sapply(corpus.Number, function(x) str_replace_all(x, "[^a-z']", " "), USE.NAMES = F)
                corpus.Number <- corpus(corpus.Number)
                corpus.Tokens <- tokens(corpus.Number, what = "word", ngrams = 2:5)
                rm(corpus.Number)
                corpus.Tokens <- unlist(corpus.Tokens, use.names = F) %>% as.data.table()
                names(corpus.Tokens) <- "nGram"
                ngram.Table <- rbind(ngram.Table, corpus.Tokens[, list(Count = .N), by = list(nGram)])
                rm(corpus.Tokens)
                gc()
            }
            print(Sys.time() - To); print("Almost there, one more. You know the drill!")
            
            save(ngram.Table, file = paste0(path, "ngram.Table4.RData"))
            rm(ngram.Table)
            
        ## Tokenize Chunk 5 of 5
            training.Path <- paste0(path, "training_", 17:20, ".txt")
            ngram.Table <- data.table()
            
            To <- Sys.time()
            for (file.path in training.Path) {
                print(file.path)
                corpus.Number <- readLines(file.path)
                corpus.Number <- sapply(corpus.Number, tolower, USE.NAMES = F)
                corpus.Number <- sapply(corpus.Number, function(x) str_replace_all(x, "[^a-z']", " "), USE.NAMES = F)
                corpus.Number <- corpus(corpus.Number)
                corpus.Tokens <- tokens(corpus.Number, what = "word", ngrams = 2:5)
                rm(corpus.Number)
                corpus.Tokens <- unlist(corpus.Tokens, use.names = F) %>% as.data.table()
                names(corpus.Tokens) <- "nGram"
                ngram.Table <- rbind(ngram.Table, corpus.Tokens[, list(Count = .N), by = list(nGram)])
                rm(corpus.Tokens)
                gc()
            }
            print(Sys.time() - To); print("Finally, that took forever!")
            
            save(ngram.Table, file = paste0(path, "ngram.Table5.RData"))
            rm(ngram.Table)
```

### 5. Filter each table using the profanity index
    
Note: nasty.txt is a file that was reduced from the "List-of-Dirty-Naughty-Obscene-and-Otherwise-Bad-Words-master" acquired from Google. The original file was filtering 1700 some profanities, many of which were not words or would have already been filtered in the processing of text. These were removed by hand and saved as 'nasty.txt' in order to improve the efficiency of profanity filtration. Each of the 5 ngram table chunks were filtered separately and saved per memory allocation issues. 

```r
 nasty <- unlist(fread(paste0(working.directory, "nasty.txt"), header = F, fill = T))
        
    # ^ looks for start
    # [] acts as NOT
    # $ looks at end 
    
    ## Filter Chunk 1 of 5
        load(paste0(path, "ngram.Table", 1, ".RData"))
        ngram.Table <- ngram.Table[, list(Phrase = sub("_[^_]+$", "", nGram), 
                                          Prediction = sub("^([^_]+_)+", "", nGram), Count)]
        ngram.Table <- ngram.Table[!(Prediction %in% nasty)]
        ngram.Table <- ngram.Table[, Phrase.Count := sum(Count), Phrase]
        ngram.Table <- ngram.Table[Phrase.Count > 2] # Need to have > 2 for processing capabilities, cuts down my more than 50%
        save(ngram.Table, file = paste0(path, "filtered_", 1, ".RData"))
        rm(ngram.Table); gc()
        
    ## Filter Chunk 2 of 5
        load(paste0(path, "ngram.Table", 2, ".RData"))
        ngram.Table <- ngram.Table[, list(Phrase = sub("_[^_]+$", "", nGram), 
                                          Prediction = sub("^([^_]+_)+", "", nGram), Count)]
        ngram.Table <- ngram.Table[!(Prediction %in% nasty)]
        ngram.Table <- ngram.Table[, Phrase.Count := sum(Count), Phrase]
        ngram.Table <- ngram.Table[Phrase.Count > 2]
        save(ngram.Table, file = paste0(path, "filtered_", 2, ".RData"))
        rm(ngram.Table); gc()
        
    ## Filter Chunk 3 of 5
        load(paste0(path, "ngram.Table", 3, ".RData"))
        ngram.Table <- ngram.Table[, list(Phrase = sub("_[^_]+$", "", nGram), 
                                          Prediction = sub("^([^_]+_)+", "", nGram), Count)]
        ngram.Table <- ngram.Table[!(Prediction %in% nasty)]
        ngram.Table <- ngram.Table[, Phrase.Count := sum(Count), Phrase]
        ngram.Table <- ngram.Table[Phrase.Count > 2]
        save(ngram.Table, file = paste0(path, "filtered_", 3, ".RData"))
        rm(ngram.Table); gc()
        
    ## Filter Chunk 4 of 5
        load(paste0(path, "ngram.Table", 4, ".RData"))
        ngram.Table <- ngram.Table[, list(Phrase = sub("_[^_]+$", "", nGram), 
                                          Prediction = sub("^([^_]+_)+", "", nGram), Count)]
        ngram.Table <- ngram.Table[!(Prediction %in% nasty)]
        ngram.Table <- ngram.Table[, Phrase.Count := sum(Count), Phrase]
        ngram.Table <- ngram.Table[Phrase.Count > 2]
        save(ngram.Table, file = paste0(path, "filtered_", 4, ".RData"))
        rm(ngram.Table); gc()
        
    ## Filter Chunk 5 of 5
        load(paste0(path, "ngram.Table", 5, ".RData"))
        ngram.Table <- ngram.Table[, list(Phrase = sub("_[^_]+$", "", nGram), 
                                          Prediction = sub("^([^_]+_)+", "", nGram), Count)]
        ngram.Table <- ngram.Table[!(Prediction %in% nasty)]
        ngram.Table <- ngram.Table[, Phrase.Count := sum(Count), Phrase]
        ngram.Table <- ngram.Table[Phrase.Count > 2]
        save(ngram.Table, file = paste0(path, "filtered_", 5, ".RData"))
        rm(ngram.Table); gc()
        
        rm(nasty)
```

### 6. Concatenate Tables

```r
    load(paste0(path, "filtered_", 1, ".RData"))
    ngram.all <- ngram.Table
    load(paste0(path, "filtered_", 2, ".RData"))
    ngram.all <- rbind(ngram.all, ngram.Table)
    load(paste0(path, "filtered_", 3, ".RData"))
    ngram.all <- rbind(ngram.all, ngram.Table)
    load(paste0(path, "filtered_", 4, ".RData"))
    ngram.all <- rbind(ngram.all, ngram.Table)
    load(paste0(path, "filtered_", 5, ".RData"))
    ngram.all <- rbind(ngram.all, ngram.Table)
    ngram.Table <- ngram.all
    rm(ngram.all)
    save(ngram.Table, file = paste0(path, "filtered.RData"))
    
    rm(ngram.Table)
```

### 7. Process any aggregates, calculate probabilities, and save as text file

Load the saved filtered data

```r
    load(paste0(path, "filtered.RData"))
```

Sum the phrases and predictions in the table, then sort by phrase

```r
    ngram.Table <- ngram.Table[, lapply(.SD, sum, na.rm = T), by = list(Phrase, Prediction)]
    ngram.Table <- setorder(ngram.Table, Phrase, -Count)[, index := seq_len(.N), by = Phrase]
```

Set the number of predictions to be returned, then calculate probabilites for each prediction following a phrase

```r
    ngram.Table <- ngram.Table[index <= 8] 
    ngram.Table[, Probability := Count / Phrase.Count]
    ngram.Table <- setorder(ngram.Table, Phrase, -Probability)[, index := seq_len(.N), by = Phrase]
```

Write to file, named 'filtered.txt', which will be sourced and read by the Prognosticator Function and deployed in the ShinyApp.

```r
    fwrite(ngram.Table[, c("Phrase", "Prediction", "Probability")], 
           file = paste0(working.directory, "Prognosticator/filtered.txt"), append = F)
    rm(ngram.Table); gc()
```
    
## Execute
### 8. Source 'Prognosticator.R'

After all of the above code has been executed once, only the Prognosticator.R function needs to be run to make the predictions. In order to Run prognosticator, a string must be input in quotations, and then the function will output it's top 8 predictions with their associated probabilities.  


```r
    source("Prognosticator.R")
```



## References: 
### Acknowledgements 
    
Without some of the following references and their insightful approaches to distinct aspects of this process, this code would not have near the accuracy or fluidity that I would have liked. I'm grateful to those who have shared their code on github as well as the great compendium of theoretical knowledge that has been provided by the Johns Hopkins Data Science Specialization faculty. 

1. http://www.aclweb.org/anthology/D07-1090.pdf
2. https://github.com/wright13/DataScienceCapstoneSBO
3. https://github.com/dmodjeska/word_prediction/blob/master/predict.R
4. https://github.com/youyouworld010/DataScienceCapstone/blob/master/Prediction%20Model.Rmd
5. https://github.com/bradleyboehmke/word_prediction_app/blob/master/extracting_n-grams.R
6. https://github.com/yeongwei/coursera-datascience-capstone/blob/master/week3-buildingPredictiveModel.R
7. https://github.com/yufree/Capstone/blob/master/finalreport.Rmd
8. https://github.com/j-wang/DataScienceCapstone
9. https://github.com/ThachNgocTran/KatzBackOffModelImplementationInR/blob/master/estimateProbability.R
10.https://rstudio-pubs-static.s3.amazonaws.com/48094_bb9c77ab4c234074a66726961829af0b.html#/1
11.http://www.modsimworld.org/papers/2015/Natural_Language_Processing.pdf
12.https://github.com/ambodi/LanguageModelWithStupidBackoff/blob/master/backoff.R
13.http://spring2015.cs-114.org/wp-content/uploads/2016/01/NgramModels.pdf
14.http://l2r.cs.uiuc.edu/~danr/Teaching/CS546-09/Papers/Katz87.pdf
15.https://rpubs.com/erodriguez/nlpquanteda
16.http://rpubs.com/erodriguez/milestone1
17.https://rpubs.com/jbonsak/capstonefinal
18.https://rstudio-pubs-static.s3.amazonaws.com/304141_7cc99677660842599c2a35698adddfa8.html
