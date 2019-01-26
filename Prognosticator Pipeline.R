### Prognostication Pipeline ###

# Preprocessing
    # 1. Set Directory, Path, Open Libraries
    # 2. Open the Corpora, Read raw test, Count lines
    # 3. Split the Corpora into different sets and write 20 smaller files
# Processing
    # 4. Read lines from the new split files and save 4 files each to 5 tables
    # 5. Filter each table using profanity index
    # 6. Concatenate all of the tables and save
    # 7. Process aggregates, probabilites, order, and save as text
# Execute
    # 8. Source Prognosticator

## Prognosticator Pipeline runs the acquisition and processing of raw corpora from Twitter, Blog, and News Sources for the Coursera Capstone Project in the Data Science Specialization. This is the 9th version of the pipeline, which has been optimized for use on a laptop computer with 8 GB of RAM. The tradeoffs considered in making this were the available RAM, the number of files that the function was broken up into, and then the number of n-grams that were possible to process with the available memory. Breaking up the files into 20 corpora and then processing n-grams for 5 overall files, this program is able to acquire data up to 5-grams before producing memory errors. 

### Pre-Processing ###
    # 1. Set Directory, Path, Open Libraries
        working.directory <- "C:/Users/Caroline/Desktop/Coursera-SwiftKey/"
        setwd(working.directory)
        path <- paste0(working.directory, "data/final/en_US/")
        
        suppressMessages(library(quanteda))
        suppressMessages(library(data.table))
        suppressMessages(library(readtext))
        suppressMessages(library(dplyr))
        
    # 2. Open the Corpora, Read raw test, Count lines
        ## Read the Raw Text
        options(warn = -1)
        unzip("Coursera-Swiftkey.zip", exdir = "data", overwrite = FALSE, setTimes = FALSE)
        blog.con <- file(paste0(path, "en_US.blogs.txt"), "rb", encoding = "UTF-8")
        news.con <- file(paste0(path, "en_US.blogs.txt"), "rb", encoding = "UTF-8")
        twitter.con <- file(paste0(path, "en_US.twitter.txt"), "rb", encoding = "UTF-8")
        
        ## Determine the Number of Lines in each Corpus File
        blog.lines <- countLines(paste0(path, "en_US.blogs.txt"))
        news.lines <- countLines(paste0(path, "en_US.news.txt"))
        twitter.lines <- countLines(paste0(path, "en_US.twitter.txt"))
        
        
    # 3. Split the Corpora into different sets and write 20 smaller files
        ## Set seed, establish training and test paths
        set.seed(1117)
        training <- lapply(paste0(path, "training_", 1:20, ".txt"), file, "w")
        testing <- file(paste0(path, "testing.txt"), "w")
        
        ## Define the Corpus Axe text splitter Function
        corpus.Axe <- function(corpus.con, corpus.lines) {
            training.Number <- rbinom(corpus.lines, 1, 0.9)
            Number <- rep_along(training.Number, 1:20)
            for (i in 1:corpus.lines) {
                Lines <- readLines(corpus.con, n = 1, skipNul = T, encoding = "UTF-8") # n = 1  one line at a time
                if (training.Number[i]) {
                    writeLines(Lines, con = training[[Number[i]]])
                } else { 
                    writeLines(Lines, con = testing)}
            }
            close(corpus.con)
        }    
        
        ## Read lines from each corpus and split into the new files
        corpus.Axe(blog.con, blog.lines)
        corpus.Axe(twitter.con, twitter.lines)
        corpus.Axe(news.con, news.lines)
        
        invisible(lapply(training, close))
        close(testing)
        rm(blog.con, blog.lines, news.con, news.lines, twitter.con, twitter.lines)

### Procesing the Corpora ###    
    ## Note: After each chunk was read, R had to be reset. While this was originally written to be iterated through all 5 of the chunks, R crashed while reading the second chunk. I tried processing in the fewest sets, but due to constraints of the computer, the optimal size training set was in 5 chunks of 4 training files.  
        
    # 4. Read lines from the new split files and save 4 files each to 5 tables
        ## Tokenize Chunk 1 of 5
            training.Path <- paste0(path, "training_", 1:4, ".txt")
            ngram.Table <- data.table()
            
            To <- Sys.time()
            for (file.path in training.Path) {
                print(file.path)
                corpus.Number <- readLines(file.path)
                corpus.Number <- sapply(corpus.Number, tolower, USE.NAMES = F) # removes all capital letters
                corpus.Number <- sapply(corpus.Number, function(x) str_replace_all(x, "[a-z']", " "), USE.NAMES = F)
                # above line replaces all text that is not a lower case letter or an apostrophe [] = NOT
                corpus.Number <- corpus(corpus.Number) # converts type string to corpus
                corpus.Tokens <- tokens(corpus.Number, what = "word", ngrams = 2:5) 
                # above line tokenizes the corpus into n-grams from 2-grams to 5-grams by word
                rm(corpus.Number)
                corpus.Tokens <- unlist(corpus.Tokens, use.names = F) %>% as.data.table()
                # above line converts the tokens data.frame into a data.table, which is less intensive for processing
                names(corpus.Tokens) <- "nGram" # adds a heading called "nGram"
                ngram.Table <- rbind(ngram.Table, corpus.Tokens[, list(count = .N), by = list(nGram)])
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
                corpus.Number <- sapply(corpus.Number, function(x) str_replace_all(x, "[a-z']", " "), USE.NAMES = F)
                corpus.Number <- corpus(corpus.Number)
                corpus.Tokens <- tokens(corpus.Number, what = "word", ngrams = 2:5)
                rm(corpus.Number)
                corpus.Tokens <- unlist(corpus.Tokens, use.names = F) %>% as.data.table()
                names(corpus.Tokens) <- "nGram"
                ngram.Table <- rbind(ngram.Table, corpus.Tokens[, list(count = .N), by = list(nGram)])
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
                corpus.Number <- sapply(corpus.Number, function(x) str_replace_all(x, "[a-z']", " "), USE.NAMES = F)
                corpus.Number <- corpus(corpus.Number)
                corpus.Tokens <- tokens(corpus.Number, what = "word", ngrams = 2:5)
                rm(corpus.Number)
                corpus.Tokens <- unlist(corpus.Tokens, use.names = F) %>% as.data.table()
                names(corpus.Tokens) <- "nGram"
                ngram.Table <- rbind(ngram.Table, corpus.Tokens[, list(count = .N), by = list(nGram)])
                rm(corpus.Tokens)
                gc()
            }
            print(Sys.time() - To)
            
            save(ngram.Table, file = paste0(path, "ngram.Table3.RData"))
            rm(ngram.Table)
            
        ## Tokenize Chunk 4 of 5
            training.Path <- paste0(path, "training_", 13:16, ".txt")
            ngram.Table <- data.table()
            
            To <- Sys.time()
            for (file.path in training.Path) {
                print(file.path)
                corpus.Number <- readLines(file.path)
                corpus.Number <- sapply(corpus.Number, tolower, USE.NAMES = F)
                corpus.Number <- sapply(corpus.Number, function(x) str_replace_all(x, "[a-z']", " "), USE.NAMES = F)
                corpus.Number <- corpus(corpus.Number)
                corpus.Tokens <- tokens(corpus.Number, what = "word", ngrams = 2:5)
                rm(corpus.Number)
                corpus.Tokens <- unlist(corpus.Tokens, use.names = F) %>% as.data.table()
                names(corpus.Tokens) <- "nGram"
                ngram.Table <- rbind(ngram.Table, corpus.Tokens[, list(count = .N), by = list(nGram)])
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
                corpus.Number <- sapply(corpus.Number, function(x) str_replace_all(x, "[a-z']", " "), USE.NAMES = F)
                corpus.Number <- corpus(corpus.Number)
                corpus.Tokens <- tokens(corpus.Number, what = "word", ngrams = 2:5)
                rm(corpus.Number)
                corpus.Tokens <- unlist(corpus.Tokens, use.names = F) %>% as.data.table()
                names(corpus.Tokens) <- "nGram"
                ngram.Table <- rbind(ngram.Table, corpus.Tokens[, list(count = .N), by = list(nGram)])
                rm(corpus.Tokens)
                gc()
            }
            print(Sys.time() - To); print("Finally, that took forever!")
            
            save(ngram.Table, file = paste0(path, "ngram.Table5.RData"))
            rm(ngram.Table)
            
            
    # 5. Filter each table using profanity index
        # Note: nasty.txt is file that was reduced from the "List-of-Dirty-Naughty-Obscene-and-Otherwise-Bad-Words-master" acquired from Google. The original file was filtering a lot of unnecessary profanities, so these were removed by hand so that the filtering could be more efficient
        # Each of the 5 ngram.Table Chunks were Filtered Separately and saved per memory issues from earlier
        
        nasty <- unlist(fread(paste0(working.directory, "nasty.txt"), header = F, fill = T))
        
        # ^ looks for start
        # [] acts as NOT
        # $ looks at end 
        
        ## Filter Chunk 1 of 5
            load(paste0(path, "ngram.Table", 1, ".RData"))
            ngram.Table <- ngram.Table[, list(phrase = sub("_[^_]+$", "", nGram), 
                                              prediction = sub("^([^_]+_)+", "", nGram), count)]
            # Above line looks in the 
            ngram.Table <- ngram.Table[!(prediction %in% nasty)]
            ngram.Table <- ngram.Table[, phrase.count := sum(count), phrase]
            ngram.Table <- ngram.Table[phrase.count > 2]
            save(ngram.Table, file = paste0(path, "filtered_", 1, ".RData"))
            rm(ngram.Table); gc()
            
        ## Filter Chunk 2 of 5
            load(paste0(path, "ngram.Table", 2, ".RData"))
            ngram.Table <- ngram.Table[, list(phrase = sub("_[^_]+$", "", nGram), 
                                              prediction = sub("^([^_]+_)+", "", nGram), count)]
            ngram.Table <- ngram.Table[!(prediction %in% nasty)]
            ngram.Table <- ngram.Table[, phrase.count := sum(count), phrase]
            ngram.Table <- ngram.Table[phrase.count > 2]
            save(ngram.Table, file = paste0(path, "filtered_", 2, ".RData"))
            rm(ngram.Table); gc()
            
        ## Filter Chunk 3 of 5
            load(paste0(path, "ngram.Table", 3, ".RData"))
            ngram.Table <- ngram.Table[, list(phrase = sub("_[^_]+$", "", nGram), 
                                              prediction = sub("^([^_]+_)+", "", nGram), count)]
            ngram.Table <- ngram.Table[!(prediction %in% nasty)]
            ngram.Table <- ngram.Table[, phrase.count := sum(count), phrase]
            ngram.Table <- ngram.Table[phrase.count > 2]
            save(ngram.Table, file = paste0(path, "filtered_", 3, ".RData"))
            rm(ngram.Table); gc()
            
        ## Filter Chunk 4 of 5
            load(paste0(path, "ngram.Table", 4, ".RData"))
            ngram.Table <- ngram.Table[, list(phrase = sub("_[^_]+$", "", nGram), 
                                              prediction = sub("^([^_]+_)+", "", nGram), count)]
            ngram.Table <- ngram.Table[!(prediction %in% nasty)]
            ngram.Table <- ngram.Table[, phrase.count := sum(count), phrase]
            ngram.Table <- ngram.Table[phrase.count > 2]
            save(ngram.Table, file = paste0(path, "filtered_", 4, ".RData"))
            rm(ngram.Table); gc()
            
        ## Filter Chunk 5 of 5
            load(paste0(path, "ngram.Table", 5, ".RData"))
            ngram.Table <- ngram.Table[, list(phrase = sub("_[^_]+$", "", nGram), 
                                              prediction = sub("^([^_]+_)+", "", nGram), count)]
            ngram.Table <- ngram.Table[!(prediction %in% nasty)]
            ngram.Table <- ngram.Table[, phrase.count := sum(count), phrase]
            ngram.Table <- ngram.Table[phrase.count > 2]
            save(ngram.Table, file = paste0(path, "filtered_", 5, ".RData"))
            rm(ngram.Table); gc()
            
            rm(nasty)
            
            
    # 6. Concatenate all of the tables and save
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
        
        
    # 7. Process aggregates, probabilites, order, and save as text
        load(paste0(path, "filtered.RData"))
        
        ngram.Table <- ngram.Table[, lapply(.SD, sum, na.rm = T), by = list(phrase, prediction)]
        ngram.Table <- setorder(ngram.Table, phrase, -count)[, index := seq_len(.N), by = phrase]
        
        
        ngram.Table <- ngram.Table[index <= 8] 
        ngram.Table[, probability := count / phrase.count]
        ngram.Table <- setorder(ngram.Table, phrase, -probability)[, index := seq_len(.N), by = phrase]
        fwrite(ngram.Table[, c("phrase", "prediction", "probability")], 
               file = paste0(path, "filtered.txt"), append = F)
        rm(ngram.Table); gc()
        
    # 8. Source Prognosticator
        source("Prognosticator.R")