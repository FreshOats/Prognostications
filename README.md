# Prognostications

## Prognostication Pipeline

### Preprocessing
    # 1. Set Directory, Path, Open Libraries
    # 2. Open the Corpora, Read raw test, Count lines
    # 3. Split the Corpora into different sets and write 20 smaller files
### Processing
    # 4. Read lines from the new split files and save 4 files each to 5 tables
    # 5. Filter each table using profanity index
    # 6. Concatenate all of the tables and save
    # 7. Process aggregates, probabilites, order, and save as text
### Execute
    # 8. Source Prognosticator

Prognosticator Pipeline runs the acquisition and processing of raw corpora from Twitter, Blog, and News Sources for the Coursera Capstone Project in the Data Science Specialization. This is the 9th version of the pipeline, which has been optimized for use on a laptop computer with 8 GB of RAM. The tradeoffs considered in making this were the available RAM, the number of files that the function was broken up into, and then the number of n-grams that were possible to process with the available memory. Breaking up the files into 20 corpora and then processing n-grams for 5 overall files, this program is able to acquire data up to 5-grams before producing memory errors. 
