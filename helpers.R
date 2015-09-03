# Helper functions

readGramsFromMeta <- function(file, order, min_count=2){
    # reads ngrams output by MeTa
    #
    # args: - file: source file
    #       - order: ngram order
    #       - unk_words: out of vocabulary words (OOV), will be read as NA
    #
    # returns: a data table containing the ngrams as separate words and the count
    
    old_names <- paste0("V",1:(order+1))
    new_names <- c(paste0("word",1:order), "count")
    
    type    <- c(rep("character",order),"numeric")
    
    ngrams <- fread(input = file, sep = " ", header = FALSE, colClasses = type, showProgress = TRUE)
    
    for (i in 1:(order+1)){
        setnames(ngrams, old_names[i], new_names[i])
    }
    ngrams$count  <- as.integer(ngrams$count)
    
    ngrams <- ngrams[count>=min_count,]
    ngrams[, count:=count-1]
    
    return(ngrams)
}