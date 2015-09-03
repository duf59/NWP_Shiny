## helpers.R ##

sentence_preprocess <- function(sentence, start_tag = FALSE){
    # Preprocess a sentence from which we want to predict the next word :
    # - remove punctuation
    # - lowercase
    # - split into tokens
    #
    # args: - sentence: character
    #       - start_tag: boolean, should we add a start tag?
    #
    # returns: a character vector containing the tokens
    
    output <- gsub("([[:punct:]])"," ",sentence) # separate punctuation
    output <- tolower(sentence)
    output <- stripWhitespace(output)
    output <- unlist(strsplit(output, " "))
    if (start_tag) output <- c("<s>", output)
    return(output)
}


predict <- function(database, raw_input, method = 'qML', npred = 3, max_order = 4){
    # main function for next word prediction. check input, preprocess the input sentence,
    # and call nextWord() to perform the prediction
    #
    # args: - database: a SQLite database storing the language model
    #         (i.e. table of ngrams with associated ML/KN probability)
    #       - raw_input: an input string from which we want to predict the next word
    #       - method: character, the method used. 'ML' for Maximum Likelihood (default)
    #                                             'KN' for interpolated Kneyser-Ney Smoothing
    #       - max_order: the maximal ngram order model (should match with the database...) (default 4)
    #
    # returns: a dataframe with the top 4 next words and their probability
    
    # Checking inputs
    if (length(raw_input) == 0 | length(raw_input) > 1 | typeof(raw_input) != "character"){
        print("Error in predict(). 'raw_input' must be a character vector of length 1.")
        return()
    }
    
    # Preprocessing the input sentence
    # print("Preprocessing input sentence ....")
    input_tokens <- sentence_preprocess(raw_input, start_tag = FALSE)
    # cat("Length of input is: ", length(input_tokens), ". Max Ngram model order is ",max_order,"\n")
    L    <- length(input_tokens)
    if (L > (max_order-1)) input_tokens <- input_tokens[(L-max_order+2):L]
    # cat("Input sentence is: '", input_tokens, "'\n")
    
    # Define iterative function for prediction
    nextWord <- function(input, measure, N, nopred = ""){
        # iterative function performing next word prediction, 
        #
        # args: - input: chatacter_vector, the preprocessed input string from
        #         which we want to predict the next word
        #       - N: number of potentiel next word to retrieve
        #       - nopred: character vector, word that were predicter in a higher order model
        #                 that we do not want to predict again
        #
        # returns: a dataframe with the top 4 next words, the order of the model
        # they have been retrieved from, and their probability
        
        #cat("\nCalling nextWord with: ")
        #print(input)
        #cat("Blacklist is: ")
        #print(nopred)
        #cat("N = ",N,"\n")
        
        L     <- length(input)
        order <- L + 1
        
        if (order == 1){ # base case
            
            query  <- sprintf("SELECT word1, %s FROM gram_1 ORDER BY %s DESC LIMIT %d",measure, measure, N)
            result <- dbGetQuery(conn, query)
            colnames(result) <- c("word",measure)
            result$order <- 1
            
            result <- result %>% filter(!(word %in% nopred))
            return(result)
            
        } else{
            
            word  <- paste0("word",order)
            table <- paste0("gram_",order)
            match <- paste(paste0(c("word"),c(1:(order-1)), " = ", paste0('"',input,'"')), collapse = " AND ")
            query = sprintf("SELECT %s, %s FROM %s WHERE %s ORDER BY %s DESC LIMIT %d",word, measure, table, match, measure, N)
            
            result <- dbGetQuery(conn, query)
            
            if (nrow(result) > 0){
                #cat("Found", nrow(result), "results:\n")
                #print(result)
                colnames(result) <- c("word",measure)
                result$order     <- order
                result <- result %>% filter(!(word %in% nopred))
            }
            
            if (nrow(result) >= N){ # return top N results
                #cat("Found enough, truncating output list to:")
                #print(result[1:N,])
                return(result[1:N,])
            } else if (nrow(result) > 0){ # return result + look in lower order model
                updated_nopred <- c(nopred, result$word)
                return(rbind(result, nextWord(input[-1], measure, N - nrow(result), nopred =  updated_nopred)))
            } else { # just return lower order model
                return(nextWord(input[-1], measure, N, nopred =  nopred))
            }
        }
    }
    
    conn <- dbConnect(SQLite(), database)
    result <- nextWord(input_tokens, measure = method, N = npred)
    dbDisconnect(conn)
    return(result)
}


