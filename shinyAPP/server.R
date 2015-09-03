## server.R ##

library(tm)
library(sqldf)
library(dplyr)

source("helpers.R")

db <- "data/NWP.SQLite"  # database containing ngram language models
npred <- 5               # number of potential next words to compute
max_order <- 4           # higher order ngram model in db

function(input, output) {
    output$prediction   <- renderTable({ predict(database = db,
                                                 raw_input = input$text,
                                                 method = "PKN",
                                                 npred = npred,
                                                 max_order = max_order)[, 1:2]})
}