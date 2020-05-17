# May 16 2020
# Wordcloud in R
# Alex Van Plantinga
# Visualization of word usage in Thus Spake Zarathustra by Nietzsche

# obtain text from gutenbergr
library(gutenbergr)

gutenberg_metadata[which(startsWith(gutenberg_metadata$author, "Nietzsche") ),]
#1998 Thus Spake Zarathustra
nietzsche = gutenberg_download(1998)

library(wordcloud)
library(tm)
library(SnowballC)
library(readr)
library(dplyr)

docs = Corpus(VectorSource(nietzsche))
inspect(docs)

toSpace <- content_transformer(function (x , pattern ) gsub(pattern, " ", x))
docs <- tm_map(docs, toSpace, "/")
docs <- tm_map(docs, toSpace, "@")
docs <- tm_map(docs, toSpace, "\\|")



# Convert the text to lower case
docs <- tm_map(docs, content_transformer(tolower))
# Remove numbers
docs <- tm_map(docs, removeNumbers)
# Remove english common stopwords
docs <- tm_map(docs, removeWords, stopwords("english"))
# Remove your own stop word
# specify your stopwords as a character vector
docs <- tm_map(docs, removeWords, c("nietzsche", "blabla1", "blabla2", "-", "chapter")) 
# Remove punctuations
docs <- tm_map(docs, removePunctuation)
# Eliminate extra white spaces
docs <- tm_map(docs, stripWhitespace)
# Text stemming
#docs <- tm_map(docs, stemDocument)

dtm <- TermDocumentMatrix(docs)

m <- as.matrix(dtm)
v <- sort(rowSums(m),decreasing=TRUE)
d <- data.frame(word = names(v),freq=v)

head(d, 10)

fileName = "C:/Users/Lenovo/Demo/mostcommonwords1000.txt"

commonwords = read.csv(fileName, header = FALSE)

d = d[which(!d$word %in% 
              c(commonwords$V1[1:200], "-")) , ]

d = d[which(nchar(d$word) > 1),]

res<- wordcloud(words = d$word, freq = d$freq, lang = "english", 
                max.words = 120, colorPalette = "black")
