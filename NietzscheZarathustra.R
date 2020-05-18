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
#inspect(docs)

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
              c(commonwords$V1[1:200], "-", "clich<e9>", "clich\xe9")) , ]
d = d[-c(3084, 3717),]
d = d[which(nchar(d$word) > 1),]

res<- wordcloud(words = d$word, freq = d$freq, lang = "english", 
                max.words = 120, colorPalette = "black")


library(readr)
library(RCurl)
library(XML)

# ll = list()
# for(i in 1:9){
# url = paste0("https://en.wikiquote.org/wiki/Dragon_Ball_Z:_Season_",i) 
# read_lines(url)
# doc.html = htmlTreeParse(read_lines(url), useInternal = TRUE)
# doc.text = unlist(xpathApply(doc.html, '//dd', xmlValue))
# # Replace all newline notation with spaces and all quotes with nothing
# doc.text = gsub('\\n', ' ', doc.text)
# doc.text = gsub('\\"', '', doc.text)
# doc.text = paste(doc.text, collapse = ' ')
# ll[[i]] = doc.text
# }
# dbz = paste(unlist(ll), collapse = " ")

#write.table(dbz, file = "dbz.txt")
#write.table(nietzsche, file = "nietzsche.txt")

fileName = "C:/Users/Lenovo/Demo/mostcommonwords1000.txt"
commonwords = read.csv(fileName, header = FALSE)

nietzsche = read_lines("C:/Users/Lenovo/TextAnalysis/nietzsche.txt")
nietzsche = paste(nietzsche, collapse = " ")
fileName = "C:/Users/Lenovo/TextAnalysis/dbz.txt"
dbz = read_lines(fileName)
dbz = paste(dbz, collapse = " ")

nietzsche = iconv(nietzsche, to = "utf-8", sub="")
dbz = iconv(dbz, to = "utf-8", sub="")

docs <- Corpus(VectorSource(c(nietzsche, dbz)))

# https://stackoverflow.com/questions/18504559/twitter-data-analysis-error-in-term-document-matrix

docs <- docs %>%
  tm_map(removePunctuation) %>%
  tm_map(removeNumbers) %>%
  tm_map(tolower)  %>%
  tm_map(stripWhitespace) %>%
  tm_map(PlainTextDocument) 

docs <- tm_map(docs, removeWords, c("nietzsche", "blabla1", "blabla2", "-", "chapter", commonwords$V1[1:120], stopwords("english"))) 


tdm <- TermDocumentMatrix(docs) %>%
  as.matrix()

colnames(tdm) = c("Zarathustra", "DBZ")
# remove words by rownames of tdm
tdm = tdm[ ! rownames(tdm) %in% c("-", "chapter", commonwords$V1[1:120]) , ]

par(mfrow=c(1,1))
#par(mar=c(2,2,2,2))
comparison.cloud(tdm, random.order=FALSE, colors = c("black","darkorange"),
                 title.size=1.5, max.words=400)

