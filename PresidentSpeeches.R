# May 16 2020
# Wordcloud in R
# Alex Van Plantinga

# Comparison of President Trump's speeches from before and after
# the 2020 coronavirus pandemic

library(wordcloud)
library(tm)
library(SnowballC)
library(readr)
library(dplyr)
# compare President Trump's word usage in speeches from before and after the Coronavirus Pandemic
# Source: https://millercenter.org/the-presidency/presidential-speeches
mystring <- read_file("C:/Users/Lenovo/Demo/TextAnalysis/TrumpBeforeCovid19.txt")
mystring2 <- read_file("C:/Users/Lenovo/Demo/TextAnalysis/TrumpAfterCovid19.txt")

fileName = "C:/Users/Lenovo/Demo/mostcommonwords1000.txt"
commonwords = read.csv(fileName, header = FALSE)


docs <- Corpus(VectorSource(c(mystring, mystring2))) %>%
  tm_map(removePunctuation) %>%
  tm_map(removeNumbers) %>%
  tm_map(tolower)  %>%
  tm_map(removeWords, c( "-", "chapter", "iii", "applause","’ve", "’t", "’ll", "’m", "’d", "’re", "’s",
                         "xvi", "cleanthes","philo", 
                         "footnote", "things", "prop", commonwords, stopwords("english"))) %>%
  tm_map(stripWhitespace) %>%
  tm_map(PlainTextDocument) 



tdm <- TermDocumentMatrix(docs) %>%
  as.matrix()
colnames(tdm) = c("President Trump\n before Covid19", "President Trump\n after Covid19")

# remove words by rownames of tdm
tdm = tdm[ ! rownames(tdm) %in% c("-", "chapter", "iii", "applause","’ve", "’t", "’ll", "’m", "’d", "’re", "’s"),]
tdm = tdm[which(nchar(rownames(tdm)) > 1), ]

findFreqTerms(TermDocumentMatrix(docs), lowfreq = 30)

par(mfrow=c(1,1))
#par(mar=c(2,2,2,2))
comparison.cloud(tdm, random.order=FALSE, colors = c("indianred3","lightsteelblue3"),
                 title.size=1.5, max.words=400)


# word associations
democrats = findAssocs(TermDocumentMatrix(docs), terms = "democrats", corlimit = 0.01)
incredible = findAssocs(TermDocumentMatrix(docs), terms = "incredible", corlimit = 0.01)

c(unlist(democrats), unlist(incredible))
