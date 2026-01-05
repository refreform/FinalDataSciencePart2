
library(tm)
library(R.utils)
library(data.table)
library(ggplot2)
library(stringi)
library(plotly)
library(slam)
set.seed(237237021)
file1 <- "en_US.twitter.txt"
file2 <- "en_US.news.txt"
file3 <- "en_US.blogs.txt"

#--------------------

data1 <- readLines(file1, encoding = "latin1", warn = FALSE)
data2 <- readLines(file2, encoding = "latin1", warn = FALSE)
data3 <- readLines(file3, encoding = "latin1", warn = FALSE)

#--------------------

# Frequency
sample_tweets <- sample(data1, 10000)
sample_news <- sample(data2, 10000)
sample_blogs <- sample(data3, 10000)
samples<-c(sample_tweets,sample_news,sample_blogs)

# Creating the corpus
corp <- VCorpus(VectorSource(samples))
corp <- tm_map(corp,content_transformer(tolower))
corp <- tm_map(corp, removePunctuation)
corp <- tm_map(
  corp,
  content_transformer(stri_trans_general),
  "Any-Latin; Latin-ASCII"
)
corp <- tm_map(corp, stripWhitespace)
corp <- tm_map(corp, removePunctuation)
dtm <- DocumentTermMatrix(corp)
freq <- sort(colSums(as.matrix(dtm)), decreasing = TRUE)

#tokenization and trailing words 
Tokenizer <- function(x) {
  unlist(lapply(ngrams(words(x), 2), paste, collapse = "_"))
}

# creating table of tokens and their frequency 
dtm_bigram <- DocumentTermMatrix(corp, 
                                 control = list(tokenize = Tokenizer))
bigram_freq <- slam::col_sums(dtm_bigram)

#--------------------

bigram_df <- data.frame(
  bigram = names(bigram_freq),
  count = as.numeric(bigram_freq),
  stringsAsFactors = FALSE
)

bigram_df$prev <- sub("_.*$", "", bigram_df$bigram)
bigram_df$nex <- sub("^.*_", "", bigram_df$bigram)

#-------------------

distinct_followers <- aggregate(
  nex ~ prev,
  data = bigram_df,
  FUN = function(x) length(unique(x))
)

max(distinct_followers$nex)
distinct_followers[distinct_followers$nex == max(distinct_followers$nex), ]

# The is the most commonly used word!

best_next <- aggregate(
  count ~ prev + nex,
  data = bigram_df,
  FUN = sum
)

best_next <- best_next[order(best_next$prev, -best_next$count), ]

best_next <- best_next[!duplicated(best_next$prev), ]

write.csv(best_next,"nextword.csv",row.names=TRUE)
