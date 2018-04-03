# Twitter sentiment analysis using Latent Dirichlet Allocation

# -------- setup -----
# change directory
setwd("/Users/heidihurst/Documents/UK/ucl-gis/ucl_msc/term2/stdm/")

# packages
install.packages(c("stringr","SnowballC","lda","LDAvis", "leaflet"))
library("plyr")
library("stringr")
library("tm") 
library("SnowballC")
library("lda")
library("LDAvis")
library("leaflet")

# -------- preprocessing ----
# load twitter data
Twitter <- read.csv("TwitterSample.csv")
Tweets_corpus <- Corpus(VectorSource(Twitter$TWEET_TEXT))

# process
Tweets_corpus <- tm_map(Tweets_corpus, tolower)
Tweets_corpus <- tm_map(Tweets_corpus, removePunctuation)
Tweets_corpus <- tm_map(Tweets_corpus, removeNumbers)
Tweets_corpus <- tm_map(Tweets_corpus, function(x) gsub("http[[:alnum:]]*","",x))
Tweets_corpus <- tm_map(Tweets_corpus, function(x) iconv(x, "latin1", "ASCII", sub=""))
Tweets_corpus <- tm_map(Tweets_corpus, removeWords, stopwords("SMART"))
Tweets_corpus <- tm_map(Tweets_corpus, removeWords, c("london","im","ive","dont","didnt"))

# convert and stem, removing whitespace
Tweets_corpus <- tm_map(Tweets_corpus, PlainTextDocument)
Tweets_corpus <- tm_map(Tweets_corpus, stemDocument)
Tweets_corpus <- tm_map(Tweets_corpus, stripWhitespace)

# unlist corpus
Tweet_Clean <- lapply(Tweets_corpus$content$content, as.character)
# bind with twitter data
Twitter$Tweet_Clean <- as.character(Tweet_Clean)
# check it out 
Twitter[1:10,]

# -------- preparation for topic modeling -------
# tokenizing - chop into words
doc.list <- strsplit(unlist(Tweet_Clean), "[[:space:]]+")
# compute table of terms
term.table <- table(unlist(doc.list))
term.table <- sort(term.table, decreasing = TRUE)
term.table <- term.table[term.table>3]
vocab <- names(term.table)

# format for IDA package
get.terms <- function(x) {
  index <- match(x, vocab)
  index <- index[!is.na(index)]
  rbind(as.integer(index-1), as.integer(rep(1,length(index))))
}
documents <- lapply(doc.list, get.terms)

# compute summary statistics
D <- length(documents)
W <- length(vocab)
doc.length <- sapply(documents, function(x) sum(x[2,]))
N <- sum(doc.length)
term.frequency <- as.integer(term.table)

# -------- fit LDA model -------
K <- 20
G <- 1000
alpha <- 0.1
eta <- 0.1

t1 <- print(Sys.time())
lda_fit <- lda.collapsed.gibbs.sampler(documents = documents, K=K, vocab=vocab, num.iterations=G, alpha=alpha, eta=eta)
t2 <- print(Sys.time())
t2-t1 # 2.4

# ------- LDA outputs --------
top_words <- top.topic.words(lda_fit$topics, 30,by.score=TRUE)
# create interactive visualizations
theta <- t(apply(lda_fit$document_sums + alpha, 2, function(x) x/sum(x)))
phi <- t(apply(t(lda_fit$topics) + eta, 2, function(x) x/sum(x)))

Tweet_Topics <- list(phi=phi, theta=theta, doc.length=doc.length, vocab=vocab, term.frequency = term.frequency)
Tweet_Topics_json <- with(Tweet_Topics, createJSON(phi,theta,doc.length, vocab, term.frequency))
serVis(Tweet_Topics_json) # opens in separate  view

# ------- Map of tweets --------
doc_topic <- apply(lda_fit$document_sums, 2, function(x) which(x == max(x))[1])
Twitter$topic <- doc_topic

#  make leaflet map in R
factpal <- colorFactor(topo.colors(20), Twitter$topic)
m <- leaflet(Twitter) %>%
  addTiles() %>%
  addCircleMarkers(~LON, ~LAT, color=~factpal(topic), stroke=TRUE, fillOpacity = 0.8,
                   popup = paste("Tweet:", Twitter$TWEET_TEXT,"<br>",
                                 "Time:",Twitter$STATUE_DAT,"<br>",
                                 "Topic:",Twitter$topic))
m # print to display


