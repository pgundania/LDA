library(ndjson)
reviews <- ndjson :: stream_in("C:/Users/Bharat Rao/Desktop/video.json")
View(reviews)
reviews


library(quanteda)
library(stm)
library(tm)
library(NLP)
library(openNLP)
library(ggplot2)
library(ggdendro)
library(cluster)
library(fpc)  

require(quanteda)

#Generate DFM
corpus<- toLower(reviews$reviewText, keepAcronyms = FALSE) 
cleancorpus <- tokenize(corpus, 
                        removeNumbers=TRUE,  
                        removePunct = TRUE,
                        removeSeparators=TRUE,
                        removeTwitter=FALSE,
                        verbose=TRUE)

dfm<- dfm(cleancorpus,
          toLower = TRUE, 
          ignoredFeatures =stopwords("english"), 
          verbose=TRUE, 
          stem=TRUE)
# Reviewing top features
topfeatures(dfm, 50)    # displays 50 features

#####################
# Hierc. Clustering 
#####################
dfm.tm<-convert(dfm, to="tm")
dfm.tm #highly sparse data here as 89% zeros it has which means words occur only few times
dtmss <- removeSparseTerms(dfm.tm, 0.85)
dtmss
d.dfm <- dist(t(dtmss), method="euclidian")
fit <- hclust(d=d.dfm, method="average")
hcd <- as.dendrogram(fit)

require(cluster)
k<-5
plot(hcd, ylab = "Distance", horiz = FALSE, 
     main = "Five Cluster Dendrogram", 
     edgePar = list(col = 2:3, lwd = 2:2))
rect.hclust(fit, k=k, border=1:5) # draw dendogram with red borders around the 5 clusters

ggdendrogram(fit, rotate = TRUE, size = 4, theme_dendro = FALSE,  color = "blue") +
  xlab("Features") + 
  ggtitle("Cluster Dendrogram")

require(fpc)   
d <- dist(t(dtmss), method="euclidian")   
kfit <- kmeans(d, 5)   
clusplot(as.matrix(d), kfit$cluster, color=T, shade=T, labels=2, lines=0)


#######################################
### Advanced method for Topic Modeling
#######################################


library(dplyr)
require(magrittr)
library(tm)
library(ggplot2)
library(stringr)
library(NLP)
library(openNLP)

#passing Full Text to variable office1
office1<-reviews$reviewText

#Cleaning corpus
stop_words <- stopwords("SMART")
## additional junk words showing up in the data
stop_words <- c(stop_words, "said", "the", "also", "say", "just", "like","for", 
                "us", "can", "may", "now", "year", "according", "mr")
stop_words <- tolower(stop_words)


office1 <- gsub("'", "", office1) # remove apostrophes
office1 <- gsub("[[:punct:]]", " ", office1)  # replace punctuation with space
office1 <- gsub("[[:cntrl:]]", " ", office1)  # replace control characters with space
office1 <- gsub("^[[:space:]]+", "", office1) # remove whitespace at beginning of documents
office1 <- gsub("[[:space:]]+$", "", office1) # remove whitespace at end of documents
office1 <- gsub("[^a-zA-Z -]", " ", office1) # allows only letters
office1 <- tolower(office1)  # force to lowercase

## get rid of blank docs
office1 <- office1[office1 != ""]

# tokenize on space and output as a list:
doc.list <- strsplit(office1, "[[:space:]]+")

# compute the table of terms:
term.table <- table(unlist(doc.list))
term.table <- sort(term.table, decreasing = TRUE)


# remove terms that are stop words or occur fewer than 5 times:
del <- names(term.table) %in% stop_words | term.table < 5
term.table <- term.table[!del]
term.table <- term.table[names(term.table) != ""]
vocab <- names(term.table)

# now put the documents into the format required by the lda package:
get.terms <- function(x) {
  index <- match(x, vocab)
  index <- index[!is.na(index)]
  rbind(as.integer(index - 1), as.integer(rep(1, length(index))))
}
documents <- lapply(doc.list, get.terms)
 
#############
# Compute some statistics related to the data set:
D <- length(documents)  # number of documents (1)
W <- length(vocab)  # number of terms in the vocab (1741)
doc.length <- sapply(documents, function(x) sum(x[2, ]))  # number of tokens per document [312, 288, 170, 436, 291, ...]
N <- sum(doc.length)  # total number of tokens in the data (56196)
term.frequency <- as.integer(term.table) 

# MCMC and model tuning parameters:
K <- 10 #no of topics
G <- 3000 #run 3000 times
alpha <- 0.02
eta <- 0.02

# Fit the model:
library(lda)
set.seed(357)
t1 <- Sys.time()
fit <- lda.collapsed.gibbs.sampler(documents = documents, K = K, vocab = vocab, 
                                   num.iterations = G, alpha = alpha, 
                                   eta = eta, initial = NULL, burnin = 0,
                                   compute.log.likelihood = TRUE)
t2 <- Sys.time()
## display runtime
t2 - t1  #time span

theta <- t(apply(fit$document_sums + alpha, 2, function(x) x/sum(x)))
phi <- t(apply(t(fit$topics) + eta, 2, function(x) x/sum(x)))

office_for_LDA <- list(phi = phi,
                     theta = theta,
                     doc.length = doc.length,
                     vocab = vocab,
                     term.frequency = term.frequency)

library(LDAvis)
library(servr) #run a vis where your comp being server

# create the JSON object to feed the visualization:
json <- createJSON(phi =office_for_LDA$phi, 
                   theta = office_for_LDA$theta, 
                   doc.length = office_for_LDA$doc.length, 
                   vocab = office_for_LDA$vocab, 
                   term.frequency = office_for_LDA$term.frequency)

serVis(json, out.dir = 'visBA', open.browser = TRUE)
getwd("visBA")

# ek folder create hoga jisko upload karna hailibrary(quanteda)

