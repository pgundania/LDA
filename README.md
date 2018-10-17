# LDA VISUALIZATION FOR AMAZON OFFICE PRODUCT REVIEWS

the following Assignment was designed to build an interactive LDA environment to interpret the topics for Amazon Office Product Reviews. The reviews are in a .json file which has 10,260 reviews.
This file was read in RStudio and analyzed by using the below code:

```
library(ndjson)
reviews <- ndjson :: stream_in("C:/Users/priya/Desktop/office.json")
View(reviews)

```
### LDA ANALYSIS AND VISUALIZATION

Linear Discrimant Analysis is a classification method which produces a interactive interface for topic modelling with the help of packages *lda* and *LDAvis*

### Fitting the model 

```
library(lda)
set.seed(357)
t1 <- Sys.time()
fit <- lda.collapsed.gibbs.sampler(documents = documents, K = K, vocab = vocab, 
                                   num.iterations = G, alpha = alpha, 
                                   eta = eta, initial = NULL, burnin = 0,
                                   compute.log.likelihood = TRUE)
t2 <- Sys.time()

```
### Display the runtime

```
t2 - t1  #time span
theta <- t(apply(fit$document_sums + alpha, 2, function(x) x/sum(x)))
phi <- t(apply(t(fit$topics) + eta, 2, function(x) x/sum(x)))

office_for_LDA <- list(phi = phi,
                     theta = theta,
                     doc.length = doc.length,
                     vocab = vocab,
                     term.frequency = term.frequency)
                     
```
### Creating JSON object to feed the visualization 

```
library(LDAvis)
library(servr) 
# create the JSON object to feed the visualization:
json <- createJSON(phi =office_for_LDA$phi, 
                   theta = office_for_LDA$theta, 
                   doc.length = office_for_LDA$doc.length, 
                   vocab = office_for_LDA$vocab, 
                   term.frequency = office_for_LDA$term.frequency)

serVis(json, out.dir = 'vis', open.browser = TRUE)

```
# OUTPUT 

The output is stored on your local computer in a folder named vis which has 5 files with different formats i.e .js .html .css .json
The index.html gives you and output and you can check the [output](https://cdn.rawgit.com/pgundania/LDA/master/officeBA/index.html)

The topic modelling for Office data has given 10 most relevant topics. When we consider the topic 1, the LDAvis shows the top 30 terms associated with topic 1. The lambda values gives the ranking of terms based on their relevance to each of these topics.

In the example, if we consider topic 1, the most relevant terms are printer,print,paper,printing,scanner for a lambda of 1. Topic 2, deals with the notes. We can find words like notes, post, words, sticky.Similarly, the terms for different topics can be sorted using this method for different values of lambda. The link for the LDAvis interactive file is given below.

https://htmlpreview.github.io/#topic=2&lambda=1&term=

