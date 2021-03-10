###############################################
#Topic Modeling using Latent Semantic Analysis#
###############################################

#We construct the example matrix
human <- c(1,0,0,1,0,0,0,0,0)
interface <- c(1,0,1,0,0,0,0,0,0)
computer <- c(1,1,0,0,0,0,0,0,0)
user <- c(0,1,1,0,1,0,0,0,0)
system <- c(0,1,1,2,0,0,0,0,0)
response <- c(0,1,0,0,1,0,0,0,0)
time <- c(0,1,0,0,1,0,0,0,0)
EPS <- c(0,0,1,1,0,0,0,0,0)
survey <- c(0,1,0,0,0,0,0,0,1)
trees <- c(0,0,0,0,0,1,1,1,0)
graph <- c(0,0,0,0,0,0,1,1,1)
minor <- c(0,0,0,0,0,0,0,1,1)
X <- rbind(human,interface,computer,user,system,response,time,EPS,survey,trees,graph,minor)
colnames(X) <- c("c1","c2","c3","c4","c5","m1","m2","m3","m4")

#measure the intial correlation between several rows
cor(human,user, method="spearman")
cor(human,minor, method="spearman")

#Do the singular value decomposition on the X matrix
SVD <- svd(X)
print(SVD)
rownames(SVD$u) = rownames(X) 
rownames(SVD$v) = colnames(X)
u <- as.matrix(SVD$u)
v <- as.matrix(SVD$v)
d <- SVD$d
#we can set all but the first two singular values to zero. this means that r=2
d[3:9] <- 0
d_r <- as.matrix(diag(d))
#we put the matrix back together but now with only keeping the first two singular values
X_r <- u %*% d_r %*% t(v)
print(round(X_r,2))

#measure the correlations among the rows(=the words)
cor(t(X_r), method="pearson")
cor(t(X_r), method="spearman")

#measure the correlations among the columns(=the documents)
cor(X,method="pearson")
cor((X_r), method="pearson")
#the differences between the different topics should now be more clearly defined

##################################################
#Topic Modeling using Latent Dirichlet Allocation#
##################################################

#We will use the topicmodels package
install.packages("topicmodels")
install.packages("quanteda")
library(topicmodels)
library(quanteda)
#the package contains a dataset of Associated Press articles

#we have to set k to the requested number of topics
#set a seed so that the output of the model is predictable
AP_topic_model <- LDA(AssociatedPress, k = 10, control = list(seed = 1234))
AP_topic_model
#get the 20 terms that are most associated with the topics
terms(AP_topic_model, 20)
#get the most likely topics for each document
t(topics(AP_topic_model, 2))

#We can use tidyverse to produce nicer graphics. 
#See the book Text Mining with R for further information and code.
install.packages("tidytext")
library(tidytext)
library(dplyr)
library(ggplot2)

#Beta refers to the word probabilities per topic
AP_topics <- tidy(AP_topic_model, matrix = "beta")
ap_top_terms <- 
  AP_topics %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)
ap_top_terms %>%
  mutate(term = reorder(term, beta)) %>%
  ggplot(aes(term, beta, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  coord_flip()

#Gamma refers to the topic probabilities per document
ap_documents <- tidy(AP_topic_model, matrix = "gamma")
ap_documents

tidy(AssociatedPress) %>%
  filter(document == 6) %>%
  arrange(desc(count))

##################################################
#Structural Topic Modeling, or STM. 
##################################################

#STM is very similar to LDA, but it employs meta data about documents 
#(such as the name of the author or the date in which the document was produced) 
#to improve the assignment of words to latent topics in a corpus.

#import 13246 blog posts 
google_doc_id <- "1LcX-JnpGB0lU1iDnXnxB6WFqBywUKpew" # google file ID
poliblogs<-read.csv(sprintf("https://docs.google.com/uc?id=%s&export=download", google_doc_id), stringsAsFactors = FALSE)

#The `textProcessor` function in STM automatically removes a) punctuation; b) stop words; c) numbers, and d) stems each word.
install.packages("stm")
library(stm)
processed <- textProcessor(poliblogs$documents, metadata = poliblogs)

#the `stm` package  requires us to store the documents, meta data, and "vocab"---or total list of words described in the documents---in separate objects (see code below). 
#The first line of code eliminates both extremely common terms and extremely rare terms, as is common practice in topic modeling, since such terms make word-topic assignment much more difficult.
out <- prepDocuments(processed$documents, processed$vocab, processed$meta)
docs <- out$documents
vocab <- out$vocab
meta <-out$meta

#Before we run our first model, we have to make another decision about the number of topics we might expect to find in the corpus. Let's start out with 10. 
#We also need to specify how we want to use the meta data. This model uses both the "rating" variable (that describes whether the blog is liberal or conservative) as well as the day or date variable to improve topic classification.
First_STM <- stm(documents = out$documents, vocab = out$vocab,
                 K = 10, prevalence =~ rating + s(day) ,
                 max.em.its = 75, data = out$meta,
                 init.type = "Spectral", verbose = FALSE)

#inspect our results by browsing the top words associated with each topic
plot(First_STM)

#The `stm` package has another useful function called `findThoughts` which extracts passages from documents within the corpus that load high on topics specified by the user.
findThoughts(First_STM, texts = poliblogs$documents,
             n = 1, topics = 3)

#The `stm` package has a useful function called `searchK` which allows the user to specify a range of values for `k`, runs STM models for each value of 'k', and then outputs multiple goodness-of-fit measures that are very useful in identifying a range of values of `k` that provide the best fit for the data. The syntax of this function is very similar to the `stm` function, except that the user specifies a range for `k` as one of the arguments. In the code below, we search all values of `k` between 7 and 10.
findingk <- searchK(out$documents, out$vocab, K = c(7:10),
                    prevalence =~ rating + s(day), data = meta, verbose=FALSE)
plot(findingk)

#One of the principal advantages of STM is that one can examine the relationship between topics and various covariates of interest. 
#Here we use the `estimateEffect` function to examine the relationship between the liberal/conservative `rating` variable and the first 10 topics, as well as time (`day`).
predict_topics<-estimateEffect(formula = 1:10 ~ rating + s(day), stmobj = First_STM, metadata = out$meta, uncertainty = "Global")

#Once we have the model, we can plot the relationships. The code below picks three topics and plots them according to their association with the liberal/conservative `rating` variable.
plot(predict_topics, covariate = "rating", topics = c(3, 5, 9),
     model = First_STM, method = "difference",
     cov.value1 = "Liberal", cov.value2 = "Conservative",
     xlab = "More Conservative ... More Liberal",
     main = "Effect of Liberal vs. Conservative",
     xlim = c(-.1, .1), labeltype = "custom",
     custom.labels = c('Topic 3', 'Topic 5','Topic 9'))


#We can also plot change in the prevalence of topic over time. The code below plots change in the prevalence of topic 3.
plot(predict_topics, "day", method = "continuous", topics = 3,
     model = z, printlegend = FALSE, xaxt = "n", xlab = "Time (2008)")
monthseq <- seq(from = as.Date("2008-01-01"),
                to = as.Date("2008-12-01"), by = "month")
monthnames <- months(monthseq)
axis(1,at = as.numeric(monthseq) - min(as.numeric(monthseq)),
     labels = monthnames)

######################



