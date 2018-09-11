library(caret)
######## use parallel to save time 
library(parallel)
library(doParallel)



cluster<- makeCluster(detectCores()-1)
registerDoParallel(cluster)

fitControl <- trainControl(method="cv", number=3, allowParallel = TRUE)
# Train.


#### efter att matris med respons Y r skapad i data.prework.r


trows<- createDataPartition(all_data$V2, p=0.7) # 70 % traingin

train<- hm[trows[[1]],]
test<- hm[-trows[[1]],]

table(train$y)

#bayesglm i metod kanske ocks? 
fit <- train(y ~ ., data =train , method = 'bayesglm', trControl= fitControl)

# Check accuracy on training.
preds<- predict(fit, newdata = test)
fit
confusionMatrix(preds, test$y)

# stop the cluster, important #######
stopCluster(cluster)
registerDoSEQ()
##############################
# hitta bästa n of topics 


#perplexity, evaluates a probabilistic model by measuring the log likelihood of a held out test set, perplex is a measure 
# of how well a prob dist or prob model predicts a samplpe 

traindata<- as.data.frame(s3_2[trows[[1]],1])
testdata<- as.data.frame(s3_2[-trows[[1]],1])

### test 
test_corpus<-VCorpus(VectorSource(testdata[,1]))
test_corpus<- tm_map(test_corpus, content_transformer(tolower))
test_corpus<-tm_map(test_corpus, removeWords, c(stopwords("swedish"), "ska"))
test_corpus<-tm_map(test_corpus, stemDocument, language="swedish")

tdm_test <- DocumentTermMatrix(test_corpus, list(removePunctuation = TRUE, removeWords, stopwords("swedish"), removeNumbers = TRUE))
## train 
train_corpus<-VCorpus(VectorSource(traindata[,1]))
train_corpus<- tm_map(train_corpus, content_transformer(tolower))
train_corpus<-tm_map(train_corpus, removeWords, c(stopwords("swedish"), "ska"))
train_corpus<-tm_map(train_corpus, stemDocument, language="swedish")

tdm_train <- DocumentTermMatrix(train_corpus, list(removePunctuation = TRUE, removeWords, stopwords("swedish"), removeNumbers = TRUE))



################## test och train på stora datat 

traindata<- as.data.frame(big_corpus[trows[[1]],1])
testdata<- as.data.frame(big_corpus[-trows[[1]],1])

### test 
test_corpus<-VCorpus(VectorSource(testdata[,1]))
test_corpus<- tm_map(test_corpus, content_transformer(tolower))
test_corpus<-tm_map(test_corpus, removeWords, c(stopwords("swedish"), "ska"))
test_corpus<-tm_map(test_corpus, stemDocument, language="swedish")

tdm_test <- DocumentTermMatrix(test_corpus, list(removePunctuation = TRUE, removeWords, stopwords("swedish"), removeNumbers = TRUE))
## train 
train_corpus<-VCorpus(VectorSource(traindata[,1]))
train_corpus<- tm_map(train_corpus, content_transformer(tolower))
train_corpus<-tm_map(train_corpus, removeWords, c(stopwords("swedish"), "ska"))
train_corpus<-tm_map(train_corpus, stemDocument, language="swedish")

tdm_train <- DocumentTermMatrix(train_corpus, list(removePunctuation = TRUE, removeWords, stopwords("swedish"), removeNumbers = TRUE))


rowTotals <- apply(tdm_train , 1, sum) #Find the sum of words in each Document
tdm_train   <- tdm_train[rowTotals> 0, ]           #remove all docs without words
which(rowTotals==0) #250 från train. 


rowTotals <- apply(tdm_test , 1, sum) #Find the sum of words in each Document
tdm_test   <- tdm_test[rowTotals> 0, ]           #remove all docs without words
which(rowTotals==0) #250 från train. 

#####
less_sparse<- removeSparseTerms(dtm_big,0.999)

rowTotals <- apply(less_sparse , 1, sum) #Find the sum of words in each Document
less_sparse   <- less_sparse[rowTotals> 0, ]           #remove all docs without words

tdm_train_ls<- removeSparseTerms(tdm_train,0.99)
tdm_test_ls<- removeSparseTerms(tdm_test,0.99)

rowTotals <- apply(tdm_train_ls , 1, sum) #Find the sum of words in each Document
tdm_train_ls   <- tdm_train_ls[rowTotals> 0, ]           #remove all docs without words


rowTotals <- apply(tdm_test_ls , 1, sum) #Find the sum of words in each Document
tdm_test_ls   <- tdm_test_ls[rowTotals> 0, ]           #remove all docs without words


perplexity_df <- data.frame(train=numeric(), test=numeric())
topics <- c(2:3)
burnin = 100
iter = 1000
keep = 50

set.seed(12345)
for (i in topics){
  
  fitted <- LDA(tdm_train_ls, k = i, method = "Gibbs",
                control = list(burnin = burnin, iter = iter, keep = keep) )
  perplexity_df[i,1] <- perplexity(fitted, newdata = tdm_train_ls)
  perplexity_df[i,2]  <- perplexity(fitted, newdata = tdm_test_ls) 
}

#

ldamod<- LDA(less_sparse, k = 3, method="Gibbs", control=list(burnin = 100, iter= 1000, keep= 50))
posterior(ldamod, less_sparse[1:10,])
terms(ldamod, 20)
##plotting the perplexity of both train and test

g <- ggplot(data=perplexity_df, aes(x= as.numeric(row.names(perplexity_df)))) + labs(y="Perplexity",x="Number of topics") + ggtitle("Perplexity of hold out  and training data")

g <- g + geom_line(aes(y=test), colour="red")
g <- g + geom_line(aes(y=train), colour="green")
g


############################# k fold cv med parallel 


cluster <- makeCluster(detectCores(logical = TRUE) - 1) # leave one CPU spare...
registerDoParallel(cluster)

clusterEvalQ(cluster, {
  library(topicmodels)
})

folds <- 5
splitfolds <- sample(1:folds, 1310, replace = TRUE)
candidate_k <- c(2:15) # candidates for how many topics
#clusterExport(cluster, c("train_set", "burnin", "iter", "keep", "splitfolds", "folds", "candidate_k"))

# we parallelize by the different number of topics.  A processor is allocated a value
# of k, and does the cross-validation serially.  This is because it is assumed there
# are more candidate values of k than there are cross-validation folds, hence it
# will be more efficient to parallelise
system.time({
  results <- foreach(j = 1:length(candidate_k), .combine = rbind) %dopar%{
    k <- candidate_k[j]
    results_1k <- matrix(0, nrow = folds, ncol = 2)
    colnames(results_1k) <- c("k", "perplexity")
    for(i in 1:folds){
      train_set <- less_sparse[splitfolds != i , ]
      valid_set <- less_sparse[splitfolds == i, ]
      
      fitted <- LDA(train_set, k = k, method = "Gibbs",
                    control = list(burnin = burnin, iter = iter, keep = keep) )
      results_1k[i,] <- c(k, perplexity(fitted, newdata = valid_set))
    }
    return(results_1k)
  }
})
# stop the cluster, important #######
stopCluster(cluster)
registerDoSEQ()
results_df <- as.data.frame(results)
save(results_df, file = "5foldgibbs.rda")
ggplot(results_df, aes(x = k, y = perplexity)) +
  geom_point() +
  geom_smooth(se = FALSE) +
  ggtitle("5-fold cross-validation of topic modeling with the Elliston dataset",
          "(ie five different models fit for each candidate number of topics)") +
  labs(x = "Candidate number of topics", y = "Perplexity when fitting the trained model to the hold-out set")



######### LDA , ska göras efter "data_prework.R 

library("topicmodels")
library(tidytext)
library(tidyr)
library(dplyr)
#LDA 


LDA(tdm,8)
#
#Error in LDA(tdm, 8) : 
#  Each row of the input matrix needs to contain at least one non-zero entry


rowTotals <- apply(tdm , 1, sum) #Find the sum of words in each Document
tdm.new   <- tdm[rowTotals> 0, ]           #remove all docs without words
#ta bort från Y 
empt_rows<- which(rowTotals==0)
Yparti<- Yparti[-empt_rows]

ldatest<- LDA(tdm.new, 3)


topics_tidy<- tidy(ldatest)

top_terms<- topics_tidy %>%
  group_by(topic) %>%
  top_n(20,beta) %>%
  ungroup() %>%
  arrange(topic, -beta)

top_terms

stopwords("swedish")

library(ggplot2)
theme_set(theme_bw())

top_terms %>%
  mutate(term = reorder(term, beta)) %>%
  ggplot(aes(term, beta)) +
  geom_bar(stat = "identity") +
  facet_wrap(~ topic, scales = "free") +
  theme(axis.text.x = element_text(size = 15, angle = 90, hjust = 1))

topics_lda_gamma <- tidy(ldatest, matrix = "gamma")
topics_lda_gamma


