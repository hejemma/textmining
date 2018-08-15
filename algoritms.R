
######## use parallel to save time 
library(parallel)
library(doParallel)



cluster<- makeCluster(detectCores()-1)
registerDoParallel(cluster)

fitControl <- trainControl(method="cv", number=3, allowParallel = TRUE)
# Train.


#### efter att matris med respons Y r skapad i data.prework.r


trows<- createDataPartition(hm$y, p=0.7) # 70 % traingin

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


######### LDA , ska göras efter "data_prework.R 



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


