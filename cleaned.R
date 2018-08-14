## hela datat  

# bara tweets 
corpus<-VCorpus(VectorSource(all_data$V1))
#lowercase 
corpus<- tm_map(corpus, content_transformer(tolower))
#stopwords 
corpus<-tm_map(corpus, removeWords, stopwords("swedish"))

corpus<-tm_map(corpus, stemDocument, language="swedish")

#library(corpus)

#text_tokens(all_data[1,2], stemmer="swe")

tdm <- DocumentTermMatrix(corpus, list(removePunctuation = TRUE, removeWords, stopwords("swedish"), removeNumbers = TRUE))
###
nonsparse <- removeSparseTerms(tdm, 0.999)

tmat<- as.matrix(nonsparse)
hm<-as.data.frame(tmat)
hm<- cbind(hm, all_data$V2)
colnames(hm)[ncol(hm)]<-'y'
hm$y<-as.factor(hm$y)

head(hm)

###

tmat<- as.matrix(tdm)
hm<-as.data.frame(tmat)
hm<- cbind(hm, all_data$V2)
colnames(hm)[ncol(hm)]<-'y'
hm$y<-as.factor(hm$y)
rm(tmat)

# ta bort http 
#library(tidyverse) 
#hm2 <- hm %>% select(-contains("http"))

### some statistics 

freq<- colSums(as.matrix(tdm[,1:ncol(tdm)-1]))
ord <- order(freq,decreasing=TRUE)
freq[head(ord)]
freq[tail(ord)]
ord[1:10]
freq[ord[1:100]]

findAssocs(tdm, "sverig", 0.1)









#ta bort ovanliga ord 
ettor<- (which(freq==1))
colSums(as.matrix(hm[,1831]))

colnames(hm[,1831])


cluster<- makeCluster(detectCores()-1)
registerDoParallel(cluster)

fitControl <- trainControl(method="cv", number=2, allowParallel = TRUE)
# Train.

trows<- createDataPartition(hm$y, p=0.7) # 70 % traingin

train<- hm[trows[[1]],]
test<- hm[-trows[[1]],]

table(train$y)

#bayesglm i metod kanske också 
fit <- train(y ~ ., data =train , method = 'glmnet', trControl= fitControl)  #56 % acc 

fit_glmnet<- fit



fit <- train(y ~ ., data =train , method = 'polr', trControl= fitControl)# typ alla blir preddade som C 


##check different number hidden nodes 

acctable<- matrix(0,ncol=2,nrow=6)
acctable[,1]<-hidden
hidden<- c(10,25,50,70,90,110)
################## bara 64 på 70 gömda 
for (i in 1:6){
  f<-train(y~., data=train, method="mlp", size=hidden[i], trControl= fitControl)
  pr<- predict(f, newdata=test)
  m<- confusionMatrix(pr, test$y)
  acctable[i,2]<-m$overall[[1]]
}



# Check accuracy on training.
preds<- predict(fit, newdata = test)
fit$modelInfo
cmat<-confusionMatrix(preds, test$y)

cmat$overall[1]










stopCluster(cluster)
registerDoSEQ()
