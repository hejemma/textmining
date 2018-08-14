rm(list=ls())

setwd("~/master2ht/text mining/projekt")
dir()
v<- read.csv2("V.csv", sep=",")
c<-read.csv2("Centerpartiet.csv", sep=",")
kd<-read.csv2("kdriks.csv", sep=",")
lib<-read.csv2("liberalerna.csv",sep=",")
miljo<-read.csv2("miljopartiet.csv",sep=",")
m<-read.csv2("nya_moderaterna.csv", sep=",")
s<-read.csv2("socialdemokrat.csv", sep=",")
sd<-read.csv2("sdriks.csv", sep=",")


###############################################################
all_data<- rbind(v,c,kd,lib,miljo,m,s,sd)

#ta bort alla @ och det som f?ljer 
#all_data$V1<- gsub("@\\w+ *","", all_data$V1)

#ta bort alla hashtags 
#all_data$V1<- gsub("#\\w+ *","", all_data$V1)


# ta bort alla tecken som ej ?r bokst?ver 
all_data$V1<-gsub("[^[:alnum:] ]", "", all_data$V1)


#ta bort alla l?nkadresser 

all_data$V1<- gsub("http\\w+ *","", all_data$V1)


# ta bort alla tommar rader 
empty_ind<- which(nchar(as.matrix(all_data[,2]))==0)
all_data<- all_data[-empty_ind,]
rm(empty_ind)
#reset row count after removing rows 
rownames(all_data)<-NULL


#remove csvfiles from envir
rm(v,c,kd,lib,miljo,m,s,sd)




head(all_data)
library(tm)
library(caret)
## test sample 
set.seed(930107)
small_data<-all_data[sample(nrow(all_data),400, replace=F),]

# bara tweets 
small_corpus<-VCorpus(VectorSource(small_data$V1))

small_corpus<- tm_map(small_corpus, content_transformer(tolower))

small_corpus<-tm_map(small_corpus, removeWords, stopwords("swedish"))

small_corpus<-tm_map(small_corpus, stemDocument, language="swedish")

tdm$dimnames

tdm <- DocumentTermMatrix(small_corpus, list(removePunctuation = TRUE, removeWords, stopwords("swedish"), removeNumbers = TRUE))
tmat<- as.matrix(tdm)
hm<-as.data.frame(tmat)
hm<- cbind(hm, small_data$V2)
colnames(hm)[ncol(hm)]<-'y'
hm$y<-as.factor(hm$y)


# ta bort http 
library(tidyverse) 
hm2 <- hm %>% select(-contains("http"))



######## use parallel to save time 
library(parallel)
library(doParallel)



cluster<- makeCluster(detectCores()-1)
registerDoParallel(cluster)

fitControl <- trainControl(method="cv", number=3, allowParallel = TRUE)
# Train.

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

