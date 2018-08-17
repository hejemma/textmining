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

setwd("~/master2ht/text mining/projekt/textmining")

head(all_data)
library(tm)
library(caret)
## test sample 
set.seed(930107)
small_data<-all_data[sample(nrow(all_data),400, replace=F),]

#parti
Yparti <- small_data$V2

# tar bort alla konstiga tecken förutom citattecken 
small_data2<- data.frame(gsub("[[:punct:]]", "", small_data$V1))
colnames(small_data2)<-"sm"

# tar bort citattecknen 
small_corpus<-data.frame(gsub("[^[:alnum:]///' ]", "", small_data2$sm))



# ta bort alla länkar 
s3<- as.data.frame(gsub("http\\w+ *"," ", small_corpus[,1]))
s3_2<- as.data.frame(gsub("www\\w+ *"," ", s3[,1]))

#empty rows after cleaning , char =1 av nån anledning 
empty_ind<- which(nchar(as.character(s3_2[,1]))==1)

#### bara om empty_ind !=0 
s3_2<- data.frame(s3_2[-empty_ind,])
Yparti<- Yparti[-empty_ind]



# bara tweets 
small_corpus<-VCorpus(VectorSource(s3_2[,1]))

small_corpus<- tm_map(small_corpus, content_transformer(tolower))

small_corpus<-tm_map(small_corpus, removeWords, c(stopwords("swedish"), "ska"))

small_corpus<-tm_map(small_corpus, stemDocument, language="swedish")


tdm <- DocumentTermMatrix(small_corpus, list(removePunctuation = TRUE, removeWords, stopwords("swedish"), removeNumbers = TRUE))


################################ DTM på all data 

# tar bort alla konstiga tecken förutom citattecken 
all_data2<- data.frame(gsub("[[:punct:]]", "", all_data$V1))
colnames(all_data2)<-"sm"

# tar bort citattecknen 
big_corpus<-data.frame(gsub("[^[:alnum:]///' ]", "", all_data2$sm))



# ta bort alla länkar 
big_corpus<- as.data.frame(gsub("http\\w+ *"," ", big_corpus[,1]))
big_corpus<- as.data.frame(gsub("www\\w+ *"," ", big_corpus[,1]))

#empty rows after cleaning , char =1 av nån anledning 
empty_ind<- which(nchar(as.character(big_corpus[,1]))==0)

#### bara om empty_ind !=0 
s3_2<- data.frame(s3_2[-empty_ind,])
Yparti<- Yparti[-empty_ind]



# bara tweets 
big_corpus<-VCorpus(VectorSource(big_corpus[,1]))

big_corpus<- tm_map(big_corpus, content_transformer(tolower))

big_corpus<-tm_map(big_corpus, removeWords, c(stopwords("swedish"), "ska"))

big_corpus<-tm_map(big_corpus, stemDocument, language="swedish")


dtm_big <- DocumentTermMatrix(big_corpus, list(removePunctuation = TRUE, removeWords, stopwords("swedish"), removeNumbers = TRUE))





## för att göra predictions. 
tmat<- as.matrix(tdm.new)
hm<-as.data.frame(tmat)
hm<- cbind(hm, Yparti)
colnames(hm)[ncol(hm)]<-'y'
hm$y<-as.factor(hm$y)


#tf idf 

tdm_tfidf<- weightTfIdf(tdm, normalize = TRUE)
inspect(tdm_tfidf)












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

