################mnaive bayes 

library(e1071)

## skapa dtm_big från data_prework 
freq_words<- findFreqTerms(dtm_big,5)

dtmover5<- dtm_big[,freq_words]

## för CV 
K<- 6580/5

data<-1:6582
K<-5
N<-length(data)
list_nb<- list()

#Create 10 equally size folds
folds <- cut(seq(1,nrow(dtmover5)),breaks=5,labels=FALSE)
table(folds)
#Perform 10 fold cross validation


naiveBayes_cv<- function(K, X,Y){
  starttime<- Sys.time()
  folds <- cut(seq(1,nrow(X)),breaks=K,labels=FALSE)
  list_nb<-list()
  for(i in 1:K){
    #Segement your data by fold using the which() function 
    testIndexes <- which(folds==i,arr.ind=TRUE)
    testData <- X[testIndexes, ]
    trainData <- X[-testIndexes, ]
    train_labels<-Y[-testIndexes]
    test_labels<- Y[testIndexes]
  
  #freq_words<- findFreqTerms(trainData,5)
  
  #trainData<- trainData[,freq_words]
  #testData<- testData[,freq_words]
  
    train_rowtotals<- apply(trainData,1,sum)
    trainData<- trainData[train_rowtotals>0,]
    train_emptyind<- which(train_rowtotals==0)
    train_labels<-train_labels[-train_emptyind]
  
    test_rowtotals<- apply(testData,1,sum)
    testData<- testData[test_rowtotals>0,]
    test_emptyind<- which(test_rowtotals==0)
    test_labels<- test_labels[-test_emptyind]
  
  
    convert_counts <- function(x) {
      x <- ifelse(x > 0, "Yes", "No")
    }
  
  
    trainData <- apply(trainData, MARGIN = 2, convert_counts)
    testData <- apply(testData, MARGIN = 2, convert_counts)
  
  #dtm_freq_train$dimnames
    classifier <- naiveBayes(trainData, train_labels, type="raw")
    test_pred <- predict(classifier, testData)
  
  
    conf.mat <- confusionMatrix(test_pred, test_labels)
  
  list_nb[[i]]<- conf.mat
  
  print(paste("iteration",i, "done", sep=" "))
  #print(paste("iter",i,"train",trainData, "test",testData, sep=" "))
  #Use the test and train data partitions however you desire...
  }
  endtime<- Sys.time()
  print(endtime-starttime)
  return(list_nb)
}

naivebayes_result<- naiveBayes_cv(5, dtmover5, Parti)

naivebayes_result_raw<- naiveBayes_cv(5, dtmover5, Parti)


save(naivebayes_result, file="naiveBayesResultsutanHashtags.rda")

#flip the confusionmat so predictions in rows. 
t(naivebayes_result[[5]]$table)

################ 
load("naivebayesResults.rda")

precision<- matrix(0,8,5)
recall<- matrix(0,8,5)

rownames(precision)<- rownames(naivebayes_result[[1]]$byClass)
rownames(recall)<- rownames(naivebayes_result[[1]]$byClass)

#get speci

for (i in 1:5){
  fold<- naivebayes_result[[i]]
  for (j in 1:8){
    precision[j,i]<- fold$byClass[j,5]
    recall[j,i]<- fold$byClass[j,6]
  }
      
}


#means 

data.frame(apply(recall,1,mean))





##################################################
 
trind<- ceiling(6582 *0.7)
dtm_big_train <- dtm_big[1:trind, ]
dtm_big_test <- dtm_big[-(1:trind), ]


train_labels<- Parti[1:trind]
test_labels<- Parti[-(1:trind)]

#prop.table(table(train_labels))
#prop.table(table(test_labels))


freq_words <- findFreqTerms(dtm_big_test, 5)
#str(freq_words)

#f<-data.frame(freq_words)
#rm(f)

### tar endast med minst 5 freq words 

dtm_freq_train <- dtm_big_train[ , freq_words]
dtm_freq_test <- dtm_big_test[ , freq_words]

# ta bort tomma rader 

TrainRowTotals <- apply(dtm_freq_train , 1, sum) #Find the sum of words in each Document
dtm_freq_train   <- dtm_freq_train[TrainRowTotals> 0, ]           #remove all docs without words
Trainemptyind<-which(TrainRowTotals==0) 

train_labels<- train_labels[-Trainemptyind]

TestRowTotals <- apply(dtm_freq_test , 1, sum) #Find the sum of words in each Document
dtm_freq_test   <- dtm_freq_test[TestRowTotals> 0, ]           #remove all docs without words
Testemptyind<-which(TestRowTotals==0) 

test_labels<- test_labels[-Testemptyind]

convert_counts <- function(x) {
  x <- ifelse(x > 0, "Yes", "No")
}


train <- apply(dtm_freq_train, MARGIN = 2, convert_counts)
test <- apply(dtm_freq_test, MARGIN = 2, convert_counts)

#dtm_freq_train$dimnames
classifier <- naiveBayes(train, train_labels)
test_pred <- predict(classifier, test)

# confusion mat 

conf.mat <- confusionMatrix(test_pred, test_labels)

conf.mat$byClass
confmat<- table(test_pred, test_labels)
(sum(diag(confmat))/sum(confmat))*100  # accuracy 71,14 procent. 

# med minst freq 5 ord: 71,14 % 
# minst freq 2 : 70,126 % 
#med minst freq 10: 64,5 %
#####################################################################################
#SVM 
#####################################################################################





classifier_svm_linear <- svm(x=dtm_freq_train, y=train_labels, kernel = "linear") 
svm_pred<- predict(classifier_svm_linear, dtm_freq_test)

confusionMatrix(svm_pred, test_labels)


trainmat<-cbind(as.matrix(dtm_freq_train), train_labels)
colnames(trainmat)[ncol(trainmat)]<- "Y"


library(rminer)
M <- fit(Y~., data=trainmat, model="svm")
svm.imp <- Importance(M, data=as.data.frame(trainmat))

Mpred<- predict(M, as.matrix(dtm_freq_train))
confusionMatrix(Mpred, test_labels)


indexorder<- order(svm.imp$imp, decreasing = TRUE)
viktiga<- indexorder[1:50]
colnames(trainmat)[viktiga]

svm_imp<-svm.imp

save(svm.imp,file="svm_imp.rda")
