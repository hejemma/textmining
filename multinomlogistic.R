################# multinomial logistic regression. 

library(nnet)

dtm_freq_train
dtm_freq_test
train_labels
test_labels
#traindf<- as.data.frame(as.matrix(dtm_freq_train))
#traindf<- data.frame(traindf, train_labels)

#testdf<- as.data.frame(as.matrix(dtm_freq_test))

#traindf$
#match("for",colnames(testdf))
#h<-which( colnames(testdf)=="for" )

#colnames(testdf)[470]<-"for."

multmod<- multinom(train_labels ~ ., data = traindf,MaxNWts =10000000)

pred<- predict(multmod, testdf)

confusionmatMULTINOM<- confusionMatrix(pred, test_labels)
save(confusionmatMULTINOM, file="confusion_multinom.rda")


##############################################################################


dfbig<-as.data.frame(as.matrix(dtmover5))
forindex<-which(colnames(dfbig)=="for")   #something wrong with the word "for". in this code 
dfbig<- dfbig[,-forindex]
X<-dfbig
Y<-Parti
K<-5
i<-1
##### kör det här i koden. 
multlog_cv<- function(K, X,Y){
  ncols<- ncol(X)
  start_time<- Sys.time()
  folds <- cut(seq(1,nrow(X)),breaks=K,labels=FALSE)
  list_mlog<-list()
  for(i in 1:K){
    #Segement your data by fold using the which() function 
    testIndexes <- which(folds==i,arr.ind=TRUE)
    testData <- X[testIndexes, ]
    trainData <- X[-testIndexes, ]
    train_labels<-Y[-testIndexes]
    test_labels<- Y[testIndexes]
    
    #freq_words<- findFreqTerms(trainData,5)
    trainData<- data.frame(cbind(trainData, train_labels))
    #trainData<- trainData[,freq_words]
    #testData<- testData[,freq_words]
    train_rowtotals<- apply(trainData[,1:ncol(X)],1,sum)
    trainData<- trainData[train_rowtotals>0,]
    train_emptyind<- which(train_rowtotals==0)
    train_labels<-train_labels[-train_emptyind]
    
    test_rowtotals<- apply(testData,1,sum)
    testData<- testData[test_rowtotals>0,]
    test_emptyind<- which(test_rowtotals==0)
    test_labels<- test_labels[-test_emptyind]
    

    
    
    classifier<- multinom(train_labels ~ ., data = trainData,MaxNWts =10000000)
    
    test_pred<- predict(classifier, testData)
    
    
    conf.mat <- confusionMatrix(test_pred, test_labels)
    
    list_mlog[[i]]<- conf.mat
    
    print(paste("iteration",i, "done", sep=" "))
    #print(paste("iter",i,"train",trainData, "test",testData, sep=" "))
    #Use the test and train data partitions however you desire...
  }
  end_time <- Sys.time()
  print(end_time-start_time)
  return(list_mlog)
}

multilog_result<- multlog_cv(5, dfbig, Parti)
save(multilog_result, file="multiLogResultsutanhashtags.rda")

multilog_result

precision<- matrix(0,8,5)
recall<- matrix(0,8,5)

rownames(precision)<- rownames(multilog_result[[1]]$byClass)
rownames(recall)<- rownames(multilog_result[[1]]$byClass)

#get speci

for (i in 1:5){
  fold<- multilog_result[[i]]
  for (j in 1:8){
    precision[j,i]<- fold$byClass[j,5]
    recall[j,i]<- fold$byClass[j,6]
  }
  
}


#means 
data.frame(apply(precision,1,mean))
data.frame(apply(recall,1,mean))

