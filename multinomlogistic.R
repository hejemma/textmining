################# multinomial logistic regression. 

library(nnet)

dtm_freq_train
dtm_freq_test
train_labels
test_labels


traindf<- as.data.frame(as.matrix(dtm_freq_train))
traindf<- data.frame(traindf, train_labels)

testdf<- as.data.frame(as.matrix(dtm_freq_test))

traindf$
match("for",colnames(testdf))
which( colnames(testdf)=="for" )

colnames(testdf)[470]<-"for."

multmod<- multinom(train_labels ~ ., data = traindf,MaxNWts =10000000)

pred<- predict(multmod, testdf)

confusionmatMULTINOM<- confusionMatrix(pred, test_labels)
save(confusionmatMULTINOM, file="confusion_multinom.rda")
