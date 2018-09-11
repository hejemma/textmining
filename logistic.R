################logisstic regression 
library(glmnet)

## skapa dtm_big från data_prework 

#test och train. 70/30 , raderana är redan random 
trind<- ceiling(6582 *0.7)
dtm_big_train <- as.matrix(dtm_big[1:trind, ])
dtm_big_test <- as.matrix(dtm_big[-(1:trind), ])


train_labels<- Parti[1:trind]
test_labels<- Parti[-(1:trind)]

prop.table(table(train_labels))
prop.table(table(test_labels))


freq_words <- findFreqTerms(dtm_big_train, 5)
str(freq_words)

#f<-data.frame(freq_words)
#rm(f)

### tar endast med minst 5 freq words 

dtm_freq_train <- dtm_big_train[ , freq_words]
dtm_freq_test <- dtm_big_test[ , freq_words]
inspect(dtm_freq_train)








# Check accuracy on training.
preds<- predict(fit, newdata = test)
fit
confusionMatrix(preds, test$y)


