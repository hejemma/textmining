
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

