

sd_ind<-which(varImp(fit)$importance[[8]]>10)
sd<-varImp(fit)$importance[8]
sd$ord<- rownames(sd)

sd<-sd[sd_ind,]
rownames(sd)<-NULL


windows()
wordcloud(words = sd$ord, freq = sd$sdriks, min.freq = 1,
          random.order=FALSE, rot.per=0.35, 
          colors=brewer.pal(8, "Dark2"))




wordcloud(words = vanster$ord, freq = vanster$vansterpartiet, min.freq = 1,
         random.order=FALSE, rot.per=0.35,max.words=200, 
          colors=brewer.pal(8, "Dark2"))

library(wordcloud)



lib_ind<-which(varImp(fit)$importance[[4]]>10)
lib<-varImp(fit)$importance[4]
lib$ord<- rownames(lib)

lib<-lib[lib_ind,]
rownames(lib)<-NULL


windows()
wordcloud(words = lib$ord, freq = lib$liberalerna, min.freq = 1,
          random.order=TRUE, rot.per=0.35, 
          colors=brewer.pal(8, "Dark2"))


