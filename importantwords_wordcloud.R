#########################
library(wordcloud)
library(RColorBrewer)
#######################

head(sd)
cloudicloud<- function(data){
# ta bort alla tecken som ej ?r bokst?ver 
  all_data<-data.frame(gsub("[^[:alnum:] ]", "", data))
  
# tar bort alla konstiga tecken förutom citattecken 
  all_data<- data.frame(gsub("[[:punct:]]", "", all_data[,1]))

# tar bort citattecknen 
  all_data<-data.frame(gsub("[^[:alnum:]///' ]", "", all_data[,1]))

# ta bort alla länkar 
  s3<- as.data.frame(gsub("http\\w+ *"," ", all_data[,1]))
  s3_2<- as.data.frame(gsub("www\\w+ *"," ", s3[,1]))

  corpus<-VCorpus(VectorSource(s3_2[,1]))

  corp<- corpus %>%
    tm_map(content_transformer(tolower))%>%
    tm_map(stemDocument, language="swedish")%>%
    tm_map(removeWords, c(stopwords("swedish"), "ska","centerpartiet","moderaterna","sverigedemokraterna",
                        "vänsterpartiet","vansterpartiet","socialdemokraterna","liberalerna","kristdemokraterna","miljöpartiet",
                        "sjöstedt","buschebba","bjorklundjan","isabellalovin","fridolin","kristersson","löfven","svpol","annieloof",
                        "sverig","int","vill")) %>%
    tm_map(removeNumbers)

  return(corp)
}
rm(data)

wordcloud(corp)
vcorp<-cloudicloud(v$V1)
ccorp<-cloudicloud(c$V1)
kdcorp<-cloudicloud(kd$V1)
libcorp<-cloudicloud(lib$V1)
mpcorp<-cloudicloud(miljo$V1)
mcorp<-cloudicloud(m$V1)
scorp<-cloudicloud(s$V1)
sdcorp<-cloudicloud(sd$V1)

display.brewer.all()
wordcloud(ccorp, max.words = 70, random.order = FALSE,colors=brewer.pal(8, "Pastel2"))
wordcloud(kdcorp, max.words = 70, random.order = FALSE,colors=brewer.pal(8, "YlGnBu"))####
wordcloud(libcorp, max.words = 70, random.order = FALSE,colors=brewer.pal(8, "RdPu"))
wordcloud(mpcorp, max.words = 70, random.order = FALSE,colors=brewer.pal(8, "Greens"))
wordcloud(mcorp, max.words = 70, random.order = FALSE,colors=brewer.pal(8, "Dark2"))
wordcloud(scorp, max.words = 70, random.order = FALSE,colors=brewer.pal(8, "OrRd"))
wordcloud(sdcorp, max.words = 70, random.order = FALSE,colors=brewer.pal(8, "YlGnBu"))
wordcloud(vcorp, max.words = 70, random.order = FALSE,colors=brewer.pal(8, "OrRd"))



v<- read.csv2("V.csv", sep=",")
c<-read.csv2("Centerpartiet.csv", sep=",")
kd<-read.csv2("kdriks.csv", sep=",")
lib<-read.csv2("liberalerna.csv",sep=",")
miljo<-read.csv2("miljopartiet.csv",sep=",")
m<-read.csv2("nya_moderaterna.csv", sep=",")
s<-read.csv2("socialdemokrat.csv", sep=",")
sd<-read.csv2("sdriks.csv", sep=",")



















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



lib_ind<-which(varImp(fit)$importance[[4]]>10)
lib<-varImp(fit)$importance[4]
lib$ord<- rownames(lib)

lib<-lib[lib_ind,]
rownames(lib)<-NULL


windows()
wordcloud(words = lib$ord, freq = lib$liberalerna, min.freq = 1,
          random.order=TRUE, rot.per=0.35, 
          colors=brewer.pal(8, "Dark2"))


