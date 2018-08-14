library(RSelenium)
driver<- rsDriver(browser=c("chrome"))
#remDr<- driver[["client"]]


#öppna chrome fönster 
#remDr$open()
asd<-driver[["client"]]


#öppna chrome 
asd$open()
#gå till sida 
#asd$navigate("http://www.google.com/")
#webelem<- asd$findElement(using="css", "[name='q']")
#webelem$sendKeysToElement(list("Linköping","\uE007"))




#########Tar alla tweets, "skroll"= antal tryck på end för att komma till slutet av sidan så den laddar fler tweets, maximum 40. 



skrapan<- function(parti, skroll){

  asd$open()
#gå till sida 
  asd$navigate(paste("http://www.twitter.com/",parti,sep=""))


  #trycker på end för att komma till sidslut och ladda fler. 
    
  webElem <- asd$findElement("css", "body")
  i<- 0
  while (i <= skroll){
    webElem$sendKeysToElement(list(key = "end"))

    Sys.sleep(2)  # "mänsklig paus"
    i<- i+1  
  }

#  webElem$sendKeysToElement(list(key = "end"))
  

  #webelem$sendKeysToElement(list(key="page_down"))
  tweets<- asd$findElements(using="class name", 
                          value="js-tweet-text-container")
  
  
  n<- length(tweets)
  
  tweetmat<-matrix(ncol=1,nrow=n)
  
  partiname<-as.matrix(rep(parti,n))
  
  #fyll matris med tweetsen 
  for (i in 1:n){
  tweetmat[i,1]<-as.character(tweets[[i]]$getElementText())
  }
  
  #stäng webläsare
  asd$close()
  
  tweetmat2<-cbind(tweetmat,partiname)
  return(tweetmat2)
  

}  

###Test 
#v<-skrapan("vansterpartiet",40)
#V<-as.data.frame(v)
#write.csv(V, file="V.csv")

twitterkonton<- c("Centerpartiet","kdriks","liberalerna","miljopartiet",
                  "nya_moderaterna", "socialdemokrat","sdriks","vansterpartiet")


#skrapa för alla partier, runt 40 tryck på "end" för att skrolla ner verkar vara maximum. 
#sparar varje partis tweets i CSV 

for (i in 1:length(twitterkonton)){
  data<-skrapan(twitterkonton[i],40)
  DATA<-as.data.frame(data)
  write.csv(DATA, file=paste(twitterkonton[i],".csv",sep=""))
}
 
