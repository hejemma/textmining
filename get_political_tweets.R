library(RSelenium)
driver<- rsDriver(browser=c("chrome"))
#remDr<- driver[["client"]]


#�ppna chrome f�nster 
#remDr$open()
asd<-driver[["client"]]


#�ppna chrome 
asd$open()
#g� till sida 
#asd$navigate("http://www.google.com/")
#webelem<- asd$findElement(using="css", "[name='q']")
#webelem$sendKeysToElement(list("Link�ping","\uE007"))




#########Tar alla tweets, "skroll"= antal tryck p� end f�r att komma till slutet av sidan s� den laddar fler tweets, maximum 40. 



skrapan<- function(parti, skroll){

  asd$open()
#g� till sida 
  asd$navigate(paste("http://www.twitter.com/",parti,sep=""))


  #trycker p� end f�r att komma till sidslut och ladda fler. 
    
  webElem <- asd$findElement("css", "body")
  i<- 0
  while (i <= skroll){
    webElem$sendKeysToElement(list(key = "end"))

    Sys.sleep(2)  # "m�nsklig paus"
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
  
  #st�ng webl�sare
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


#skrapa f�r alla partier, runt 40 tryck p� "end" f�r att skrolla ner verkar vara maximum. 
#sparar varje partis tweets i CSV 

for (i in 1:length(twitterkonton)){
  data<-skrapan(twitterkonton[i],40)
  DATA<-as.data.frame(data)
  write.csv(DATA, file=paste(twitterkonton[i],".csv",sep=""))
}
 
