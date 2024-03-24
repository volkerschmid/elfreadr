#home<-"/Users/schmid/projects/ELFdata/"
source("setup.R")
library(rvest)
library(xml2)
library(curl)
library(readr)
library(stringi)
library(stringr)

load("data/games.rda")
games23<-games[games$year==2023,]
games23<-games23[!(games23$homepoints==-1),]
games23<-games23[!is.na(games23$OT),]
downloads=data.frame(game=c(),last=c())
try({downloads=read_csv(paste0(home,"gamebooks/downloads.txt"),col_names=c("game","last"),show_col_types = FALSE)})

weeks<-(1:max(games23$Spieltag))[-10]
#weeks<-max(1,weeks-3):weeks
  
for (week in weeks)
{
  games<-games23[games23$Spieltag==week,]
  for (i in 1:dim(games)[1])
  {
    game<-games[i,]
    h=substr(stri_split_fixed(game$Home," ")[[1]],1,1)
    h=paste0(h[1],h[length(h)],collapse="")
    a=substr(stri_split_fixed(game$Away," ")[[1]],1,1)
    a=paste0(a[1],a[length(a)],collapse="")
    if ((week==12)&(i==1)){b=h;h=a;a=b}
    if (week==15)week="WC"
    if (week==16)week="PO"
    if (week==17)week="CG"
    
    gamename=paste0(h,a,"23",week)
    filename=paste0(home,"gamebooks/2023/STATS_",gamename,".pdf")
    for (url in paste0("https://storage.googleapis.com/leaguetool-dfc3d.appspot.com/gamebooks/2023/",c("W","w",rep("",3)),week,"/S",c(rep("TATS",3),"tats","TAT"),"_",h,a,"23",if(week<10){"0"},week,".pdf")){
    try({
      #print(url)
      lastmodified<-curlGetHeaders(url, timeout=3)
      lastmodified<-lastmodified[grepl("last-modified:",lastmodified)]
      lastmodified<-substr(lastmodified,21,44)
      do = FALSE
      if (sum(downloads$game==gamename)==0)do=TRUE
      if (!do)if(downloads$last[downloads$game==gamename]<lastmodified)do=TRUE
      if (do)
      {
        print(url)
        curl_download(url,filename)
        print(paste(gamename,"-",lastmodified))
        downloads=downloads[!(downloads$game==gamename),]
        downloads=rbind(downloads,data.frame("game"=gamename,"last"=lastmodified))
      }
    }, silent=TRUE)
    }
}}
write_csv(downloads,file=paste0(home,"gamebooks/downloads.txt"),col_names=FALSE)
  