#home<-"/Users/schmid/projects/ELFdata/"
source("setup.R")
library(rvest)
library(xml2)
library(curl)
library(readr)
library(stringi)
postgames <- read_html("https://europeanleague.football/past-games",show_col_types = FALSE)
postgames<-html_nodes(postgames,"body")
gamelinks<-xml2::xml_find_all(postgames,"//a")
gamelinks<-gamelinks[grepl("live-games",gamelinks)]
gamelinks<-stri_split_fixed(gamelinks,'"')
gamelinks<-sapply(gamelinks,function(y)return(y[[2]]))

downloads=data.frame(game=c(),last=c())
try({downloads=read_csv(paste0(home,"gamebooks/downloads.txt"),col_names=c("game","last"),show_col_types = FALSE)})

for(gamelink in gamelinks[sample(1:length(gamelinks),6)])
  {
  game <- read_html(paste0("https://europeanleague.football",gamelink))
  game<-html_nodes(game,"body")
  link<-xml2::xml_find_all(game,"//a")
  gamename<-stri_split_fixed(gamelink,"/")[[1]][3]
  if (substr(gamename,1,2)=="23")
    {
    link<-link[grepl(".pdf",link)|grepl(".PDF",link)]
    link<-link[!grepl("_Media",link)] 
    if (length(link)>0){
      link<-stri_split_fixed(link,'"')[[1]][2]
      lastmodified<-curlGetHeaders(link, timeout=3)[5]
      lastmodified<-substr(lastmodified,21,44)
      print(gamename)
      do = FALSE
      if (sum(downloads$game==gamename)==0)do=TRUE
      if (!do)if(downloads$last[downloads$game==gamename]<lastmodified)do=TRUE
      if (do)
      {
        curl_download(link,paste0(home,"2023/gamebooks/",gamename,".pdf"))
        print(paste(gamename,"-",lastmodified))
        downloads=downloads[!(downloads$game==gamename),]
        downloads=rbind(downloads,data.frame("game"=gamename,"last"=lastmodified))
      }
    }
  }
}
write_csv(downloads,file=paste0(home,"gamebooks/downloads.txt"),col_names=FALSE)
  