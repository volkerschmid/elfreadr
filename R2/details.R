source("setup.R")
library(pdftools)
library(dplyr)
library(readr)
library(stringi)

games<-data.frame()
for (year in c(2021,2022))
{
pdffiles<-list.files(paste0(home,"gamebooks/",year),full.names = TRUE)
#pdffiles<-pdffiles[grepl(paste0(home,"gamebooks/",year),pdffiles)]
for (pdffile in pdffiles)
  {
  test<-pdf_text(pdffile)
  test<-test[[1]][[1]]
  test<-stri_split_fixed(test,"\n")[[1]]
  att<-test[grep("Attendance",test)]
  att<-as.integer(stri_split_fixed(att,":")[[1]][2])
  teams <- stri_split_fixed(test[3]," vs ")[[1]]
  teams <- unlist(stri_split_fixed(teams," ("))[1:2]
  teams <- trimws(teams)
  date <- stri_split_fixed(test[7]," • ")[[1]]
  city <- stri_split_fixed(date[2],"Site: ")[[1]][2]
  stadium <- stri_split_fixed(date[3],"Stadium: ")[[1]][2]
  date <- stri_split_fixed(date[1],"Date: ")[[1]][2]
  date <- as.Date(date,format = "%b %d, %Y")  
  games<-rbind(games,data.frame("date"=date,"city"=city,"stadium"=stadium,"away"=teams[1],"home"=teams[2],"attendance"=att))
}
}

games<-games[order(games$attendance,decreasing = TRUE),]
games$rank<-1:dim(games)[1]

games$neutral<-(format(games$date,"%m")=="09")&(as.numeric(format(games$date,"%d"))>23)

games<-games[1:15,]

games$game<-paste(games$away,ifelse(games$neutral,"vs","@"),games$home)
games$date<-format(games$date,"%d %B %Y")
games$stadium<-paste(games$stadium,games$city,sep=", ")
games<-games[,c(7,1,3,9)]
knitr::kable(games,row.names=FALSE)

