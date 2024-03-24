year=2022
source("setup.R")
source(paste0(home,"functions/correctnames.R"))
library(pdftools)
library(dplyr)
library(readr)
library(stringi)

pdffiles<-list.files(paste0(home,"gamebooks/",year),full.names = TRUE)
#pdffiles<-pdffiles[grepl(paste0(home,"gamebooks/",year),pdffiles)]

line2col<-function(dat,split="  "){
  temp<-strsplit(dat,split) 
  return(temp[[1]][temp[[1]]!=""])
}

for (pdffile in pdffiles)
  {
  print(pdffile)
  test<-pdf_text(pdffile)
  playbyplay<-c()
  for (i in 7:(length(test)-1))
  {
    playbyplay<-c(playbyplay,unlist(stri_split_fixed(test[[i]],"\n")))
  }
  playbyplay<-trimws(playbyplay)
  
  playbyplay<-playbyplay[!(playbyplay=="")]
    
  kick<-c()
  for (i in 1:length(playbyplay))
  {
    if(substr(playbyplay[i],1,12)=="Play-by-Play")kick<-c(kick,i)
  }
  playbyplay<-playbyplay[-c(kick,kick+1)]
  
  i <- 1
  plays <- c() 
  while(i<=length(playbyplay)){
    play <- playbyplay[i]
    do = TRUE
    while(do)
    {
      do =FALSE
      trynext <- unlist(strsplit(substr(playbyplay[i+1],1,4),"-"))
      trynext<-unlist(strsplit(trynext," "))
      if(any(is.na(trynext))){break}
      if (length(trynext)==1){
        play<-paste(play,playbyplay[i+1])
        i<-i+1
        do=TRUE
      }
      if (!do)if (is.na(as.numeric(trynext))[1])
        {
          play<-paste(play,playbyplay[i+1])
          i<-i+1
          do=TRUE
        }
      if(!do)if (is.na(as.numeric(trynext))[2]&
          trynext[2]!="G")
      {
        play<-paste(play,playbyplay[i+1])
        i<-i+1
        do=TRUE
      }
    }
    plays<-c(plays,play)
    i<-i+1
  }
  
  for (p in plays[-1])
  {
    pp <- unlist(strsplit(p," "))
    pp <- pp[!(pp=="")]
    try <- unlist(strsplit(pp[1],"-"))
    lineos <- pp[c(2,3)]
    j=4 
    what=NA
    while(is.na(what))
    {
      if (pp[j]=="rush")what="rush"
      if (pp[j]=="pass")what="pass"
      if (pp[j]=="punt")what="punt"
      if (pp[j]=="sacked")what="sack"
      if (pp[j]=="PENALTY")what="penalty"
      if (pp[j]=="Fumble")what="fumble"
      if (pp[j]=="field")what="fieldgoal"
      if (pp[j]=="wins")what="kickoff"
      if (pp[j]=="ball")what="ballon"
      if (pp[j]=="kick")what="kickoff"
      if (pp[j]=="kickoff")what="kickoff"
      if (pp[j]=="1st")what="penalty"
      if (pp[j]=="2nd")what="penalty"
      if (pp[j]=="3rd")what="penalty"
      if (pp[j]=="4th")what="penalty"
      if (pp[j]=="Timeout")what="timeout"
      if (pp[j]=="Start")what="startquarter"
      if (pp[j]=="QB")what="QBchange"
      if (pp[j]=="wearing")what="NRchange"
      if (pp[j]=="Clock")what="clockset"
      if (pp[j]=="Vogel).")what="ignore"
      if (pp[j]=="Bowen).")what="ignore"
      if (pp[j]=="Richardson).")what="ignore"
      if (pp[j]=="Sam).")what="ignore"
      j<-j+1
    }
  }
  
}
