year=2023
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

defstats<-data.frame()
for (pdffile in pdffiles)
  {
  test<-pdf_text(pdffile)
  write(test[[4]],file = "temp.txt")
#  stats<-read_fwf("temp.txt",fwf_widths(c(3,19,5,4,6,10,9,4,6,9,4,4,4)),skip=4)
  stats<-read_table("temp.txt",skip=3)
  stats<-stats[!is.na(stats$'#'),]
  stats<-stats[!(stats[,1]=="Totals"),]
  bre<-which(stats$'#'=="#")
  stats1<-stats[1:(bre-1),]
  stats2<-stats[(bre+1):dim(stats)[1],]
  teams<-stri_split_fixed(pdffile,"-")[[1]]
  teams<-unlist(stri_split_fixed(teams,"."))
  teams<-unlist(stri_split_fixed(teams,"/"))
  week <- floor(as.numeric(as.Date(teams[5],format="%y%m%d")-as.Date("2022-05-28"))/7)
  w<-which(teams%in%c("hamburg","rhein","leipzig","istanbul","berlin","barcelona","frankfurt","tirol","cologne","wroclaw","stuttgart","vienna"))
  teams<-teams[w]

  w<-is.na(as.numeric(unlist((stats1[,3]))))
  while(sum(w)>0)
  {
    stats1[w,2]<-apply(stats1[w,2:3],1,paste, collapse=" ")
    stats1[w,3:(dim(stats1)[2]-1)]<-stats1[w,4:(dim(stats1)[2])]
    #if(dim(stats1)[2]>14)stats1[w,14]<-stats1[w,15]
    w<-is.na(as.numeric(unlist((stats1[,3]))))
  }

  #w<-(stats2$Vname=="TEAM")
  #if(sum(w)>0)
  #{
    #stats2[w,4:14]<-stats2[w,3:13]
   # stats2[w,3]<-""
   # print(stats2[w,])
 # }
  
  w<-is.na(as.numeric(unlist((stats2[,3]))))
  while(sum(w)>0)
  {
    stats2[w,2]<-apply(stats2[w,2:3],1,paste, collapse=" ")
    stats2[w,3:(dim(stats2)[2]-1)]<-stats2[w,4:(dim(stats2)[2])]
    #if(dim(stats2)[2]>14)stats2[w,14]<-stats2[w,15]
    w<-is.na(as.numeric(unlist((stats2[,3]))))
  }
  stats1<-stats1[,1:13]
  stats2<-stats2[,1:13]
  stats1$team<-teams[1]
  stats2$team<-teams[2]
  stats1$Opponent<-teams[2]
  stats2$Opponent<-teams[1]
  stats1$week<-stats2$week<-week
  names(stats2)<-names(stats1)<-c("Nr","Name","Solo", "Ast", "Total", "Sacks-Yds", "TFL-Yds", "FF", "FR-Yds", "Int-Yds", "BrUp", 
                     "Blks",  "QBH", "Team", "Opponent","Week")
  defstats <- rbind(defstats,stats1,stats2)
}

defstats$Name<-as.factor(defstats$Name)
defstats$Nr<-as.numeric(defstats$Nr)
defstats$Week<-as.numeric(defstats$Week)

defstats$Solo<-as.numeric(defstats$Solo)
defstats$Solo[is.na(defstats$Solo)]<-0
defstats$Ast<-as.numeric(defstats$Ast)
defstats$Ast[is.na(defstats$Ast)]<-0
defstats$Total<-as.numeric(defstats$Total)
defstats$Total[is.na(defstats$Total)]<-0

defstats$Team[defstats$Team=="barcelona"]<-"Barcelona Dragons"
defstats$Team[defstats$Team=="hamburg"]<-"Hamburg Sea Devils"
defstats$Team[defstats$Team=="berlin"]<-"Berlin Thunder"
defstats$Team[defstats$Team=="frankfurt"]<-"Frankfurt Galaxy"
defstats$Team[defstats$Team=="stuttgart"]<-"Stuttgart Surge"
defstats$Team[defstats$Team=="rhein"]<-"Rhein Fire"
defstats$Team[defstats$Team=="wroclaw"]<-"Wroclaw Panthers"
defstats$Team[defstats$Team=="istanbul"]<-"Istanbul Rams"
defstats$Team[defstats$Team=="cologne"]<-"Cologne Centurions"
defstats$Team[defstats$Team=="tirol"]<-"Raiders Tirol"
defstats$Team[defstats$Team=="vienna"]<-"Vienna Vikings"
defstats$Team[defstats$Team=="leipzig"]<-"Leipzig Kings"
defstats$Team<-as.factor(defstats$Team)

defstats$Opponent[defstats$Opponent=="barcelona"]<-"Dragons"
defstats$Opponent[defstats$Opponent=="hamburg"]<-"Sea Devils"
defstats$Opponent[defstats$Opponent=="berlin"]<-"Thunder"
defstats$Opponent[defstats$Opponent=="frankfurt"]<-"Galaxy"
defstats$Opponent[defstats$Opponent=="stuttgart"]<-"Surge"
defstats$Opponent[defstats$Opponent=="rhein"]<-"Fire"
defstats$Opponent[defstats$Opponent=="wroclaw"]<-"Panthers"
defstats$Opponent[defstats$Opponent=="istanbul"]<-"Rams"
defstats$Opponent[defstats$Opponent=="cologne"]<-"Centurions"
defstats$Opponent[defstats$Opponent=="tirol"]<-"Tirol"
defstats$Opponent[defstats$Opponent=="vienna"]<-"Vikings"
defstats$Opponent[defstats$Opponent=="leipzig"]<-"Kings"
defstats$Opponent<-as.factor(defstats$Opponent)

temp=stri_split_fixed(defstats$`Sacks-Yds`,"-")
w<-which(lapply(temp,length)==3)
for (i in w)
  temp[[i]]=c(temp[[i]][1],-as.numeric(temp[[i]][3]))
temp=matrix(unlist(temp),ncol=2, byrow=TRUE)
defstats$Sacks=as.numeric(temp[,1])
defstats$Sacks[is.na(defstats$Sacks)]<-0
defstats$SacksYds=as.numeric(temp[,2])
defstats$SacksYds[is.na(defstats$SacksYds)]<-0

temp=stri_split_fixed(defstats$`TFL-Yds`,"-")
w<-which(lapply(temp,length)==3)
for (i in w)
  temp[[i]]=c(temp[[i]][1],-as.numeric(temp[[i]][3]))
temp=matrix(unlist(temp),ncol=2, byrow=TRUE)
defstats$TFL=as.numeric(temp[,1])
defstats$TFL[is.na(defstats$TFL)]<-0
defstats$TFLYds=as.numeric(temp[,2])
defstats$TFLYds[is.na(defstats$TFLYds)]<-0

temp=stri_split_fixed(defstats$`FR-Yds`,"-")
w<-which(lapply(temp,length)==3)
for (i in w)
  temp[[i]]=c(temp[[i]][1],-as.numeric(temp[[i]][3]))
temp=matrix(unlist(temp),ncol=2, byrow=TRUE)
defstats$FR=as.numeric(temp[,1])
defstats$FR[is.na(defstats$FR)]<-0
defstats$FRYds=as.numeric(temp[,2])
defstats$FRYds[is.na(defstats$FRYds)]<-0

temp=stri_split_fixed(defstats$`Int-Yds`,"-")
w<-which(lapply(temp,length)==3)
for (i in w)
  temp[[i]]=c(temp[[i]][1],-as.numeric(temp[[i]][3]))
temp=matrix(unlist(temp),ncol=2, byrow=TRUE)
defstats$Int=as.numeric(temp[,1])
defstats$Int[is.na(defstats$Int)]<-0
defstats$IntYds=as.numeric(temp[,2])
defstats$IntYds[is.na(defstats$IntYds)]<-0

defstats$FF<-as.numeric(defstats$FF)
defstats$FF[is.na(defstats$FF)]<-0
defstats$BrUp<-as.numeric(defstats$BrUp)
defstats$BrUp[is.na(defstats$BrUp)]<-0
defstats$Blks<-as.numeric(defstats$Blks)
defstats$Blks[is.na(defstats$Blks)]<-0
defstats$QBH<-as.numeric(defstats$QBH)
defstats$QBH[is.na(defstats$QBH)]<-0

defstats$Name<-correct.names(as.character(defstats$Name),team=defstats$Team,year=year)

save(defstats,file=paste0(home,"stats/defensive_players_single_",year,".Rda"))