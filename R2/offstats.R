library(pdftools)
library(stringi)
library(dplyr)
source("setup.R")
pdffiles<-list.files("gamebooks/2022",full.names = TRUE)
#pdffiles<-pdffiles[grepl("game",pdffiles)]

line2col<-function(dat,split="  "){
  temp<-strsplit(dat,split) 
  return(temp[[1]][temp[[1]]!=""])
}

rushing<-passing<-receiving<-punting<-returns<-fieldgoals<-kickoffs<-fumble<-data.frame()
pdffile=pdffiles[sample(length(pdffiles),1)]
for (pdffile in pdffiles)
  {
  print(pdffile)
  test<-pdf_text(pdffile)

  teams<-stri_split_fixed(pdffile,"-")[[1]]
  teams<-unlist(stri_split_fixed(teams,"."))
  teams<-unlist(stri_split_fixed(teams,"/"))
  week <- floor(as.numeric(as.Date(teams[5],format="%y%m%d")-as.Date("2022-05-28"))/7)
  w<-which(teams%in%c("hamburg","rhein","leipzig","istanbul","berlin","barcelona","frankfurt","tirol","cologne","wroclaw","stuttgart","vienna"))
  teams<-teams[w]
  
  test2<-unlist(strsplit(test[[3]],"\n"))[-(1:5)]

  #rushing
  cat(".")
  i<-1
  do<-TRUE
  done1<-done2<-FALSE
  while(do)
  {
    test3<-unlist(strsplit(test2[i]," "))
    test3<-test3[test3!=""]
    if(length(test3)==0){i=i+1}
    else{
    # check for "TEAM" stats
    t<-which(test3=="Team"|test3=="TEAM"|test3=="TM")
    t<-sort(c(1:length(test3),t))
    test3<-test3[t]

    if(length(test3)==18)rushing<-rbind(rushing,
                   c(test3[1:9],teams[1],teams[2]),
                   c(test3[10:18],teams[2],teams[1]))
    if (!done1)
    {
      if (test3[1]=="Totals"){done1=TRUE;test3<-test3[-(1:8)]}
    }
    if(done1&length(test3)>0)
    {
      if (test3[1]=="Totals"){do=FALSE}else{rushing<-rbind(rushing,c(test3[1:9],teams[2],teams[1]))}
    }
    if(done2&length(test3)>0)
    {
      if (test3[1]=="Totals"){do=FALSE}else{rushing<-rbind(rushing,c(test3[1:9],teams[1],teams[2]))}
    }
    if ((!done2)&length(test3)>9)
    {
      if (test3[10]=="Totals"){done2=TRUE;rushing<-rbind(rushing,c(test3[1:9],teams[1],teams[2]))}
    }
    if(length(test3)==0)do=FALSE
    i=i+1
    }
  }
  
  #passing
  i=i+1
  do<-TRUE
  done1<-done2<-FALSE
  while(do)
  {
    test3<-unlist(strsplit(test2[i]," "))
    test3<-test3[test3!=""]
    if(length(test3)==0){i=i+1}
    else{
    # check for "TEAM" stats
    t<-which(test3=="Team"|test3=="TEAM"|test3=="TM")
    t<-sort(c(1:length(test3),t))
    test3<-test3[t]
    
    if(length(test3)==14)passing<-rbind(passing,
                                        c(test3[1:7],teams[1],teams[2]),
                                        c(test3[8:14],teams[2],teams[1]))
    if (!done1)
    {
      if (test3[1]=="Totals"){done1=TRUE;test3<-test3[-(1:6)]}
    }
    if(done1&length(test3)>0)
    {
      if (test3[1]=="Totals"){do=FALSE}else{passing<-rbind(passing,c(test3[1:7],teams[2],teams[1]))}
    }
    if(done2&length(test3)>0)
    {
      if (test3[1]=="Totals"){do=FALSE}else{passing<-rbind(passing,c(test3[1:7],teams[1],teams[2]))}
    }
    if ((!done2)&length(test3)>7)
    {
      if (test3[8]=="Totals"){done2=TRUE;passing<-rbind(passing,c(test3[1:7],teams[1],teams[2]))}
    }
    if(length(test3)==0)do=FALSE
    i=i+1
    }
  }
  
  #Receiving
  cat(".")
  
  i=i+1
  do<-TRUE
  done1<-done2<-FALSE
  while(do)
  {
    test3<-unlist(strsplit(test2[i]," "))
    test3<-test3[test3!=""]
    if(length(test3)==0){i=i+1}
    else{
    # check for "TEAM" stats
    t<-which(test3=="Team"|test3=="TEAM"|test3=="TM")
    t<-sort(c(1:length(test3),t))
    test3<-test3[t]
    
    if(length(test3)==12)receiving<-rbind(receiving,
                                        c(test3[1:6],teams[1],teams[2]),
                                        c(test3[7:12],teams[2],teams[1]))
    if (!done1)
    {
      if (test3[1]=="Totals"){done1=TRUE;test3<-test3[-(1:5)]}
    }
    if(done1&length(test3)>0)
    {
      if (test3[1]=="Totals"){do=FALSE}else{receiving<-rbind(receiving,c(test3[1:6],teams[2],teams[1]))}
    }
    if(done2&length(test3)>0)
    {
      if (test3[1]=="Totals"){do=FALSE}else{receiving<-rbind(receiving,c(test3[1:6],teams[1],teams[2]))}
    }
    if ((!done2)&length(test3)>6)
    {
      if (test3[7]=="Totals"){done2=TRUE;receiving<-rbind(receiving,c(test3[1:6],teams[1],teams[2]))}
    }
    if(length(test3)==0)do=FALSE
    i=i+1
    }
  }
  
  # punting
  cat(".")
  i=i+1
  do<-TRUE
  done1<-done2<-FALSE
  while(do)
  {
    test3<-unlist(strsplit(test2[i]," "))
    test3<-test3[test3!=""]
    if(length(test3)==0){i=i+1}
    else{
    # check for "TEAM" stats
    t<-which(test3=="Team"|test3=="TEAM"|test3=="TM")
    t<-sort(c(1:length(test3),t))
    test3<-test3[t]
    
    if(length(test3)==16)punting<-rbind(punting,
                                          c(test3[1:8],teams[1],teams[2]),
                                          c(test3[9:16],teams[2],teams[1]))
    if (!done1)
    {
      if (test3[1]=="Totals"){done1=TRUE;test3<-test3[-(1:7)]}
    }
    if(done1&length(test3)>0)
    {
      if (test3[1]=="Totals"){do=FALSE}else{punting<-rbind(punting,c(test3[1:8],teams[2],teams[1]))}
    }
    if(done2&length(test3)>0)
    {
      if (test3[1]=="Totals"){do=FALSE}else{punting<-rbind(punting,c(test3[1:8],teams[1],teams[2]))}
    }
    if ((!done2)&length(test3)>8)
    {
      if (test3[9]=="Totals"){done2=TRUE;punting<-rbind(punting,c(test3[1:8],teams[1],teams[2]))}
    }
    if(length(test3)==0)do=FALSE
    i=i+1
    }
  }
  
  # returns
  cat(".")
  i=i+2
  do<-TRUE
  done1<-done2<-FALSE
  n<-11
  while(do)
  {
    test3<-unlist(strsplit(test2[i]," "))
    test3<-test3[test3!=""]
    if(length(test3)==0){i=i+1}
    else{
      
    # check for "TEAM" stats
    t<-which(test3=="Team"|test3=="TEAM"|test3=="TM")
    t<-sort(c(1:length(test3),t))
    test3<-test3[t]
    
    if(length(test3)==(2*n))returns<-rbind(returns,
                                        c(test3[1:n],teams[1],teams[2]),
                                        c(test3[n+(1:n)],teams[2],teams[1]))
    if (!done1)
    {
      if (test3[1]=="Totals"){done1=TRUE;test3<-test3[-(1:(n-1))]}
    }
    if(done1&length(test3)>0)
    {
      if (test3[1]=="Totals"){do=FALSE}else{returns<-rbind(returns,c(test3[1:n],teams[2],teams[1]))}
    }
    if(done2&length(test3)>0)
    {
      if (test3[1]=="Totals"){do=FALSE}else{returns<-rbind(returns,c(test3[1:n],teams[1],teams[2]))}
    }
    if ((!done2)&length(test3)>n)
    {
      if (test3[n+1]=="Totals"){done2=TRUE;returns<-rbind(returns,c(test3[1:n],teams[1],teams[2]))}
    }
    if(length(test3)==0)do=FALSE
    i=i+1
    }
  }

  # field goals
  cat(".")
  i=i+2
  do<-TRUE
  done1<-done2<-FALSE
  n<-7
  while(do)
  {
    test3<-unlist(strsplit(test2[i]," "))
    test3<-test3[test3!=""]
      
      if(length(test3)==(2*n))fieldgoals<-rbind(fieldgoals,
                                             c(test3[1:n],teams[1],teams[2]),
                                             c(test3[n+(1:n)],teams[2],teams[1]))
      if(length(test3)==0)do=FALSE
      i=i+1
  }
  
  # Kickoffs
  i=i+1
  do<-TRUE
  done1<-done2<-FALSE
  n<-7
  while(do)
  {
    test3<-unlist(strsplit(test2[i]," "))
    test3<-test3[test3!=""]
    
    if(length(test3)==(2*n))kickoffs<-rbind(kickoffs,
                                              c(test3[1:n],teams[1],teams[2]),
                                              c(test3[n+(1:n)],teams[2],teams[1]))
    if(length(test3)==0)do=FALSE
    i=i+1
  }
  
  # FUMBLES
  cat(".")
  i=i+1
  do<-TRUE
  done1<-done2<-FALSE
  while(do)
  {
    test3<-unlist(strsplit(test2[i]," "))
    test3<-test3[test3!=""]   
    if(length(test3)>0)
    if (test3[1]=="FUMBLES:"){
      test3<-unlist(strsplit(test2[i],": "))[2]
      test4 <-unlist(strsplit(test3,".", fixed=TRUE))
      test4<-unlist(strsplit(test4,";",fixed=TRUE))             
      test4<-unlist(strsplit(test4," ",fixed=TRUE))     
      test4<-unlist(strsplit(test4,"-",fixed=TRUE))     
      test4<-test4[test4!=""]   
      test5<-as.numeric(test4)
      test5<-which(!is.na(test5))
      if(length(test5)>0)
        {
        test5<-test5[seq(2,length(test5),by=2)]
        teams0<-teams
        for (tt in 1:length(test5)){
          fu<-test4[test5[tt]-(3:0)]
          fumble<-rbind(fumble,c(fu,teams0))
          if (tt<length(test5))if(test5[tt+1]-test5[tt]>5)teams0<-teams[2:1]
        }
      }
      do=FALSE
    }
    i=i+1
  }
  cat("\n")
}
rushing0<-rushing
rushing0[,1]<-apply(rushing[,1:2],1,paste, collapse=" ")
rushing0<-rushing0[,-2]
  for (i in 2:8)
  rushing0[,i]<-as.numeric(rushing0[,i])
names(rushing0)<-c("Name","N","Gain","Loss","Net","TD","Long","Avg","Team","Opponent")

if(1){
  rushing0 %>% 
  group_by(Name,Team) %>% 
  #summarise(cond_disp = sum(disp))
  summarise(Games=n(),
            Gain = sum(Gain, na.rm = TRUE),
            Loss = sum(Loss, na.rm = TRUE),
            Net = sum(Net, na.rm = TRUE),
            TD = sum(TD, na.rm = TRUE),
            Long = max(Long, na.rm=TRUE),
            Avg = mean(Avg, na.rm=TRUE))
}
