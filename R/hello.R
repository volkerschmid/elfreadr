#' Title
#'
#' @param pdffile
#'
#' @return
#' @export
#'
#' @import pdftools stringi
#' @examples

extractgamebook <- function(pdffile) {

  test<-pdf_text(pdffile)

  ## Page 1

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
  game<-data.frame("date"=date,"city"=city,"stadium"=stadium,"away"=teams[1],"home"=teams[2],"attendance"=att)

  # Score by quarters
  l <- grep("Score by Quarters",test)
  awayq <- stri_split_fixed(test[l+1],"  ")[[1]]
  awayq<-awayq[awayq!=""]
  awaypoints<-awayq[length(awayq)]
  awayq1 <- as.integer(awayq[2])
  awayq2 <- as.integer(awayq[3])
  awayq3 <- as.integer(awayq[4])
  awayq4 <- as.integer(awayq[5])
  if (length(awayq)==7){awayot <- as.integer(awayq[6]); OT=TRUE}
  homeq <- stri_split_fixed(test[l+2],"  ")[[1]]
  homeq<-homeq[homeq!=""]
  hompoints<-homeq[length(homeq)]
  homeq1 <- as.integer(homeq[2])
  homeq2 <- as.integer(homeq[3])
  homeq3 <- as.integer(homeq[4])
  homeq4 <- as.integer(homeq[5])
  if (length(homeq)==7)homeot <- as.integer(homeq[6])

  # Score by quarters (skipped for now, use play by play)
  l1 <- grep("1st",test)
  l2 <- grep("2nd",test)
  l3 <- grep("3rd",test)
  l4 <- grep("4th",test)
  l6 <- grep("Kickoff",test)

  # Rest
  l6 <- grep("Kickoff",test)
  temp <- stri_split_fixed(test[l6]," • ")


}
