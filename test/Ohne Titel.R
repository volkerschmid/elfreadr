library(stringi)

#link="https://github.com/volkerschmid/elfdata/blob/main/gamebooks/2023/STATS_BDHG231.pdf"
pdffile<-"temp/STATS_BDHG231.pdf"
#curl::curl_download(link,pdffile)

data <- nflreadr::load_pbp(2019)

data<-data[1:100,]
