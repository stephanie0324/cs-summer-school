


#PTT 網路爬蟲抓出所有文章內文所對應的網址
library(RCurl)
library(XML)
from <- 6943 # 2018-07-19
to   <- 6944 # 2018-07-20
prefix = "https://www.ptt.cc/bbs/movie/index"

data <- list()
for( id in c(from:to) )
{
  url  <- paste0( prefix, as.character(id), ".html" )
  html <- htmlParse( getURL(url) )
  url.list <- xpathSApply( html, "//div[@class='title']/a[@href]", xmlAttrs )
  data <- rbind( data, as.matrix(paste('https://www.ptt.cc', url.list, sep='')) )
}
data <- unlist(data)

head(data)

#利用所有文章的網址去抓所有文章內文, 並解析出文章的內容並依照 hour 合併儲存。
library(dplyr)
library(RCurl)
library(XML)
getdoc <- function(url)
{
  html <- htmlParse( getURL(url) )
  doc  <- xpathSApply( html, "//div[@id='main-content']", xmlValue )
  time <- xpathSApply( html, "//*[@id='main-content']/div[4]/span[2]", xmlValue )
  temp <- gsub( "  ", " 0", unlist(time) )
  part <- strsplit( temp, split=" ", fixed=T )
  timestamp <- part[[1]][4]
  timestamp <- strsplit( timestamp, split=":", fixed=T )
  hour <- timestamp[[1]][1]
  name <- paste0('./DATA/', hour, ".txt")
  write(doc, name, append = TRUE)
}

sapply(data, getdoc)

#建立文本資料結構與基本文字清洗
library(tm)
library(tmcn)
d.corpus <- Corpus( DirSource("./DATA") )
d.corpus <- tm_map(d.corpus, removePunctuation)
d.corpus <- tm_map(d.corpus, removeNumbers)
d.corpus <- tm_map(d.corpus, function(word) {
  gsub("[A-Za-z0-9]", "", word)
})

#進行斷詞，並依照日期建立文本矩陣
library(jiebaR)
library(jiebaRD)
mixseg = worker()
jieba_tokenizer = function(d)
{
  unlist( segment(d[[1]], mixseg) )
}
seg = lapply(d.corpus, jieba_tokenizer)

count_token = function(d)
{
  as.data.frame(table(d))
}
tokens = lapply(seg, count_token)

n = length(seg)
TDM = tokens[[1]]
colNames <- names(seg)
colNames <- gsub(".txt", "", colNames)
for( id in c(2:n) )
{
  TDM = merge(TDM, tokens[[id]], by="d", all = TRUE)
  names(TDM) = c('d', colNames[1:id])
}
TDM[is.na(TDM)] <- 0
library(knitr)
kable(head(TDM))
kable(tail(TDM))

#tdm to tfidf

tdm<-as.matrix(TDM[,2:n+1])
tf <- apply(as.matrix(TDM[,2:(n+1)]), 2, sum )
library(Matrix)
idfCal<- function(word_doc)
{ 
  log2( n / nnzero(word_doc) ) 
}
idf <- apply(as.matrix(TDM[,1:(n+1)]), 1, idfCal)

doc.tfidf <- TDM

tempY = matrix(rep(c(as.matrix(tf)), each = length(idf)), nrow = length(idf))
tempX = matrix(rep(c(as.matrix(idf)), each = length(tf)), ncol = length(tf), byrow = TRUE)
doc.tfidf[,2:(n+1)] <- (doc.tfidf[,2:(n+1)] / tempY) * tempX

stopLine = rowSums(doc.tfidf[,2:(n+1)])
delID = which(stopLine == 0)

kable(head(doc.tfidf[delID,1]))

