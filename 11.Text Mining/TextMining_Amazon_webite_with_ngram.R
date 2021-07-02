#setwd("D:\\Assignment__Rstudio\\Text_mining\\")
install.packages("rvest")
install.packages("XML")
install.packages("magrittr")
install.packages("textcat")
install.packages("tm")
library(tm)
library(rvest)
library(XML)
library(magrittr)

# Review extraction
url <- "https://www.amazon.in/dp/B077PWJQZM#customerReviews"
Glacial <- NULL
for(i in 1:10){
  murl <- read_html(as.character(paste(url,i,sep = "=")))
  rev <- murl%>%
    html_nodes(".review-text")%>%
    html_text()
  Glacial <- c(Glacial,rev)
}
write.table(Glacial,"Glacial.txt",row.names = F)

OnePlus8  <- unique(readLines("Glacial.txt"))

#cleaning https://
df_OnePlus8  <- gsub(pattern = "http.*",replacement = "",x=OnePlus8 )
df_OnePlus8  <- gsub("http.","",df_OnePlus8 )
df_OnePlus8  <- gsub("#.*","",df_OnePlus8 )
df_OnePlus8  <- gsub("@.*","",df_OnePlus8 )

library(textcat)
table(textcat(df_OnePlus8 ))
df_OnePlus8 [which(textcat(df_OnePlus8 )=="norwegian")]
consider <- c(which(textcat(df_OnePlus8 )!="norwegian"))
df_OnePlus8  <- df_OnePlus8 [consider]

#stopwords
stop <- readLines(file.choose())
Glacial_corp <- Corpus(VectorSource(df_OnePlus8 ))
Glacial_corp <- tm_map(Glacial_corp,removePunctuation)
Glacial_corp <- tm_map(Glacial_corp,removeWords,stop)
Glacial_corp <- tm_map(Glacial_corp,stripWhitespace)
mytxt = as.data.frame(inspect(Glacial_corp))
#TDM
Glacial_tdm <- TermDocumentMatrix(Glacial_corp)

#convert tdm to dtm
Glacial_dtm <- t(Glacial_tdm)
rowtotals <- apply(Glacial_dtm,1,sum)
Glacial_dtm2 <- Glacial_dtm[rowtotals>3,]
Glacial_dtm2$dimnames$Terms

#LDA
library(topicmodels)
Glacial_LDA <- LDA(x=Glacial_dtm2,10)
Glacial_LDA_terms <- terms(Glacial_LDA,5)
Glacial_LDA_terms
topics <- terms(Glacial_LDA)
table <- table(names(topics),unlist(topics))
library(cluster)
library(dendextend)
cluster <- hclust(dist(table),method = "ward.D2")
colr <- color_branches(cluster,k=4)
plot(colr)

#NLP
library(syuzhet)
Glacials <- get_sentences(df_OnePlus8 )
class(Glacials)

#sentiment analysis
sentiments <- c("syuzhet","afinn","bing","nrc","custom")
A <- NULL
sentimentlist <- NULL
for(i in sentiments[1:4]){
  sentimentlist[[i]] <- get_sentiment(Glacials,method = i)
  A[[i]] <- table(get_sentiment(Glacials,method = i))
}
A
sentimentlist

#plot for NRC
plot(sentimentlist$nrc,type = "l",main = "NRC plot")
abline(h=0,col ="red")
abline(h=1,col ="blue")
abline(h=2,col="yellow")

# sentences with negative emotion values
negative <- sentimentlist$nrc[which.min(sentimentlist$nrc)]
Glacials[which(sentimentlist$nrc==negative)]

#sentences with positive emotions values
positive <- sentimentlist$nrc[which.max(sentimentlist$nrc)]
Glacials[which(sentimentlist$nrc==positive)]

#emotion plot
nrc_data <- get_nrc_sentiment(Glacials)
barplot(sort(colSums(prop.table(nrc_data[,1:8]))),cex.names = 0.8,col = 1:8)

#word cloud
library(tm)
library(SnowballC)
library(wordcloud)
library(RColorBrewer)

freq <- rowSums(as.matrix(Glacial_tdm))
length(freq)
order <- order(freq,decreasing = TRUE)
head(freq)
data_frame <- data.frame(word=names(freq),freq=freq)
data_frame <- data_frame[-8,]
wordcloud(words = data_frame$word,freq = data_frame$freq,min.freq = 2,max.words = 200,random.order = F,colors = brewer.pal(10,"Dark2"))
#Operations on term-document matrix
findFreqTerms(Glacial_dtm,lowfreq = 5)
findAssocs(Glacial_dtm,terms = "fast",corlimit = 0.3)
head(data_frame,10)
barplot(data_frame[(1:11),]$freq,names.arg = data_frame[(1:11),]$word,col = "navyblue")
# Bar plot of the frequency for the top10
barplot(data_frame[1:10,]$freq, las = 2, 
        names.arg = data_frame[1:10,]$word,
        col ="lightblue", main ="Most frequent words",
        ylab = "Word frequencies")
#Feature extraction using n-grams************
library(wordcloud)
library(qdap)
library(RColorBrewer)
library(RWeka)
install.packages("ngram")
install.packages("topicmodels")
install.packages("dplyr")

#***************** ngram feature selection*****************

library(ngram)
mytxt = as.data.frame(inspect(Glacial_corp))
get.ngrams(ngram(as.character(mytxt),3))
my<-get.ngrams(ngram(as.character(mytxt),3))

#************Bigram******************

minfreq_bigram<-2
bitoken <- NGramTokenizer(Glacial_corp, Weka_control(min=2,max=2))
 two_word <- data.frame(table(bitoken))
sort_two <- two_word[order(two_word$Freq,decreasing=TRUE),]
wordcloud(sort_two$bitoken,sort_two$Freq,random.order=FALSE,scale = 
              c(2,0.35),min.freq = minfreq_bigram,colors = brewer.pal(8,"Dark2"),max.words=150)

# Trigrams 

minfreq_trigram <- 5

token_delim <- " \\t\\r\\n.!?,;\"()"
tritoken <- NGramTokenizer(Glacial_corp, Weka_control(min=3,max=3, delimiters = token_delim))
three_word <- data.frame(table(tritoken))
sort_three <- three_word[order(three_word$Freq,decreasing=TRUE),]
wordcloud(sort_three$tritoken, sort_three$Freq, random.order=FALSE,min.freq = minfreq_trigram,scale = c(1.2,0.35),colors = brewer.pal(8,"Dark2"),max.words=150)



