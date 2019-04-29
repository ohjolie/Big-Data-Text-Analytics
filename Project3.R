library(tm)
library(quanteda)
library(readtext)
library(tokenizers)




#Question A#########################################################################################



##
#read data
##

setwd("F://workplace for R")
getwd()
SAT <- VCorpus(DirSource(".",ignore.case = TRUE,mode="text",encoding="UTF-8"))

#display SAT
SAT
inspect(SAT)
str(SAT)

#Extract a document from SAT
test1<-SAT[[2]]
test1

SATtdm <- DocumentTermMatrix(SAT)
SATtdm
inspect(SATtdm[1:6,1:12])

test1tf <- termFreq(test1)
test1tf

#Corpus Management
SATlow <- tm_map(SAT,content_transformer(tolower))
SATlow

removeNumPunct <-
function(x) gsub("[^[:alpha:][:space:]]*","", x)
SATcl <- tm_map(SATlow,content_transformer(removeNumPunct))

myStopWords <- c(stopwords('english'))
myStopWords

SATstop <- tm_map(SATcl,removeWords,myStopWords)
inspect(SATstop[1])

SATtdm2 <- TermDocumentMatrix(SATstop,control = list(wordlengths = c(1,Inf)))
SATtdm2



freqTerms <- findFreqTerms(SATtdm2,lowfreq = 4)
freqTerms

##strange result
statesAssoc <- findAssocs(SATtdm2,"states",0.5)
statesAssoc

termFreq1 <- rowSums(as.matrix(SATtdm2))
termFreqsub <-subset(termFreq1,termFreq1>=6)
termFreqdf <- as.data.frame(names(termFreq1),freq = termFreq1)
termFreq1

##remove Sparse Terms
SATtdm2
Sparsetdm2 <- removeSparseTerms(SATtdm2,sparse = 0.75)
inspect(Sparsetdm2)
Sparsetdm2

##Finding Informative Words
test2 <- SATstop[[1]]
inspect(test2)

termFreqSub2<-subset(termFreq1, termFreq1 >= 40)
fit <- hclust(dist(termFreqSub2,method = "euclidean"), method = "ward.D2")
plot(fit)


m1 <- as.matrix(termFreq1)
word.freq <- sort(rowSums(m1),decreasing = T)
word.freq


#wordcloud
library(wordcloud)

pal <- brewer.pal(9,"BuGn")
pal <- pal[-(1:4)]
wordcloud(words = names(word.freq),freq = word.freq,min.freq = 20,random.color = F,colors = pal)

SATdtm <- DocumentTermMatrix(SATstop)
freq <- colSums(as.matrix(SATdtm))
SATdtm
length(freq)

ord <- order(freq,decreasing = TRUE)
freq[head(ord)]
freq[tail(ord)]


SATdtmr <- DocumentTermMatrix(SATstop,control = list(wordLengths = c(4,20)))
SATdtmr 

freqr <-colSums(as.matrix(SATdtmr))
ordr <- order(freqr,decreasing = TRUE)
freqr[head(ordr)]
freqr[tail(ordr)]


#Tokenization
library(quanteda)
SAT2 <- SATcl$content[1]
SAT2
SAT2txt <-SAT2[[1]]$content
SAT2txt
SAT2tokens <- tokens(SAT2txt)
SAT2tokens
  
  
  
SAT1 <- SAT$content[1]
SAT1
SAT1txt <- SAT1[[1]]$content
SAT1txt

SAT1tokens <- tokens(SAT1txt)
SAT1tokens


#Sentiment Analysis
library(syuzhet)
SAT1Sent <- syuzhet::get_nrc_sentiment(SAT1txt)
SAT1Sent

SAT1sdt <- rowSums(SAT1Sent)
SAT1sdt[1:50]
SAT1rdt<-rowSums(SAT1Sent)
SAT1rdt[1:50]
SAT1CDT<-colSums(SAT1Sent)
SAT1CDT

#Text weighting
SATdfm <- dfm(SAT1tokens)
SATfreq <-docfreq(SATdfm)
SATfreq

SATweights2 <- dfm_weight(SATdfm,scheme="prop")
str(SATweights2)

SATtfidf <- dfm_tfidf(SATdfm,scheme_tf = "count",scheme_df = "inverse")
SATtfidf@i
#the end of Question A#################################################################################



#Question B############################################################################################
data <- readtext('DrJekyllAndMrHyde.txt')

#################b###############
#delete \n
test <- gsub("\r?\n|\r", ' ',data$text)

#split into sentences using tokens
sentence1 <- tokens(test, what = "sentence", remove_url=TRUE)
sentence1
sentence_words.1 <- tokenize_words(sentence1[[1]])
lengths.1 <- sapply(sentence_words.1, length)
longest.1 <- sort(lengths.1, TRUE)
longest.1
for (n in 1:10) {
  for (i in 1:1345){
    if(lengths.1[i] == longest.1[n]){
      print(paste0("The ", n ,"th longest sentence is:"))
      print(sentence1[[1]][i])
      print('')} 
  } 
}



#split into sentences using tokenize_sentences
sentence2 <- tokenize_sentences(test)

sentence_words.2 <- tokenize_words(sentence2[[1]])
lengths.2 <- sapply(sentence_words.2, length)
longest.2 <- sort(lengths.2, TRUE)
longest.2
for (n in 1:10) {
  for (i in 1:1513){
    if(lengths.2[i] == longest.2[n]){
      print(paste0("The ", n ,"th longest sentence is:"))
      print(sentence2[[1]][i])} 
  } 
}
#end of Question B#######################################################################

# Question C#############################################################################
ws<-VCorpus(DirSource(".", ignore.case = TRUE, mode = "text"))
ws

inspect(ws)
str(ws)
test1<-ws[[1]]
##
pg1<-ws[[13]]
pg2<-ws[[9]]
pg3<-ws[[8]]
pg4<-ws[[1]]
pg5<-ws[[10]]
pg6<-ws[[6]]
pg7<-ws[[5]]
pg8<-ws[[4]]
pg9<-ws[[12]]
pg10<-ws[[2]]
pg11<-ws[[3]]
##
wsdtm<-DocumentTermMatrix(ws)
wsdtm
wstdm<-TermDocumentMatrix(ws)
wstdm
inspect(wstdm[1:12, 1])

test1tf<-termFreq(test1)
test1tf
test1df<-as.data.frame(test1tf)
test1df

wslow<-tm_map(ws, content_transformer(tolower))
wslow

removeNumPunct<-function(x) gsub("[^[:alpha:][:space:]]*", "", x)
wscl<-tm_map(wslow, content_transformer(removeNumPunct))

myStopWords<-c(stopwords('english'))
myStopWords

wsstop<-tm_map(wscl, removeWords, myStopWords)
inspect(wsstop[1:13])

wstdm2<-TermDocumentMatrix(wsstop[1], control = list(wordlengths = c(1, Inf)))
wstdm2

freqTerms<-findFreqTerms(wstdm2, lowfreq = 4)
freqTerms

statesAssoc<-findAssocs(wstdm2, "states", 0.5)
statesAssoc

termFreq<-rowSums(as.matrix(wstdm2))
termFreqSub<-subset(termFreq, termFreq >= 6)
termFreqdf<-as.data.frame(names(termFreq), freq = termFreq)
termFreq

wstdm2

sparsetdm2<-removeSparseTerms(wstdm2, sparse = 0.75)
inspect(sparsetdm2)
sparsetdm2

#p3stop<-wsstop[[1]]
#inspect(p3stop)

termFreqSub2<-subset(termFreq, termFreq >= 40)
fit <- hclust(dist(termFreqSub,method = "euclidean"), method = "ward.D2")
plot(fit)

word.freq<-sort(termFreq, decreasing = T)
word.freq

#install.packages("wordcloud")
library("wordcloud")

pal<-brewer.pal(9, "BuGn")
pal<-pal[-(1:4)]
wordcloud(words = names(word.freq), freq = word.freq, min.freq =  3, random.order = F, colors = pal)
#end of Question C######################################################################################

#Question D
##find longest words
SATdtmr
imax <- length(SATdtmr$dimnames$Terms)
Tlength <- nchar(SATdtmr$dimnames$Terms)
longest <- max(nchar(SATdtmr$dimnames$Terms))
#SATerm <- tokens(SAT1txt,"word",remove_numbers = TRUE, remove_punct = TRUE,
#                remove_symbols = TRUE, remove_separators = TRUE,
#                 remove_twitter = TRUE, remove_hyphens = TRUE, remove_url = TRUE)
#SATerm

for (i in 1:imax) {
  if(Tlength[i]==longest){
    print(SATdtmr$dimnames$Terms[i])
  }
}

