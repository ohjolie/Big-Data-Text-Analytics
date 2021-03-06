library(tm)
library(quanteda)
library(readtext)
library(tokenizers)
library(corpustools)
library(stringi)
library(tidytext)
library(wordcloud)



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
#set workspace
setwd("/Users/zhuolinyang/Documents/RWorkspace/Project 3/Chapter")

#get data from file
ws<-VCorpus(DirSource(".", ignore.case = TRUE, mode = "text"))

#clean data
wslow<-tm_map(ws, content_transformer(tolower))

removeNumPunct<-function(x) gsub("[^[:alpha:][:space:]]*", "", x)
wscl<-tm_map(wslow, content_transformer(removeNumPunct))

myStopWords<-c(stopwords('english'))

wsstop<-tm_map(wscl, removeWords, myStopWords)
inspect(wsstop[1:12])

#seperate each chapter
ch0<-wsstop[12]
ch1<-wsstop[8]
ch2<-wsstop[7]
ch3<-wsstop[1]
ch4<-wsstop[9]
ch5<-wsstop[6]
ch6<-wsstop[5]
ch7<-wsstop[4]
ch8<-wsstop[11]
ch9<-wsstop[2]
ch10<-wsstop[3]
ch11<-wsstop[10]

#generate and display dendrogram and the WordCloud
for(i in 0:11){
  wstdm<-TermDocumentMatrix(get(paste("ch", i, sep = "")), control = list(wordLengths = c(1, Inf)))
  
  termFreq<-rowSums(as.matrix(wstdm))
  termFreqdf<-as.data.frame(names(termFreq), freq = termFreq)
  
  #generate and display dendrogram
  termFreqSub<-subset(termFreq, termFreq >= 5)
  fit <- hclust(dist(termFreqSub,method = "euclidean"), method = "ward.D2")
  plot(fit)
  
  #generate and display WordCloud
  word.freq<-sort(termFreq, decreasing = T)
  pal<-brewer.pal(9, "BuGn")
  pal<-pal[-(1:4)]
  wordcloud(words = names(word.freq), freq = word.freq, min.freq = 3, random.order = F, colors = pal)
}
#end of Question C######################################################################################

#Question D
##find longest words
for(j in 1:12){
  Chapterj <- DocumentTermMatrix(SATstop[j])
  #Chapterj$dimnames$Terms
  imax <- length(Chapterj$dimnames$Terms)
  Tlength <- nchar(Chapterj$dimnames$Terms)
  longest <- max(nchar(Chapterj$dimnames$Terms))
  for (i in 1:imax) {
    if(Tlength[i]==longest){
      print(c("charpter",j))
      print(Chapterj$dimnames$Terms[i])
    }
  }
  
}

#find the longest sentence in each chapter
setwd("/Users/jolie/Documents/Rworkspace/Big-Data-Text-Analytics/txt")
getwd()
chapter <- list()
chapter[[1]] <- readtext('TheStart.txt')
chapter[[2]] <- readtext('StoryOfTheDoor.txt')
chapter[[3]] <- readtext('SearchForMrHYDE.txt')
chapter[[4]] <- readtext('DrJekyllWasQuiteAtEase.txt')
chapter[[5]] <- readtext('TheCarewMurderCase.txt')
chapter[[6]] <- readtext('IncidentOfTheLetter.txt')
chapter[[7]] <- readtext('IncidentOfDrLanyon.txt')
chapter[[8]] <- readtext('IncidentOfDrLanyon.txt')
chapter[[9]] <- readtext('TheLastNight.txt')
chapter[[10]] <- readtext('DrLanyob’sNarrative.txt')
chapter[[11]] <- readtext('HenryJekyll’sFullStatementOfTheCase.txt')
chapter[[12]] <- readtext('TheEnd.txt')

chap.clean <- list()
sentence <- list()
sentence_words <- list()
sentence_length <- list()
longest <- list()
shortest <- list()
longest_sentence <- list()
shortest_sentence <- list()
for(i in 1:12){
 chap.clean[[i]] <- gsub("\r?\n|\r", ' ',chapter[[i]]$text)
 sentence[i] <- tokenize_sentences(chap.clean[[i]])
 sentence_words[[i]] <- tokenize_words(sentence[[i]])
 sentence_length[[i]] <- sapply(sentence_words[[i]], length)
 longest[i] <- max(sentence_length[[i]])
 longest_sentence[[i]] <- sentence[[i]][which(sentence_length[[i]][]==longest[i])]
 print(paste0("The longest sentence in chapter ", i ," is:"))
 print(longest_sentence[[i]])
 shortest[i] <- min(sentence_length[[i]])
 shortest_sentence[[i]] <- sentence[[i]][which(sentence_length[[i]][]==shortest[i])]
 print(paste0("The shortest sentence in chapter ", i ," is:"))
 print(shortest_sentence[[i]])
} 


#the end of QuestionD#######################################################################

#Question e#################################################################################
library(wordnet)
setDict("C:/Program Files (x86)/WordNet/2.1/dict")
Sys.setenv(WNHOME = "C:/Program Files (x86)/WordNet/2.1")
getDict()
library(quanteda)
Chapter1 <- DocumentTermMatrix(SATstop[8])
nouns <- list()
verbs <- list()

imax <- length(Chapter1$dimnames$Terms)
for (i in 1:imax) {
  filter <- getTermFilter("ExactMatchFilter", Chapter1$dimnames$Terms[i], TRUE) 
  if(nchar(Chapter1$dimnames$Terms[i])>4){
    terms <- getIndexTerms("VERB", 5, filter) 
    sapply(terms, getLemma)
    verbs <- c(verbs,sapply(terms, getLemma))
    
    terms <- getIndexTerms("NOUN", 5, filter) 
    sapply(terms, getLemma)
    nouns <- c(nouns,sapply(terms, getLemma))
  }
}
nouns
verbs
#the end of Question e###################################################


#Question F###############################################################
library(zipfR)
WFS.spc <- spc(Vm = word.freq,m=1:length(word.freq))
#WFS.spc
plot(WFS.spc)
with(WFS.spc, plot(m, Vm, main="Frequency Spectrum"))
#the end of Question F###################################################



#Question G###############################################################
#install.packages("SentimentAnalysis")
library(SentimentAnalysis)

#bigrams
bigram_tdm <- TermDocumentMatrix(ch1, control=list(wordLengths = c(7, Inf), tokenize=function(x) ngram_tokenize(x, ngmin=2, ngmax=2)))
freq = sort(rowSums(as.matrix(bigram_tdm)),decreasing = TRUE)
freq.df = data.frame(word=names(freq), freq=freq)
head(freq.df, 20)
words <- strsplit(as.character(freq.df$word)," ")
imax <- length(freq.df$word)
bigram <- data.frame(Bigram=factor(),Freq=integer())
for(i in 1:imax){
  if(nchar(words[[i]][1]) > 6 && nchar(words[[i]][2]) > 6){
    bigram <-rbind(bigram,freq.df[i,])
  }
}
head(bigram, 20)

#trigrams
trigram_tdm <- TermDocumentMatrix(ch1, control=list(wordLengths = c(7, Inf), tokenize=function(x) ngram_tokenize(x, ngmin=3, ngmax=3)))
freq = sort(rowSums(as.matrix(trigram_tdm)),decreasing = TRUE)
freq.df = data.frame(word=names(freq), freq=freq)
head(freq.df, 20)
words <- strsplit(as.character(freq.df$word)," ")
imax <- length(freq.df$word)
trigram <- data.frame(Trigram=factor(),Freq=integer())
for(i in 1:imax){
  if(nchar(words[[i]][1]) > 6 && nchar(words[[i]][2]) > 6 && nchar(words[[i]][3]) > 6){
    trigram <-rbind(trigram,freq.df[i,])
  }
}
head(trigram, 20)
#the end of Question G#############################################################

#Question H###############################################################

#Corpus tools
tc = create_tcorpus(gsub("[^[:alpha:][:space:]]*","", readtext('StoryOfTheDoor.txt',encoding="UTF-8")))
tc$preprocess('token', 'stem', remove_stopwords = TRUE, use_stemming = TRUE)
dtm<-as.matrix(tc$dtm(feature="stem"))
dtm
#words occur together with word door
words <= tc$feature_associations(query="door*")
words

#Stringi
text<-gsub("\n"," ", readtext('StoryOfTheDoor.txt',encoding="UTF-8"))
stri_reverse(text)
stri_trim(text)
stri_split_boundaries(stri_trim(text), stri_opts_brkiter(type="sentence"))
stri_stats_general(text)

#quanteda
text<-gsub("\n"," ", readtext('StoryOfTheDoor.txt',encoding="UTF-8"))
text_corpus <- corpus(text)
summary(text_corpus)
text_tokens <- tokens(text, remove_punct = TRUE)
text_tokens
text_dfm <- dfm(text_tokens)
tstat_freq <- textstat_frequency(text_dfm, n = 5, groups = "lang")

#tidytext
get_sentiments()

#the end of Question H#############################################################


