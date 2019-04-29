library(tm)
library(quanteda)
library(readtext)
library(tokenizers)

#Question B
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
#end of Question B

# Question C
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
#end of Question C
