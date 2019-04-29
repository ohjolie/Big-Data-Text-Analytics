library(tm)
library(quanteda)
library(readtext)
library(tokenizers)


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

