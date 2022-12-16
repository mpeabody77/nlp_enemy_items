#############################################
# Basic Text Similarity Syntax              #
#                                           #
# by Kirk A Becker                          #
# 2022-04-01                                #
#                                           #
# NCME Enemy Item Detection Workshop        #
#############################################

install.packages("tm")
install.packages("stopwords")
install.packages("lsa")
install.packages("textstem")
install.packages("stringr")
library(tm)
library(stringr)
library(stopwords)
library(lsa)
library(textstem)

#Read in item bank
#setwd("C:\\Users\\beckka.PEROOT\\Dropbox\\NCME enemies and NLP workshop 2022\\Test Similarity")
items<-read.table("item examples for similarity.txt",header=TRUE,fill=TRUE,sep="\t")
stoplist<-read.delim("stoplist.txt")

#For any component of the item, we'll want to clean out the punctuation,
#numbers, white space, and pesky special characters
stem<-removePunctuation(paste(items$Stem,items$Key,sep=" "))
stem<-tolower(stem)
stem<-removeNumbers(stem)
stem<-str_replace(stem,"\t"," ")
stem<-str_replace(stem,"\n"," ")
stem<-stripWhitespace(stem)

#Create a matrix with a column for item id and a column for each word in the
#item component. I'm separately lemmatizing and stemming each word here. 
#Larger/more complex document sets might identify strengths of the different
#options (e.g., stemming might be better or wors for highly specific text)
idword<-matrix("",ncol=4)
colnames(idword)<-c("ID","word","stem","lemma")
idword<-idword[-1,]

for(i in 1:length(stem)){
 sp<-stem[i]
 sp<-str_split(sp," ")
#stem words - I believe the default here is specific to English
 sp2<-stemDocument(stem[i])
 sp2<-str_split(sp2," ")
#lemmatize words
 sp3<-lemmatize_strings(stem[i])
 sp3<-str_split(sp3," ")
 idword<-rbind(idword,cbind(rep(items$ItemID[i],length(sp2[[1]])),sp[[1]],sp2[[1]],sp3[[1]]))
}

#at this point you might remove stopwords
#if removing phrases (e.g., "which of the following"), use str_replace prior
#to the creation of the idword matrix
idword<-subset(idword,idword[,2]!="")
for(j in 1:nrow(stoplist)){
 idword<-subset(idword,idword[,2]!=stoplist[j,1])
}

#now the nrow(idword) reduced to 150 from 299


#nrow(dwi):12 (number of questions); ncol(dwi):96 (number of stems) 
#Create a document term matrix
dwi<-table(idword[,1],idword[,3])

#LDA
dwicos<-cosine(t(dwi))
stempairs<-data.frame(rep(rownames(dwicos),times=ncol(dwicos)),stack(data.frame(dwicos)))
#head(stempairs)


#note - this will only work if the item ids don't contain spaces
stempairs<-subset(stempairs,stempairs[,1]!=stempairs[,3])
hist(stempairs[,2])

