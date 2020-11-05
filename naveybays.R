salary_data_train=read.csv(file.choose())
salary_data_test=read.csv(file.choose())
View(salary_data_train)
View(salary_data_test)
salary_data_train$race<-factor(salary_data_train$race)
str(salary_data_train$race)
table(salary_data_train$race)


salary_data_test$race<-factor(salary_data_test$race)
str(salary_data_test$race)
table(salary_data_test$race)


##build a corpus using the text mining (tm) package 
install.packages("tm")
library(tm)
sms_Corpus<-Corpus(VectorSource(sms_raw$text))
## clean up the corpus using tm_map()
corpus_clean<-tm_map(sms_corpus,tolower)
corpus_clean<-tm_map(corpus_clean,removeNumbers)
corpus_clean<-tm_map(corpus_clean,removeWords,stopwords())
corpus_clean<-tm_map(corpus_clean,removePunctuation)
corpus_clean<-tm_map(corpus_clean,stripWhitespace)
corpus_clean<-tm_map(corpus_clean,PlainTextDocument)
##create a document-term space matrix
sms_dtm<-DocumentTermMatrix(corpus_clean)
sms_dtm
## creating training and test datasets
sms_raw_train<-sms_raw[1:4169, ]
sms_raw_test<-sms_raw[4170:5559, ]

sms_dtm_train<-sms_dtm[1:4169, ] 
sms_dtm_test<-sms_dtm[4170:5559, ]

sms_corpus_train<-corpus_clean[1:4169]
sms_corpus_test<-corpus_clean[4170:5559]

##check that proportion of spam is similar
prop.table(table(sms_corpus_train$type))
prop.table(table(sms_corpus_test$type))

##indicator feature for frequent word
sms_dict<-findFreqTerms(sms_dtm_train,5)
sms_train<-DocumentTermMatrix(sms_corpus_test,list(dictionary=sms_dict))
sms_test<-DocumentTermMatrix(sms_corpus_test,list(dictionary=sms_dict))
View(sms_train)
## Convert counts to a factor

convert_counts<-function(x){
  x<-ifelse(x>0,1,0)
  x<-factor(x,levels = c(0,1),labels = c("No","Yes"))
}
##apply () convert _ count () to columns of train / test data
sms_train<-apply(sms_train,MARGIN = 2,convert_counts)
sms_test<-apply(sms_test,MARGIN = 2,convert_counts)

##training model on the data
install.packages("e1071")
library(e1071)

sms_classifier<-naiveBayes(sms_raw_train,sms_raw_train$type)
sms_classifier

## Evaluating model performance
sms_test_pred<-predict(sms_classifier,sms_test)
View(sms_test)
library(gmodels)
CrossTable(sms_test_pred,sms_raw_test$type,prop.chisq=FALSE,prop.t=FALSE,prop.r=FSLSE,dnn=c('predicted','actual'))
sms_classifier2<-naiveBayes(sms_raw_train,sms_raw_train$type,laplace = 1)
sms_test_pred2<-predict(sms_classifier2,sms_test)
CrossTable(sms_test_pred2,sms_raw_test$type,prop.chisq = FALSE,prop.t = FALSE,prop.r = FALSE)
