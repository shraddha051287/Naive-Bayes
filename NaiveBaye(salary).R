setwd("D:\\ML\\R\\Naive Bayes")
library(mlbench)
salary_data_train<-read.csv(file.choose())
salary_data_test<-read.csv(file.choose())

View(salary_data_train)
View(salary_data_test)
help("salary_data_train")
summary(salary_data_train)
summary(salary_data_test)
barplot(table(as.factor(salary_data_train[,8]),as.factor(salary_data_train[,9])),legend=c("White","Black"))
plot(as.factor(salary_data_train[salary_data_train$race=="white",9]))
plot(as.factor(salary_data_train[salary_data_train$race=="Black",8]))

str(salary_data_train)
train_x<-salary_data_train[,c()]

# na values by column and class

na_by_col_race<-function(col,cls){
  return(sum(is.na(salary_data_train[,col]) & salary_data_train$race==rac))
}

na_by_col_race("V1","White")
na_by_col_race("V1","Black")

p_y_col_race<-function(col,race){
  sum_y<-sum(salary_data_train[,col]=="y" &salary_data_train$race==race,na.rm = T)
  sum_n<-sum(salary_data_train[,col]=="n" & salary_data_train$race==race,na.rm = T)
  return (sum_y/(sum_y+sum_n))
}
p_y_col_race(2,'White')  
p_y_col_race(2,'Black')
na_by_col_race(2,'White')
na_by_col_race(2,'Black')


# imputing missing values 

set.seed(3)
train<-order(runif(290))
test<--train
training<-salary_data_train[train,]
testing<-salary_data_train[test,]


library(e1071)
model<-naiveBayes(training$race~.,data=training)
pred<-predict(model,newdata = testing[,-8])
mean(pred==testing[,8])

acc<-NULL


for (i in 1:20){train<-order(runif(350))
set.seed(350)
test<--train
training<-salary_data_train[train,]
testing<-salary_data_train[test,]
model<-naiveBayes(training$race~.,data=training[,-9])
pred<-predict(model,testing[,-1])
acc<-c(acc,mean(pred==testing[,1]))

}
acc



# Naive Bayes for the continuous data

data()
table(salary_data_train[,9])
View(train)
str(train)
train<-order(runif(135))
t1<-train
t2<-train
test<--train
training<-salary_data_train[train,]
testing<-salary_data_test[test,]
table(training[,9])
table(testing[,9])
library(e1071)
model_salary_data_train<-naiveBayes(training$race~.,data = training[,-9])
pred_race<-predict(model_salary_data_train,testing[,-9])
mean(pred_race==testing[,8])
table(pred_race)
table(testing[,8])

boxplot(salary_data_train$capitaloss.age)
boxplot(salary_data_test$race.sex)
summary(salary_data_train$race.sex)

