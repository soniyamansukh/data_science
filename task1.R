#GRIP TASK 1:Predicting the percentage of student based on the no. of study hours.
#Subject:Data Science And Business Analytics
#Name:Soniya Sunil Mansukh

#Packages required to install ggplot2,readr,caTools,DT,curl
#loading the libraries required from the package
library(ggplot2)
library(readr)
library(caTools)

#loading the given dataset
data<- read_csv("https://raw.githubusercontent.com/AdiPersonalWorks/Random/master/student_scores%20-%20student_scores.csv")
View(data)

#plotting hours vs percentage graph
plot(data$Hours,data$Scores,xlab = "Hours",ylab = "Percentage",main="Hours vs Percentage",col="blue")

#Finding the correlation
cor(data$Hours,data$Scores)

#Splitting the data
set.seed(2)
split<- sample.split(data,SplitRatio = 0.8)
split
train<- subset(data,split="TRUE")
test<- subset(data,split="FALSE")
train
test

#Creating Model based on the data
Model<- lm(data$Scores ~.,data = train)
summary(Model)

#Plotting regression graph
plot(data$Hours,data$Scores,xlab = "Hours",ylab = "Percentage",main = "Hours vs Percentage",col="blue")
abline(Model,col="red")

#Predicting the data
Predict<- predict(Model,test)
Predict

#Comparing actual vs predicted data
dataf<- data.frame(Actual=test$Scores,Predicted=Predict)
DT::datatable(dataf)

#Predicting percentage for given hours
#hours is 9.25
new_data<- data.frame(Hours=c(9.25))
predict(Model,newdata = new_data)


#Finding Mean Absolute Error
rms<- sqrt(mean(Predict- data$Scores)^2)
rms

