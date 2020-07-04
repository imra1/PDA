#Load libraries
library(class)
library(gmodels)
library(caret)
library(lattice)

#Load the data set
churn_dataset <- read.csv('WA_Fn-UseC_-Telco-Customer-Churn.csv', stringsAsFactors = TRUE)

#check structure of the dataset
str(churn_dataset)

#Select the columns for classification
churn_dataset.subset <- churn_dataset[c('gender','SeniorCitizen','Partner','Dependents','tenure','PhoneService','InternetService', 'PaymentMethod' , 'TotalCharges', 'Churn')]

#check for missing values
sapply(churn_dataset.subset, function(value) sum(is.na(value)))

#remove missing values rows
churn_dataset.subset <- churn_dataset.subset[complete.cases(churn_dataset.subset), ] 


#convert values as numeric
must_convert<-sapply(churn_dataset.subset,is.factor)      
converted<-sapply(churn_dataset.subset[,must_convert],unclass)    
churn_customers<-cbind(churn_dataset.subset[,!must_convert],converted)


#check dataset structure 
str(churn_customers)

#Normalization
normalize <- function(value) {
  return ((value - min(value)) / (max(value) - min(value))) }

#normalized data set in the 'loan.subset.n' variable and also we're removing the 'Churn' variable
churn_customers.subset <- as.data.frame(lapply(churn_customers[,1:9], normalize))

str(churn_customers.subset)

#Split the data set
set.seed(1234)
indx = sample(2, nrow(churn_customers.subset), replace=TRUE, prob=c(0.67, 0.33)) 
churn_customers.subset.train = churn_customers[indx==1,1:9] 
churn_customers.subset.test = churn_customers[indx==2,1:9] 


churn_customers.subset.trainLabels = churn_customers[indx==1,10]
churn_customers.subset.testLabels = churn_customers[indx==2, 10]


churn_customers.subset.pred = knn(train = churn_customers.subset.train, test = churn_customers.subset.test, cl = churn_customers.subset.trainLabels, k=83)
churn_customers.subset.pred


CrossTable(x = churn_normalized.testLabels, y = churn_normalized.pred3, prop.chisq=FALSE)
confusionMatrix(table(churn_normalized.pred3 ,churn_normalized.testLabels))