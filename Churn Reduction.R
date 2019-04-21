rm(list=ls())
getwd()
#importing the data
churn_train=read.csv("E:/Edwisor/Projects/Churn Reduction/Train_data.csv",header=T)
churn_test =read.csv("E:/Edwisor/Projects/Churn Reduction/Test_data.csv",header=T)


# installing packages

x = c("ggplot2", "corrgram", "DMwR", "caret", "randomForest", "C50", "e1071","gridExtra",
      "MASS",'DataCombine', "gbm")

lapply(x, require, character.only = TRUE)
rm(x)



#Combining test_data and train_data for Exploratory Data Analysis
train_data=rbind(churn_train,churn_test)
str(churn_train)


#Extracting predictive features
train_data = subset(train_data,select = -c(phone.number,area.code,state))

#Assigning labels to categorical variables

for(i in 1:ncol(train_data)){
  
  if(class(train_data[,i]) == 'factor'){
    
    train_data[,i] = factor(train_data[,i], labels=(1:length(levels(factor(train_data[,i])))))
    
  }
}


str(train_data)

#Missing values Analysis
missing_val=data.frame(apply(train_data,2,function(x){sum(is.na(x))}))





#Getting only numeric data from the dataset
numeric_index=sapply(train_data, is.numeric)

numeric_data=train_data[,numeric_index]
num_names=colnames(numeric_data)
num_names

#Creating Box plot of all the numeric data for outlier analysis

for ( i in 1:length(num_names))
{
  assign(paste0("gn",i), ggplot(aes_string(y = (num_names[i]), x = "Churn"), data = subset(train_data))+ 
           stat_boxplot(geom = "errorbar", width = 0.5) +
           geom_boxplot(outlier.colour="red", fill = "grey" ,outlier.shape=18,
                        outlier.size=1, notch=FALSE) +
           theme(legend.position="bottom")+
           labs(y=num_names[i],x="Churn")+
           ggtitle(paste(num_names[i])))
           
}

#Displaying box plot together using grid Extra function
gridExtra::grid.arrange(gn1,gn2,gn3,gn4, ncol=4)
gridExtra::grid.arrange(gn5,gn6,gn7,gn8,ncol=4)
gridExtra::grid.arrange(gn9,gn10,gn11,gn12,ncol=4)
gridExtra::grid.arrange(gn13,gn14,gn15,ncol=3)




#Removing all the outliers
for(i in num_names){
  val = train_data[,i][train_data[,i] %in% boxplot.stats(train_data[,i])$out]
  train_data[,i][train_data[,i] %in% val] = NA
}


#Calculating Missing values in data frame(train_data)
missing_val_removed=data.frame(apply(train_data,2,function(x){sum(is.na(x))}))

#KNN Imputation for imputing missing values
train_data = knnImputation(train_data, k = 5)
sum(is.na(train_data))



#Feature Selection

#Plot Correlation to check the dependency among numeric variables

corrgram(train_data[,numeric_index], order = F,
         upper.panel=panel.pie, text.panel=panel.txt, main = "Correlation Plot")



#Chi-square test for factor variable reduction
factor_index=sapply(train_data,is.factor)
factor_data=train_data[,factor_index]
colnames(factor_data)
factor_data_length = length(colnames(factor_data))

for(i in 1 : 2){
  print(names(factor_data)[i])
  print(chisq.test(table(factor_data$Churn,factor_data[,i])))
}



#Dimesionality Reduction

train_data = subset(train_data,select = -c(total.day.charge,total.eve.charge,total.night.charge,total.intl.charge))

#Feature Scaling
#Normality check

hist(train_data$total.day.minutes)# Data is normally distributed


#Standardisation
num_names_std=list("account.length","number.vmail.messages","total.day.minutes","total.day.calls","total.eve.minutes","total.eve.calls","total.night.minutes","total.night.calls","total.intl.minutes","total.intl.calls","number.customer.service.calls")
for(i in num_names_std){
   print(i)
   train_data[,i] = (train_data[,i] - mean(train_data[,i]))/
                                  sd(train_data[,i])
  }
#Model Development

#Separating test and train data for model development

train=train_data[1:3333,]
test =train_data[3334:5000,]
rownames(test) <- NULL


#train_copy=train
#train=train_copy
#test_copy=test
#test=test_copy


## Decision Tree(C5.0)
C50_model=C5.0(Churn~.,train,trials=100,rules=TRUE)
summary(C50_model)
C50_predications=predict(C50_model,test[,-14],type="class")  

#Evaluate the performance of classification model
confMat_C50=table(test$Churn,C50_predications)
confusionMatrix(confMat_C50)

#False Negative rate
#FNR = FN/FN+TP
FNR=81/(81+143)
FNR
#Accuracy: 94.6
##FNR: 36.16

#Random Forest

RF_model= randomForest(Churn~.,train, importance = TRUE, ntree=400)
RF_Predictions = predict(RF_model, test[,-14])
confMatrix_RF = table(test$Churn, RF_Predictions)
confusionMatrix(confMatrix_RF)

#False Negative rate
#FNR = FN/FN+TP
FNR=10
#Accuracy: 93.5
#FNR: 47.32




#Logistic Regression
logit_model = glm(Churn~., data=train, family = "binomial")
logit_Predit = predict(logit_model,newdata = test)
logit_Predit = ifelse(logit_Predit>0.5,2,1)
confMarix_LR = table(test$Churn,logit_Predit)
confusionMatrix(confMarix_LR)



#Accuracy: 87.4
#FNR: 92.41


#Naive Bayes

NB_Predict = naiveBayes(Churn~., train)
NB_Predictions = predict(NB_Predict, test[,-14], type ='class')
confMatrix_NB = table(test$Churn, NB_Predictions)
confusionMatrix(confMatrix_NB)

#Accuarcy: 88.42
#FNR: 78.57
