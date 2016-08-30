#The script builts two different logistic regression models to predict class of Transformers , plots graphs 
# and finds the accuracy 
# author : Preeti Sah

#setting the directory
setwd("C:/Users/vijetasah/Desktop/IS/HW2")
getwd()

#read the csv file
sample_data<-read.csv("IF1-FEB22-detailed.csv")

#remove the columns year and Interfacial Tension 
data_logistic<-sample_data[c(-1,-7)]

#for each class create different columns
data_logistic[c("Class_B","Class_G","Class_N","Class_M")]<-NA
data_logistic$Class_N<-ifelse(data_logistic$Class=="N",1,0)
data_logistic$Class_M<-ifelse(data_logistic$Class=="M",1,0)
data_logistic$Class_B<-ifelse(data_logistic$Class=="B",1,0)
data_logistic$Class_G<-ifelse(data_logistic$Class=="G",1,0)

#splitting data into traininh and testing
set.seed(78)
index <- sample(1:nrow(data_logistic), size<-0.60*nrow(data_logistic))
data_logistic_training<- data_logistic[index,]
data_logistic_testing<- data_logistic[-index,]
data_logistic_testing_corAttributes<-data_logistic[-index,]
data_logistic_testing_multinomial<-data_logistic[-index,]

colnames(data_logistic_training)
dependent_logistic_var<-colnames(data_logistic_training[c(7:10)])

#Building logistic model based on all attributes 
independent_logistic_var<-colnames(data_logistic_training[c(1:5)])

logistic_model_All<-list()

for(i in dependent_logistic_var){
  logistic_model_All[[i]]<-glm(as.formula(paste(i,"~",paste(independent_logistic_var,collapse = "+"))),data=data_logistic_training)
}

lapply(logistic_model_All,summary)

for (i in dependent_logistic_var){
  print(i)
  plot(logistic_model_All[[i]])
}
  

#to predict the values of testing data
predicted_Class_values<-list()
for (i in dependent_logistic_var){
  predicted_Class_values[[i]]<-predict(logistic_model_All[[i]],data_logistic_testing)
}
 
data_logistic_testing$predicted_ClassB<-predicted_Class_values[[1]]
data_logistic_testing$predicted_classG<-predicted_Class_values[[2]]
data_logistic_testing$predicted_classN<-predicted_Class_values[[3]]
data_logistic_testing$predicted_classM<-predicted_Class_values[[4]]

data_logistic_testing$predicted_Class<-colnames(data_logistic_testing[11:14])[apply(data_logistic_testing[11:14],1,which.max)]

library(stringi)
data_logistic_testing$predicted_Class<-stri_sub(data_logistic_testing$predicted_Class,-1,-1)
write.csv(data_logistic_testing,"./LogisticRegression_All_TestingData.csv")

#Building confusion matrix
confusion_matrix_All<-table(data_logistic_testing$predicted_Class,data_logistic_testing$Class)
confusion_matrix_All<-as.data.frame(confusion_matrix_All)
write.csv(confusion_matrix_All,"./confusion_matrix_All.csv")

#plot the roc curves
library(pROC)

roc_B<-roc(Class_B ~ predicted_ClassB,data=data_logistic_testing)
plot(roc_B)

roc_G<-roc(Class_G ~ predicted_classG,data=data_logistic_testing)
plot(roc_G)

roc_N<-roc(Class_N ~ predicted_classN,data=data_logistic_testing)
plot(roc_N)

roc_M<-roc(Class_M ~ predicted_classM,data=data_logistic_testing)
plot(roc_M)


#using multinomial
library(nnet)
fit <- multinom(Class ~ ., data=data_logistic_training)
summary (fit) 
predict <- predict(fit, test, "probs")
predict_Class <- predict (fit, test)

#Logistic Model for highly correlated attribute
#using random forest 
library("hydroGOF")
library("randomForest")

library(randomForest)
independent_logistic_var_list<-c()
for (i in 1:5){
  independent_logistic_var_list<-c(independent_logistic_var_list,independent_logistic_var[i])
}

random_logistic<-randomForest(as.formula(paste("Class~",paste(independent_logistic_var_list,collapse = "+"))),data=data_logistic_training)
Logistic_randomForest_All<-importance(random_logistic)
write.csv(as.data.frame(Logistic_randomForest_All),"./Logistic_RandomForest_All.csv")

#cor attributes logistic Regression
independent_logistic_corAttributes<-c("Total.Acid.Number..mgKOH..g.","Colour")

logistic_model_corAttributes<-list()

#building logistic model
for(i in dependent_logistic_var){
  logistic_model_corAttributes[[i]]<-glm(as.formula(paste(i,"~",paste(independent_logistic_corAttributes,collapse = "+"))),data=data_logistic_training)
}

lapply(logistic_model_corAttributes,summary)

#plot the model
for (i in dependent_logistic_var){
  print(i)
  plot(logistic_model_corAttributes[[i]])
}

#to predict the values of testing data

predicted_Class_corvalues<-list()
for (i in dependent_logistic_var){
  print(i)
  predicted_Class_corvalues[[i]]<-predict(logistic_model_corAttributes[[i]],data_logistic_testing_corAttributes)
}

data_logistic_testing_corAttributes$predicted_ClassB<-predicted_Class_corvalues[[1]]
data_logistic_testing_corAttributes$predicted_classG<-predicted_Class_corvalues[[2]]
data_logistic_testing_corAttributes$predicted_classN<-predicted_Class_corvalues[[3]]
data_logistic_testing_corAttributes$predicted_classM<-predicted_Class_corvalues[[4]]

data_logistic_testing_corAttributes$predicted_Class<-colnames(data_logistic_testing_corAttributes[11:14])[apply(data_logistic_testing_corAttributes[11:14],1,which.max)]

library(stringi)
data_logistic_testing_corAttributes$predicted_Class<-stri_sub(data_logistic_testing_corAttributes$predicted_Class,-1,-1)

write.csv(data_logistic_testing_corAttributes,"./LogisticRegression_Cor_Testing.csv")

#confusion matrix for highly corelated values 
confusion_matrix_Corelated<-table(data_logistic_testing_corAttributes$predicted_Class,data_logistic_testing_corAttributes$Class)
write.csv(as.data.frame(confusion_matrix_Corelated),"./confusionMatrix_Correlated.csv")

#plot ROC curves
library(pROC)

roc_B<-roc(Class_B ~ predicted_ClassB,data=data_logistic_testing_corAttributes)
plot(roc_B)

roc_G<-roc(Class_G ~ predicted_classG,data=data_logistic_testing_corAttributes)
plot(roc_G)

roc_N<-roc(Class_N ~ predicted_classN,data=data_logistic_testing_corAttributes)
plot(roc_N)

roc_M<-roc(Class_M ~ predicted_classM,data=data_logistic_testing_corAttributes)
plot(roc_M)

