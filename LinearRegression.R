#The script builts two different linear regression models , plots graphs 
#and predict the values of Interfacial Tension of Transformers and finds the accuracy
# author : Preeti Sah
setwd("C:/Users/vijetasah/Desktop/IS/HW2")
getwd()

sample_data<-read.csv("IF1-FEB22-detailed.csv")
data_linear<-sample_data[c(-1,-8)]


#splitting the data into training and testing data sets
set.seed(101)
index <- sample(1:nrow(data_linear), size<-0.60*nrow(data_linear))
data_linear_training<- data_linear[index,]
data_linear_testingAll<- data_linear[-index,]
data_linear_testing_CorAttributes<-data[-index,]

#building the linear model based on all attributes
linearModel_All<-lm(Interfacial.Tension..mN.m. ~ .,data=data_linear_training)
summary(linearModel_All)
plot(linearModel_All)


#testing the model on the testing dataset
predicted_dependent_var<-predict(linearModel_All,data_linear_testingAll)
data_linear_testingAll$predicted_values<-predicted_dependent_var
write.csv(data_linear_testingAll,"./LinearModel_All_TestingData.csv")
#method to calculate rmse values
rmse <- function(actual_data,predicted_data)
{
  sqrt(mean((actual_data-predicted_data)^2))
}

#finding the RMSE values from the data_linear_testingAll dataset
rmse_linearModel_All<-rmse(data_linear_testingAll[c(6)],data_linear_testingAll[c(7)])

#building the linear model based on highly corelated attributes and checking the accuracy
#building the corelation matrix to find the highly corealted atributes
corelation_matrix <- cor(data_linear_training)
cor_matrix_df<-as.data.frame(corelation_matrix)
write.csv(cor_matrix_df,"./LinearModel_CorrelationMatrix.csv")

#store all the column names of the matrix
column_name<-c()
#direct function dont need to create empty list
column_name<-c(column_name,colnames(data_linear_training))
#column_name<-colnames(data)
print(column_name)


#declaring the dependent variable
dependent_name<-column_name[6]
#dont need to store just remember the name and index because only one dependent variable
#finding the independent variables from matrix and storing the coefficient and 
indepedent_coefficients<-c()
independent_indexes<-c()

for (i in 1:5){
  values<-corelation_matrix[6,i]
  if(abs(values)>0.5){
    indepedent_coefficients<-c(indepedent_coefficients,values)
    independent_indexes<-c(independent_indexes,i)
    
  }
}

independent_linear_var<-c()
for(i in 1:length(independent_indexes)){
  independent_linear_var<-c(independent_linear_var,column_name[independent_indexes[i]])
}

linearModel_corAttributes<-lm(as.formula(paste("Interfacial.Tension..mN.m. ~", paste(independent_linear_var,collapse = "+"))), data = data_linear_training)
summary(linearModel_corAttributes)
plot(linearModel_All)

#testing the model on the testing dataset
predicted_dependent_var<-predict(linearModel_corAttributes,data_linear_testing_CorAttributes)
data_linear_testing_CorAttributes$predicted_values<-predicted_dependent_var

write.csv(data_linear_testing_CorAttributes,"./LinearRegression_Correlated_TestingData.csv")

#finding the RMSE values from the data_linear_testingAll dataset
rmse_linearModel_corAttributes<-rmse(data_linear_testing_CorAttributes[c(6)],data_linear_testing_CorAttributes[c(7)])

