library(dplyr)
library(gmodels)
library(caret)
library(devtools)
library(installr)
library(pROC)
library(C50)
library(irr)
library(e1071)
library(rpart)
library(rpart.plot)



# Reading in and exmploring the data
sms_results <- read.csv(file.choose())

##Preprocessing
#1. Removing ID columns 
modeldata<- sms_results[-c(1,2)]

#There are no NAs in the Data
#2.Converting Categorical columns into factors
modeldata$gender<-as.factor(modeldata$gender)
modeldata$Partner<-as.factor(modeldata$Partner)
modeldata$SeniorCitizen<-as.factor(modeldata$SeniorCitizen)
modeldata$Dependents<-as.factor(modeldata$Dependents)
modeldata$InternetService<-as.factor(modeldata$InternetService)
modeldata$MultipleLines<-as.factor(modeldata$MultipleLines)
modeldata$PhoneService<-as.factor(modeldata$PhoneService)
modeldata$OnlineSecurity<-as.factor(modeldata$OnlineSecurity)
modeldata$OnlineBackup<-as.factor(modeldata$OnlineBackup)
modeldata$DeviceProtection<-as.factor(modeldata$DeviceProtection)
modeldata$TechSupport<-as.factor(modeldata$TechSupport)
modeldata$StreamingMovies<-as.factor(modeldata$StreamingMovies)
modeldata$StreamingTV<-as.factor(modeldata$StreamingTV)
modeldata$Contract<-as.factor(modeldata$Contract)
modeldata$PaperlessBilling<-as.factor(modeldata$PaperlessBilling)
modeldata$PaymentMethod<-as.factor(modeldata$PaymentMethod)
modeldata$Churn<-as.factor(modeldata$Churn)

#2. Changing the '?' in the Total Chargers into a NULL and convrting the column into numeric column.
modeldata$TotalCharges<-gsub("?", 'NULL', modeldata$TotalCharges , fixed=TRUE)
modeldata$TotalCharges<-as.numeric(modeldata$TotalCharges) #NAs Introducd by Coercin in this column

#3. Capping Outliers from Numerical columns
x <- modeldata$tenure   
qnt <- quantile(x, probs=c(.25, .75), na.rm = T)
caps <- quantile(x, probs=c(.05, .95), na.rm = T)
H <- 1.5 * IQR(x, na.rm = T)
x[x < (qnt[1] - H)] <- caps[1]
x[x > (qnt[2] + H)] <- caps[2]
modeldata$tenure   <-x

x <- modeldata$MonthlyCharges   
qnt <- quantile(x, probs=c(.25, .75), na.rm = T)
caps <- quantile(x, probs=c(.05, .95), na.rm = T)
H <- 1.5 * IQR(x, na.rm = T)
x[x < (qnt[1] - H)] <- caps[1]
x[x > (qnt[2] + H)] <- caps[2]
modeldata$MonthlyCharges   <-x

x <- modeldata$TotalCharges   
qnt <- quantile(x, probs=c(.25, .75), na.rm = T)
caps <- quantile(x, probs=c(.05, .95), na.rm = T)
H <- 1.5 * IQR(x, na.rm = T)
x[x < (qnt[1] - H)] <- caps[1]
x[x > (qnt[2] + H)] <- caps[2]
modeldata$TotalCharges   <-x

#4. Replacing Nas in the Total chares column by the mean of the column
modeldata$TotalCharges[is.na(modeldata$TotalCharges)==TRUE] <- mean(modeldata$TotalCharges,na.rm=TRUE)


#FInal Data
modeldata %>% summary()

set.seed(123)
train_sample <- sample(7043, 5174)


# Inspect the classifications of our train and test sets
df_train <- modeldata[train_sample, ]
df_test <- modeldata[-train_sample, ]


# Inspect the classifications of our train and test sets
table(df_train$Churn)/5174
table(df_test$Churn)/(7000-5174)

set.seed(123)

tree_1 <- rpart(Churn~ ., 
                data = df_train, 
                control = rpart.control())

prp(tree_1,
    faclen = 0, 
    cex = 0.5, 
    extra = 1)  


conf_matrix_1 <- table(df_train$Churn,
                            predict(tree_1,
                                    type="class"))


accuracy_train <- sum(diag(conf_matrix_1))/sum(conf_matrix_1)
(1-accuracy_train)*100

# Adjusting the Minimum Split Size
tree_2 <- rpart(Churn~ ., 
                data = df_train, 
                control = rpart.control(minsplit = 10))

prp(tree_2, 
    faclen = 0, 
    cex = 0.8, 
    extra = 1)  


conf_matrix_2 <- table(df_train$Churn,
                       predict(tree_2,
                               type="class"))

accuracy_train <- sum(diag(conf_matrix_2))/sum(conf_matrix_2)
(1-accuracy_train)*100
# Adjusting the Minimum Bucket Size
tree_3 <- rpart(Churn ~ ., 
                data = df_train, 
                control = rpart.control(minbucket = 5))

prp(tree_3, 
    faclen = 0, 
    cex = 0.8, 
    extra = 1)  


conf_matrix_3 <- table(df_train$Churn,
                       predict(tree_3,
                               type="class"))
accuracy_train <- sum(diag(conf_matrix_3))/sum(conf_matrix_3)
(1-accuracy_train)*100
# Adjusting the max depth
tree_4 <- rpart(Churn ~ ., 
                data = df_train, 
                control = rpart.control(maxdepth=10))

prp(tree_4, 
    faclen = 0, 
    cex = 0.8, 
    extra = 1)  


conf_matrix_4 <- table(df_train$Churn,
                       predict(tree_4,
                               type="class"))

accuracy_train <- sum(diag(conf_matrix_4))/sum(conf_matrix_4)
(1-accuracy_train)*100
# All three confusion matrices
conf_matrix_1
conf_matrix_2
conf_matrix_3
conf_matrix_4

tree_5 <- rpart(Churn ~ ., 
                data = df_train, 
                control = rpart.control(cp=0.0001,minsplit = 7,maxdepth=10,minbucket = 1))

prp(tree_5, 
    faclen = 0, 
    cex = 0.15, 
    extra = 1)  


conf_matrix_5 <- table(df_train$Churn,
                       predict(tree_5,
                               type="class"))

accuracy_unpruned_train <- sum(diag(conf_matrix_5))/sum(conf_matrix_5)
conf_matrix_5

# Pruning the Decision Tree
printcp(tree_5)

min_xerror <- tree_5$cptable[,"xerror"] %>% 
  which.min()

bestcp <- tree_5$cptable[min_xerror,"CP"]


# Step 3: Prune the tree using the best cp.
tree_5.pruned <- prune(tree_5, cp = bestcp)


prp(tree_5.pruned, 
    faclen = 0, 
    cex = 0.8, 
    extra = 1)  

conf_matrix_6 <- table(df_train$Churn,
                       predict(tree_5.pruned,
                               type="class"))
accuracy_pruned_train <- sum(diag(conf_matrix_6))/sum(conf_matrix_6)

accuracy_pruned_train
