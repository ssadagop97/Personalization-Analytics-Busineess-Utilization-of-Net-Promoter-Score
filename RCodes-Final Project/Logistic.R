# Reading in and exmploring the data
aprioridata <- read.csv(file.choose())
summary(aprioridata)
##Preprocessing
#1. Removing ID columns 
modeldata<- aprioridata[-c(1)]

#There are no NAs in the Data
#2.Converting Categorical columns into factors
modeldata$age<-as.factor(modeldata$age)
modeldata$income<-as.factor(modeldata$income)
modeldata$Children<-as.factor(modeldata$Children)

#3. Capping Outliers from Numerical columns(Income and Age)
x <- modeldata$age   
qnt <- quantile(x, probs=c(.25, .75), na.rm = T)
caps <- quantile(x, probs=c(.05, .95), na.rm = T)
H <- 1.5 * IQR(x, na.rm = T)
x[x < (qnt[1] - H)] <- caps[1]
x[x > (qnt[2] + H)] <- caps[2]
modeldata$age   <-x

x <- modeldata$income   
qnt <- quantile(x, probs=c(.25, .75), na.rm = T)
caps <- quantile(x, probs=c(.05, .95), na.rm = T)
H <- 1.5 * IQR(x, na.rm = T)
x[x < (qnt[1] - H)] <- caps[1]
x[x > (qnt[2] + H)] <- caps[2]
modeldata$income   <-x

x <- modeldata$Children   
qnt <- quantile(x, probs=c(.25, .75), na.rm = T)
caps <- quantile(x, probs=c(.05, .95), na.rm = T)
H <- 1.5 * IQR(x, na.rm = T)
x[x < (qnt[1] - H)] <- caps[1]
x[x > (qnt[2] + H)] <- caps[2]
modeldata$Children   <-x

#4. Replacing Nas in the Total chares column by the mean of the column
modeldata$TotalCharges[is.na(modeldata$TotalCharges)==TRUE] <- mean(modeldata$TotalCharges,na.rm=TRUE)


#5. bUCKETING nUMERICAL cOLUMNS ACCORDING TO THEIR DISTRIBUTION
quantile(modeldata$tenure,seq(0, 1, 0.10))
modeldata$tenure<-ifelse(modeldata$tenure<=6,"<6 months",
                         ifelse(modeldata$tenure<=20,"6-20 months",
                                ifelse(modeldata$tenure<=50, "20-50 months",">50 months")))
table(modeldata$tenure)
quantile(modeldata$MonthlyCharges,seq(0, 1, 0.10))
modeldata$MonthlyCharges<-ifelse(modeldata$MonthlyCharges<=23,"<23",
                                 ifelse(modeldata$MonthlyCharges<=58,"23-58",
                                        ifelse(modeldata$MonthlyCharges<=80, "58-80 months","80 months")))
table(modeldata$MonthlyCharges)

quantile(modeldata$TotalCharges,seq(0, 1, 0.10))
modeldata$TotalCharges<-ifelse(modeldata$TotalCharges<=250,"<250",
                               ifelse(modeldata$TotalCharges<=930,"250 to 930",
                                      ifelse(modeldata$TotalCharges<=2500, "930 to 2500",
                                             ifelse(modeldata$TotalCharges<=4400, "2500 to 4400",">4000"))))
table(modeldata$TotalCharges)

modeldata$tenure<-as.factor(modeldata$tenure)
modeldata$MonthlyCharges<-as.factor(modeldata$MonthlyCharges)
modeldata$TotalCharges<-as.factor(modeldata$TotalCharges)
#FInal Data
modeldata %>% summary()
