
df_new<-read.csv("C:\\Users\\Krishna\\Desktop\\Data Science Lab\\data.csv")

summary(df_new) #To see the structure of our model dataset

df_new$Likelihood.to.recommend[which(is.na(df_new$Likelihood.to.recommend))] = 0
df_new$Likelihood.to.recommend<-as.factor(df_new$Likelihood.to.recommend)
#df_new$Price.Sensitivity<-as.factor(df_new$Price.Sensitivity)
#df_new<-df_new[,-1]
#Splitting it into train and test to evaluate our model
install.packages('caTools')
library(caTools)

###Apriori Model

#Looking at the summary,we take the variables which are highly significant (***) to use in our apriori model.
# We choose 11 variables for the Apriori model. 
library(arules)
library(arulesViz)

df_apriori<- df_new[,c("Partner.Name","Age","Gender","Airline.Status","Price.Sensitivity",
                       "Loyalty","Type.of.Travel","Flights.Per.Year","Departure.Delay.in.Minutes","Arrival.Delay.in.Minutes"
                       ,"Flight.cancelled","Likelihood.to.recommend")]
df_apriori$Likelihood.to.recommend<-as.factor(df_apriori$Likelihood.to.recommend)
df_apriori$Price.Sensitivity<-as.factor(df_apriori$Price.Sensitivity)
#Rule set for detractors. Number of rules increase if support value decreases
ruleset_detractor <- apriori(df_apriori,
                   # Specify threshold of 0.005 for support, and 0.5 for confidence. That is, show only those values that are higher than the thresholds.
                   parameter = list(support=0.15, confidence = 0.05, minlen = 3),
                   appearance = list(default="lhs", rhs = ("Likelihood.to.recommend=0")))             

inspectDT(ruleset_detractor)

#Rule set for promoters. Number of rules increase if support value decreases
ruleset_promoter <- apriori(df_apriori,
                   # Specify threshold of 0.005 for support, and 0.5 for confidence. That is, show only those values that are higher than the thresholds.
                   parameter = list(support=0.05, confidence = 0.05, minlen = 3),
                   appearance = list(default="lhs", rhs = ("Likelihood.to.recommend=1")))             

inspectDT(ruleset_promoter)
#We choose the best association by looking at the value of lift. 
#if there are multiple associations with the same lift values, we subsequently move on to the confidence and the support values.

#We can improve the model by creating Derived variables from our current variables
##We now have to choose rules which logically explain our problem statement