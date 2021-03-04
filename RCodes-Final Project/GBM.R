# Cluster
df_new<-read.csv("C:\\Users\\Krishna\\Desktop\\data.csv")
df_new<-df_new[,-1]
install.packages("gbm")
library(gbm)
library(caTools)

set.seed(88)
split <- sample.split(df_new$Likelihood.to.recommend, SplitRatio = 0.75)
train <- subset(df_new, split == TRUE)
test <- subset(df_new, split == FALSE)
set.seed(123)
# for reproducibility
set.seed(123)

# train GBM model
gbm.fit2 <- gbm(
  formula = Likelihood.to.recommend ~ .,
  distribution = "gaussian",
  data = train,
  n.trees = 5000,
  interaction.depth = 3,
  shrinkage = 0.1,
  cv.folds = 5,
  n.cores = NULL, # will use all cores by default
  verbose = FALSE
)  
print(gbm.fit2)
# find index for n trees with minimum CV error
min_MSE <- which.min(gbm.fit2$cv.error)

# get MSE and compute RMSE
sqrt(gbm.fit2$cv.error[min_MSE])
## [1] 23112.1

# plot loss function as a result of n trees added to the ensemble
gbm.perf(gbm.fit2, method = "cv")

par(mar = c(5, 8, 1, 1))
summary(
  gbm.fit2, 
  cBars = 10,
  method = relative.influence, # also can use permutation.test.gbm
  las = 2
)