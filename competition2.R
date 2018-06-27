library(data.table)
library(dplyr)
library(ggplot2)
library(Matrix)
library(xgboost)
library(rpart)
library(randomForest)
library(ROSE)
library(car)
library(caret)
library(lubridate)
train <- read.csv("C:/Users/Arsenyerbol/Desktop/train.csv")
test <- read.csv("C:/Users/Arsenyerbol/Desktop/test.csv")
library(missForest)
fulltrain.mis <- missForest(train,maxiter = 5,ntree = 250)
fulltest.mis <- missForest(test,maxiter = 5,ntree = 250)


class(fulltrain.mis)
fulltrainnew <- as.data.frame(fulltrain.mis$ximp)
class(fulltrainnew)
fulltestnew <- as.data.frame(fulltest.mis$ximp)
fulltrainnew <- head(fulltrainnew, -1) 


library(randomForest)
set.seed(123)
rf <- randomForest(SalePrice~.,data=fulltrainnew,type="regression",ntree=500,do.trace=TRUE) 
library(caret)
p1 <- predict(rf,fulltrainnew)
p1  
  
write.csv(data.frame(Id=test$Id,SalePrice=predict),"file.csv",row.names = F)


#XGBOOST
library(xgboost)
library(Select)
train_matrix <- data.matrix(select(fulltrainnew,-SalePrice))

test_matrix <- data.matrix(select(fulltestnew,-SalePrice))
fulltestnew$SalePrice <- 0
train_target <- fulltrainnew$SalePrice
test_target <- fulltestnew$SalePrice
dtrain <- xgb.DMatrix(data=train_matrix,label=train_target)
ctest <- xgb.DMatrix(data=test_matrix,label=test_target)

watchlist <- list(train=dtrain,test=ctest)
bst <- xgb.train(data=dtrain,
                 nround=500,
                 #maximize = FALSE,
                 #early_stopping_rounds = 10,
                 #watchlist = watchlist,
                 max_depth=7,
                 objective = "reg:linear",
                 eval_metric = "rmse",
                 alpha=0.01,
                 lambda=0.01,
                 colsample_bytree=0.7,
                 subsample = 0.7
)
predict <- predict(bst,ctest)
RMSE(test_target,predict)

cv <- xgb.cv(data=dtrain,
             nround=500,
             #maximize = FALSE,
             #early_stopping_rounds = 10,
             watchlist = watchlist,
             nfold=6,
             max_depth=7,
             objective = "reg:linear",
             eval_metric = "rmse",
             
             alpha=0.01,
             lambda=0.01,
             colsample_bytree=0.7,
             subsample = 0.7
)

#lm

fit <- lm(SalePrice~.,sample_n(fulltrainnew,1459))
shapiro.test(fit$residuals)
log_method <- function(x){
  ifelse(abs(x)>0,log(abs(x)),0)
}
str(fulltrainnew)

cols_num <- sapply(fulltrainnew, function(x){is.numeric(x)})

train_num_log <- as.data.frame(sapply(fulltrainnew[,cols_num],log_method))
test_num_log <- as.data.frame(sapply(fulltestnew[,cols_num],log_method))
str(train_num_log)
fit_log <- lm(SalePrice~.,sample_n(train_num_log,1459))
shapiro.test(exp(fit_log$residuals))
summary(fit)
summary(fit_log)



check_log <- lm((fit_log$residuals^2)~.,sample_n(train_num_log,1459))
summary(check_log)


pred_log <- exp(predict(fit_log,test_num_log))


RMSE(exp(test_num_log$SalePrice),pred_log)


write.csv(data.frame(Id=test$Id,SalePrice=train_num_log$SalePrice),"newfile.csv",row.names = F)

