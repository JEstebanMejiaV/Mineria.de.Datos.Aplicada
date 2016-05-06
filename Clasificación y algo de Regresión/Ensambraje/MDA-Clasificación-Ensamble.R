'''
Minería de Datos Aplicada
Universidad Nacional de Colombia

Ejemplos Ensamble

Juan Esteban Mejía Velásquez

'''

library(C50)

library(caret)


data(churn)


churnTrain = churnTrain[,! names(churnTrain) %in% c("state","area_code", "account_length") ]

set.seed(123)

ind = sample(2, nrow(churnTrain), replace = TRUE, prob=c(0.7,0.3))
trainset = churnTrain[ind == 1,]
testset = churnTrain[ind == 2,]


install.packages("adabag")
library(adabag)


set.seed(123)

churn.bagging = bagging(churn ~ ., data=trainset, mfinal=10)

churn.bagging$importance

churn.predbagging= predict.bagging(churn.bagging, newdata=testset)

churn.predbagging$confusion


churn.baggingcv = bagging.cv(churn ~ ., v=10, data=trainset,mfinal=10)

churn.baggingcv$confusion

1-churn.baggingcv$error

## Boosting

set.seed(123)

churn.boost = boosting(churn ~.,data=trainset,mfinal=10,coeflearn="Freund", boos=FALSE
                       , control=rpart.control(maxdepth=3))

churn.boost.pred = predict.boosting(churn.boost,newdata=testset)

churn.boost.pred$confusion

churn.boostcv = boosting.cv(churn ~ ., v=10, data=trainset,
                            mfinal=5,control=rpart.control(cp=0.01))

churn.boostcv$confusion


