library(foreign)
library(corpcor)
library(tseries)
library(quantmod)
library(plyr)
library(dplyr)
library(DMwR)
library(Hmisc)
library(ROCR)
#library(CausalImpact)
#library(caret)
#library(e1071)
#library(keras)
#library(tensorflow)
#library(devtools)

library(randomForest)

data<-read.xport("/Users/chenkunyu/Desktop/数据分析/第二次/DEMO_I.XPT")
data_food2<-read.xport("/Users/chenkunyu/Desktop/数据分析/第二次/DR2IFF_I.XPT")
data_food3<-read.xport("/Users/chenkunyu/Desktop/数据分析/第二次/DR1TOT_I.XPT")
data_food4<-read.xport("/Users/chenkunyu/Desktop/数据分析/第二次/DR2TOT_I.XPT")
data_food1<-read.xport("/Users/chenkunyu/Desktop/数据分析/第二次/DR2IFF_I.XPT")


data_all = left_join(data, data_food2, by = "SEQN")

data_all = left_join(data, data_food1, by = "SEQN")
rm(data_food1)
gc()
data_all = left_join(data, data_food3, by = "SEQN")
rm(data_food3)
gc()
data_all = left_join(data, data_food4, by = "SEQN")
rm(data_food4)
gc()

data_all <- apply(data_all,2, function(data_all){
  if(sum(is.na(data_all))>0){
    data_all[is.na(data_all)]<- quantile(data_all,na.rm = T, probs = 0.5)
  }
  data_all
})

mental <- read.xport("/Users/chenkunyu/Desktop/数据分析/第二次/DPQ_I.XPT")

mental <- apply(mental,2, function(mental){
  if(sum(is.na(mental))>0){
    mental[is.na(mental)]<- quantile(mental,na.rm = T, probs = 0.5)
  }
  mental
})


temp = vector(mode="numeric", length = nrow(mental))
mental = cbind(mental,temp)
for (i in 1:nrow(mental)){
  if ((mental[i,2]>1)|(mental[i,3]>1)|(mental[i,4]>1)|(mental[i,5]>1)|(mental[i,6]>1)|(mental[i,7]>1)|(mental[i,8]>1)|(mental[i,9]>1)|(mental[i,10]>1))
    mental[i,12]<-1
  else mental[i,12]<-0
}

mental = mental[,c(1,12)]

final <- merge(mental,data_all)
final2=final[-2]

set.seed(243523)
hm <- nrow(final)
data_pick <- sample(hm,0.7*hm)

training <- final[data_pick,]
testing <- final[-data_pick,]


training_dataframe = training[0:1721,]
index <- as.numeric(row.names(training_dataframe))
training_dataframe = training_dataframe[order(index), ]

training_new = data.matrix(training_dataframe)
testing_new = data.matrix(testing)

training_temp1 = training_new[,2:132]
training_temp2 = training_new[,1]
testing_temp1 = testing_new[,2:132]
testing_temp2 = as.matrix(testing_temp1[,1])
# length(training_temp1)
# length(training_temp2)
mymodel <- randomForest(training_temp1,training_temp2,ntree=500,importance = TRUE,proximity = TRUE)

print(mymodel)

mypredict = predict(mymodel, testing_temp1)
prediction1 = prediction(mypredict, testing_temp2)
auc = performance(prediction1, 'auc')
auc@y.values

plot = varImpPlot(mymodel, sort=TRUE, n.var = 40)




Call:
  randomForest(x = training_temp1, y = training_temp2, ntree = 500,      importance = TRUE, proximity = TRUE) 
Type of random forest: regression
Number of trees: 500
No. of variables tried at each split: 43

Mean of squared residuals: 8616500
% Var explained: -3.33


> auc@y.values
[[1]]
[1] 0.4916033


