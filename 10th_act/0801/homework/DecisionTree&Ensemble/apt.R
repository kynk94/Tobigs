rm(list=ls())
setwd("~/Tobigs/homework_0801/DecisionTree&Ensemble")

library(caret)
library(mice)
library(gbm)
library(hydroGOF)

apt_train <- read.csv('apt_train.csv')
apt_test <- read.csv('apt_test.csv')

#test셋의 price 예측하기
str(apt_train) #factor = asile_type, earthquake, heat_source, heat_type
str(apt_test)

is.na(apt_train)

colSums(is.na(apt_train))
#28663*0.2=5732.6
#결측값 5733개 이상인 변수 모두 제거, ID제거
#building_coverage_ratio, commute_dmc, commute_seongsu, commute_yongsan, commute_chungmuro,
#floor_area_ratio, floor_min, parking_inside, parking_outside, permission_date, slope
apt_train = subset(apt_train,select=-c(building_coverage_ratio,commute_dmc,commute_seongsu,commute_yongsan,commute_chungmuro,floor_area_ratio,floor_min,parking_inside,parking_outside,permission_date,slope))
apt_train = apt_train[-1]

colSums(is.na(apt_test))
#2973*0.2=594.6
#결측값 595개 이상인 변수 모두 제거
#train과 동일
apt_test = subset(apt_test,select=-c(building_coverage_ratio,commute_dmc,commute_seongsu,commute_yongsan,commute_chungmuro,floor_area_ratio,floor_min,parking_inside,parking_outside,permission_date,slope))

#mc 이거 2시간 걸림. 결측치 채우고 나서 나중에 불러올 수 있게 csv로 저장함.
#mc <- mice(apt_train[,!names(apt_train) %in% 'price'], method='rf')
#miceOutput <- complete(mc)
#apt_train2 <- cbind(miceOutput,price=apt_train$price)
#write.csv(apt_train2,'apt_train2.csv',row.names = F)
#mc <- mice(apt_test, method='rf') #price가 없으니까 그냥 함
#apt_test2 <- complete(mc)
#write.csv(apt_test2,'apt_test2.csv',row.names = F)
apt_train2 <- read.csv('apt_train2.csv')
apt_test2 <- read.csv('apt_test2.csv')

colSums(is.na(apt_train2))
colSums(is.na(apt_test2))
str(apt_train2)
str(apt_test2)

asile_type_dm = dummyVars('~ asile_type', apt_train2)
asile_type_dm = data.frame(predict(asile_type_dm, apt_train2))
earthquake_dm = dummyVars('~ earthquake', apt_train2)
earthquake_dm = data.frame(predict(earthquake_dm, apt_train2))
heat_source_dm = dummyVars('~ heat_source', apt_train2)
heat_source_dm = data.frame(predict(heat_source_dm, apt_train2))
heat_type_dm = dummyVars('~ heat_type', apt_train2)
heat_type_dm = data.frame(predict(heat_type_dm, apt_train2))
apt_train3 <- apt_train2[!colnames(apt_train2) %in% c('asile_type','earthquake','heat_source','heat_type')]
apt_train3 <- cbind(apt_train3,asile_type_dm,earthquake_dm,heat_source_dm,heat_type_dm)
str(apt_train3)

asile_type_dm = dummyVars('~ asile_type', apt_test2)
asile_type_dm = data.frame(predict(asile_type_dm, apt_test2))
earthquake_dm = dummyVars('~ earthquake', apt_test2)
earthquake_dm = data.frame(predict(earthquake_dm, apt_test2))
heat_source_dm = dummyVars('~ heat_source', apt_test2)
heat_source_dm = data.frame(predict(heat_source_dm, apt_test2))
heat_type_dm = dummyVars('~ heat_type', apt_test2)
heat_type_dm = data.frame(predict(heat_type_dm, apt_test2))
apt_test3 <- apt_test2[!colnames(apt_test2) %in% c('asile_type','earthquake','heat_source','heat_type')]
heat_source.OIL = data.frame(heat_source.OIL=0)
apt_test3 <- cbind(apt_test3,asile_type_dm,earthquake_dm,heat_source_dm,heat_source.OIL,heat_type_dm)
str(apt_test3)

#train을 train과 test로 나눈다.
idx = createDataPartition(apt_train3$price, p = 0.7, list=F)
train = apt_train3[idx,]
test = apt_train3[-idx,]
str(train)

#변수가 많기 때문에 모델에서 Random 탐색은 시간이 너무 많이 걸림. 때문에 격자 탐색만 사용 
#Random Forest
rf.grid = expand.grid(
  .mtry = c(1,3,5)
)
control = trainControl(method='cv', search='grid', number=5,verbose = TRUE)
rf.model <- train(
  price ~ .,
  data = train,
  tuneGrid = rf.grid,
  trControl = control,
  method = 'rf'
)

#factor가 아니기 때문에 confusionMatrix는 못한다. 다른 평가 방법은 뭐가 있지?
rf.model
pred.rf <- predict(rf.model,subset(test,select=-c(price)))

#xgboost
xgb.grid = expand.grid(
  nrounds = c(300,500),
  eta = c(0.03,0.05),
  gamma = c(3,5),
  max_depth = c(4,6),
  min_child_weight = c(6,8),
  colsample_bytree = c(0.3,0.5),
  subsample = c(0.2,0.6)
)

control = trainControl(method='cv', search='grid', number=5,verbose = TRUE)
xgb.model <- train(
  price ~ .,
  data = train,
  tuneGrid = xgb.grid,
  trControl = control,
  method = 'xgbTree'
)

xgb.model
pred.xgb <- predict(xgb.model,subset(test,select=-c(price)))

#gbm
gbm.grid = expand.grid(
  shrinkage = c(0.1,0.3),
  interaction.depth = c(3,6,9),
  n.minobsinnode = c(5,10,15),
  n.trees = c(500,100,1500)
)

control = trainControl(method='cv', search='grid', number=5,verbose = TRUE)
gbm.model <- train(
  price ~ .,
  data = train,
  tuneGrid = gbm.grid,
  trControl = control,
  method = 'gbm'
)

gbm.model
pred.gbm <- predict(gbm.model,subset(test,select=-c(price)))

pred.rf
pred.xgb
pred.gbm
test$price
msc.rf <- mse(pred.rf,test$price)
msc.xgb <- mse(pred.xgb,test$price)
msc.gbm <- mse(pred.gbm,test$price)
#gbm 모델이 가장 값이 작음 -> gbm 모델이 best
msc.rf  #79396862
msc.xgb #69812252
msc.gbm #43215520

#Random Forest
rf.grid = expand.grid(
  .mtry = c(1,3,5)
)
control = trainControl(method='cv', search='grid', number=5,verbose = TRUE)
rf.model2 <- train(
  price ~ .,
  data = apt_train3,
  tuneGrid = rf.grid,
  trControl = control,
  method = 'rf'
)

#factor가 아니기 때문에 confusionMatrix는 못한다. 다른 평가 방법은 뭐가 있지?
rf.model2
pred.rf2 <- predict(rf.model2,apt_test3)

#xgboost
xgb.grid = expand.grid(
  nrounds = c(300,500),
  eta = c(0.03,0.05),
  gamma = c(3,5),
  max_depth = c(4,6),
  min_child_weight = c(6,8),
  colsample_bytree = c(0.3,0.5),
  subsample = c(0.2,0.6)
)

control = trainControl(method='cv', search='grid', number=5,verbose = TRUE)
xgb.model2 <- train(
  price ~ .,
  data = apt_train3,
  tuneGrid = xgb.grid,
  trControl = control,
  method = 'xgbTree'
)

xgb.model2
pred.xgb2 <- predict(xgb.model2,apt_test3)

#gbm
gbm.grid = expand.grid(
  shrinkage = c(0.1,0.3),
  interaction.depth = c(3,6,9),
  n.minobsinnode = c(5,10,15),
  n.trees = c(500,100,1500)
)

control = trainControl(method='cv', search='grid', number=5,verbose = TRUE)
gbm.model2 <- train(
  price ~ .,
  data = apt_train3,
  tuneGrid = gbm.grid,
  trControl = control,
  method = 'gbm'
)

gbm.model2
pred.gbm2 <- predict(gbm.model2,apt_test3)

#3가지 모델의 결과값을 averaging하여 새로운 변수 mean.price로 데이터셋 생성
pred.rf2
pred.xgb2
pred.gbm2
pred.price=cbind(pred.rf2,pred.xgb2,pred.gbm2)
write.csv(pred.price,'pred.price.csv',row.names = F)

pred.price=read.csv('pred.price.csv')
mean.price=rowMeans(pred.price)
apt_test4=cbind(apt_test3,mean.price)
str(apt_test4)

#앞서 apt_train을 train,test셋으로 나누어 모델을 평가했을 때, gbm모델이 가장 성능이 좋았다.
#때문에 gbm모델로 train한다.
gbm.grid = expand.grid(
  shrinkage = c(0.1,0.3),
  interaction.depth = c(3,6,9),
  n.minobsinnode = c(5,10,15),
  n.trees = c(500,100,1500)
)

control = trainControl(method='cv', search='grid', number=5,verbose = TRUE)
gbm.model3 <- train(
  mean.price ~ .,
  data = apt_test4,
  tuneGrid = gbm.grid,
  trControl = control,
  method = 'gbm'
)

gbm.model3
pred.gbm3 <- predict(gbm.model3,subset(apt_test4,select=-c(mean.price)))
apt_test <- read.csv('apt_test.csv')
apt_fin = apt_test
apt_fin$price = pred.gbm3
str(apt_fin)
write.csv(apt_fin,'apt_fin.csv',row.names = F)

#stacking 함수 만들기. 전처리 된 데이터를 넣으면 예측 도출.
stacking = function(data_train,data_test,target){
  library(caret)
  library(gbm)
  library(hydroGOF)
  rf.grid = expand.grid(
    .mtry = c(1,3,5)
  )
  xgb.grid = expand.grid(
    nrounds = c(300,500),
    eta = c(0.03,0.05),
    gamma = c(3,5),
    max_depth = c(4,6),
    min_child_weight = c(6,8),
    colsample_bytree = c(0.3,0.5),
    subsample = c(0.2,0.6)
  )
  gbm.grid = expand.grid(
    shrinkage = c(0.1,0.3),
    interaction.depth = c(3,6,9),
    n.minobsinnode = c(5,10,15),
    n.trees = c(500,100,1500)
  )
  idx = createDataPartition(subset(data_train,select=c(target)), p = 0.7, list=F)
  train = data_train[idx,]
  test = data_train[-idx,]
  
  control = trainControl(method='cv', search='grid', number=5,verbose = TRUE)
  rf.model <- train(
    target ~ .,
    data = train,
    tuneGrid = rf.grid,
    trControl = control,
    method = 'rf'
  )
  xgb.model <- train(
    target ~ .,
    data = train,
    tuneGrid = xgb.grid,
    trControl = control,
    method = 'xgbTree'
  )
  gbm.model <- train(
    target ~ .,
    data = train,
    tuneGrid = gbm.grid,
    trControl = control,
    method = 'gbm'
  )
  pred.rf <- predict(rf.model,subset(test,select=-c(target)))
  pred.xgb <- predict(xgb.model,subset(test,select=-c(target)))
  pred.gbm <- predict(gbm.model,subset(test,select=-c(target)))
  msc.rf <- mse(pred.rf,subset(test,select=c(target)))
  msc.xgb <- mse(pred.xgb,subset(test,select=c(target)))
  msc.gbm <- mse(pred.gbm,subset(test,select=c(target)))
  rf.model2 <- train(
    target ~ .,
    data = data_train,
    tuneGrid = rf.grid,
    trControl = control,
    method = 'rf'
  )
  xgb.model2 <- train(
    target ~ .,
    data = data_train,
    tuneGrid = xgb.grid,
    trControl = control,
    method = 'xgbTree'
  )
  gbm.model2 <- train(
    target ~ .,
    data = data_train,
    tuneGrid = gbm.grid,
    trControl = control,
    method = 'gbm'
  )
  pred.rf2 <- predict(rf.model2,data_test)
  pred.xgb2 <- predict(xgb.model2,data_test)
  pred.gbm2 <- predict(gbm.model2,data_test)
  pred.target=cbind(pred.rf2,pred.xgb2,pred.gbm2)
  mean.target=rowMeans(pred.target)
  data_test2=cbind(data_test,mean.target)
  
  if(min(msc.rf,msc.xgb,msc.gbm)==msc.rf){
    rf.model3 <- train(
      mean.target ~ .,
      data = data_test2,
      tuneGrid = rf.grid,
      trControl = control,
      method = 'rf'
    )
    pred = predict(rf.model3,subset(data_test2,select=-c(mean.target)))
  }
  else if(min(msc.rf,msc.xgb,msc.gbm)==msc.xgb){
    xgb.model3 <- train(
      mean.target ~ .,
      data = data_test2,
      tuneGrid = xgb.grid,
      trControl = control,
      method = 'xgbTree'
    )
    pred = predict(xgb.model3,subset(data_test2,select=-c(mean.target)))
  }
  else if(min(msc.rf,msc.xgb,msc.gbm)==msc.gbm){
    gbm.model3 <- train(
      mean.target ~ .,
      data = data_test2,
      tuneGrid = gbm.grid,
      trControl = control,
      method = 'gbm'
    )
    pred = predict(gbm.model3,subset(data_test2,select=-c(mean.target)))
  }
  return(pred)
}
#stacking 결과로 predict한 열을 도출한다.
#pred = stacking(data_train = apt_train3,
#                data_test = apt_test3,
#                target = price)