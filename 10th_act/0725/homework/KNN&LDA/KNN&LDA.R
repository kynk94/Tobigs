######### 분석과제!
rm(list=ls())
### setwd
setwd("~/Tobigs/homework_0725/KNN&LDA")

### packages
if(!require(class)) install.packages("class"); library(class)
if(!require(kknn)) install.packages("kknn"); library(kknn)
if(!require(caret)) install.packages("caret"); library(caret)
if(!require(e1071)) install.packages("e1071"); library(e1071)
if(!require(dplyr)) install.packages("dplyr"); library(dplyr)
if(!require(data.table)) install.packages("data.table"); library(data.table)

#### load data
pro.train <- fread('profiles_train.csv')
pro.train <- as.data.frame(pro.train)
click.train <- fread('click_train.csv')
click.train <- as.data.frame(click.train)
pro.test <- fread('profiles_test.csv')
pro.test <- as.data.frame(pro.test)
click.test <- fread('click_test.csv')
click.test <- as.data.frame(click.test)
pro.train %>% head
str(pro.train)

str(click.train)
#### data preproc
### dplyr package : https://wsyang.com/2014/02/introduction-to-dplyr/
### 변수추가 1. DT
#click.train에서 id별로 집계하겠다. 체류시간을 더하겠다는 것. 해당과정을 DT에 대해서 하는 것이 과제. pro.test에는 성별이 없음. 성별 예측할 것.
a <-click.train %>% group_by(id) %>% summarise(DT = sum(st_t)) # or aggregate
pro.train <- inner_join(pro.train,a) # or merge
pro.train %>% head
a <-click.test %>% group_by(id) %>% summarise(DT = sum(st_t))
pro.test <- inner_join(pro.test,a)
pro.test %>% head
### 변수추가 2. ~
a <-click.train %>% group_by(id) %>% summarise(PV = sum(st_c))
pro.train <- inner_join(pro.train,a)
pro.train %>% head
a <-click.test %>% group_by(id) %>% summarise(PV = sum(st_c))
pro.test <- inner_join(pro.test,a)
pro.test %>% head
### 변수추가 3. ~
a <-click.train %>% group_by(id) %>% summarise(COV = length(unique(cate)))
pro.train <- inner_join(pro.train,a)
pro.train %>% head
a <-click.test %>% group_by(id) %>% summarise(COV = length(unique(cate)))
pro.test <- inner_join(pro.test,a)
pro.test %>% head
### 변수추가 4. ~
click.train$time <- substr(click.train$time,1,8)
a <-click.train %>% group_by(id) %>% summarise(Day = length(unique(time)))
pro.train <- inner_join(pro.train,a)
pro.train %>% head
click.test$time <- substr(click.test$time,1,8)
a <-click.test %>% group_by(id) %>% summarise(Day = length(unique(time)))
pro.test <- inner_join(pro.test,a)
pro.test %>% head

#### data partition
pro.train = pro.train[,-1]
pro.test = pro.test[,-1]
pro.train %>% head
pro.test %>% head
table(pro.train$gen)
pro.train$gen = as.factor(pro.train$gen)
#unique(pro.train$job)
#unique(pro.test$job)
#formula_job = 
#unique(formula_job)
#pro.test$job = as.factor(pro.test$job)
#pro.test$resid = as.factor(pro.test$resid)

#min-max 스케일링
summary(pro.train)
normalize <- function(x){
  return((x-min(x))/(max(x)-min(x)))
}
colnames(pro.train)
pro.train_normal <- as.data.frame(lapply(pro.train[-1:-3],normalize))
#pro.train_normal$job <- pro.train$job
#pro.train_normal$resid <- pro.train$resid
pro.train_normal$gen <- pro.train$gen
colnames(pro.train_normal)
colnames(pro.test)
pro.test_normal <- as.data.frame(lapply(pro.test[-1:-2],normalize))
#pro.test_normal$job <- pro.test$job
#pro.test_normal$resid <- pro.test$resid
colnames(pro.test_normal)
head(pro.train_normal)
head(pro.test_normal)

#### modeling
#options(error = utils::recover)
#pro.train_knn <- knn(pro.train_normal[-5],pro.test_normal,pro.train_normal$gen,k=10)
cv <- trainControl(method = "cv", number = 5, verbose = T)
repCv <- trainControl(method = "repeatedcv", number = 5,repeats = 3, verbose = T)

#최적 k
pro.train_cv <- train.kknn(gen~.,pro.train_normal,ks=seq(1,50,by=2),scale=T);pro.train_cv
best_k <- pro.train_cv$best.parameters$k;best_k
pro.test_pred_cv <- kknn(gen~.,train=pro.train_normal,test=pro.test_normal,k=best_k,scale=F)
pro.test_pred_cv <- pro.train_pred_cv$fitted.values
pro.test_pred_cv
#[1] 남자 여자 남자 남자 남자 남자 남자 남자 남자 남자 남자 남자 남자 남자 여자 남자 남자 남자 남자 남자 남자 남자
#[23] 여자 남자 여자 남자 남자 남자 남자 남자 남자 남자 남자 남자 남자 남자 남자 남자 남자 남자 남자 남자 남자 남자
#[45] 남자 남자 남자 남자 남자 남자 남자 남자 남자 남자 남자 여자 남자 남자 남자 여자 여자 여자 남자 남자 남자 남자
#[67] 남자 남자 남자 여자 남자 남자 남자 남자 남자 남자 남자 남자 남자 남자 남자 남자 남자 남자 여자 남자 남자 남자
#[89] 남자 남자 남자 남자 여자 남자 남자 여자 남자 남자 남자
#confusionMatrix(pro.test_pred_cv,pro.test_normal$gen)는 실제 값을 몰라서 확인할 수가 없다.

#knn
knn.grid = expand.grid(
  .k = seq(1,50,by=2)
)
train.knn <- train(gen~.,pro.train_normal, method = "knn",trControl = cv,
                   tuneGrid = knn.grid)
train.knn$results
train.knn$bestTune
predict.knn <- predict(train.knn,pro.test_normal)
predict.knn
# [1] 남자 남자 남자 남자 남자 남자 남자 남자 남자 남자 남자 남자 남자 남자 남자 남자 남자 남자 남자 여자 남자 남자
#[23] 여자 남자 남자 남자 남자 남자 남자 남자 남자 남자 남자 남자 남자 남자 남자 남자 남자 남자 남자 남자 남자 남자
#[45] 남자 남자 남자 남자 남자 남자 남자 남자 남자 남자 남자 남자 남자 남자 남자 남자 남자 여자 남자 남자 남자 남자
#[67] 남자 남자 남자 여자 남자 남자 남자 남자 남자 남자 남자 남자 남자 남자 남자 남자 남자 남자 남자 남자 남자 남자
#[89] 남자 남자 남자 남자 남자 남자 남자 남자 남자 남자 남자

#kknn
wknn.grid = expand.grid(
  .kmax = seq(1,50,by=2),
  .distance = c(1,2),
  .kernel = "optimal"
)
train.wknn <- train(gen~.,pro.train_normal, method = "kknn", trControl = cv,
                    tuneGrid = wknn.grid)
#1분정도 걸림, 결과 : Fitting kmax = 29, distance = 2, kernel = optimal on full training set
predict.wknn <- predict(train.wknn,pro.test_normal)
predict.wknn
# [1] 남자 여자 남자 남자 남자 여자 남자 남자 남자 남자 남자 남자 남자 남자 남자 남자 남자 남자 남자 여자 남자 남자
#[23] 여자 남자 여자 남자 남자 남자 남자 남자 남자 남자 남자 남자 남자 남자 남자 남자 남자 남자 남자 남자 남자 남자
#[45] 남자 남자 남자 남자 남자 남자 남자 남자 남자 남자 남자 여자 남자 남자 남자 남자 여자 여자 남자 남자 남자 남자
#[67] 남자 남자 남자 남자 남자 남자 남자 남자 남자 남자 남자 남자 남자 남자 남자 남자 남자 남자 남자 남자 남자 남자
#[89] 남자 남자 남자 남자 남자 남자 남자 여자 남자 남자 남자
confusionMatrix(predict.knn,predict.wknn) #Specificity : 0.3333

#lda
train.lda <- train(gen~.,pro.train_normal, method = "lda", trControl = repCv)
predict.lda <- predict(train.lda,pro.test_normal)
predict.lda
# [1] 남자 남자 남자 남자 남자 여자 남자 남자 남자 남자 남자 남자 남자 남자 남자 남자 남자 남자 남자 남자 남자 남자
#[23] 남자 남자 남자 남자 남자 남자 남자 남자 남자 남자 남자 남자 남자 남자 남자 남자 남자 남자 남자 남자 남자 남자
#[45] 남자 남자 남자 남자 여자 남자 남자 남자 남자 남자 남자 남자 남자 남자 남자 남자 남자 남자 남자 남자 남자 남자
#[67] 남자 남자 남자 남자 남자 남자 남자 남자 남자 남자 남자 남자 남자 남자 남자 남자 남자 남자 남자 남자 남자 남자
#[89] 남자 남자 남자 남자 남자 남자 남자 여자 남자 남자 남자
confusionMatrix(predict.lda,predict.knn) #Specificity : 0 여자인데 여자로 구분한 경우가 0이라서 0이 나옴
confusionMatrix(predict.lda,predict.wknn) #Specificity : 0.2222

#logistic
train.glm <- train(gen~.,pro.train_normal, method = "glm", trControl =cv)
predict.glm <- predict(train.glm,pro.test_normal)
predict.glm
# [1] 남자 남자 남자 남자 남자 여자 남자 남자 남자 남자 남자 남자 남자 남자 남자 남자 남자 남자 남자 남자 남자 남자
#[23] 남자 남자 남자 남자 남자 남자 남자 남자 남자 남자 남자 남자 남자 남자 남자 남자 남자 남자 남자 남자 남자 남자
#[45] 남자 남자 남자 남자 여자 남자 남자 남자 남자 남자 남자 남자 남자 남자 남자 남자 남자 남자 남자 남자 남자 남자
#[67] 남자 남자 남자 남자 남자 남자 남자 남자 남자 남자 남자 남자 남자 남자 남자 남자 남자 남자 남자 남자 남자 남자
#[89] 남자 남자 남자 남자 남자 남자 남자 여자 남자 남자 남자
confusionMatrix(predict.glm,predict.knn) #Specificity : 0 여자인데 여자로 구분한 경우가 0이라서 0이 나옴
confusionMatrix(predict.glm,predict.wknn) #Specificity : 0.2222
confusionMatrix(predict.glm,predict.lda) #Specificity : 1 예측이 똑같다

######### 분석과제 안에 들어있는 구현, Vote classifier
### function 인자값의 개수(a,b,c,d,...)는 마음껏 추가해도됩니다!
### 밑의 가이드라인은 마음대로 바꾸셔도 됩니다.
###  num : 앙상블할 모델의 갯수
###  a,b,c ... : method
### 1. 과제서 제시한 3개의 모델 우선!
### 2. 나아가서 일반화 및 파라매터 조정도 해봅시다!
voteClassifier <- function(num,b,c,d) {
  cv <- trainControl(method = "cv", number = 5, verbose = T)
  repCv <- trainControl(method = "repeatedcv", number = 5,repeats = 3, verbose = T)
  
  knn.grid = expand.grid(
    .k = seq(1,50,by=2)
  )
  train.knn <- train(gen~.,pro.train_normal, method = "knn",trControl = cv,
                     tuneGrid = knn.grid)
  train.knn$results
  train.knn$bestTune
  predict.knn <- predict(train.knn,pro.test_normal)
  predict.knn
  
  train.lda <- train(gen~.,pro.train_normal, method = "lda", trControl = repCv)
  predict.lda <- predict(train.lda,pro.test_normal)
  predict.lda
  
  train.glm <- train(gen~.,pro.train_normal, method = "glm", trControl =cv)
  predict.glm <- predict(train.glm,pro.test_normal)
  predict.glm
  
  knn = predict.knn
  lda = predict.lda
  glm = predict.glm
  test.set <- data.frame(knn,lda,glm)
  return(test.set)
}
predict.ensemble <- voteClassifier()
predict.ensemble
confusionMatrix(predict.ensemble, test.set)

#### 최종 결과물 제출
pred.test.knn <- predict(train.knn, pro.test_normal)
pred.test.lda <- predict(train.lda, pro.test_normal)
pred.test.glm <- predict(train.glm, pro.test_normal)
pred.test <- data.frame(pred.test.knn,pred.test.lda,pred.test.glm)
names(pred.test) = c("gen_knn","gen_lda","gen_glm")
write.csv(pred.test,'result.csv',row.names =F)
###