rm(list=ls())

# 유방암 진단 결과 데이터 셋
setwd("~/Tobigs/0725/KNN LDA")
### packages
if(!require(class)) install.packages("class"); library(class) # for knn
if(!require(kknn)) install.packages("kknn"); library(kknn) # for wknn
if(!require(caret)) install.packages("caret"); library(caret)
if(!require(e1071)) install.packages("e1071"); library(e1071)
if(!require(dplyr)) install.packages("dplyr"); library(dplyr)

### load data
# stringAsFactors = F: read.csv() 함수는 읽어올 때 모든 chr를 factor로 변환하는데
# 이를 자동으로 바꾸지 않고 chr 그대로 읽어오는 것
wdbc <- read.csv('data/wisc_bc_data.csv', stringsAsFactors = F)
str(wdbc) # 자료구조 확인
dim(wdbc) # 569 x 32
# radius : 반지름 / texture : 텍스처 / perimeter : 둘레 / area : 면적 /
# smmothness : 평활도 compactness : 다짐도 / concavity : 요면 /
# concave points : 요면점 / symmetry : 대칭 / fractal dimension : 프렉탈 차원

# 불필요한 id 변수 제거
wdbc <- wdbc[-1]
str(wdbc)

# 종속변수(y) 분포 확인
table(wdbc$diagnosis)
prop.table(table(wdbc$diagnosis)) #B,M의 비율
# B: 양성 / M : 악성
# 악성인 것을 잘 맞춰야한다

# 종속변수(y)인 diagnosis 변수 factor형으로 변환
wdbc$diagnosis <- factor(wdbc$diagnosis, level=c("B","M"))

# set.seed(1) : 난수 고정
set.seed(1)
# createDataPartition
# caret패키지 안에 있는 데이터 파티션, 층화추출
idx <- createDataPartition(y = wdbc$diagnosis, p = 0.7, list =FALSE)
wdbc_train <- wdbc[idx,]
wdbc_test <- wdbc[-idx,]

### 기본 knn (majority voting, 다수결) ###
# 임의의 홀수 k 를 넣어보자
# y를 지우고 넣은것들임, diagnosis 가 y값
wdbc_train %>% str
model.knn <- knn(wdbc_train[-1],wdbc_test[-1], wdbc_train$diagnosis, k=5)
confusionMatrix(model.knn, wdbc_test$diagnosis) # Specificity : 0.9048
# 실제로 악성인 것 중에 악성이라고 예측 (Specificity)

### weighted knn (가중치) ###
# distance, kernel(가중치)
# 포뮬러, 트레인, 테스트, scale=T는 디폴트 정규화 해주는 것
# scale = T하면 표준화
model.wknn <- kknn(diagnosis ~., wdbc_train, wdbc_test, k=5, scale = F)
# 속할 확률
model.wknn$prob
# 최근접 k개와의 거리
model.wknn$D
# wknn으로 예측한 값
wknn_pred <- model.wknn$fitted.values
confusionMatrix(wknn_pred, wdbc_test$diagnosis) # Specificity : 0.9524

### Feature scaling ###
summary(wdbc)
# wdbc의 데이터는 데이터의 범주가 제각각
# 변수들의 영향도를 더 정확히 보기 위해서 변수 표준화 혹은 정규화를 한다.

## min-max 스케일링
normalize <- function(x){
  return( (x-min(x))/(max(x)-min(x)) )
}

colnames(wdbc)
wdbc_normal <- as.data.frame(lapply(wdbc[-1], normalize))
summary(wdbc_normal) #  0~1 사이의 값으로 바뀜

wdbc_normal$diagnosis <- wdbc$diagnosis

wdbc_train_n <- wdbc_normal[idx,]
wdbc_test_n <- wdbc_normal[-idx,]

### 기본 knn (majority voting) ###
# 임의의 홀수 k 를 넣어보자
wdbc_pred_n <- knn(wdbc_train_n[-31],wdbc_test_n[-31], wdbc_train_n$diagnosis, k=5)
confusionMatrix(wdbc_pred_n, wdbc_test$diagnosis) # Specificity : 0.9524

### weighted knn (가중치) ###
# distance, kernel(가중치)
wknn.model <- kknn(diagnosis ~., wdbc_train_n, wdbc_test_n, k=5, scale = F)
wdbc_pred_n2 <- wknn.model$fitted.values
confusionMatrix(wdbc_pred_n2, wdbc_test$diagnosis) # Specificity : 0.9524

### 최적의 K를 구하는 방법 ###
# Cross-Validation
# ks = 실험할 k, 일부러 홀수로 지정하였다.
wdbc.cv <- train.kknn(diagnosis ~., wdbc_train_n, 
                      ks = seq(1, 50, by=2), scale = T);wdbc.cv
best_k <- wdbc.cv$best.parameters$k;best_k

wdbc_pred_cv <- kknn(diagnosis ~., train = wdbc_train_n, test = wdbc_test_n, k = best_k, scale = F)
wdbc_pred_cv <- wdbc_pred_cv$fitted.values

confusionMatrix(wdbc_pred_cv, wdbc_test$diagnosis) # Specificity : 0.9524

##########################
#### caret 패키지로 돌려보기!
### 사용 가능한 모델
## https://topepo.github.io/caret/available-models.html

######### cross validation
cv <- trainControl(method = "cv", number = 5, verbose = T)
repCv <- trainControl(method = "repeatedcv", number = 5,repeats = 3, verbose = T)

######### knn
#### tune parameter
knn.grid = expand.grid(
  .k = c(1,3,5,7,9)
)
train.knn <- train(diagnosis~.,wdbc_train_n, method = "knn",trControl = cv,
                   tuneGrid = knn.grid)
train.knn$results
train.knn$bestTune
predict.knn <- predict(train.lda,wdbc_test_n)
confusionMatrix(predict.knn, wdbc_test$diagnosis)

######### wknn
#### tune parameter
wknn.grid = expand.grid(
  .kmax = c(1,3,5),
  .distance = c(1,2),
  .kernel = "optimal"
)
train.wknn <- train(diagnosis~.,wdbc_train_n, method = "kknn", trControl = cv,
                    tuneGrid = wknn.grid)
predict.wknn <- predict(train.wknn,wdbc_test_n)
confusionMatrix(predict.wknn, wdbc_test$diagnosis)

######### lda
train.lda <- train(diagnosis~.,wdbc_train_n, method = "lda", trControl = repCv)
predict.lda <- predict(train.lda,wdbc_test_n)
confusionMatrix(predict.lda, wdbc_test$diagnosis)

######### logistic
train.glm <- train(diagnosis~.,wdbc_train_n, method = "glm", trControl =cv)
predict.glm <- predict(train.glm,wdbc_test_n)
confusionMatrix(predict.glm, wdbc_test$diagnosis)
