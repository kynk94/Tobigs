####################################################################
rm(list=ls())
getwd()
setwd("C:/Users/sojeo/Tobigs/week1_regression")

#####단순선형회귀#####
fundata <- read.csv("fundata.csv")
str(fundata)

fundata <- fundata[,-1] #index 제거
fit1 <- lm(y1 ~ x1, fundata)
summary(fit1)
fit2 <- lm(y2 ~ x2, fundata)
fit3 <- lm(y3 ~ x3, fundata)
fit4 <- lm(y4 ~ x4, fundata)

#summary(fit2);summary(fit3);summary(fit4)

par(mfrow=c(2,2)) #plots를 2X2으로 나누겠다

plot(fundata$x1, fundata$y1) #산점도
abline(fit1) #적합된 단순선형회귀식 그려서 비교하기
plot(fundata$x2, fundata$y2)
abline(fit2)
plot(fundata$x3, fundata$y3)
abline(fit3)
plot(fundata$x4, fundata$y4)
abline(fit4)

#####다중선형회귀분석#####
data <- read.csv("data.csv")
str(data)
head(data)
names(data)<-c("Y","X1","X2","X3","X4")
head(data)
####################################################
# Y : 가솔린 연비
# X1:배기량
# X2:마력 
# X3:(엔진의)압축비
# X4:무게
#######################################################

###data 탐색 및 전처리
#결측치 확인
sum(is.na(data)) #0
#있다면 data <- na.omit(data) 행별로 제거

#구조보기
par(mfrow=c(2,3))
hist(data$Y);summary(data$Y)
hist(data$X1);summary(data$X1)
hist(data$X2);summary(data$X2)
hist(data$X3);summary(data$X3)
hist(data$X4);summary(data$X4)
plot(data)

### 회귀 적합 
fit.full <- lm(Y~., data) #모든변수로 Y에 적합
summary(fit.full) # R-squared: 0.7665,	Adjusted R-squared:  0.7292
# 유의미한 변수 X1


### p-value를 기준으로 변수 제거

fit1 <- lm(Y~ . -X4, data)
summary(fit1) # R-squared:  0.7659,	Adjusted R-squared:  0.7389 
fit2 <- lm(Y~ . -X4 -X3, data)
summary(fit2) # R-squared:  0.7649,	Adjusted R-squared:  0.7475
fit3 <- lm(Y~ . -X4 -X3 -X2, data)
summary(fit3) # R-squared:  0.7601,	Adjusted R-squared:  0.7515 


### AIC기준으로 변수선택법(AIC가 낮을 수록 좋다)

# 설명변수를 넣지않은 모델
fit.con <- lm(Y ~ 1, data)
# 전진선택법
fit.forward <- step(fit.con, list(lower=fit.con, upper=fit.full), direction = "forward")
# 후진제거법
fit.backward <- step(fit.full, list(lower=fit.con, upper = fit.full), direction = "backward")
# 단계적회귀방법(stepwise)
fit.both <- step(fit.con, list(lower=fit.con, upper=fit.full), direction = "both")

summary(fit.forward) # R-squared:  0.7601,	Adjusted R-squared:  0.7515 # 유의한 변수:X1
summary(fit.backward) # R-squared:  0.7601,	Adjusted R-squared:  0.7515 # 유의한 변수:X1
summary(fit.both) # R-squared:  0.7601,	Adjusted R-squared:  0.7515 # 유의한 변수:X1

### 모델 비교
#anova(fit3, fit.both) # 지금은 같은 모델이라 사용하지 않는다.(보통 p-value가 0.05보다 크면 유의미한 차이 없다고 판단)

### 모형 진단
### PLOT
# Residuals vs Fitted : 선형성, 오차의 등분산성, 독립성 판단에 사용. 만약 잔차와 예측값사이에 어떤 규칙이 발견된다면 그 규칙을 변수로 추가 할 수 있다. 
# Normal Q-Q : 정규성을 볼 수 있는 그림. 정규성을 만족하면 y=x 직선위에 모든점이 올라와야 된다. 
# Scale-Location : 등분산성을 판단하는 plot, 흩어짐의 정도가 일정해야 함 
#                  흩어짐의 정도가 치우쳐있음, 등분산성에 문제가 있다고 판단한다.
# Residual vs Leverage : 
# 개개의 관찰치에 대한 정보를 제공한다.이상치/큰 지레점/영향관측치를 확인할 수 있다
# 이상치(outlier): 회귀모형으로 잘 예측되지 않는 관측치(즉 아주 큰 양수/음수의 residual)
# 지레점(high leverage point): 예측변수측의 이상치
# 영향치(influential observation): 통계 모형 계수 결정에 불균형한 영향을 미치는 관측치
par(mfrow=c(2,2))
plot(fit.both)
# Residuals vs Fitted - 오차의 등분산성, 선형성, 독립성 판단
# normal Q-Q - 정규성 판단

## shapiro.test 정규성검정
e <-resid(fit.both) # 잔차
shapiro.test(e) # p-value = 0.8677 정규성 만족

# 독립성
plot(predict(fit.both), e) # 딱히 패턴이 보이진 않는다.
plot(predict(fit.both), e, type = 'o')

#install.packages("car")
#vif, outlierTest 등
library(car)

# 다중 공선성
vif(fit.both) #에러 난다.
vif(fit.full)

### 자료진단
# 이상점 
outlierTest(fit.both)
# Nullfor the Bonferonniadjusted outlier test is the observation is an outlier.
## Bonferonni p가 < .05이면 이상점으로 판단
## 이상점 없음

# 영향력 관측값 
influence.measures(fit.both) # *표시 : 영향력 의심되는 관측값 

#influencePlot(fit.both, id.method="identify", main="Influence Plot",
#              sub="Circle size is proportional to Cook’s distance") #원의 크기가 크면 영향력 관측값 의심
#influenceIndexPlot(fit.both, id.n=3)
# Cook's distance measures how much an observation influences the overall model or predicted values
#influencePlot(fit.both, id.n=6)
# Creates a bubble-plot combining the display of Studentizedresiduals, hat-values, and Cook's distance (represented in the circles).

### 결론
summary(fit.both) # y=33.487803-0.47056x1
                  # R-squared:  0.7601,	Adjusted R-squared:  0.7515
### 회귀분석을 통한 예측값
predict(fit.both)


############################################################################################
##### 지시 변수 처리하기 #####
##data 불러오기
data2 <- read.csv("education1960_70.csv",header=T,stringsAsFactors=FALSE)
str(data2)
head(data2)

#summary(data2)
#names(data2)

####################################################
# STATE : 주
# Y : 개인당 들어가는 교육 비용
# X1:수입
# X2:18세 이하 거주자 (천명)
# X3:도시 거주 인구(천명)
# Region: Northeast(1),North Central(2),South(3),West(4)
# Year: 1960 or 1970
#######################################################

##data 탐색 및 전처리
#결측치 확인
sum(is.na(data2)) #0
#있다면 data <- na.omit(data2) 행별로 제거

#구조보기
par(mfrow=c(2,3))
hist(data2$Y);summary(data2$Y)
hist(data2$X1);summary(data2$X1)
hist(data2$X2);summary(data2$X2)
hist(data2$X3);summary(data2$x3)
hist(data2$Region);summary(data2$Region)
hist(data2$Year);summary(data2$Year)

#Region 가변수 처리(4개-> 지시변수는 3개)
data2$Z1<-ifelse(data2$Region=="1",1,0)
data2$Z2<-ifelse(data2$Region=="2",1,0) 
data2$Z3<-ifelse(data2$Region=="3",1,0)
names(data2)
unique(data2$STATE)
data2 <- data2[,c(-1,-6)]
names(data2)
####################################################
# Y : 개인당 들어가는 교육 비용
# X1:수입
# X2:18세 이하 거주자 (천명)
# X3:도시 거주 인구(천명)
# Year: 1960 or 1970
# Z1=0 Z2=0 Z3=0 : Region = West
# Z1=1 Z2=0 Z3=0 : Region = Northeast
# Z1=0 Z2=1 Z3=0 : Region = North Central
# Z1=0 Z2=0 Z3=1 : Region = South
#######################################################