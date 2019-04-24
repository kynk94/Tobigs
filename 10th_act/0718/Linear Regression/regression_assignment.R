rm(list=ls())
# 데이터
library(MASS)
data(Boston)
str(Boston)

##### 변수설명 ########################################
# crim : per capita crime rate by town. 1인당 범죄율
# zn :proportion of residential land zoned for lots over 25,000 sq.ft. 2,5000제곱피트이상 주거토지비율
# indus :proportion of non-retail business acres per town. 비판매업 비율
# chas : Charles River dummy variable (= 1 if tract bounds river; 0 otherwise). 강이 집 근처면 1 멀면 0
# nox : nitrogen oxides concentration (parts per 10 million). 질산화물 농도
# rm : average number of rooms per dwelling. 집당 평균 방 수
# age : proportion of owner-occupied units built prior to 1940. 1940년 이전 건설된 점유건물 비율
# dis : weighted mean of distances to five Boston employment centres. 5개 직장밀집지?고용센터? 거리 가중
# rad : index of accessibility to radial highways. 방사형 고속도로 접근성
# tax : full-value property-tax rate per \$10,000. 1만달러당 재산세
# ptratio : pupil-teacher ratio by town. 학생, 교사 비율
# black : 1000(Bk - 0.63)^2 where Bk is the proportion of blacks by town. 흑인 비율
# lstat : lower status of the population (percent). 하위층 비율
# medv : median value of owner-occupied homes in \$1000s.
#######################################################

##### 회귀분석하기 #####
### medv(본인 소유의 주택가격(중앙값) 단위: $1,000)을 예측해주세요
#medv
fit1=lm(medv~., Boston)
summary(fit1)
#F는 연구모델의 적합성, P는 가설의 유의성 T는 변수들의 영향 * 갯수가 중요도를 의미
#분포곡선에서 x축에 T,F값이 존재하고 P는 T,F보다 큰 부분의 면적을 의미 -> 데이터연관성이 99.5%안에 들어야 유의미하다는 뜻이었네.
#T=Estimate/Std.Error 즉 Estimate에 비해 Error가 클수록 의미가 없다.
#sumary의 마지막부분인 Pr은 T이상인 부분의 면적. 면적이 작을수록 유의미.
#F는?
#R-squared가 0.7406이니까 74%를 설명한다는 것
#마지막 p-value는 모델에대한 F 검정의 P값, 이 경우 2.2e-16
plot(Boston$crim, Boston$medv)    #T  -3.287
plot(Boston$zn, Boston$medv)      #T   3.382
plot(Boston$indus, Boston$medv)   #T   0.334 -> 지워도 되는 변수, 보니까 균등하게 분포하면 지우네
plot(Boston$chas, Boston$medv)    #T   3.118 -> 근데 얘는 0,1로 나뉘는 변수, 강에서 멀수록 2.687만큼 비싸다? 상식과 반대됨
plot(Boston$nox, Boston$medv)     #T  -4.651
plot(Boston$rm, Boston$medv)      #T   9.116
plot(Boston$age, Boston$medv)     #T   0.052 -> 지워도 되는 변수
plot(Boston$dis, Boston$medv)     #T  -7.368
plot(Boston$rad, Boston$medv)     #T   4.613 -> 얘도 1단위 변수인데 어떻게 해야하지
plot(Boston$tax, Boston$medv)     #T  -3.280
plot(Boston$ptratio, Boston$medv) #T  -7.283지
plot(Boston$black, Boston$medv)   #T   3.467
plot(Boston$lstat, Boston$medv)   #T -10.347

library(car)
vif(fit1) #5~10이면 크다.

fit.con <- lm(medv~ 1, Boston)
#fit.step = step(fit1,direction = 'both')
fit.step = step(fit.con, list(lower=fit.con, upper=fit1), direction = 'both')
summary(fit.step) #indus, age가 지워졌다. chas도 지우고 따로 비교해야 하지 않을까?
vif(fit.step)

outlierTest(fit.step)
influence.measures(fit.step)
influencePlot(fit.step)
#influencePlot(fit.step, id.method="identify", main="Influence Plot", sub="Circle size")
#influencePlot(fit1, id.method="identify", main="Influence Plot", sub="Circle size is proportional to Cook’s distance")
influenceIndexPlot(fit.step, id.n=3)
Boston2=Boston[-c(369,381),]
Boston3=Boston2[,-c(Boston2$rad,Boston2$tax)]
fit1=lm(medv~., Boston3)
summary(fit1)
fit.con <- lm(medv~ 1, Boston3)
fit.step = step(fit.con, list(lower=fit.con, upper=fit1), direction = 'both')
summary(fit.step)
outlierTest(fit.step)
influence.measures(fit.step)
influencePlot(fit.step)

yhat = predict(fit.step,newdata = Boston3,type = 'response')
head(yhat)
plot(fit.step$fitted.values,Boston3$medv) #medv가 50인 이상점들이 있는듯? 3개가 영향값인것같고 지워야 할 것으로 보이는데 어떻게 지우지?
abline(0,1,col='blue')

mean((Boston3$medv-yhat)^2) #이상점 영향값 지우기 전 21.89993이 나온다.
