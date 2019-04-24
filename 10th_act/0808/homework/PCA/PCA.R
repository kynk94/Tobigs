rm(list=ls())

library(caret)
library(hydroGOF)
#Encoded with CP949
setwd("~/Tobigs/homework_0808/PCA")

facebook=read.csv('Features_Variant_5.csv')
str(facebook)
facebookname=c("Like","Checkin","Interest","Category",
               "X1","X2","X3","X4","X5","X6","X7","X8","X9","X10",
               "X11","X12","X13","X14","X15","X16","X17","X18","X19","X20",
               "X21","X22","X23","X24","X25",
               "Comment.Base","Comment.last24h","Comment.last48h","Comment.1st24h","Comment.24h-48h",
               "Base.Time","Post.Length","Post.Share","Post.promoted","Hhrs",
               "Pub.sun","Pub.mon","Pub.tue","Pub.wed","Pub.thu","Pub.fri","Pub.sat",
               "Base.sun","Base.mon","Base.tue","Base.wed","Base.thu","Base.fri","Base.sat","Next.commentsNo")
colnames(facebook)=facebookname
#length(unique(facebook$Post.promoted))=1이므로 삭제한다. 분산이 0이기 때문이다.
facebook=subset(facebook,select=-c(Post.promoted))

facebook$Category = as.factor(facebook$Category)
Category.dummy = dummyVars('~Category',facebook)
Category.dummy = data.frame(predict(Category.dummy,facebook))
facebookdm = facebook[!colnames(facebook) %in% c('Category')]
facebookdm = cbind(facebookdm,Category.dummy)
#Base Time으로부터 다음 Hhrs동안의 댓글 개수에 관한 데이터.
#이를 토대로 Testset의 댓글 개수 예측하기
#Comment.Base=1st24h+last48h+last24h 최대 72시간의 데이터
#Base.Time이 24보다 작으면 1st24h=last24h 48보다 작으면 1st24h+@=last24h @=1st24h이후 0~24시간 안에 추가로 달린 댓글 수

#train, test 분할
idx = createDataPartition(facebook[-4]$Next.commentsNo, p = 0.7, list=F)
train = facebook[-4][idx,]
test = facebook[-4][-idx,]
train_label <- facebook[-4][idx, 52] ; test_label <- facebook[-4][-idx, 52]
train = subset(train,select=-c(Next.commentsNo))
test = subset(test,select=-c(Next.commentsNo))

#PCA
facebookPCA = prcomp(scale(train))
plot(facebookPCA, type="l")
summary(facebookPCA)
n=min(which(summary(facebookPCA)[[6]][3,] >= 0.85)) #17
trainPRC <- as.matrix(train) %*% facebookPCA$rotation
testPRC <- as.matrix(test) %*% facebookPCA$rotation
trainF <- cbind(as.data.frame(trainPRC[,1:n]), train_label)
testF <- cbind(as.data.frame(testPRC[,1:n]), test_label)
colnames(trainF)[n+1]="label"; colnames(testF)[n+1]="label"
str(trainF)

#선형회귀 결과
fit <- lm(label~., data = trainF)
summary(fit)
  # Call:
  # lm(formula = label ~ ., data = trainF)
  # 
  # Residuals:
  # Min      1Q  Median      3Q     Max
  # -381.19   -6.06   -1.03    3.61 1595.84
  # 
  # Coefficients:
  # Estimate Std. Error t value Pr(>|t|)
  # (Intercept)  2.555328   0.461331   5.539 3.05e-08 ***
  # PC1          0.046536   0.001232  37.787  < 2e-16 ***
  # PC2         -0.040632   0.003559 -11.417  < 2e-16 ***
  # PC3         -0.146831   0.056144  -2.615  0.00892 **
  # PC4          0.015486   0.100857   0.154  0.87797
  # PC5         -0.035470   0.071150  -0.499  0.61812
  # PC6          0.046503   0.055094   0.844  0.39863
  # PC7          0.216437   0.023732   9.120  < 2e-16 ***
  # PC8          0.143249   0.024841   5.767 8.10e-09 ***
  # PC9          0.940087   0.055297  17.001  < 2e-16 ***
  # PC10         0.733920   0.033740  21.752  < 2e-16 ***
  # PC11         1.441954   0.121202  11.897  < 2e-16 ***
  # PC12         0.878808   0.161230   5.451 5.03e-08 ***
  # PC13         0.106676   0.158300   0.674  0.50039
  # PC14        -0.094295   0.123405  -0.764  0.44480
  # PC15         1.050039   0.140296   7.484 7.23e-14 ***
  # PC16        -0.533712   0.057745  -9.243  < 2e-16 ***
  # PC17         0.697857   0.042930  16.256  < 2e-16 ***
  # ---
  # Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
  # 
  # Residual standard error: 27.84 on 139303 degrees of freedom
  # Multiple R-squared:  0.3301,	Adjusted R-squared:   0.33
  # F-statistic:  4038 on 17 and 139303 DF,  p-value: < 2.2e-16

#test 데이터 불러오기
test1=read.csv("Test_Case_1.csv")
test2=read.csv("Test_Case_2.csv")
test3=read.csv("Test_Case_3.csv")
test4=read.csv("Test_Case_4.csv")
test5=read.csv("Test_Case_5.csv")
test6=read.csv("Test_Case_6.csv")
test7=read.csv("Test_Case_7.csv")
test8=read.csv("Test_Case_8.csv")
test9=read.csv("Test_Case_9.csv")
test10=read.csv("Test_Case_10.csv")
colnames(test1)=facebookname
colnames(test2)=facebookname
colnames(test3)=facebookname
colnames(test4)=facebookname
colnames(test5)=facebookname
colnames(test6)=facebookname
colnames(test7)=facebookname
colnames(test8)=facebookname
colnames(test9)=facebookname
colnames(test10)=facebookname

#test데이터를 하나의 셋으로 만들어준다.
testset=rbind(test1,test2,test3,test4,test5,test6,test7,test8,test9,test10)
testset1=subset(testset,select=-c(Category,Post.promoted,Next.commentsNo))

#testset에 rotation을 곱한다.
test1PRC=as.matrix(testset1) %*% facebookPCA$rotation
test1PRC1=as.data.frame(test1PRC[,1:n])
test1F=cbind(test1PRC1,testset[54])
str(test1F)
fit1=lm(Next.commentsNo~.,data=test1F)

#fit1의 선형회귀 결과
summary(fit1)
#   Call:
#   lm(formula = Next.commentsNo ~ ., data = test1F)
# 
#   Residuals:
#   Min      1Q  Median      3Q     Max 
#   -408.44  -23.64   -9.17   12.78 1752.01 
# 
#   Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
#   (Intercept) 14.51754    6.29896   2.305 0.021390 *  
#   PC1          0.09262    0.03492   2.653 0.008116 ** 
#   PC2         -0.04161    0.05349  -0.778 0.436820    
#   PC3          0.87804    1.74113   0.504 0.614171    
#   PC4         -1.91894    3.14833  -0.610 0.542329    
#   PC5         -0.24960    1.40208  -0.178 0.858743    
#   PC6          0.95442    1.84162   0.518 0.604402    
#   PC7          0.26340    0.72054   0.366 0.714773    
#   PC8          0.03058    0.81453   0.038 0.970064    
#   PC9          4.28935    1.84885   2.320 0.020547 *  
#   PC10         5.43844    1.20112   4.528 6.70e-06 ***
#   PC11         9.69196    3.31089   2.927 0.003499 ** 
#   PC12         0.33852    5.27666   0.064 0.948861    
#   PC13        -9.42444    7.67886  -1.227 0.219998    
#   PC14        -0.59591    2.83873  -0.210 0.833773    
#   PC15        22.79188    5.45388   4.179 3.19e-05 ***
#   PC16        -9.10220    2.04009  -4.462 9.08e-06 ***
#   PC17         5.05855    1.30605   3.873 0.000115 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
#   Residual standard error: 91.07 on 972 degrees of freedom
#   Multiple R-squared:  0.2643,	Adjusted R-squared:  0.2514 
#   F-statistic: 20.54 on 17 and 972 DF,  p-value: < 2.2e-16

fit_pred <- predict(fit, type="response", newdata = test1PRC1)
test_pred <- round(fit_pred)
#음수값은 0으로 바꾼다.
for(i in 1:length(test_pred)){
  if(test_pred[i]<0)test_pred[i]=0
}
fb.compare=cbind(as.data.frame(test_pred),testset[54])
colnames(fb.compare)=c("Predict","N_of_Comments")
boxplot(fb.compare,ylim=c(1,300))
par(mfrow=c(1,1))
hist(fb.compare$Predict,xlim=c(0,650))
hist(fb.compare$N_of_Comments,xlim=c(0,650))
mse(fb.compare[1],fb.compare[2]) #mse=9167.008