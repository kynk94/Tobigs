#K-means 구현하기

rm(list=ls())

#I Encoded with CP949
library(caret)
library(dplyr)

iris=iris
wine=read.table("http://archive.ics.uci.edu/ml/machine-learning-databases/wine/wine.data", sep=",")
colnames(wine) <- c("Cvs","Alcohol","Malic acid","Ash","Alcalinity of ash", "Magnesium", "Total phenols", "Flavanoids", "Nonflavanoid phenols", "Proanthocyanins", "Color intensity", "Hue", "OD280/OD315 of diluted wines", "Proline") # 변수명 설정 

#min-max normalize 함수
normalize = function(x){
  return((x-min(x))/(max(x)-min(x)))
}
pro.train_normal <- as.data.frame(lapply(iris[-5],normalize))

K_means = function(dataset){
  normalize = function(x){
    return((x-min(x))/(max(x)-min(x)))
  }
  dataset=as.data.frame(lapply(dataset,normalize))
  nle=length(dataset)
  #nc는 elbow-point 기법으로 최적 값을 구할 수 있다.
  #지금은 사용할 데이터 둘 다 표적이 3가지이므로 3으로 정의한다.
  nc=3 #length(unique(iris$Species))
  nr=length(dataset[,1])
  #dataset에서 샘플링 후 index 순서대로 정렬한다.
  a=sample_n(dataset,nc)
  a=a[c(order(as.numeric(row.names(a)))),]
  b=matrix(nrow=nr,ncol=nc)
  c=matrix(nrow=nc,ncol=length(dataset))
  k=0
  #샘플링 한 데이터로부터의 유클리드 거리를 b에 저장한다.
  for(i in 1:nc){
    for(j in 1:nr){
      b[j,i]=apply((dataset[j,]-a[i,])^2,1,sum)
      b[j,i]=b[j,i]^0.5
    }
  }
  #반복을 위해 while 안에 집어넣는다. 총 5회 반복
  while(1){
    #거리의 최소값을 구하고 그 때의 j가 클러스터번호가 된다.
    for(i in 1:nr){
      min=max(b)+1
      for(j in 1:nc){
        if(min>b[i,j]){
          min=b[i,j]
          dataset[i,nle+1]=j
        }
      }
    }
    #군집의 중심을 c에 저장한다.
    #예시)첫번째 분류결과 1로 분류된 것들의 평균을 c[1,]에 저장.
    for(i in 1:nc){
      c[i,]=apply(dataset[dataset[nle+1]==i,],2,mean)[-(nle+1)]
    }
    #군집 중심과 데이터셋의 유클리드거리를 b에 저장한다.
    for(i in 1:nc){
      for(j in 1:nr){
        b[j,i]=apply((dataset[-(nle+1)][j,]-c[i,])^2,1,sum)
        b[j,i]=b[j,i]^0.5
      }
    }
    k=k+1
    #총 5회 반복 후 종료한다.
    if(k>4){
      return(dataset[,nle+1])
      break
    }
  }
}
################################################################
################################################################
#   iris data
irispred=K_means(iris[-5])

irispred
#  [1] 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 3 2 3 2 2 2 3 2 2 2
# [61] 2 2 2 2 2 2 2 2 2 2 3 2 2 2 2 2 2 3 2 2 2 2 2 2 2 2 3 2 2 2 2 2 2 2 2 2 2 2 2 2 3 3 3 3 3 3 2 3 3 3 3 3 3 2 3 3 3 3 3 2
#[121] 3 2 3 3 3 3 3 3 3 3 3 3 3 2 2 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3
as.numeric(iris$Species)
#  [1] 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2
# [75] 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3
#[149] 3 3
confusionMatrix(as.factor(as.numeric(iris$Species)),as.factor(irispred))
# Confusion Matrix and Statistics
# 
# Reference
# Prediction  1  2  3
# 1 50  0  0
# 2  0 44  6
# 3  0  6 44
# 
# Overall Statistics
# 
# Accuracy : 0.92           
# 95% CI : (0.8644, 0.958)
# No Information Rate : 0.3333         
# P-Value [Acc > NIR] : < 2.2e-16      
# 
# Kappa : 0.88           
# Mcnemar's Test P-Value : NA             
# 
# Statistics by Class:
# 
# Class: 1 Class: 2 Class: 3
# Sensitivity            1.0000   0.8800   0.8800
# Specificity            1.0000   0.9400   0.9400
# Pos Pred Value         1.0000   0.8800   0.8800
# Neg Pred Value         1.0000   0.9400   0.9400
# Prevalence             0.3333   0.3333   0.3333
# Detection Rate         0.3333   0.2933   0.2933
# Detection Prevalence   0.3333   0.3333   0.3333
# Balanced Accuracy      1.0000   0.9100   0.9100

################################################################
################################################################
#   wine data
winepred=K_means(wine[-1])

winepred
#  [1] 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 2
# [61] 2 3 2 2 2 2 1 2 2 2 2 1 2 1 1 2 2 2 2 2 2 2 2 3 2 2 2 2 2 2 2 2 2 2 2 1 2 2 1 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 3 2
#[121] 2 2 2 2 2 2 2 2 2 2 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3
wine$Cvs
#  [1] 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2
# [75] 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3
#[149] 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3
confusionMatrix(as.factor(as.numeric(wine$Cvs)),as.factor(winepred))
# Confusion Matrix and Statistics
# 
# Reference
# Prediction  1  2  3
# 1 59  0  0
# 2  6 62  3
# 3  0  0 48
# 
# Overall Statistics
# 
# Accuracy : 0.9494          
# 95% CI : (0.9062, 0.9766)
# No Information Rate : 0.3652          
# P-Value [Acc > NIR] : < 2.2e-16       
# 
# Kappa : 0.9237          
# Mcnemar's Test P-Value : NA              
# 
# Statistics by Class:
# 
# Class: 1 Class: 2 Class: 3
# Sensitivity            0.9077   1.0000   0.9412
# Specificity            1.0000   0.9224   1.0000
# Pos Pred Value         1.0000   0.8732   1.0000
# Neg Pred Value         0.9496   1.0000   0.9769
# Prevalence             0.3652   0.3483   0.2865
# Detection Rate         0.3315   0.3483   0.2697
# Detection Prevalence   0.3315   0.3989   0.2697
# Balanced Accuracy      0.9538   0.9612   0.9706