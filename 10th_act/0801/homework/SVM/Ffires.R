rm(list=ls())
graphics.off()

setwd("~/Tobigs/homework_0801/SVM")

library(e1071)
library(kernlab)
library(caret)
library(scatterplot3d)
Ffires = read.csv('Ffires.csv')
#FFMC,DMC,DC,ISI = Forest Fire Weather 지수
#데이터를 눈으로 쭉 보니 RH외의 변수들은 area에 영향을 주지는 않는 것으로 보인다.
#그냥 불이 났고, 불이 커질수도 있고 아닐수도 있고.
#RH만 다르고 거의 비슷한 데이터를 가지고 날짜만 다른데 area가 어떤건 0 어떤건 100이 넘음 이게 기준이 될 수 있나 의문이 생긴다.
#RH낮으면 큰 불이 나기 쉬움 - Large가 많다.
str(Ffires)

n=length(Ffires$area)
for(i in 1:n){
  if(Ffires$area[i]<=5){
    Ffires$area[i]='small'
  }else{
    Ffires$area[i]='large'
  }
}
Ffires$area=as.factor(Ffires$area)

month_dm = dummyVars('~ month', Ffires)
month_dm = data.frame(predict(month_dm, Ffires))
day_dm = dummyVars('~ day', Ffires)
day_dm = data.frame(predict(day_dm, Ffires))
Ffiresdm <- Ffires[!colnames(Ffires) %in% c('month','day')]
Ffiresdm <- cbind(Ffiresdm,month_dm,day_dm)
#3d플롯 위한 기존 DF month 변경
for(i in 1:n){
       if(Ffires$month[i]=='jan'){Ffires$monthnum[i]=1}
  else if(Ffires$month[i]=='feb'){Ffires$monthnum[i]=2}
  else if(Ffires$month[i]=='mar'){Ffires$monthnum[i]=3}
  else if(Ffires$month[i]=='apr'){Ffires$monthnum[i]=4}
  else if(Ffires$month[i]=='may'){Ffires$monthnum[i]=5}
  else if(Ffires$month[i]=='jun'){Ffires$monthnum[i]=6}
  else if(Ffires$month[i]=='jul'){Ffires$monthnum[i]=7}
  else if(Ffires$month[i]=='aug'){Ffires$monthnum[i]=8}
  else if(Ffires$month[i]=='sep'){Ffires$monthnum[i]=9}
  else if(Ffires$month[i]=='oct'){Ffires$monthnum[i]=10}
  else if(Ffires$month[i]=='nov'){Ffires$monthnum[i]=11}
  else if(Ffires$month[i]=='dec'){Ffires$monthnum[i]=12}
}
Ffires$month = Ffires$monthnum;Ffires=subset(Ffires,select=-c(monthnum))
str(Ffires)
str(Ffiresdm)

#large = blue, small = red
while(1){
  graphics.off()
  for(i in seq(0,90,by=3)){
    scatterplot3d(x=Ffiresdm$X,y=Ffiresdm$Y,z=Ffires$month,scale.y=0.7,xlab='X',ylab='Y',zlab='Month',main='원본',
                  pch=c(19,4)[Ffiresdm$area],color=c('blue','red')[Ffires$area],
                  angle=i)
    Sys.sleep(0.05)
  }
  par(mfrow=c(2,2))
  scatterplot3d(x=Ffiresdm$X,y=Ffiresdm$Y,z=Ffires$month,scale.y=0.7,xlab='X',ylab='Y',zlab='Month',main='원본',
                pch=c(19,4)[Ffiresdm$area],color=c('blue','red')[Ffires$area],
                angle=45)
  break
}
plot(Ffiresdm$X,Ffiresdm$Y,pch=c(19,4)[Ffiresdm$area],
     col=c('blue','red')[Ffiresdm$area],xlim=c(1,9),ylim=c(1,9),xlab='X',ylab='Y',main='원본')

#RH에 대해서 플롯
while(1){
  graphics.off()
  plot(10*Ffiresdm$X+Ffiresdm$Y,Ffiresdm$RH,pch=c(19,4)[Ffiresdm$area],
       col=c('blue','red')[Ffiresdm$area],xlim=c(0,100),ylim=c(0,100),xlab='10X+Y',ylab='RH',main='RH')
  break
}
scatterplot3d(x=Ffiresdm$X,y=Ffiresdm$Y,z=Ffiresdm$RH,scale.y=0.7,xlab='X',ylab='Y',zlab='RH',main='RH',
              pch=c(19,4)[Ffiresdm$area],color=c('blue','red')[Ffires$area],
              angle=45)

#SVM
svm.model<- svm(area~., data=Ffiresdm, gamma=1, cost=1)
addmargins(table(Ffiresdm$area, svm.model$fitted))

#원본과 비교하여 플롯
while(1){
  par(mfrow=c(2,2))
  scatterplot3d(x=Ffiresdm$X,y=Ffiresdm$Y,z=Ffires$month,scale.y=0.7,xlab='X',ylab='Y',zlab='Month',main='원본',
                pch=c(19,4)[Ffiresdm$area],color=c('blue','red')[Ffires$area],
                angle=45)
  plot(Ffiresdm$X,Ffiresdm$Y,pch=c(19,4)[Ffiresdm$area],
       col=c('blue','red')[Ffiresdm$area],xlim=c(1,9),ylim=c(1,9),xlab='X',ylab='Y',main='원본')
  scatterplot3d(x=Ffiresdm$X,y=Ffiresdm$Y,z=Ffires$month,scale.y=0.7,xlab='X',ylab='Y',zlab='Month',main='g=1 c=1',
                pch=c(19,4)[svm.model$fitted],color=c('blue','red')[svm.model$fitted],
                angle=45)
  plot(Ffiresdm$X,Ffiresdm$Y,pch=c(19,4)[svm.model$fitted],
       col=c('blue','red')[svm.model$fitted],xlim=c(1,9),ylim=c(1,9),xlab='X',ylab='Y',main='g=1 c=1')
  break
}
sum(Ffiresdm$area==svm.model$fitted)/length(Ffiresdm$area) #0.9787

#train, test 분리
idx = createDataPartition(Ffiresdm$area, p = 0.7, list=F)
train = Ffiresdm[idx,]
test = Ffiresdm[-idx,]
svm.model_train<- svm(area~., data=train, gamma=1, cost=1,scale=FALSE)
svm.predict_test<-predict(svm.model_train,newdata=test)
addmargins(table(test$area,svm.predict_test))

#test가 너무 작아서 large가 희귀하게 등장함. 의미가 없다.
graphics.off()
plot(test$X,test$Y,pch=c(19,4)[svm.predict_test],
     col=c('blue','red')[svm.predict_test],xlim=c(1,9),ylim=c(1,9),main='test g1c1')

tune.svm <- tune(svm,area~., data=Ffiresdm,
                 kernel="radial", ranges =list(gamma=c(0.1,1,10),
                                               cost=c(0.1,1,10)));warnings()
summary(tune.svm)

#polynomial 커널
Ffires.model1 <- ksvm(area ~ ., data=Ffiresdm,
                     type = "C-bsvc", kernel = "polydot",
                     kpar = list(degree = 2), C = 10,
                     prob.model = TRUE)
Ffires.poly=predict(Ffires.model1, subset(Ffiresdm,select=-c(area)))

while(1){
  par(mfrow=c(2,2))
  scatterplot3d(x=Ffiresdm$X,y=Ffiresdm$Y,z=Ffires$month,scale.y=0.7,xlab='X',ylab='Y',zlab='Month',main='원본',
                pch=c(19,4)[Ffiresdm$area],color=c('blue','red')[Ffires$area],
                angle=45)
  plot(Ffiresdm$X,Ffiresdm$Y,pch=c(19,4)[Ffiresdm$area],
       col=c('blue','red')[Ffiresdm$area],xlim=c(1,9),ylim=c(1,9),xlab='X',ylab='Y',main='원본')
  scatterplot3d(x=Ffiresdm$X,y=Ffiresdm$Y,z=Ffires$month,scale.y=0.7,xlab='X',ylab='Y',zlab='Month',main='poly 2nd order g=10',
                pch=c(19,4)[Ffires.poly],color=c('blue','red')[Ffires.poly],
                angle=45)
  plot(Ffiresdm$X,Ffiresdm$Y,pch=c(19,4)[Ffires.poly],
       col=c('blue','red')[Ffires.poly],xlim=c(1,9),ylim=c(1,9),xlab='X',ylab='Y',main='poly 2nd order g=10')
  break
}
sum(Ffiresdm$area==Ffires.poly)/length(Ffiresdm$area) #0.9826

#rbf 커널
Ffires.model2 <- ksvm(area ~ ., data = Ffiresdm,
                     type = "C-bsvc", kernel = "rbfdot",
                     kpar = list(sigma = 1), C = 10,
                     prob.model = TRUE)
#predict(Ffires.model2, subset(Ffiresdm,select=-c(area)), type = "probabilities") #0.29 정도면 large로 분류되네
#predict(Ffires.model2, subset(Ffiresdm,select=-c(area)), type = "decision")
Ffires.rbf=predict(Ffires.model2, subset(Ffiresdm,select=-c(area)))

while(1){
  par(mfrow=c(2,2))
  scatterplot3d(x=Ffiresdm$X,y=Ffiresdm$Y,z=Ffires$month,scale.y=0.7,xlab='X',ylab='Y',zlab='Month',main='원본',
                pch=c(19,4)[Ffiresdm$area],color=c('blue','red')[Ffires$area],
                angle=45)
  plot(Ffiresdm$X,Ffiresdm$Y,pch=c(19,4)[Ffiresdm$area],
       col=c('blue','red')[Ffiresdm$area],xlim=c(1,9),ylim=c(1,9),xlab='X',ylab='Y',main='원본')
  scatterplot3d(x=Ffiresdm$X,y=Ffiresdm$Y,z=Ffires$month,scale.y=0.7,xlab='X',ylab='Y',zlab='Month',main='rbf g=1 c=10',
                pch=c(19,4)[Ffires.rbf],color=c('blue','red')[Ffires.rbf],
                angle=45)
  plot(Ffiresdm$X,Ffiresdm$Y,pch=c(19,4)[Ffires.rbf],
       col=c('blue','red')[Ffires.rbf],xlim=c(1,9),ylim=c(1,9),xlab='X',ylab='Y',main='rbf g=1 c=10')
  break
}
sum(Ffiresdm$area==Ffires.rbf)/length(Ffiresdm$area) #0.9942

#Hyperbolic 커널
Ffires.model3 <- ksvm(area ~ ., data=Ffiresdm,
                     type = "C-bsvc", kernel = "tanhdot",
                     kpar = list(scale = 0.03), C = 10,
                     prob.model = TRUE)
Ffires.tanh=predict(Ffires.model3, subset(Ffiresdm,select=-c(area)))

while(1){
  par(mfrow=c(2,2))
  scatterplot3d(x=Ffiresdm$X,y=Ffiresdm$Y,z=Ffires$month,scale.y=0.7,xlab='X',ylab='Y',zlab='Month',main='원본',
                pch=c(19,4)[Ffiresdm$area],color=c('blue','red')[Ffires$area],
                angle=45)
  plot(Ffiresdm$X,Ffiresdm$Y,pch=c(19,4)[Ffiresdm$area],
       col=c('blue','red')[Ffiresdm$area],xlim=c(1,9),ylim=c(1,9),xlab='X',ylab='Y',main='원본')
  scatterplot3d(x=Ffiresdm$X,y=Ffiresdm$Y,z=Ffires$month,scale.y=0.7,xlab='X',ylab='Y',zlab='Month',main='tanh scale=0.03 g=10',
                pch=c(19,4)[Ffires.tanh],color=c('blue','red')[Ffires.tanh],
                angle=45)
  plot(Ffiresdm$X,Ffiresdm$Y,pch=c(19,4)[Ffires.tanh],
       col=c('blue','red')[Ffires.tanh],xlim=c(1,9),ylim=c(1,9),xlab='X',ylab='Y',main='tanh scale=0.03 g=10')
  break
}
sum(Ffiresdm$area==Ffires.tanh)/length(Ffiresdm$area) #0.8008

#gaussian rbf가 0.9942로 가장 높았다.
#tanh는 정확도가 낮음.
#하지만 전체적으로 데이터 수가 작아서 유의미하지 않아 보인다.