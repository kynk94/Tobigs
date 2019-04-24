rm(list=ls())

load("psub.RData")
str(psub)

names(psub)
summary(psub$SCHL)
#no high school diploma=1 중졸
#Associate's degree=2 전문학사
#Bachelor's degree=3 학사
#Doctorate degree=4 박사
#GED or alternative credential=5 검정고시
#Master's degree=6 석사
#Professional degree=7 전문학위 (의사/변호사 같은)
#Regular high school diploma=8 고졸
#some college credit, no degree=9 대학 기록 있음, 학위 없음
#3,4,6,7이 학사 이상이다.

psub$bachdeg=as.numeric(psub$SCHL)
n=length(psub$bachdeg)
for(i in 1:n){
  if(psub$bachdeg[i]==3||psub$bachdeg[i]==4||psub$bachdeg[i]==6||psub$bachdeg[i]==7){
    psub$bachdeg[i]=1
  }else{
    psub$bachdeg[i]=0
  }
}
psub$bachdeg=as.factor(psub$bachdeg)
str(psub$bachdeg)

select1 = colnames(psub)[c(12,15,73,108,290)]
select2 = colnames(psub)[c(12,15,73,108)]
formula1 = formula(paste("bachdeg~",paste(select2, collapse=" + ")))
psub=psub[select1]
str(psub)

library(caret)
idx = createDataPartition(psub$bachdeg, p=0.7, list=F)
psub_train = psub[idx,]
psub_test = psub[-idx,]

model.glm1 = glm(formula1, psub_train, family = binomial)
pred.glm1 = as.numeric(predict(model.glm1, psub_test, type = "response") > 0.5)
confusionMatrix(as.factor(pred.glm1),as.factor(psub_test$bachdeg))
table(pred.glm1)

psub_train_up = upSample(subset(psub_train, select=-bachdeg), psub_train$bachdeg)
formula2 = formula(paste("Class~",paste(select2, collapse=" + ")))

model.glm2 = glm(formula2, psub_train_up, family = binomial)
pred.glm2 = as.numeric(predict(model.glm2, psub_test, type = "response") > 0.5)
confusionMatrix(as.factor(pred.glm2),psub_test$bachdeg)
table(pred.glm2)
