rm(list=ls())

library(mlbench)
data(BreastCancer)
str(BreastCancer) #Class benign=1 양성, malignant=2 악성
#?BreastCancer
BreastCancer = BreastCancer[,-1] #ID 제거
BreastCancer$Cl.thickness = as.numeric(BreastCancer$Cl.thickness)
BreastCancer$Cell.size = as.numeric(BreastCancer$Cell.size)
BreastCancer$Cell.shape = as.numeric(BreastCancer$Cell.shape)
BreastCancer$Marg.adhesion = as.numeric(BreastCancer$Marg.adhesion)
BreastCancer$Epith.c.size = as.numeric(BreastCancer$Epith.c.size)
#BreastCancer$Bare.nuclei = as.numeric(BreastCancer$Bare.nuclei)
#BreastCancer$Bl.cromatin = as.numeric(BreastCancer$Bl.cromatin)
#BreastCancer$Normal.nucleoli = as.numeric(BreastCancer$Normal.nucleoli)
#BreastCancer$Mitoses = as.numeric(BreastCancer$Mitoses)
BreastCancer$Class = as.factor(ifelse(BreastCancer$Class == "benign",0,1))
str(BreastCancer)

summary(BreastCancer)
table(BreastCancer$Class)

library(caret)
#?glm

while(1){
acc = 0
j=0
for (i in 1:50) {
idx = createDataPartition(BreastCancer$Class, p=0.7, list=F)
BreastCancer_train = BreastCancer[idx,]
BreastCancer_test = BreastCancer[-idx,]

Up_BreastCancer_train = upSample(subset(BreastCancer_train,select=-Class),BreastCancer_train$Class)

glm2=glm(Class~., data = Up_BreastCancer_train, family = binomial)
predict2=as.numeric(predict(glm2, newdata = BreastCancer_test, type = 'response') > 0.5)
result = confusionMatrix(as.factor(predict2), as.factor(BreastCancer_test$Class))
result
accuracy = as.numeric(result$overall['Accuracy'])
acc = acc + accuracy
j=j+1
}
acc_mean = acc / j
print(acc_mean)
break
}

#0.938011