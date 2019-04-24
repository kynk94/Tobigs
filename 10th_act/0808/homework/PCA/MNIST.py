from scipy import io
from sklearn.model_selection import train_test_split
from sklearn.ensemble import RandomForestClassifier
from sklearn.metrics import accuracy_score
from sklearn.metrics import confusion_matrix
from sklearn.linear_model import LogisticRegression as lr
from sklearn.decomposition import PCA
import pandas as pd
import numpy as np
import matplotlib
import matplotlib.pyplot as plt
import seaborn as sns
import time

mnist=io.loadmat('mnist-original.mat')
md=mnist['data'] #784*70000
ml=mnist['label'] #1*70000

#train_test_split은 col은 그대로고 row만 랜덤추출함.
#때문에 행과 열을 바꿔준다. 전치행렬
#애초에 mat파일 자체가 행,열이 반대인듯 하다.
md=md.T
ml=ml.T

#6:1로 train,test 분할
md_train,md_test,ml_train,ml_test = train_test_split(
      md,ml,test_size=1/7,random_state=1)

#이미지 플롯
def plot_digits(instances, images_per_row=5, **options):
    size = 28
    images_per_row = min(len(instances), images_per_row)
    images = [instance.reshape(size,size) for instance in instances]
    n_rows = (len(instances) - 1) // images_per_row + 1
    row_images = []
    n_empty = n_rows * images_per_row - len(instances)
    images.append(np.zeros((size, size * n_empty)))
    for row in range(n_rows):
        rimages = images[row * images_per_row : (row + 1) * images_per_row]
        row_images.append(np.concatenate(rimages, axis=1))
    image = np.concatenate(row_images, axis=0)
    plt.imshow(image, cmap = matplotlib.cm.binary, **options)
    plt.axis("off")

#Softmax
def lrsoftmax(matrix):
      smdf = sm.decision_function(matrix)
      smdfe = np.exp(smdf)
      prob = smdfe/smdfe.sum(axis=1).reshape(len(smdfe.sum(axis=1)),1)
      return(prob)
def pcalrsoftmax(matrix):
      smdf = smpca.decision_function(matrix)
      smdfe = np.exp(smdf)
      prob = smdfe/smdfe.sum(axis=1).reshape(len(smdfe.sum(axis=1)),1)
      return(prob)

#기존 데이터 확인
plot_digits(md_train[::2100])
plt.show()

#RF 분류기
print('Random Forest Classifying Start')
rf_time_start = time.time()
rf = RandomForestClassifier(n_estimators=100, oob_score=True, random_state=1)
rf.fit(md_train,ml_train.ravel())
rfpredict = rf.predict(md_test)
rfaccuracy = accuracy_score(ml_test,rfpredict)
rf_time = time.time()-rf_time_start
print(f'Test Label : {ml_test.ravel()}')
print(f'Rf predict : {rfpredict}')
print('Accuracy : %.4f Out of bag : %.4f\nTime : %.2f sec\n'
      %(rfaccuracy, rf.oob_score_, rf_time))
#Accuracy : 0.9697      Time : 46.58 sec
ax1 = plt.subplot(111)
ax1.set_title('Random Forest')
ax1.set_ylabel('Predict')
#예측 플롯
pd.DataFrame(rfpredict[0:50],columns=['Predict']).plot(ax=ax1)
plt.show()
#heatmap 플롯
rfcm = pd.DataFrame(confusion_matrix(ml_test,rfpredict))
sns.heatmap(rfcm, annot=True)
plt.show()

#Softmax 분류기
print('Softmax Classifying Start')
sm_time_start = time.time()
sm = lr(max_iter=5).fit(md_train,ml_train.ravel())
probtrain = lrsoftmax(md_train)
probtest = lrsoftmax(md_test)
smpredict = sm.predict(md_test)
smaccuracy = accuracy_score(ml_test,smpredict)
sm_time = time.time()-sm_time_start
print(f'Test Label : {ml_test.ravel()}')
print(f'Sm predict : {smpredict}')
print('Accuracy : %.4f\nTime : %.2f sec\n'%(smaccuracy,sm_time))
#Accuracy : 0.9143      Time : 2810.69 sec
#예측 플롯
ax1 = plt.subplot(211)
ax1.set_title('SoftMAX')
ax1.set_ylabel('Probability')
pd.DataFrame(probtest[0:50]).plot(ax=ax1)
ax2 = plt.subplot(212)
ax2.set_ylabel('Predict')
pd.DataFrame(smpredict[0:50],columns=['Predict']).plot(ax=ax2)
plt.show()
#heatmap 플롯
smcm = pd.DataFrame(confusion_matrix(ml_test,smpredict))
sns.heatmap(smcm, annot=True)
plt.show()

#PCA
print('PCA start')
pca = PCA(n_components=0.96,random_state=1)
pca.fit(md_train)
mdn = pca.n_components_  #mdn = 179
print(f'Number of Components : {mdn}')
pca = PCA(n_components=mdn,random_state=1)
print('PCA fitting\n')
pca.fit(md_train)
md_train_reduced = pca.transform(md_train)
md_test_reduced = pca.transform(md_test)
md_recovered = pca.inverse_transform(md_train_reduced)
#원본과 복원 비교
plt.subplot(121)
plt.title('Original')
plot_digits(md_train[::2100])
plt.subplot(122)
plt.title('Recovered')
plot_digits(md_recovered[::2100])
plt.show()

#PCA - RF 분류기
print('PCA - Random Forest Classifying Start')
rfpca_time_start = time.time()
rfpca = RandomForestClassifier(n_estimators=100, oob_score=True, random_state=1)
rfpca.fit(md_train_reduced,ml_train.ravel())
rfpcapredict = rfpca.predict(md_test_reduced)
rfpcaaccuracy = accuracy_score(ml_test,rfpcapredict)
rfpca_time = time.time()-rfpca_time_start
print(f'Test Label : {ml_test.ravel()}')
print(f'Rf predict : {rfpcapredict}')
print('Accuracy : %.4f Out of bag : %.4f\nTime : %.2f sec\n'
      %(rfpcaaccuracy, rfpca.oob_score_, rfpca_time))
#Accuracy : 0.9466      Time : 108.29 sec
ax1 = plt.subplot(111)
ax1.set_title('PCA - Random Forest')
ax1.set_ylabel('Predict')
#예측 플롯
pd.DataFrame(rfpcapredict[0:50],columns=['Predict']).plot(ax=ax1)
plt.show()
#heatmap 플롯
rfpcacm = pd.DataFrame(confusion_matrix(ml_test,rfpcapredict))
sns.heatmap(rfpcacm, annot=True)
plt.show()

#PCA - Softmax 분류기
print('PCA - Softmax Classifying Start')
smpca_time_start = time.time()
smpca = lr(max_iter=5).fit(md_train_reduced,ml_train.ravel())
probtrain_reduced = pcalrsoftmax(md_train_reduced)
probtest_reduced = pcalrsoftmax(md_test_reduced)
smpcapredict = smpca.predict(md_test_reduced)
smpcaaccuracy = accuracy_score(ml_test,smpcapredict)
smpca_time = time.time()-smpca_time_start
print(f'Test Label : {ml_test.ravel()}')
print(f'Sm predict : {smpcapredict}')
print('Accuracy : %.4f\nTime : %.2f sec\n'%(smpcaaccuracy,smpca_time))
#Accuracy : 0.9149      Time : 925.86 sec
#예측 플롯
ax1 = plt.subplot(211)
ax1.set_title('PCA - SoftMAX')
ax1.set_ylabel('Probability')
pd.DataFrame(probtest_reduced[0:50]).plot(ax=ax1)
ax2 = plt.subplot(212)
ax2.set_ylabel('Predict')
pd.DataFrame(smpcapredict[0:50],columns=['Predict']).plot(ax=ax2)
plt.show()
#heatmap 플롯
smpcacm = pd.DataFrame(confusion_matrix(ml_test,smpcapredict))
sns.heatmap(smpcacm, annot=True)
plt.show()

#TimeTable
time_data={'Softmax':[sm_time,smpca_time],
           'RandomForest':[rf_time,rfpca_time]}
timetable = pd.DataFrame(time_data,
                         columns=['RandomForest','Softmax'],
                         index=['Time','PCA Time'])
'''
          RandomForest      Softmax
Time         46.581252  2810.692777
PCA Time    108.294196   925.859520
'''
