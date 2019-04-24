from sklearn.naive_bayes import GaussianNB as GNB
from sklearn.model_selection import KFold
from sklearn.model_selection import cross_val_score as cvs
import pandas
import seaborn as sns
import matplotlib.pyplot as plt

#csv파일을 읽어와 dataframe에 저장한다.
df = pandas.read_csv('train.csv')

#이 데이터는 타이타닉 생존 데이터다.
#Survived (0 = No, 1= Yes)
#Pclass   (1 = 1st, 2 = 2nd, 3 = 3rd)
#SibSp    (동승 형제,배우자 수)
#Parch    (동승 부모,자녀 수)
#Embarked (탑승한 곳, C = Cherbourg, Q = Queenstown, S = Southapmton)

#항목명 확인, PassengerId, Name, Ticket 제거, SibSp와 Parch 합치기
#print(df.columns)
df.drop('PassengerId',axis=1,inplace=True)
df.drop('Name',axis=1,inplace=True)
df.drop('Ticket',axis=1,inplace=True)
df['Family'] = df['SibSp'] + df['Parch']
df.drop('SibSp',axis=1,inplace=True)
df.drop('Parch',axis=1,inplace=True)
#결측값 확인
#print(df.isnull().sum())

#Cabin은 전체 891개 데이터 중 644개가 비어있다. 따라서 N으로 빈 값을 채우고 영어만 남긴다.
#Age는 평균값으로 빈 값을 채운다. 나이 평균 28세. 젊다.
#Embarked는 2개가 비어있으므로 가장 많은 값으로 빈 값을 채운다. S 644 C 168 Q 77
df['Cabin'] = df['Cabin'].fillna('N')
df['Cabin'] = df['Cabin'].apply(lambda x:x[0])
#for i in range(0,len(df['Age'])):
#    if df['Age'].isnull()[i]==True:
#        print(i)
df['Age'] = df['Age'].fillna(df['Age'].median())
#print(df['Embarked'].value_counts())
df['Embarked'] = df['Embarked'].fillna('S')
#print(df.isnull().sum())

#그래프 플롯
def plotdata():
    sns.set_style("whitegrid")
    sns.set_palette(sns.color_palette("Accent",10))
    sns.lmplot('Age','Survived',hue='Sex',data=df.loc[df['Age']>=12]) #12살 이상은 점점 생존률 올라감.
    sns.lmplot('Family','Survived',hue='Sex',data=df)
    sns.lmplot('Fare','Survived',hue='Sex',data=df) #Fare가 높을수록 생존률 높다.
plotdata()
#Cabin,Embarked,Pclass는 factor다.
#아래 두 줄은 char요소를 숫자로 바꾸는 방법이다.
#df['Cabin'] = df['Cabin'].map({'A':0,'B':1,'C':2,'D':3,'E':4,'F':5,'G':6,'N':7,'T':8})
#df['Embarked'] = df['Embarked'].map({'S':0,'C':1,'Q':2})
#0, 1, 2... 식으로 들어가도 factor로 인식 하지 않고 상수로 인식하는것으로 추측된다다.
#때문에 각 각 변수로 분리하여 맞으면 1 틀리면 0으로 구분한다.
Cabin_dummies = pandas.get_dummies(df['Cabin'],prefix='Cabin')
df = pandas.concat([df,Cabin_dummies],axis=1)
df.drop('Cabin',axis=1,inplace=True)
Embarked_dummies = pandas.get_dummies(df['Embarked'],prefix='Embarked')
df = pandas.concat([df,Embarked_dummies],axis=1)
df.drop('Embarked',axis=1,inplace=True)
Pclass_dummies = pandas.get_dummies(df['Pclass'],prefix='Pclass')
df = pandas.concat([df,Pclass_dummies],axis=1)
df.drop('Pclass',axis=1,inplace=True)

#Survived 변수 분리, Sex female=0, male=1 변환
df['Sex'] = df['Sex'].map({'female':0,'male':1})
df_X=df.drop('Survived',axis=1)
df_X=df_X.values
df_Y=df['Survived']
df_Y=df_Y.values

sum=0
n=10
kf=KFold(n_splits=n, shuffle=True)
for train,test in kf.split(df_X):
    df_X_train, df_X_test = df_X[train], df_X[test]
    df_Y_train, df_Y_test = df_Y[train], df_Y[test]
    Y_pred = GNB().fit(df_X_train,df_Y_train).predict(df_X_train)
    scores = cvs(GNB(), df_X_train, df_Y_train, cv=kf, scoring='accuracy')
    #print("Train:",train,"Test:",test)
for i in range(0,n):
      sum+=scores[i]
accuracy=sum/n #매번 다르지만 0.75정도 나온다.
#print(scores)
print(accuracy)
plt.show()
