import pandas as pd
from tqdm import tqdm, trange
from konlpy.tag import *
from gensim.models import Word2Vec
from sklearn.cluster import KMeans
import time

kkma=Kkma()

df = pd.read_csv('tw_Cr.csv')

#필요없는 열 삭제
print(df.columns)
df.drop('Unnamed: 0',axis=1,inplace=True)
df.drop('from',axis=1,inplace=True)
df.drop('Date',axis=1,inplace=True)

# : 로 구분해서 split하고 트위터 id는 삭제.
data=pd.DataFrame(list(df['x'].str.split(': ')))[1].values

#tokenize 함수 설정
#먼저 pos으로 tokenize를 해봤는데 최종 결과를 보니 필요없겠다 싶어 nouns으로 바꿨다.
def tokenize(doc):
    #return ['/'.join(t) for t in kkma.pos(doc)]
    return kkma.nouns(doc)

#전체 data를 tokenize한다.
#kkma를 사용했기 때문에 1시간 20분 소요
#okt를 쓰면 훨씬 빠르게 끝나던데 결과를 봤을 때 눈에 띄는 차이는 모르겠다.
train = [tokenize(data[i]) for i in trange(len(data))]
print(train[0])

#Word2Vec 모델링
embedding_model = Word2Vec(train,size=100,window=2,min_count=30,workers=4,iter=5,sg=1)
embedding_model.wv.most_similar(positive=train[0],topn=5)

#kmeans로 분류
start = time.time()
word_vectors = embedding_model.wv.vectors
num_clusters = word_vectors.shape[0]/50
num_clusters = int(num_clusters)
kmeans_clustering = KMeans(n_clusters=num_clusters)
idx=kmeans_clustering.fit_predict(word_vectors)
elapsed = time.time()-start
print(f'Num of Clusters : {num_clusters} Time : {elapsed}')

#원래 군집 확인
print(pd.unique(df['Keyword']))

#15개 군집만 print하여 확인해본다.
idx=list(idx)
names = embedding_model.wv.index2word
word_centroid_map = {names[i]:idx[i] for i in range(len(names))}
for cluster in range(0,15):
    print("\nCluster {}".format(cluster))
    words=[]
    for i in range(0,len(list(word_centroid_map.values()))):
        if (list(word_centroid_map.values())[i]==cluster):
            words.append(list(word_centroid_map.keys())[i])
    print(words)
