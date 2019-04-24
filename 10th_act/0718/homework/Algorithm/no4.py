import numpy as np

#a=[10,30,5,60]
a=[30,35,15,5,10,20,25]
a=[40,30,30,50]
#크기 n인 행렬 M을 만든다.
#이때 우리는 최소 배송비를 알아야하므로, M에는 가장 큰 값을 저장한다.
#j,k=i+j 사이의 수 m에 대해 (즉, i크기의 행렬) M에 각 배송비를 더하여 대입한다.
#M[j,k]은 32비트 최대 수이므로 해당 과정을 1회 거쳤을 때 자연스럽게 배송비에 대한 값이 들어간다. 
#cost=[70,60,80]
def chopchop(array):
    n=len(array)
    M=np.zeros((n,n))
    for i in range(1,n):
        for j in range(1,n-i):
            k=i+j
            M[j,k]=pow(2,32)
            print(M)
            for m in range(j,k):
                M[j,k]=min(M[j,k],M[j,m]+M[m+1,k])#+a[j-1]*a[m]*a[k])
            #M[j,k] += cost[j]
    print(M)
    return M[1,n-1]

minfee=chopchop(a)
print('최소 배송비 : %d 원'%minfee)

