import numpy as np

#명령창으로부터 지급할 금액, 동전 종류를 입력받는다.
n = int(input('지급할 금액을 입력하시오 : '))
s = input('모든 동전 단위를 입력하시오 (띄어쓰기로 구분) : ').split()
print('\n')
#n=8
#s=[1,3,5]

#중복되는 경우를 방지하기 위해 동전 종류를 오름차순으로 정렬한다.
#수식적 계산을 위해 char형인 s를 int형으로 바꾼다.
s=sorted(s)
k=len(s)
for i in range(0,k):
    s[i]=int(s[i])

#경우의 수에 대한 배열 a를 만든다.
#이 때 가장 작은 단위로 지급하는 방법 1가지는 반드시 있으므로 a[0]=1이다.
a=np.zeros(n+1)
a[0]=1

#n원에 대해, 각 동전 종류별로 j가 각 단위보다 커지면 a에 이전 수를 더하고,
#다음 동전 종류에 대해서 해당 작업을 반복한다.
for i in range(0,k):
    for j in range(1,n+1):
        if (j-s[i])>=0:
            a[j]+=a[j-s[i]]

print('지급 방법의 경우의 수는 %d 가지 입니다.'%int(a[n]))
