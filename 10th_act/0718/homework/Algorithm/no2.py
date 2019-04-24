import random
import numpy as np

#n = 5
#a=np.array([[1,1,1,1,0],[1,1,1,1,1],[0,1,1,1,1],[1,1,1,0,1],[1,1,1,1,1]])

#크기가 n인 정사각행렬을 만들며 a에 저장한다.
n = int(input('정사각행렬의 크기를 입력하시오. : '))
a = np.zeros((n,n))
for i in range(0,n):
    for j in range(0,n):
        a[i,j]=random.randint(0,1)
print(a)

#행렬의 2행2열의 원소부터 오른쪽 아래로 차근차근 확인해 나간다.
#각 원소의 왼쪽,왼쪽위,위를 확인해 그 수가 크기가 몇인 사각형에 포함됐었는지 확인한다.
#각 원소의 해당방향의 수 중 min값+1을 자기 자신에 대입하며, 이 수가 사각형의 크기가 된다.
#대입하면서, 최대인 값을 maximum에 저장한다.
def large(matrix):
    maximum=1
    for i in range(1,n):
        for j in range(1,n):
            if not a[i-1,j-1]==0 and not a[i-1,j]==0 and not a[i,j-1]==0 and not a[i,j]==0:
                a[i,j] = min(a[i-1,j-1],a[i-1,j],a[i,j-1])+1
                if a[i,j]>maximum:
                    maximum = a[i,j]
    return pow(maximum,2)

area = large(a)

print('\n')
print(a)
print('\n')
print('Max area = %d'%area)
