a = [10,5,20,40,22,25,30,15,50] #기존 문제에 5,22 추가
#[10,20,25,30,50]
#[5,20,22,25,30,50] 가장 긴 수열
print(a)
n = len(a)

#수열길이를 확인하기 위해 a와 크기가 같게 b배열을 만든다.
b=[]
for i in range(0,n):
    b.append(1)
#print(b)

#가장 긴 수열의 제일 처음은 무조건 a[0]이 되므로 i는 1부터 시작한다.
#a[i]와 이전의 값들과 비교하며 a[i]가 더 크고, b[i]가 이전의 값과 같거나 작으면, b[i]에 이전의 값+1을 한다.
#위 말의 뜻은 더 긴 수열이 확인됐다는 뜻이며, b의 가장 큰 값이 가장 긴 수열의길이가 된다.
for i in range(1,n):
    for j in range(0,i):
        if a[j]<a[i] and b[i]<=b[j]:
            b[i]=b[j]+1
#print(b)
print('가장 긴 수열의 길이 = %d'%max(b))




#10
#5
#5 20
#5 20 40
#5 20 22
#5 20 22 25
#5 20 22 25 30
#5 20 22 25 30 50
