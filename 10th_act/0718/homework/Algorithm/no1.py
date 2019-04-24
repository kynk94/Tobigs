import random
import csv
import time
import os

#명령창으로부터 input값을 얻어 n에 저장한다.
#n이 1000,2000으로 클수록 생각보다 오래 걸리길래, 몇 초가 걸렸는지 궁금해서 시간을 확인했다.
#n=1000 - 1.859초, n=3000 - 14.752초 
n=int(input("삼각형의 크기(층)를 입력하시오 : "))
cal_start = time.time()

#Input값을 토대로 n층의 랜덤한 삼각형을 만들어, triangle.csv에 저장한다.
with open('triangle.csv', 'w', newline='') as fw:
    wr = csv.writer(fw,delimiter=',')
    for i in range(1,n+1):
        a=[]
        j = 1
        while j <= i:
            a.append(random.randint(0,99))
            j=j+1
        wr.writerow(a)
fw.close

#저장한 triangle.csv를 읽어오며 각 row는 하나의 배열이므로 a배열은 2차원 배열이 된다.
with open('triangle.csv', 'r') as fo:
    reader_csv = csv.reader(fo)
    i=0
    for row in reader_csv:
        a[i]=row
        i=i+1
        if n <= 30:
            print(row)
i=n

#triangle을 불러온 뒤 경로합을 구하기 위해 아래층에서부터 올라간다.
#수열의 개념이므로, i에 대해 일반화 하면 알고리즘을 만들 수 있다.
#i-1층의 j번째에 i층의 j,j+1번째 수 중 더 큰 값을 더하면 i층을 지우고 i-1번째 층까지만 생각 해도 된다.
#이 개념으로 맨 윗층까지 반복하면 맨 윗층은 i층까지의 최대 경로합과 같아진다.
while i >= 2:
    #print(a[i-1])
    for j in range(0,i-1):
        a[i-2][j]=int(a[i-2][j])+max(int(a[i-1][j]),int(a[i-1][j+1]))
        #print(a[i-2][j])
    i=i-1
    
elapsed = int(1000*(time.time() - cal_start))

#맨 윗층인 a[0][0]이 최대 경로합이다.
print('Calculate time : %d ms\n최대 경로합 : %d'%(elapsed,a[0][0]))
os.system('Pause')
