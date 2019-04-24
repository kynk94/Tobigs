#잘못된 값을 입력해도 다시 입력받게 알고리즘을 짠다.
#T 입력 시 문자가 있으면 다시 입력받는다.
#R 입력 시 처음이 숫자가 아니면 다시 입력받는다.
#R 입력 시 띄어쓰기로 구분하여 입력하지 않았으면 다시 입력받는다.
def value_select():
      while True:
            try:
                  T=int(input('Type number of Test case : '))
                  print(f'T : {T}')
                  break
            except ValueError:
                  print('Don\'t type character.')

      R=[0]*T
      for i in range(0,T):
            while True:
                  while True:
                        R[i]=input(f'Type No.{i+1} R   ex)3 aBC : ')
                        R[i]=R[i].strip()
                        try:
                              int(R[i][0])
                              try:
                                    k=0
                                    for j in range(0,len(R[i])):
                                          if R[i][j]==' ':
                                                k=j
                                    if k==0:
                                          int('a')
                                    break      
                              except ValueError:
                                    print('You must split input by space.')
                        except ValueError:
                              print('You must Type integer in 1st input.')
                  break
      return(R)

#결과를 도출하는 outR 함수
def outR(array):
      a=[]
      for i in range(0,T):
            b=[]
            a.append(array[i].split(' '))
            a[i][1]=list(a[i][1])
            n=len(a[i][1])
            for j in range(0,n):
                  b.append(a[i][1][j]*int(a[i][0]))
            a[i][1]=''
            for j in range(0,n):
                  a[i][1]=a[i][1]+b[j]
      return(a)

#main부분
R = value_select()
T = len(R)
output = outR(R)
for i in range(0,T):
      print(f'No.{i+1} R : {R[i]}   Output : {output[i][1]}')
