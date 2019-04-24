def count_alphabet(s):
    alphabet = ['A','B','C','D','E','F','G','H','I','J','K','L','M','N','O','P','Q','R','S','T','U','V','W','X','Y','Z']
    ac = [0]*len(alphabet)
    count=0
    count1=0
    i=0
    #입력받은 문자를 모두 대문자로 바꾼다.
    s=s.upper()
    #alphabet 리스트와 비교하며 개수를 센다.
    for c in alphabet:
        count = s.count(c)
        ac[i]=count
        i=i+1
    #최대값을 가지는 알파벳을 반환하며 만약 최대값이 중복되면 ??를 반환한다.
    for i in range(0,len(alphabet)):
        if max(ac)==ac[i]:
            count1=count1+1
            j=i
    if count1 >= 2:
        return('??')
    else:
        return(alphabet[j])
#계속 반복하되 \n이 입력되면 종료한다.
while True:
    s=input('If you want to escape, just hit the Enter key.\nType String : ')
    if s == '':
        break
    ss = count_alphabet(s)
    print(ss)
