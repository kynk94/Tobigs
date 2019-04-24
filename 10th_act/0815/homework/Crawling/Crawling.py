from selenium import webdriver
from selenium.common.exceptions import *
from tqdm import trange
from bs4 import BeautifulSoup as bs
import requests
import re
import numpy as np
import pandas as pd

#크롬드라이버 옵션 설정
options = webdriver.ChromeOptions()
options.add_argument('headless')
options.add_argument('window-size=1920x1080')
options.add_argument('user-agent=Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/68.0.3440.106 Safari/537.36')
driver = webdriver.Chrome('chromedriver', chrome_options=options)
#driver.set_window_size(1080,720)
driver.implicitly_wait(2)
driver.get('https://google.com')

#구글 검색창에 autopilot을 입력한다.
elem=driver.find_element_by_id('lst-ib')
elem.clear()
elem.send_keys('autopilot')
elem.submit()
driver.implicitly_wait(1)

#검색어에 대한 뉴스를 보기 위해서 뉴스 탭으로 이동
tab = driver.find_elements_by_css_selector('a.q.qs')
for i in tab:
    if ('뉴스' in i.text):
        newstab=i.get_attribute('href')
        driver.get(newstab)
        break

#pagenum까지 페이지를 넘기며 url을 저장한다.
print('Get URL')
pagenum=10
article_urls=[]
if (driver.find_element_by_css_selector('td.cur').text=='1'):
    article = driver.find_elements_by_css_selector('a.l.lLrAF')
    article_urls = [i.get_attribute('href') for i in article]
for i in trange(pagenum):
    page=driver.find_elements_by_css_selector('a.fl')
    for j in page:
        if j.text==str(i+1):
            alpha=j.get_attribute('href')
            driver.get(alpha)
            article = driver.find_elements_by_css_selector('a.l.lLrAF')
            article_urls.extend([k.get_attribute('href') for k in article])
            break
print(f'10%% of URL Print\n{article_urls[0:len(article_urls)/10]}\n')

#각 url에 접속해서 기사 전문을 크롤링한다.
#try, except를 통해 에러가 나지 않도록 코드를 짠다.
#약 22분 소요
print('Start Crawling')
c=[]
for i in trange(len(article_urls)):
    try:
        driver.get(article_urls[i])
        #StaleElementReferenceException은 Element를 찾을 때 아무것도 없으면 발생하는 것 같다.
        #한번 더 찾으면 에러가 안생기기 때문에 Except에서 다시 찾게 한다.
        try:
            atext=driver.find_elements_by_css_selector('p')
            a=[j.text for j in atext]
        except StaleElementReferenceException:
            atext=driver.find_elements_by_css_selector('p')
            a=[j.text for j in atext]
        b=''
        for k in range(0,len(a)):
            b=b+a[k]
        #크롤링한 기사가 100자보다 작으면 문제가 발생한것으로 인식하여 드라이버를 일시정지시키고 크롤링을 다시 실행한다.
        if(len(b)<=100):
            driver.implicitly_wait(5)
            try:
                atext=driver.find_elements_by_css_selector('p')
                a=[j.text for j in atext]
            except StaleElementReferenceException:
                atext=driver.find_elements_by_css_selector('p')
                a=[j.text for j in atext]
            b=''
            for k in range(0,len(a)):
                b=b+a[k]
            #만약 크롤링을 다시 했는데에도 기사가 100자보다 작다면 Selenium에서 읽어오지 못하는거로 생각했다.
            #수정할 방법이 있겠지만 그냥 강제로 ValueError를 발생시켜 except로 넘겨버린다.
            if(len(b)<=100):
                int('Error')
        c.append(b)
    #ValueError가 발생한다면 requests를 사용해서 해당 url에 접속하여 기사를 크롤링한다.
    except ValueError:
        try:
            response = requests.get(article_urls[i])
            html = response.text
            soup=bs(html,'html.parser')
            b=''
            for tit in soup.find_all('p')[1:]:
                b=b+str(tit.find(text=True))
            c.append(b)
        #requests를 사용하여 크롤링할 때 ValueError가 발생하면 해당 url에 대해서는 크롤링하지 않는다.
        #대신에 확인 할 수 있게 url만 저장해 둔다.
        except ValueError:
            error=article_urls[i]
            c.append(error)

#크롤링이 끝났으므로 드라이버를 종료한다.
driver.quit()

#기사 전문에 대해 \n과 \r을 찾아서 띄어쓰기로 바꿔준다.
for i in range(0,len(c)):
    c[i]=re.sub('\n|\r',' ',c[i])

#결과를 데이터프레임으로 만들어 csv로 저장한다.
data=np.array([article_urls,c])
pd.DataFrame(data.T,columns=['URL','Article']).to_csv('Google-autopilot.csv',index=False)
