from selenium import webdriver
from selenium.webdriver.common.keys import Keys
driver = webdriver.Chrome('chromedriver.exe')
driver.get('https://naver.com')
delay = 3
driver.implicitly_wait(delay)

your_id = ""
password = ""

element_login = driver.find_element_by_class_name('lg_local_btn') # 로그인 창 클릭 
element_login.click()

element_id = driver.find_element_by_id("id") # id 텍스트 입력 상자
element_id.clear() # 해당 부분을 지워둬라 

driver.implicitly_wait(delay)

element_pw = driver.find_element_by_id("pw") # 비밀번호 텍스트 입력 상자
element_pw.clear() # 해당 부분을 지워둬라 

element_et = driver.find_element_by_class_name('btn_global')

element_id.send_keys(your_id) # 아이디 입력
element_pw.send_keys(password) # 비밀번호 입력

element_et.submit() # 로그인 버튼 클릭

driver.find_element_by_class_name('an_icon').click() # 메일 페이지 열기
titles = driver.find_elements_by_class_name('mail_title')

a = driver.find_elements_by_partial_link_text("광고") # "광고"라는 글자가 들어간 메일을 모두 찾아라 
a[1].text # 2번째 광고 보여줘 
