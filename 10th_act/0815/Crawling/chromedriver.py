from selenium import webdriver

options = webdriver.ChromeOptions()
#options.add_argument('headless')
#options.add_argument('window-size=1920x1080')

options.add_argument('user-agent=Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/68.0.3440.106 Safari/537.36')
driver = webdriver.Chrome('chromedriver', chrome_options=options)

driver.get('http://naver.com')
driver.implicitly_wait(3)
'''
login_form=driver.find_element_by_id("loginForm")
#user_agent = driver.find_element_by_css_selector('user_agent').text
#print('User-Agent: ',user_agent)

driver.get_screenshot_as_file('naver_main_headless.png')

driver.quit()
'''
