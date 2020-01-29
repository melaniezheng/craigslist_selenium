from selenium import webdriver
from selenium.webdriver.support.ui import WebDriverWait
from selenium.webdriver.support import expected_conditions as EC
from selenium.webdriver.common.by import By
from selenium.common.exceptions import TimeoutException
from datetime import datetime

from bs4 import BeautifulSoup
import urllib.request
import pandas as pd
import csv
import re
import traceback
import itertools as it


class CraiglistScraper(object):
    def __init__(self, boro, min_price, max_price, min_bedrooms, max_bedrooms, dogs_ok = 0, cats_ok = 0, no_broker_fee = 0):
        self.boro = boro
        self.max_price = max_price
        self.min_price = min_price
        self.min_bedrooms = min_bedrooms
        self.max_bedrooms = max_bedrooms
        self.dogs_ok = dogs_ok
        self.cats_ok = cats_ok
        self.no_broker_fee = no_broker_fee
        self.driver = webdriver.Chrome("/Users/melaniezheng/Downloads/chromedriver")
        self.delay = 5

    def generate_url(self):
        # to define more rules using dogs ok, cats ok, etc...
        url = ''
        starter = f"https://{location}.craigslist.org/search/{self.boro}/apa?&min_price={self.min_price}&max_price={self.max_price}&min_bedrooms={self.min_bedrooms}&max_bedrooms={self.max_bedrooms}"
        if self.dogs_ok == 0 and self.cats_ok == 0 and self.no_broker_fee == 0:
            url = starter
        elif self.dogs_ok == 0 and self.cats_ok == 0 and self.no_broker_fee == 1:
            url = starter + "&broker_fee=1"
        elif self.dogs_ok == 1 and self.cats_ok == 0 and self.no_broker_fee == 0:
            url = starter + "&pets_dog=1"
        elif self.dogs_ok == 0 and self.cats_ok == 1 and self.no_broker_fee == 0:
            url = starter + "&pets_cat=1"
        elif self.dogs_ok == 1 and self.cats_ok == 1 and self.no_broker_fee == 1:
            url = starter + "&pets_cat=1&pets_dog=1&broker_fee=1"
        return url

    def get_total_count(self):
        self.driver.get(self.generate_url())
        try:
            wait = WebDriverWait(self.driver, self.delay)
            wait.until(EC.presence_of_element_located((By.ID, "searchform")))
        except TimeoutException:
            print("First URL Loading took too much time")
    
        total_count = self.driver.find_elements_by_class_name("totalcount")
        for count in total_count:
            pages = count.text#.split("$")
            #page = pages[0]
            print("PAGE: " + pages)

        return int(pages)

    def generate_url_lst(self, count):
        # generate a list of urls for each page...
        url_lst = []
        url = self.generate_url()
        for page in range(0, count, 120):
            page = str(page)
            url_pp = url + "&s=" + page
            url_lst.append(url_pp)
        return url_lst

    def load_url(self, url):
        self.driver.get(url)
        try:
            wait = WebDriverWait(self.driver, self.delay)
            wait.until(EC.presence_of_element_located((By.ID, "searchform")))
        except TimeoutException:
            print("Loading took too much time")


    def extract_posts(self, single_page_result=[]):

        html_page = urllib.request.urlopen(self.generate_url())
        soup = BeautifulSoup(html_page, "lxml")
        for result in soup.findAll("p", {"class": "result-info"}):
            single_post = {}
            try:
                url = result.find("a", {"class": "result-title hdrlnk"})
                url = url["href"]
            except:
                continue
            try:
                post_date = result.find("time", {"class": "result-date"})
                post_date = post_date["datetime"]
            except:
                post_date = ""
            try: 
                title = result.find('a').string
            except:
                title = ""
            try:
                price = result.find('span', {'class':'result-price'}).string
            except:
                price = ""
            try:
                neighborhood = result.find('span', {'class':'result-hood'}).string.strip()
            except:
                neighborhood = ""
            try:
                bedrooms = result.find('span', {'class':'housing'}).string
            except:
                bedrooms = ""
            try:
                housing = result.find('span', {'class':'housing'}).string.split("-")
                if len(housing)>1:
                    bedrooms = housing[0]
                    size = housing[1]
            except:
                size = ""

            single_post['url'] = url
            single_post['post_date'] = post_date
            single_post['title'] = title
            single_post['price'] = price
            single_post['boro'] = self.boro
            single_post['neighborhood'] = neighborhood
            single_post['bedrooms'] = bedrooms
            single_post['size'] = size
            single_post['dogs_ok'] = self.dogs_ok
            single_post['cats_ok'] = self.cats_ok
            single_post["no_broker_fee"] = self.no_broker_fee

            single_page_result.append(single_post)
        return single_page_result
        
    def quit(self):
        self.driver.close()

    def write_to_tsv(self, all_posts):
        now = datetime.now()
        file_date = now.strftime("%Y-%m-%d")
        filepath = "./data/" + file_date + "_" + boro + "_dogs_" + str(self.dogs_ok) + \
            "cats_" + str(self.cats_ok) + "fee_" + str(self.no_broker_fee) + "_temp.tsv"
        fieldnames = ['url','post_date','title','price','boro','neighborhood','bedrooms','size','dogs_ok', 'cats_ok','no_broker_fee']
        with open(filepath, "w") as output_file:
            dict_writer = csv.DictWriter(output_file, fieldnames=fieldnames, delimiter='\t')
            dict_writer.writeheader()
            dict_writer.writerows(all_posts)  

location = "newyork"
boro_list = ["mnh","brk","que","brx","stn"] #brk for BK, que for Queens, brx for Bronx, stn for Staten Island
min_price = "500"
max_price = "10000"
min_bedrooms = "0"
max_bedrooms = "6"

for boro in boro_list:
    print(boro)
    scraper = CraiglistScraper(boro, min_price, max_price, min_bedrooms, max_bedrooms)
    count = scraper.get_total_count()
    url_lst = scraper.generate_url_lst(count)
    print(url_lst)
    all_posts = []
    for url in url_lst:
        scraper.load_url(url)
        all_posts.append(scraper.extract_posts())
    all_posts = list(it.chain(*all_posts))
    print("length of all posts: ")
    print(len(all_posts))
    scraper.write_to_tsv(all_posts)
scraper.quit()