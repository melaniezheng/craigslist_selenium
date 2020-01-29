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

    def load_url(self):
        self.driver.get(self.generate_url())
        try:
            wait = WebDriverWait(self.driver, self.delay)
            wait.until(EC.presence_of_element_located((By.ID, "searchform")))
        except TimeoutException:
            print("Loading took too much time")

    def extract_posts(self):
        all_posts = self.driver.find_elements_by_class_name("result-info")
        total_count = self.driver.find_elements_by_class_name("totalcount")

        dates = []
        titles = []
        prices = []
        neighborhoods = []
        sizes = []
        bedrooms = []
        for count in total_count:
            pages = count.text#.split("$")
            #page = pages[0]
            print("PAGE: " + pages)

        for post in all_posts:
            result = post.text.split("$")
            # print(result)

            if len(result)==2:
                title_str, price_str = result
                date_list = title_str.split(" ")
                title_str = ' '.join(date_list[2:])
            elif len(result)==3:
                date_str, title_str, price_str = result
                date_list = date_str.split(" ")
            elif len(result) <2:
                print("could not split by $")

            month = date_list[0].replace(" ","")
            day = date_list[1].replace(" ","")
            date = month + " " + day

            
            print("TITLE: " + title_str)
            # print("DATE: " + date)

            price_list = price_str.split("-")
            size = ''
            bedroom = ''
            if len(price_list) == 3:
                price, size, neighborhood = price_list
            elif len(price_list) == 2:
                price, neighborhood = price_list
                size = 'NA'
            elif len(price_list) == 1: #['2675 (Upper East Side)']
                price_list = price_list[0].split(" ")
                price = price_list[0]
                neighborhood = ' '.join(price_list[1:])
            else:
                print(" -------- need to analyze more ------")
                print(price_list)
            
            if len(price.split(" "))>=2:
                bedroom = price.split(" ")[1]
                price = price.split(" ")[0]

            # print("PRICE: " + price)
            # print("SIZE: " + size)
            # print("HOOD: " + neighborhood)
            # print("BEDROOM: " + bedroom)

            dates.append(date)
            titles.append(title_str)
            prices.append(price)
            sizes.append(size)
            neighborhoods.append(neighborhood)
            bedrooms.append(bedroom)
            
        return dates, titles, prices, bedrooms, sizes, neighborhoods

    def extract_post_urls(self):
        url_list = []
        datetime_list = []
        html_page = urllib.request.urlopen(self.generate_url())
        soup = BeautifulSoup(html_page, "lxml")
        for link in soup.findAll("a", {"class": "result-title hdrlnk"}):
            # print(link["href"])
            url_list.append(link["href"])
        for time in soup.findAll("time", {"class": "result-date"}):
            # print(time["datetime"])
            datetime_list.append(time["datetime"])
        return url_list, datetime_list
        
    def quit(self):
        self.driver.close()
    
    
    def write_to_tsv(self, boro, date, datetime_list, url, title, price, boro_lst, bedrooms, size, neighborhood):
        now = datetime.now()
        #fields = ['date','url','title','price','boro','bedrooms','size','neighborhood']
        file_date = now.strftime("%Y-%m-%d")

        output = "./data/" + file_date + "_" + boro + "_temp.tsv"
        rows = zip(date, datetime_list, url, title, price, boro_lst, bedrooms, size, neighborhood)
        with open(output, "w") as f:
            tsv_writer = csv.writer(f, delimiter='\t')
            for row in rows:
                tsv_writer.writerow(row)


location = "newyork"
boro_list = ["mnh","brk","que","brx","stn"] #brk for BK, que for Queens, brx for Bronx, stn for Staten Island
min_price = "500"
max_price = "10000"
min_bedrooms = "0"
max_bedrooms = "1"

for boro in boro_list:
    print(boro)
    scraper = CraiglistScraper(boro, min_price, max_price, min_bedrooms, max_bedrooms)
    scraper.load_url()
    date, title, price, bedrooms, size, neighborhood = scraper.extract_posts()
    url_list, datetime_list = scraper.extract_post_urls()
    scraper.quit()
    boro_lst = list(boro) * len(url_list)
    scraper.write_to_tsv(boro, date, url_list, datetime_list, title, price, boro_lst, bedrooms, size, neighborhood)