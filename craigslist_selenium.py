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

class Listing(object):
    def __init__(self, url, title, date, boro, hood, price, size, bedrooms, cats_ok, dogs_ok, no_fee):
        self.url = url
        self.title = title
        self.date = date
        self.boro = boro
        self.hood = hood
        self.price = price
        self.size = size
        self.bedrooms = bedrooms
        self.cats_allowed = cats_ok
        self.dogs_allowed = dogs_ok
        self.no_fee = no_fee

    def __repr__(self):
        return {'url':self.url, 'title':self.title, 'date':self.date, 'boro':self.boro, 'neighborhood':self.hood, 'price':self.price, 'size':self.size, 'bedrooms':self.bedrooms, \
            'cats_allowed':self.cats_allowed, 'dogs_allowed':self.dogs_allowed, 'no_fee':self.no_fee}


class CraiglistScraper(object):
    def __init__(self, boro, url, dogs_ok = 0, cats_ok = 1, no_broker_fee = 0):
        self.boro = boro
        self.url = url
        self.dogs_ok = dogs_ok
        self.cats_ok = cats_ok
        self.no_broker_fee = no_broker_fee
        self.driver = webdriver.Chrome("/Users/melaniezheng/Downloads/chromedriver")
        self.delay = 5
  
    def generate_url_lst(self, url):
        self.driver.get(url) 
        try:
            wait = WebDriverWait(self.driver, self.delay)
            wait.until(EC.presence_of_element_located((By.ID, "searchform")))
        except TimeoutException:
            print("First URL Loading took too much time")

        total_count = self.driver.find_elements_by_class_name("totalcount")
        for count in total_count: # get total count of listings
            pages = count.text
            print("PAGE: " + pages) # for debugging 

        url_lst = []
        pages = int(pages)
        for page in range(0, pages, 120):
            page = str(page)
            url_pp = url + "&s=" + page
            url_lst.append(url_pp)  # generate url list

        return url_lst

    def load_url(self, url):
        self.driver.get(url)
        try:
            wait = WebDriverWait(self.driver, self.delay)
            wait.until(EC.presence_of_element_located((By.ID, "searchform")))
        except TimeoutException:
            print("Loading took too much time")

    def filter_ok_listings(self, url):
        html_page = urllib.request.urlopen(url)
        soup = BeautifulSoup(html_page, "lxml")
        filter_ok_listings=set()
        for result in soup.findAll("p", {"class": "result-info"}):
            try:
                url = result.find("a", {"class": "result-title hdrlnk"})
                url = url["href"]
            except:
                continue
            filter_ok_listings.add(url)
        return filter_ok_listings

    def extract_posts(self, url, dogs_ok_listings, cats_ok_listings, no_fee_listings):
        html_page = urllib.request.urlopen(url)
        soup = BeautifulSoup(html_page, "lxml")
        count = 0
        single_page_result=[]
        for result in soup.findAll("p", {"class": "result-info"}):
            count = count + 1
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
            lt.find('span', {'class':'housing'}).string.split("-")
                if len(housing)>1:
                    bedrooms = housing[0]
                    size = housing[1]
            except:
                size = ""
            if url in cats_ok_listings:
                cats_ok = "1"
            else:
                cats_ok = "0"
            if url in dogs_ok_listings:
                dogs_ok = "1"
            else:
                dogs_ok = "0"
            if url in no_fee_listings:
                no_fee = "1"
            else:
                no_fee = "0"

            listing = Listing(url, title, post_date, boro, neighborhood, price, size, bedrooms, cats_ok, dogs_ok, no_fee)
            single_page_result.append(listing.__repr__())

        print("COUNT result-row: ") # for debugging
        print(count)
        return single_page_result
        
    def quit(self):
        self.driver.close()
    
    @staticmethod
    def write_to_tsv(all_posts, location):
        now = datetime.now()
        file_date = now.strftime("%Y-%m-%d")
        filepath = "./data/" + location + "_"+ file_date + "_" + boro + ".tsv"
        fieldnames = ['url','title','date','boro','neighborhood','price','size','bedrooms','cats_allowed','dogs_allowed', 'no_fee']
        try:
            with open(filepath, "w") as output_file:
                dict_writer = csv.DictWriter(output_file, fieldnames=fieldnames, delimiter='\t')
                dict_writer.writeheader()
                dict_writer.writerows(all_posts)  
        except ValueError:
            print("Problem writing to tsv file")



cities = {}
cities['newyork'] = ["mnh","brk","que","brx","stn"]
cities['losangeles'] = ["wst","sfv","lac","sgv","lgb","ant"] 
cities['chicago'] = ["chc","nch", "wcl", "sox", "nwi", "nwc"]
cities['sfbay'] = ["sfc", "sby", "eby", "pen", "nby", "scz"]
min_price = "500"
max_price = "5000"
min_bedrooms = "0"
max_bedrooms = "6"

for city in cities:
    location = city
    boro_list = cities[city]
    for boro in boro_list:
        url = f"https://{location}.craigslist.org/search/{boro}/apa?&min_price={min_price}&max_price={max_price}&min_bedrooms={min_bedrooms}&max_bedrooms={max_bedrooms}"

        scraper = CraiglistScraper(boro, url)
        
        # print(url_lst) for debugging purpose
        dog_allowed_url = url + "&pets_dog=1"
        dog_allowed_url_lst = scraper.generate_url_lst(dog_allowed_url)
        dogs_ok_listings = set()
        for dogs_allowed_url in dog_allowed_url_lst:
            scraper.load_url(dogs_allowed_url)
            dogs_ok_listings.update(scraper.filter_ok_listings(dogs_allowed_url))

        cat_allowed_url = url + "&pets_cat=1"
        cat_allowed_url_lst = scraper.generate_url_lst(cat_allowed_url)
        cats_ok_listings = set()
        for cat_allowed_url in cat_allowed_url_lst:
            scraper.load_url(cat_allowed_url)
            cats_ok_listings.update(scraper.filter_ok_listings(cat_allowed_url))

        no_fee_url = url + "&broker_fee=1"
        no_fee_url_lst = scraper.generate_url_lst(no_fee_url)
        no_fee_listings = set()
        for no_fee_url in no_fee_url_lst:
            scraper.load_url(no_fee_url)
            no_fee_listings.update(scraper.filter_ok_listings(no_fee_url))
        
        url_lst = scraper.generate_url_lst(url)
        all_posts = []
        for url in url_lst:
            scraper.load_url(url)
            all_posts.append(scraper.extract_posts(url, dogs_ok_listings, cats_ok_listings, no_fee_listings))
        all_posts = list(it.chain(*all_posts))
        print("length of all posts: ") # debugging
        print(len(all_posts)) # debugging
        CraiglistScraper.write_to_tsv(all_posts,location)

scraper.quit()

