from selenium import webdriver
from selenium.webdriver.support.ui import WebDriverWait
from selenium.webdriver.support import expected_conditions as EC
from selenium.webdriver.common.by import By
from selenium.common.exceptions import TimeoutException
from datetime import datetime

from bs4 import BeautifulSoup
import urllib.request
import csv
import re
import itertools as it

from listings import Listing

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
        try:
            self.driver.get(url)
        except Exception as e:
            print(f"ERROR: Chrome timeout, url {url}")
            print(f"ERROR: {e}")
        try:
            wait = WebDriverWait(self.driver, self.delay)
            wait.until(EC.presence_of_element_located((By.ID, "searchform")))
        except TimeoutException:
            print(f"Chrome Timeout --- delay is set to: {self.delay}")

        total_count = self.driver.find_elements_by_class_name("totalcount")
        for count in total_count: # get total count of listings
            pages = count.text
            print("PAGE: " + pages) # for debugging 

        url_lst = []
        try:
            pages = int(pages)
            for page in range(0, pages, 120):
                page = str(page)
                url_pp = url + "&s=" + page
                url_lst.append(url_pp)  # generate url list
        except UnboundLocalError:
            print('no result')

        return url_lst

    def load_url(self, url):
        try:
            self.driver.get(url)
        except Exception as e:
            print(f"Chrome Timeout, url: {url}")
            print(f"ERROR: {e}")
        try:
            wait = WebDriverWait(self.driver, self.delay)
            wait.until(EC.presence_of_element_located((By.ID, "searchform")))
        except TimeoutException:
            print(f"Chrome Timeout --- delay is set to: {self.delay}")

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
        for result in soup.findAll("li", {"class": "result-row"}):
            count = count + 1
            try:
                url = result.find("a", {"class": "result-title hdrlnk"})
                url = url["href"]
            except:
                continue
            try:
                listing_id = result["data-pid"]
            except:
                listing_id = ""
            try:
                repost_of = result["data-repost-of"]
            except:
                repost_of = ""
            try:
                post_date = result.find("time", {"class": "result-date"})
                post_date = post_date["datetime"]
            except:
                post_date = ""
            try: 
                a = result.find('p', {'class': 'result-info'})
                title = a.find('a').string.strip()
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
                bedrooms = result.find('span', {'class':'housing'}).text
                #print(f'bedrooms: {bedrooms}')
            except:
                bedrooms = ""
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

            listing = Listing(url, listing_id, repost_of, title, post_date, self.boro, neighborhood, price, bedrooms, cats_ok, dogs_ok, no_fee)
            single_page_result.append(listing.__repr__())

        return single_page_result


    def quit(self):
        self.driver.close()

    def write_to_tsv(self, all_posts, location):
        now = datetime.now()
        file_date = now.strftime("%Y-%m-%d")
        filepath = "./data/craigslist_" + file_date + "_" + location + ".tsv"
        fieldnames = ['url','listing_id','repost_of','title','date','boro','neighborhood','price','size','bedrooms','cats_allowed','dogs_allowed', 'no_fee']
        try:
            with open(filepath, "w") as output_file:
                dict_writer = csv.DictWriter(output_file, fieldnames=fieldnames, delimiter='\t')
                dict_writer.writeheader()
                dict_writer.writerows(all_posts)  
        except ValueError:
            print("Problem writing to tsv file")