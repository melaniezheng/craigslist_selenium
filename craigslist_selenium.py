from listings import Listing
from selenium_scraper import CraiglistScraper
import re
import itertools as it

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
        scraper.write_to_tsv(all_posts,location)

scraper.quit()

