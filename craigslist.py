from listings import Listing
from selenium_scraper import CraiglistScraper
import re
import itertools as it

cities = {}
cities['newyork'] = ["mnh","brk","que","brx","stn"]
cities['losangeles'] = ["wst","sfv","lac","sgv","lgb","ant"] 
cities['chicago'] = ["chc","nch", "wcl", "sox", "nwi", "nwc"]
cities['sfbay'] = ["sfc", "sby", "eby", "pen", "nby", "scz"]
cities['dallas'] = ["dal", "ftw", "mdf", "ndf", "sdf"]
cities['seattle'] = ["see" ,"est" ,"sno" ,"kit" ,"tac", "oly", "skc"]
cities['boston'] = ["gbs", "nwb", "bmw", "nos", "sob"]
cities['sandiego'] = ["csd", "nsd", "esd", "ssd"]
cities['houston'] = []
cities['philadelphia'] =[]
cities['sanantonio'] = []
cities['austin'] = []
cities['denver'] = []
# add phoenix - cph evl nph wvl, sandiego - csd nsd esd ssd

price_range = [("500","2000"),("2001","7000")]
max_bedrooms = "6"

def feature_urls(boro, url):

    dog_allowed_url = url + "&pets_dog=1"
    cat_allowed_url = url + "&pets_cat=1"
    no_fee_url = url + "&broker_fee=1"
    scraper = CraiglistScraper(boro, url)

    dog_allowed_url_lst = scraper.generate_url_lst(dog_allowed_url)
    dogs_ok_listings = set()
    for dogs_allowed_url in dog_allowed_url_lst:
        scraper.load_url(dogs_allowed_url)
        dogs_ok_listings.update(scraper.filter_ok_listings(dogs_allowed_url))

    cat_allowed_url_lst = scraper.generate_url_lst(cat_allowed_url)
    cats_ok_listings = set()
    for cat_allowed_url in cat_allowed_url_lst:
        scraper.load_url(cat_allowed_url)
        cats_ok_listings.update(scraper.filter_ok_listings(cat_allowed_url))

    no_fee_url_lst = scraper.generate_url_lst(no_fee_url)
    no_fee_listings = set()
    for no_fee_url in no_fee_url_lst:
        scraper.load_url(no_fee_url)
        no_fee_listings.update(scraper.filter_ok_listings(no_fee_url))

    return tuple([scraper,dogs_ok_listings,cats_ok_listings,no_fee_listings])

for city in cities:
    # if city in set(['newyork','losangeles']):
    #     continue
    location = city
    boro_list = cities[city]
    if boro_list == []:
        boro = ""
        all_posts = []
        for prices in price_range:
            min_price, max_price = prices
            url = f"https://{location}.craigslist.org/search/apa?&min_price={min_price}&max_price={max_price}&max_bedrooms={max_bedrooms}"
            scraper, dogs_ok_listings, cats_ok_listings, no_fee_listings = feature_urls(boro, url)
            url_lst = scraper.generate_url_lst(url) # generate urls for all pages
            if url_lst == []:
                continue
            for url in url_lst:
                scraper.load_url(url)
                all_posts.extend(scraper.extract_posts(url, dogs_ok_listings, cats_ok_listings, no_fee_listings))
        print(f"{location} has {len(all_posts)} listings.") # debugging
        scraper.write_to_tsv(all_posts,location)
    else:
        all_posts = []
        for boro in boro_list:
            for prices in price_range:
                min_price, max_price = prices
                url = f"https://{location}.craigslist.org/search/{boro}/apa?&min_price={min_price}&max_price={max_price}&max_bedrooms={max_bedrooms}"
                scraper, dogs_ok_listings, cats_ok_listings, no_fee_listings = feature_urls(boro, url)
                url_lst = scraper.generate_url_lst(url) # generate urls for all pages
                if url_lst == []:
                    continue
                for url in url_lst:
                    scraper.load_url(url)
                    all_posts.extend(scraper.extract_posts(url, dogs_ok_listings, cats_ok_listings, no_fee_listings))
        print(f"{location} has {len(all_posts)} listings.") # debugging
        scraper.write_to_tsv(all_posts,location)
scraper.quit()

