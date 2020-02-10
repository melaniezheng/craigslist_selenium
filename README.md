# craigslist_selenium

web scraping craigslist apartment listings for major us cities using selenium and beautiful soup.
craigslist.py is the main file to run.
listings.py - creates a class for fields to be scraped.
selenium_scrapers - creates a scraper class for the selenium along with additional methods.
user can update city dictionary in the craigslist.py to scrape other cities on craigslist as well.
all the data are saved in /data folder.
combine_data_to_tsv.py will combine all the listings from all cities into one file.
cleaning.py file is located in another repo (renthop_scrapy) and performs the data cleaning using pandas and numpy.
visualization.R - using R to perform data visualization
