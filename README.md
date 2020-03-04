## Web Scraping Apartment listings

1. web scraping craigslist apartment listings for major us cities using selenium and beautiful soup.
2. main file to run: <i>craigslist.py</i>
3. listings.py - creates a class for fields to be scraped.
4. selenium_scrapers - creates a scraper class for the selenium along with additional methods.
5. user can update city dictionary in the craigslist.py to scrape other cities on craigslist as well.
6. all the data are saved in /data folder.
7. combine_data_to_tsv.py will combine all the listings from all cities into one file.
8. cleaning.py file is located in another repo (renthop_scrapy) and performs the data cleaning using pandas and numpy.
9. presentation.pdf - interesting findings from this project

- also check out https://github.com/melaniezheng/craigslist_bs4 - webscraping nyc manhattan listing for each neighborhoods with <i>BeautifulSoup</i>.
- https://github.com/melaniezheng/renthop_scrapy manhattan rental listings on Renthop with <i>Scrapy</i>. 

As it turns out, unsurprisingly, Craigslist listings are much cheaper than the ones on Renthop with an exception of Soho, where apartments are more expensive on Craigslist. See more findings in Presentation.pdf.
