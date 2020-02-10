class Listing(object):
    def __init__(self, url, listing_id, repost_of, title, date, boro, hood, price, bedrooms, cats_ok, dogs_ok, no_fee, size="",):
        self.url = url
        self.listing_id = listing_id
        self.repost_of = repost_of
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
        return {'url':self.url,'listing_id':self.listing_id,'repost_of':self.repost_of, 'title':self.title, \
            'date':self.date, 'boro':self.boro, 'neighborhood':self.hood, 'price':self.price, 'size':self.size, 'bedrooms':self.bedrooms, \
            'cats_allowed':self.cats_allowed, 'dogs_allowed':self.dogs_allowed, 'no_fee':self.no_fee}
