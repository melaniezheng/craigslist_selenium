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
