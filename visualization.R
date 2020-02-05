library(data.table)
library(dplyr)
library(tidyr)
library(ggplot2)
library(plotly)
library(googleVis)



data <- read.csv(file = "data.tsv", sep = "\t",stringsAsFactors = FALSE)
data <- data %>% mutate(., title_len = nchar(title))
data$bedrooms <- sub("\n|\"\"|\\s|", "", data$bedrooms)
# class(data$price) <- "dollar"
# format.dollar <- function(x, ...) paste0("$", unclass(x))
# as.data.frame.dollar <- base:::as.data.frame.factor
substr(data$time[1], 1,2)
data$time_range = ifelse(as.integer(substr(data$time,1,2))>=8 & as.integer(substr(data$time,1,2))<=15, "8AM~4PM",
                   ifelse(as.integer(substr(data$time,1,2))>=16 & as.integer(substr(data$time,1,2))<=23, "4PM~12AM","12AM~8AM"))

# ********** Clean up bedrooms ****************
data2 <- data %>% mutate(., title = tolower(title)) %>% 
  mutate(., bedrooms = ifelse(bedrooms!="",bedrooms,
                              ifelse(grepl("1/.|1x.|1bdrm|1.bdrm|one.bdrm|1.bed|one.bed|studio|1 bed|1bed|1br|1.br|1bd|1.bd|one.bed|one.bd|one.br",title), "1br",
                                     ifelse(grepl("2/.|2x.|2.bdrm|2.bed|2.br|2 bed|2bed|2br|2.br|2bd|2.bd|two.bed|two.bd|two.br",title), "2br",
                                            ifelse(grepl("3/.|3x.|3.bdrm|3.bed|3bed|3br|3.br|3bd|3.bd|three.bed|three.bd|three.br",title),"3br",
                                                   ifelse(grepl("4/.|4x.|4.bdrm|4.bed|4bed|4br|4.br|4bd|4.bd",title), "4br",
                                                          ifelse(grepl("5/.|5x.|5.bdrm|5.bed|5bed|5br|5.br|2bd|5.bd",title), "5br",bedrooms))))))) %>% filter(., city != "Dallas")



# which city has the highest average rent?
ggplot(data2 %>% group_by(., city) %>% 
         summarise(., price = round(mean(price))) %>% 
         arrange(.,desc(price)),
       aes(city,price,label=paste0("$",price))) + 
  geom_col(aes(x=reorder(city,price)), fill = '#5815AD') +
  geom_text(size = 3, color='white', position = position_stack(reverse = T,vjust = 0.5)) +
  coord_flip() +
  ggtitle("Average rental price for major cities") +
  ylab("Average Price") +
  ggsave("./ppt/price_by_city.png", width = 6, height = 4)

ggplot(data2 %>% filter(., bedrooms %in% c("1br")) %>% 
         group_by(., city) %>% 
         summarise(., price = round(mean(price))), 
       aes(city, price, label=paste0("$",price))) +
  geom_col(aes(x=reorder(city, price)), fill = '#B696FF') +
  geom_text(size = 3, position = position_stack(reverse = T,vjust = 0.5)) +
  coord_flip() +
  ggtitle("Average rental price (1 br)") +
  ylab("Average Price") +
  ggsave("./ppt/price_by_city_1br.png", width = 6, height = 4)
            
ggplot(data2 %>% filter(., bedrooms %in% c("2br","3br")) %>% 
         group_by(., city) %>% 
         summarise(., price = round(mean(price))), 
       aes(city, price, label=paste0("$",price))) +
  geom_col(aes(x=reorder(city, price)), fill = '#B696FF') +
  geom_text(size = 3, position = position_stack(reverse = T,vjust = 0.5)) +
  coord_flip() +
  ggtitle("Average rental price (2-3 br)") +
  ylab("Average Price") +
  ggsave("./ppt/price_by_city_2-3br.png", width = 6, height = 4)
  

ggplot(data2 %>% filter(., bedrooms %in% c("4br","5br","6br")) %>% 
         group_by(., city) %>% 
         summarise(., price = round(mean(price))), 
       aes(city, price, label=paste0("$",price))) +
  geom_col(aes(x=reorder(city, price)), fill = '#B696FF') +
  geom_text(size = 3, position = position_stack(reverse = T,vjust = 0.5)) +
  coord_flip() +
  ggtitle("Average rental price (4br and more)") +
  ylab("Average Price") +
  ggsave("./ppt/price_by_city_3-4br.png", width = 6, height = 4)

# which city is the most dog friendly? most cat friendly?
pet_friendly <- data %>% group_by(., city) %>% summarise(., dogs_ok = sum(dogs_allowed), cats_ok = sum(cats_allowed)) %>% 
  left_join(.,data %>% group_by(., city) %>% summarise(., total_listing=n()), by = 'city') %>% 
  mutate(., cat_friendly = round(cats_ok/total_listing*100,2),
         dog_friendly = round(dogs_ok/total_listing*100,2))

ggplot(pet_friendly %>% gather(.,key=key,value=value,c('cat_friendly','dog_friendly')), aes(x=city,y=value,label=paste0(value,'%'))) +
  coord_flip() +
  geom_col(aes(x=reorder(city, value)), fill='#5815AD') +
  geom_text(size = 3, color='#FCFBFF',position = position_stack(reverse = T,vjust = 0.5)) +
  facet_wrap(~key, scales = "free")+
  ggtitle("Pet Friendliness") +
  ylab("pet allowed listings / total listings (%)") +
  ggsave("./ppt/price_by_city_pet_friendly.png", width = 8, height = 6)
  

# Is average listing price tend to be higher due to the pet?
data2 %>% group_by(.,dogs_allowed) %>% summarise(., round(mean(price)))
data2 %>% group_by(.,cats_allowed) %>% summarise(., round(mean(price)))

# which day do people post most listings (by city)
weekday <- data2 %>% mutate(.,weekday = weekdays(as.Date(date))) %>% 
  group_by(., weekday) %>% summarise(., perc=round(n()/nrow(data)*100)) %>% 
  select(.,weekday, perc) %>% mutate(.,weekday=factor(weekday, levels=c("Monday","Tuesday","Wednesday","Thursday","Friday","Saturday","Sunday"), 
                                                      ordered = T))

plot(gvisPieChart(weekday[order(weekday$weekday),], options=list(width=1000, height=600)))

# what time of the day people post most listings?
time_range <- data %>% group_by(., time_range) %>% summarise(.,perc=round(n()/nrow(data)*100)) %>% 
  select(.,time_range,perc) %>% mutate(.,time_range=factor(time_range, levels=c("12AM~8AM","8AM~4PM","4PM~12AM")))

ggplot(time_range[order(time_range$time_range),]) +
  geom_col(aes(x=reorder(time_range,desc(perc)),y=perc), fill=c("#5815AD","#A180FA","#D9CCFF")) +
  geom_text(aes(x = time_range,y = perc + 2, label=paste0(perc,'%')), size = 4) +
  xlab("Time Range") +
  ylab("Percentage of total listings") +
  ggsave("./ppt/time_range1.png", width = 6, height = 4)
  
P <- gvisPieChart(time_range[order(time_range$time_range),],
                  options=list(
                    width=1000, 
                    height=600,
                    colors={"['#D9CCFF','#5815AD','#A180FA']"}
                    ))
plot(P) 

#2
time_range2 <- data %>% filter(., time_range =='8AM~4PM') %>% 
  mutate(., time_range2 = ifelse(as.integer(substr(time,1,2))>=8 & as.integer(substr(time,1,2))<=9,"8AM~10AM",
                                 ifelse(as.integer(substr(time,1,2))>=10 & as.integer(substr(time,1,2))<=11,"10AM~12PM",
                                 ifelse(as.integer(substr(time,1,2))>=12 & as.integer(substr(time,1,2))<=13,"12PM~2PM", "2PM~4PM")))) %>% 
  group_by(.,time_range2) %>% summarise(.,count=n()) %>% mutate(.,time_range2=factor(time_range2,levels = c("8AM~10AM","10AM~12PM","12PM~2PM","2PM~4PM"), ordered=T))

P <- gvisPieChart(time_range2[order(time_range2$time_range2),],
                  options=list(
                    width=1000, 
                    height=600,
                    colors={"['#D6C3FA','#5815AD','#7E3FCE','#AD8AEA']"},
                    title={"Percentage of Listings by hour"}))
plot(P)

# do no fee vs fee
no_fee <- data %>% filter(., bedrooms != "") %>% 
  filter(., bedrooms != "6br") %>% mutate(., no_fee = ifelse(no_fee == 1, "No Fee", "Fee")) %>% 
  group_by(., bedrooms, no_fee) %>% 
  summarise(., price = round(mean(price))) 

ggplot(no_fee, aes(bedrooms, price)) +
  geom_col(aes(fill = no_fee),position = 'dodge') +
  scale_fill_manual(values=c("#5815AD","#D7CEEE")) +
  geom_text(aes(x = bedrooms,y = price + 110, label=paste0('$',price)), size = 3,color=c('black'),position = position_dodge2(width = 1))+
  labs(fill = "")+
  ylab("average price") +
  theme(legend.position="right") +
  ggsave("./ppt/price_by_noFee.png", width = 6, height = 4)
  
# ************************** NYC *****************************
# NYC average rental price per boro vs street easy vs zillow vs renthop 

# NYC which day has the most postings? what time frame?
nyc_weekday <- data %>% mutate(.,weekday = weekdays(as.Date(date))) %>% filter(., city=="New York") %>% 
  group_by(., weekday) %>% summarise(., perc=round(n()/nrow(data %>% filter(., city=="New York"))*100)) %>% 
  select(.,weekday, perc) %>% mutate(.,weekday=factor(weekday, levels=c("Monday","Tuesday","Wednesday","Thursday","Friday","Saturday","Sunday"), 
                                                      ordered = T))

plot(gvisPieChart(nyc_weekday[order(nyc_weekday$weekday),], 
                  options=list(width=1000, height=600,
                               colors={"['#906FF2','#7E3FCE','#5815AD','#5815AD','#906FF2','#C2ACFC','#D2C2FF']"})))

nyc_time_range <- data %>% filter(., city=="New York") %>% group_by(., time_range) %>% 
  summarise(.,perc=round(n()/nrow(data %>% filter(., city=="New York"))*100)) %>% 
  select(.,time_range,perc) %>% mutate(.,time_range=factor(time_range, levels=c("12AM~8AM","8AM~4PM","4PM~12AM")))

ggplot(nyc_time_range[order(nyc_time_range$time_range),]) +
  geom_col(aes(x=reorder(time_range,desc(perc)),y=perc), fill=c("#5815AD","#A180FA","#D9CCFF")) +
  geom_text(aes(x = time_range,y = perc + 2, label=paste0(perc,'%')), size = 4) +
  xlab("Time Range") +
  ylab("Percentage of total listings") +
  ggsave("./ppt/NYC_time_range1.png", width = 6, height = 4)


nyc_time_range2 <- data %>% filter(., city=="New York") %>% mutate(., time_range2 = ifelse(as.integer(substr(time,1,2))>=8 & as.integer(substr(time,1,2))<=11,"8AM~12PM",
                                                                                           ifelse(as.integer(substr(time,1,2))>=12 & as.integer(substr(time,1,2))<=15,"12PM~4PM",
                                                                                                  ifelse(as.integer(substr(time,1,2))>=16 & as.integer(substr(time,1,2))<=19,"4PM~8PM", 
                                                                                                         ifelse(as.integer(substr(time,1,2))>=20 & as.integer(substr(time,1,2))<=23,"8PM~12AM","12AM~8AM"))))) %>% 
  group_by(., time_range2) %>% summarise(., perc = round(n()/nrow(data %>% filter(., city=="New York"))*100)) %>% 
  select(.,time_range2,perc) %>% mutate(.,time_range2=factor(time_range2, levels=c("8AM~12PM","12PM~4PM","4PM~8PM","8PM~12AM","12AM~8AM")))
P <- gvisPieChart(nyc_time_range2[order(nyc_time_range2$time_range2),],
                  options=list(
                    width=1000, 
                    height=600,
                    colors={"['#7E3FCE','#5815AD','#906FF2','#B59CFF','#D2C2FF']"}
                  ))
plot(P) 
# NYC Median Asking Rent vs Street Easy


#**************************** WORD CLOUD ********************
library(tm)
library(SnowballC)
library(wordcloud)
library(RColorBrewer)

titles <- Corpus(VectorSource(data$title))

inspect(titles)

toSpace <- content_transformer(function (x , pattern ) gsub(pattern, " ", x))
titles <- tm_map(titles, toSpace, "/")
titles <- tm_map(titles, toSpace, "@")
titles <- tm_map(titles, toSpace, "\\|")

# clean

# Convert the text to lower case
titles <- tm_map(titles, content_transformer(tolower))
# Remove numbers
titles <- tm_map(titles, removeNumbers)
# Remove english common stopwords
titles <- tm_map(titles, removeWords, stopwords("english"))
# Remove your own stop word
# specify your stopwords as a character vector
titles <- tm_map(titles, removeWords, c("blabla1", "blabla2")) 
# Remove punctuations
titles <- tm_map(titles, removePunctuation)
# Eliminate extra white spaces
titles <- tm_map(titles, stripWhitespace)
# Text stemming
titles <- tm_map(titles, stemDocument)

dtm <- TermDocumentMatrix(titles)
m <- as.matrix(dtm)
v <- sort(rowSums(m),decreasing=TRUE)
d <- data.frame(word = names(v),freq=v)
head(d, 10)

set.seed(1234)
wordcloud(words = d$word, freq = d$freq, min.freq = 100,
          max.words=200, random.order=FALSE, rot.per=0.35, 
          colors=brewer.pal(10, "Spectral"))

