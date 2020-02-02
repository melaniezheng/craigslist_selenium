library(data.table)
library(dplyr)
library(tidyr)
library(ggplot2)
library(plotly)
library(googleVis)


# pre-process data and save to .csv. original dataset is 1GB which is too large for shiny app.
data <- read.csv(file = "data.tsv", sep = "\t",stringsAsFactors = FALSE)
data <- data %>% mutate(., title_len = nchar(title))
data$bedrooms <- sub("\n|\"\"|\\s|", "", data$bedrooms)
# class(data$price) <- "dollar"
# format.dollar <- function(x, ...) paste0("$", unclass(x))
# as.data.frame.dollar <- base:::as.data.frame.factor
substr(data$time[1], 1,2)
data$time_range = ifelse(as.integer(substr(data$time,1,2))>=8 & as.integer(substr(data$time,1,2))<=15, "8AM~4PM",
                   ifelse(as.integer(substr(data$time,1,2))>=16 & as.integer(substr(data$time,1,2))<=23, "4PM~12AM","12AM~8AM"))

# which city has the highest average rent?
ggplot(data %>% group_by(., city) %>% 
         summarise(., price = round(mean(price))) %>% 
         arrange(.,desc(price)),
       aes(city,price,label=paste0("$",price))) + 
  geom_col(aes(x=reorder(city,price)), fill = '#650E9D') +
  geom_text(size = 3) +
  coord_flip() +
  ggtitle("Average rental price per city") +
  xlab("City") +
  ylab("Average Price") +
  ggsave("price_by_city.png", width = 6, height = 4)

ggplot(data %>% filter(., bedrooms %in% c("1br")) %>% 
         group_by(., city) %>% 
         summarise(., price = round(mean(price))), 
       aes(city, price, label=paste0("$",price))) +
  geom_col(aes(x=reorder(city, price)), fill = '#B696FF') +
  geom_text(size = 3, position = position_stack(reverse = T,vjust = 0.5)) +
  coord_flip() +
  ggtitle("Average rental price (1 br)") +
  xlab("City") +
  ylab("Average Price") +
  ggsave("price_by_city_1br.png", width = 6, height = 4)
            
ggplot(data %>% filter(., bedrooms %in% c("2br","3br")) %>% 
         group_by(., city) %>% 
         summarise(., price = round(mean(price))), 
       aes(city, price, label=paste0("$",price))) +
  geom_col(aes(x=reorder(city, price)), fill = '#B696FF') +
  geom_text(size = 3, position = position_stack(reverse = T,vjust = 0.5)) +
  coord_flip() +
  ggtitle("Average rental price (2-3 br)") +
  xlab("City") +
  ylab("Average Price") +
  ggsave("price_by_city_2-3br.png", width = 6, height = 4)
  

ggplot(data %>% filter(., bedrooms %in% c("4br","5br","6br")) %>% 
         group_by(., city) %>% 
         summarise(., price = round(mean(price))), 
       aes(city, price, label=paste0("$",price))) +
  geom_col(aes(x=reorder(city, price)), fill = '#B696FF') +
  geom_text(size = 3, position = position_stack(reverse = T,vjust = 0.5)) +
  coord_flip() +
  ggtitle("Average rental price (4br and more)") +
  xlab("City") +
  ylab("Average Price") +
  ggsave("price_by_city_3-4br.png", width = 6, height = 4)

# which city is the most dog friendly? most cat friendly?
pet_friendly <- data %>% group_by(., city) %>% summarise(., dogs_ok = sum(dogs_allowed), cats_ok = sum(cats_allowed)) %>% 
  left_join(.,data %>% group_by(., city) %>% summarise(., total_listing=n()), by = 'city') %>% 
  mutate(., cat_friendly = round(cats_ok/total_listing*100,2),
         dog_friendly = round(dogs_ok/total_listing*100,2))

  
pet_friendly <- pet_friendly %>% gather(.,key=key,value=value,c('cat_friendly','dog_friendly'))

ggplot(pet_friendly, aes(x=city,y=value,label=paste0(value,'%'))) +
  coord_flip() +
  geom_col(aes(x=reorder(city, value)), fill='#650E9D') +
  geom_text(size = 3, color='#FCFBFF',position = position_stack(reverse = T,vjust = 0.5)) +
  facet_wrap(~key, scales = "free")+
  ggtitle("Pet Friendliness") +
  xlab("city") +
  ylab("pet allowed listings / total listings (%)") +
  ggsave("price_by_city_pet_friendly.png", width = 8, height = 6)
  
# which day do people post most listings (by city)

weekday <- data %>% mutate(.,weekday = weekdays(as.Date(date))) %>% 
  group_by(., weekday) %>% summarise(., perc=round(n()/nrow(data)*100)) %>% 
  select(.,weekday, perc) %>% mutate(.,weekday=factor(weekday, levels=c("Monday","Tuesday","Wednesday","Thursday","Friday","Saturday","Sunday"), 
                                                      ordered = T))

plot(gvisPieChart(weekday[order(weekday$weekday),], options=list(width=1000, height=600)))

#1
time_range <- data %>% group_by(., time_range) %>% summarise(.,perc=round(n()/nrow(data)*100)) %>% 
  select(.,time_range,perc) %>% mutate(.,time_range=factor(time_range, levels=c("12AM~8AM","8AM~4PM","4PM~12AM")))

plot(gvisPieChart(time_range[order(time_range$time_range),],options=list(width=1000, height=600)))

#2
colors <- c('rgb(211,94,96)', 'rgb(128,133,133)', 'rgb(144,103,167)', 'rgb(171,104,87)')

time_range2 <- data %>% filter(., time_range =='8AM~4PM') %>% 
  mutate(., time_range2 = ifelse(as.integer(substr(time,1,2))>=8 & as.integer(substr(time,1,2))<=9,"8AM~10AM",
                                 ifelse(as.integer(substr(time,1,2))>=10 & as.integer(substr(time,1,2))<=11,"10AM~12PM",
                                 ifelse(as.integer(substr(time,1,2))>=12 & as.integer(substr(time,1,2))<=13,"12PM~2PM", "2PM~4PM")))) %>% 
  group_by(.,time_range2) %>% summarise(.,count=n()) %>% mutate(.,time_range2=factor(time_range2,levels = c("8AM~10AM","10AM~12PM","12PM~2PM","2PM~4PM"), ordered=T))

P <- gvisPieChart(time_range2[order(time_range2$time_range2),],
                  options=list(
                    width=1000, 
                    height=600,
                    colors={"['#44009A','#9A6DE8','#723AD1','#D1B6FE']"},
                    title={"Percentage of Listings by hour"}))
plot(P)

# do no fee vs fee
no_fee <- data %>% filter(., bedrooms != "") %>% 
  filter(., bedrooms != "6br") %>% mutate(., no_fee = ifelse(no_fee == 1, "No Fee", "Fee")) %>% 
  group_by(., bedrooms, no_fee) %>% 
  summarise(., price = round(mean(price))) 
ggplot(no_fee, aes(bedrooms, price,label=paste0('$',price))) +
  geom_col(aes(fill = no_fee),position = 'dodge') +
  geom_text(size = 3,position = position_dodge2(width = 1))+
  labs(fill = "")+
  theme(legend.position="right") +
  ggsave("price_by_noFee.png", width = 6, height = 4)
  


  
  

