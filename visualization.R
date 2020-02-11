library(data.table)
library(dplyr)
library(tidyr)
library(ggplot2)
library(plotly)
library(googleVis)
library(stringr)

fix_hood <- read.csv(file = "./data/fix_hood.csv",stringsAsFactors = FALSE)

realty <- read.csv(file = "./data/realtyhop_clean.tsv",sep = "\t",stringsAsFactors = FALSE) %>% 
  rename(., 'br'='bedroom') %>% 
  mutate(., br = ifelse(br != 'Stubr', br, ifelse(type_ %in% c('Townhouse','Multifamily','Commercial','Land','Other','House'), "NA",br))) %>% 
  mutate(., br_cat = ifelse(br %in% c("Stubr","1br"),"1br & less",ifelse(br %in% c('2br', '3br'), "2~3br",ifelse(br %in% c('4br', '5br','6br'), "4br & more","NA")))) %>% 
  inner_join(., fix_hood, by='neighborhood') %>% 
  mutate(., NEIGHBORHOOD = hood)

cr <- read.csv(file = "./data/craigslist_mnh_clean.tsv",sep = "\t",stringsAsFactors = FALSE) %>% 
  mutate(., title = tolower(title)) %>% mutate(., br = gsub(' ','', br)) %>% 
  mutate(., br = ifelse(br!="",br,
                        ifelse(grepl("1/.|1x.|1bdrm|1.bdrm|one.bdrm|1.bed|one.bed|studio|1 bed|1bed|1br|1.br|1bd|1.bd|one.bed|one.bd|one.br",title), "1br",
                               ifelse(grepl("2/.|2x.|2.bdrm|2.bed|2.br|2 bed|2bed|2br|2.br|2bd|2.bd|two.bed|two.bd|two.br",title), "2br",
                                      ifelse(grepl("3/.|3x.|3.bdrm|3.bed|3bed|3br|3.br|3bd|3.bd|three.bed|three.bd|three.br",title),"3br",
                                             ifelse(grepl("4/.|4x.|4.bdrm|4.bed|4bed|4br|4.br|4bd|4.bd",title), "4br",
                                                    ifelse(grepl("5/.|5x.|5.bdrm|5.bed|5bed|5br|5.br|2bd|5.bd",title), "5br",br))))))) %>% 
  mutate(., br_cat = ifelse(br %in% c("","1br"),"1br & less",ifelse(br %in% c('2br', '3br'), "2~3br",ifelse(br %in% c('4br', '5br','6br'), "4br & more","NA")))) %>% 
  mutate(., NEIGHBORHOOD = neighborhood)

cr_repost <- cr %>% filter(., !is.na(repost_of))
nrow(cr_repost)
nrow(cr_unique) /nrow(cr) #4014
cr_unique <- cr %>% filter(., is.na(repost_of))
#2660

rh <- read.csv(file = "./data/renthop_clean.tsv",sep = "\t",stringsAsFactors = FALSE) %>% 
  mutate(., br_cat = ifelse(br %in% c("Stubr","1br"),"1br & less",ifelse(br %in% c('2br', '3br'), "2~3br",ifelse(br %in% c('4br', '5br','6br'), "4br & more","NA")))) %>% 
  inner_join(., fix_hood, by='neighborhood') %>% 
  mutate(., NEIGHBORHOOD = hood)

a <- rh %>% group_by(., neighborhood,br_cat) %>% summarise(., rent=median(price)) %>% 
  inner_join(., realty %>% filter(., price <40000000) %>% group_by(., neighborhood, br_cat) %>% 
             summarise(., expense=median(expense), sale=median(price), sale.sqf=median(price/sqf)),
             by=c('neighborhood','br_cat'))

require(scales)
ggplot(a, aes(rent, sale)) + geom_point()+
  facet_wrap(~br_cat, scales = "free")+
  scale_x_continuous(labels = comma)+
  scale_y_continuous(labels = comma)

ggplot(a, aes(rent, sale.sqf)) + geom_point()
  scale_x_continuous(labels = comma)+
  scale_y_continuous(labels = comma)
  
# write.csv(realty %>% filter(., br=='Stubr') %>% arrange(., desc(price)) %>% select(., type_,price ,url),
#           "Stubr.csv", row.names = F)

joinDF <- cr_unique %>% group_by(., NEIGHBORHOOD) %>% 
  summarise(., mean.price.cr = mean(price), med.price.cr = median(price)) %>% 
  inner_join(., rh %>% group_by(., NEIGHBORHOOD) %>% 
               summarise(., mean.price.rh = mean(price), med.price.rh = median(price)), 
             by='NEIGHBORHOOD') %>% 
  mutate(., mean_diff=mean.price.cr-mean.price.rh, med_diff=med.price.cr - med.price.rh,
         mean.diff.perc=round(mean_diff/mean.price.rh*100,2), med.diff.perc=round(med_diff/med.price.rh*100,2))

# compare median rents
ggplot(joinDF %>% gather(., key=key, value=value, c("med.price.cr","med.price.rh")) %>% 
         mutate(., key=ifelse(key=="med.price.cr","Craigslist",'Renthop')), aes(NEIGHBORHOOD, value)) +
  geom_col(aes(x=reorder(NEIGHBORHOOD, value),fill=key), position='dodge') +
  scale_fill_manual(values = c("#5815AD","#871551"))+
  labs(fill = "Median Rent")+
  xlab("")+
  ylab("")+
  ggtitle("")+
  coord_flip()+
  ggsave("./ppt/RH_vs_CR_median.png", width = 5, height = 5)

# median rent price diff.
ggplot(joinDF %>% filter(., NEIGHBORHOOD != 'Downtown') %>% group_by(., NEIGHBORHOOD) %>% 
         summarise(., mean=round(mean(med.diff.perc),2)), 
       aes(NEIGHBORHOOD, mean,label = paste0(mean,"%"))) +
  geom_col(aes(x=reorder(NEIGHBORHOOD, mean)), fill='#5815AD') +
  xlab("")+
  ylab("")+
  geom_text(size = 3, color='white', position = position_stack(reverse = T,vjust = 0.5)) +
  ggtitle("")+
  coord_flip()+
  ggsave("./ppt/RH_CR_Mean_Perc.png", width = 14, height = 5)

df <- joinDF %>% group_by(.,NEIGHBORHOOD) %>% summarise(.,mean=round(mean(med.diff.perc),2)) %>% 
  filter(., !NEIGHBORHOOD %in% c('Union Square', 'Downtown'
  ))

# med rent price diff
ggplot(df,aes(NEIGHBORHOOD, mean, label=paste0(mean,"%"))) +
  geom_col(aes(x=reorder(NEIGHBORHOOD, mean)), fill='#5815AD') +
  coord_flip()+
  xlab("")+
  ylab("")+
  geom_text(size = 3, color='white', position = position_stack(reverse = T,vjust = 0.5)) +
  ggtitle("")+
  ggsave("./ppt/RH_CR_Median_Perc.png", width = 5, height = 5)
  


#************************ CORRELATION BETWEEN RENT AND SALES *****************

rent_vs_buy <- realty %>% group_by(., NEIGHBORHOOD, br_cat ) %>% 
  summarise(., sales=median(price), sales.sqf=median(price/sqf), expense=median(expense)) %>% 
  inner_join(., cr_unique %>% group_by(., NEIGHBORHOOD, br_cat) %>% summarise(., rent=median(price), rent.sqf=median(price/ft2)),
             by=c('NEIGHBORHOOD','br_cat'))


ggplot(rent_vs_buy, aes(rent, sales))+geom_point(aes(color=br_cat), size = 2)+
  scale_x_continuous(labels = comma)+scale_y_continuous(labels = comma)+
  scale_color_manual(values = c("#D9CCFF","#A180FA","#5815AD"))+
  xlab("Median Rent") +
  ylab("Median Housing Price")+
  labs(color="Bedrooms")+
  theme(legend.position="right")+
  ggsave("./ppt/Rent_vs_Sale.png", width = 8, height = 5)
  

#******************************* NATIONAL RENTS ***************************************
data <- read.csv(file = "./data/lcty_data_clean.tsv", sep = "\t",stringsAsFactors = FALSE)
data <- data %>% mutate(., title_len = nchar(title)) %>% mutate(., br = gsub(' ','',br))
data$time_range = ifelse(as.integer(substr(data$time,1,2))>=8 & as.integer(substr(data$time,1,2))<=15, "8AM~4PM",
                   ifelse(as.integer(substr(data$time,1,2))>=16 & as.integer(substr(data$time,1,2))<=23, "4PM~12AM","12AM~8AM"))
# average postings a day
data <- data %>% filter(., city %in% c('New York','San Francisco','Boston','Los Angeles','Denver','Chicago','Austin','Seattle'))

# of postings 19,252
data %>% group_by(., date2) %>% summarise(., count=n()) %>% arrange(., desc(count)) %>% top_n(.,3) %>% summarise(., mean(count))
# of reposts  8,509
data %>% filter(., !is.na(repost_of)) %>% group_by(., date2) %>% summarise(., count=n()) %>% arrange(., desc(count)) %>% top_n(.,3) %>% summarise(., mean(count))

# ********** Clean up bedrooms ****************
data2 <- data %>% 
  mutate(., title = tolower(title), br = ifelse(br!="",br,
                              ifelse(grepl("1/.|1x.|1bdrm|1.bdrm|one.bdrm|1.bed|one.bed|studio|1 bed|1bed|1br|1.br|1bd|1.bd|one.bed|one.bd|one.br",title), "1br",
                                     ifelse(grepl("2/.|2x.|2.bdrm|2.bed|2.br|2 bed|2bed|2br|2.br|2bd|2.bd|two.bed|two.bd|two.br",title), "2br",
                                            ifelse(grepl("3/.|3x.|3.bdrm|3.bed|3bed|3br|3.br|3bd|3.bd|three.bed|three.bd|three.br",title),"3br",
                                                   ifelse(grepl("4/.|4x.|4.bdrm|4.bed|4bed|4br|4.br|4bd|4.bd",title), "4br",
                                                          ifelse(grepl("5/.|5x.|5.bdrm|5.bed|5bed|5br|5.br|2bd|5.bd",title), "5br",br)))))),
         br_cat = ifelse(br %in% c("","1br"),"1br & less",
                         ifelse(br %in% c('2br', '3br'), "2~3br",
                                ifelse(br %in% c('4br', '5br','6br'), "4br & more","NA"))))

data_uniq <- data2 %>% filter(., is.na(repost_of))

ggplot(data_uniq %>% filter(., ft2>100) %>% filter(., ft2<10000), aes(ft2, y=price))+
  xlim(0, 3000)+
  facet_wrap(~city, scales = 'free')+
  xlab('size (in sqf)')+
  ylab('rent')+
  geom_point(alpha = 0.05, size = 1,color='#5815AD')+
  geom_smooth(size=0.3, alpha=0.1)
  
ggplot(data_uniq %>% filter(., ft2>100) %>% filter(., ft2<10000), aes(ft2, y=price))+
  xlim(0, 3000)+
  facet_wrap(~city, scales = 'free')+
  xlab('size (in sqf)')+
  ylab('rent')+
  geom_point(alpha = 0.05, size = 1,color='#5815AD')+
  geom_smooth(size=0.3, alpha=0.1)

ggplot(data_uniq %>% filter(., ft2>100) %>% filter(., ft2<10000), aes(ft2))+
  #xlim(0, 000)+
  facet_wrap(~city, scales='free')+
  xlab('size (in sqf)')+
  scale_x_continuous(labels = scales::comma)+
  scale_y_continuous(labels = scales::comma)+
  geom_freqpoly(binwidth=200,color='#5815AD')

library(scales)
ggplot(data_uniq, aes(price))+
  facet_wrap(~city)+
  geom_density(color='#5815AD' ) +
  xlab('rent per month')+
  ylab("")+
  scale_x_continuous(labels = scales::dollar)+
  scale_y_continuous(labels = scales::percent)+
  ggsave("./ppt/density.png", width = 6, height = 3)

data_uniq %>% filter(., city=='Boston') %>% filter(., size<500)

# box plot cities:
ggplot(data_uniq, aes(x=reorder(city, price), y=price, group=city)) +
  geom_boxplot(fill='#5815AD', color='grey') +
  coord_flip() +
  theme_bw() +
  xlab('') +
  ylab('Rent/month')+
  ggsave("./ppt/boxplot_by_city.png", width = 6, height = 3)


# which city has the highest average rent?
ggplot(data2 %>% group_by(., city) %>% 
         summarise(., price = round(mean(price))) %>% 
         arrange(.,desc(price)),
       aes(city,price,label=paste0("$",price))) + 
  geom_col(aes(x=reorder(city,price)), fill = '#5815AD') +
  geom_text(size = 4, color='white', position = position_stack(reverse = T,vjust = 0.5)) +
  coord_flip() +
  ylab("Average Rent") +
  xlab("") +
  theme_bw() +
  theme(axis.text=element_text(size=12)) +
  ggsave("./ppt/price_by_city.png", width = 5, height = 5)

# salary adjusted
salary <- read.csv(file = "./data/salaries.csv",stringsAsFactors = FALSE, header = T, col.names = c('city','salary'))
salary_adj <- data_uniq %>% inner_join(., salary, by='city') %>% mutate(., price_prc=round(price/(salary/12)*100)) %>% 
  group_by(., city) %>% summarise(., price_prc=round(mean(price_prc),2))
ggplot(salary_adj,aes(city,price_prc,label=paste0(price_prc,'%'))) + 
  geom_col(aes(x=reorder(city,price_prc)), fill = '#5815AD') +
  geom_text(size = 4, color='white', position = position_stack(reverse = T,vjust = 0.5)) +
  coord_flip() +
  ylab("Average Rent/Average Tech Worker Salary (%)") +
  xlab("") +
  theme_bw() +
  theme(axis.text=element_text(size=12)) +
  ggsave("./ppt/price_prc_by_city.png", width = 5, height = 5)

#
df <- data_uniq %>% group_by(., city, br_cat) %>% 
  summarise(., price = round(mean(price))) %>% 
  arrange(.,br_cat,desc(price))
ggplot(df, aes(x=city,y=price,label=paste0("$",price))) + 
  geom_col(aes(x=reorder(city,price)), fill = '#5815AD') +
  geom_text(size = 4, color='white', position = position_stack(reverse = T,vjust = 0.5)) +
  coord_flip() +
  facet_wrap(~br_cat)+
  ylab("") +
  xlab("") +
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=14),#,face="bold"),
        strip.text.x = element_text(size = 14, colour = "#5815AD", angle = 0)) +
  ggsave("./ppt/price_by_city_br.png", width = 10, height = 5)


# which city is the most dog friendly? most cat friendly?
pet_friendly <- data_uniq %>% group_by(., city) %>% summarise(., dogs_ok = sum(dogs_allowed), cats_ok = sum(cats_allowed)) %>% 
  left_join(.,data_uniq %>% group_by(., city) %>% summarise(., total_listing=n()), by = 'city') %>% 
  mutate(., cat_friendly = round(cats_ok/total_listing*100,2),
         dog_friendly = round(dogs_ok/total_listing*100,2))
ggplot(pet_friendly %>% gather(.,key=key,value=value,c('cat_friendly','dog_friendly')), aes(x=city,y=value,label=paste0(value,'%'))) +
  coord_flip() +
  geom_col(aes(x=reorder(city, value)), fill='#5815AD') +
  geom_text(size = 5, color='#FCFBFF',position = position_stack(reverse = T,vjust = 0.5)) +
  facet_wrap(~key, scales = "free")+
  ylab("pet allowed listings / total listings (%)") +
  xlab("") +
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=16,face="bold"),
        strip.text.x = element_text(size = 14, face="bold",colour = "#5815AD", angle = 0)) +
  ggsave("./ppt/price_by_city_pet_friendly.png", width = 10, height = 5)
  

# Is average listing price tend to be higher due to the pet?
dogs_allowed <- data_uniq %>% group_by(.,city, dogs_allowed) %>% 
  summarise(., price=round(mean(price))) %>% 
  spread(., dogs_allowed,price ) %>% rename(., 'Yes'='1','No'='0') %>% 
  mutate(., More = Yes - No, Perc = round(More/No*100), Type='Dogs Allowed') 

cats_allowed <- data_uniq %>% group_by(.,city,cats_allowed) %>% 
  summarise(., price=round(mean(price))) %>% 
  spread(., cats_allowed,price ) %>% rename(., 'Yes'='1','No'='0') %>% 
  mutate(., More = Yes - No, Perc = round(More/No*100), Type = 'Cats Allowed') 



pets_allowed_perc <- rbind(dogs_allowed, cats_allowed)

##242C62 - background
ggplot(pets_allowed_perc, aes(city, Perc, label=paste0(Perc,'%'))) +
  coord_flip()+
  geom_col(aes(x=reorder(city, Perc)),fill = '#5815AD') +
  geom_text(size = 4, color='#FCFBFF',position = position_stack(reverse = T,vjust = 0.5)) +
  facet_wrap(~Type, scales = "free")+
  ylab("")+
  xlab("") +theme(axis.text=element_text(size=12, color='grey'),
                  axis.title=element_text(size=16,face="bold"),
                  strip.text.x = element_text(size = 14, face="bold",colour = "#5815AD", angle = 0,)) +
  ggsave("./ppt/price_by_city_pet_friendly_perc.png", width = 10, height = 5)


# what neighborhood in NYC has the most pets allowed listings?
pet_friendly <- cr_unique %>% group_by(., neighborhood) %>% summarise(., dogs_ok = sum(dogs_allowed), cats_ok = sum(cats_allowed)) %>% 
  left_join(.,cr_unique %>% group_by(., neighborhood) %>% summarise(., total_listing=n()), by = 'neighborhood') %>% 
  mutate(., cat_friendly = round(cats_ok/total_listing*100,2),
         dog_friendly = round(dogs_ok/total_listing*100,2))
ggplot(pet_friendly %>% gather(.,key=key,value=value,c('cat_friendly','dog_friendly')) %>% filter(., !neighborhood %in% c('Union Square','Downtown')), 
       aes(x=neighborhood,y=value,label=paste0(value,'%'))) +
  coord_flip() + 
  geom_col(aes(x=reorder(neighborhood, value)), fill='#5815AD') +
  geom_text(size = 4, color='#FCFBFF',position = position_stack(reverse = T,vjust = 0.5)) +
  facet_wrap(~key)+
  ylab("pet allowed listings / total listings (%)") +
  xlab("") +
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=12),
        strip.text.x = element_text(size = 14, face="bold",colour = "#5815AD", angle = 0)) +
  ggsave("./ppt/nyc_pet_friendly.png", width = 10, height = 4)

# 'pet premium'
pets_premium <- cr_unique %>% mutate(., pets_ok=ifelse(dogs_allowed==0&cats_allowed==0, "No", "Yes")) %>% 
  group_by(., neighborhood, pets_ok) %>% 
  summarise(., price = round(mean(price))) %>% 
  spread(., pets_ok, price) %>% 
  mutate(., More = Yes - No, Perc = round(More/No*100), Perc=ifelse(is.na(Perc),0,Perc))

ggplot(pets_premium %>% filter(., !neighborhood %in% c('Union Square','Downtown'))
       , aes(neighborhood, Perc, label=paste0(Perc,'%'))) +
  coord_flip()+
  geom_col(aes(x=reorder(neighborhood, Perc)),fill = '#5815AD') +
  geom_text(size = 3, color='#FCFBFF',position = position_stack(reverse = T,vjust = 0.5)) +
  ylab("Average Price for (Pets Ok - Pets Not Ok)/Pets Not Ok in (%)")+
  xlab("") +theme(axis.text=element_text(size=10),
                  axis.title=element_text(size=12),
                  strip.text.x = element_text(size = 13, face="bold",colour = "#5815AD", angle = 0,)) +
  ggsave("./ppt/nyc_pet_premium.png", width = 10, height = 5)

dogs_allowed <- cr_unique %>% group_by(.,neighborhood, dogs_allowed) %>% 
  summarise(., price=round(mean(price))) %>% 
  spread(., dogs_allowed,price ) %>% rename(., 'Yes'='1','No'='0') %>% 
  mutate(., More = Yes - No, Perc = round(More/No*100), Type='Dogs Allowed') 

cats_allowed <- cr_unique %>% group_by(.,neighborhood,cats_allowed) %>% 
  summarise(., price=round(mean(price))) %>% 
  spread(., cats_allowed,price ) %>% rename(., 'Yes'='1','No'='0') %>% 
  mutate(., More = Yes - No, Perc = round(More/No*100), Type = 'Cats Allowed') 

pets_allowed_perc <- rbind(dogs_allowed, cats_allowed) %>% 
  mutate(., Perc = ifelse(is.na(No), 0, Perc))

##242C62 - background
ggplot(pets_allowed_perc %>% filter(., !neighborhood %in% c('Union Square','Downtown')), aes(neighborhood, Perc, label=paste0(Perc,'%'))) +
  coord_flip()+
  geom_col(aes(x=reorder(neighborhood, Perc)),fill = '#5815AD') +
  geom_text(size = 4, color='#FCFBFF',position = position_stack(reverse = T,vjust = 0.5)) +
  facet_wrap(~Type, scales = "free")+
  ylab("")+
  xlab("") +theme(axis.text=element_text(size=12),
                  axis.title=element_text(size=16,face="bold"),
                  strip.text.x = element_text(size = 14, face="bold",colour = "#5815AD", angle = 0,)) +
  ggsave("./ppt/nyc_pet_premium_perc.png", width = 10, height = 5)

# compare price based on cats allowed vs cats not allowed
ggplot(cr_unique %>% group_by(., neighborhood, cats_allowed) %>% 
         summarise(., rent=round(mean(price))) %>% 
         mutate(., cats_allowed=ifelse(cats_allowed==0, "Cats Not Allowed", "Cats Allowed")),
       aes(neighborhood, rent, label=paste0('$',rent)))+
  geom_col(aes(x=reorder(neighborhood, rent)), fill = '#5815AD')+
  geom_text(size = 4, color='#FCFBFF',position = position_stack(reverse = T,vjust = 0.5)) +
  coord_flip()+
  facet_wrap(~cats_allowed)+
  xlab("")+
  ylab("Average Monthly Rent")+
  ggsave("./ppt/nyc_cat_compare.png", width = 10, height = 5)

# compare price based on dogs allowed vs gods not allowed
ggplot(cr_unique %>% group_by(., neighborhood, dogs_allowed) %>% 
         summarise(., rent=round(mean(price))) %>% 
         mutate(., dogs_allowed=ifelse(dogs_allowed==0, "Dogs Not Allowed", "Dogs Allowed")),
       aes(neighborhood, rent, label=paste0('$',rent)))+
  geom_col(aes(x=reorder(neighborhood, rent)), fill = '#5815AD')+
  geom_text(size = 4, color='#FCFBFF',position = position_stack(reverse = T,vjust = 0.5)) +
  coord_flip()+
  facet_wrap(~dogs_allowed)+
  xlab("")+
  ylab("Average Monthly Rent")+
  ggsave("./ppt/nyc_dogs_compare.png", width = 10, height = 5)

# which day do people post most listings (by city)
weekday <- data2 %>% mutate(.,weekday = weekdays(as.Date(date))) %>% 
  group_by(., weekday) %>% summarise(., perc=round(n()/nrow(data)*100)) %>% 
  select(.,weekday, perc) %>% mutate(.,weekday=factor(weekday, levels=c("Monday","Tuesday","Wednesday","Thursday","Friday","Saturday","Sunday"), 
                                                      ordered = T))

plot(gvisPieChart(weekday[order(weekday$weekday),], options=list(width=1000, height=600)))

# what time of the day people post most listings?
time_range <- data_uniq %>% group_by(., time_range) %>% summarise(.,perc=round(n()/nrow(data)*100)) %>% 
  select(.,time_range,perc) %>% mutate(.,time_range=factor(time_range, levels=c("12AM~8AM","8AM~4PM","4PM~12AM")))

ggplot(time_range[order(time_range$time_range),]) +
  geom_col(aes(x=reorder(time_range,desc(perc)),y=perc), fill=c("#5815AD","#A180FA","#D9CCFF")) +
  geom_text(aes(x = time_range,y = perc + 2, label=paste0(perc,'%')), size = 4) +
  xlab("Time Range") +
  ylab("Percentage of total listings") +
  ggsave("./ppt/time_range1.png", width = 7, height = 4)
  
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


  
# ************************** NYC *****************************

# NYC average rental price per boro vs renthop 
nyc <- data_uniq %>% filter(., city=="New York") %>% 
  mutate(., borough = ifelse(boro=="mnh", "Manhattan",
                             ifelse(boro=="brk","Brooklyn",
                                    ifelse(boro=="stn", "Staten Island",
                                           ifelse(boro=="que", "Queens", "Bronx")))))

nyc <- nyc %>% mutate(.,pets_ok=ifelse(dogs_allowed==0&cats_allowed==0, "Pets Not Allowed", "Pets Allowed"))

#density
library(scales)
ggplot(nyc, aes(price))+
  facet_wrap(~borough)+
  geom_density(color='#5815AD' ) +
  xlab('rent per month')+
  ylab("")+
  scale_x_continuous(labels = scales::dollar)+
  scale_y_continuous(labels = scales::percent)+
  ggsave("./ppt/density-nyc.png", width = 6, height = 3)

# box plot nyc:
ggplot(nyc, aes(x=reorder(borough, price), y=price, group=borough)) +
  geom_boxplot(fill='#5815AD', color='grey') +
  coord_flip() +
  theme_bw() +
  xlab('') +
  ylab('Rent/month')+
  ggsave("./ppt/boxplot_nyc.png", width = 5, height = 3)

ggplot(nyc %>% group_by(., borough) %>% summarise(., rent=round(mean(price))),
       aes(borough, rent, label=paste0("$",rent)))+
  geom_col(aes(x=reorder(borough, rent)),fill='#5815AD')+
  coord_flip()+
  geom_text(size = 4, color='#FCFBFF',position = position_stack(reverse = T,vjust = 0.5)) +
  theme_bw()+
  xlab("")+
  ylab("Average Rent/month")+
  ggsave("./ppt/by_boro_nyc.png", width = 3, height = 3)

# do no fee vs fee
no_fee <- nyc %>% 
  mutate(., no_fee = ifelse(no_fee == 1, "No Fee", "Fee")) %>% 
  group_by(., br_cat, no_fee) %>% 
  summarise(., price = round(mean(price))) 

ggplot(no_fee, aes(br_cat, price)) +
  geom_col(aes(fill = no_fee),position = 'dodge') +
  scale_fill_manual(values=c("#5815AD","#D7CEEE")) +
  geom_text(aes(x = br_cat,y = price + 110, label=paste0('$',price)), size = 3,color=c('black'),position = position_dodge2(width = 1))+
  labs(fill = "")+
  xlab("")+ylab("")+
  theme(legend.position="right") +
  ggsave("./ppt/price_by_noFee.png", width = 6, height = 4)

nyc_pets_ok_perc <- nyc %>% group_by(.,borough, pets_ok) %>% summarise(., count=n()) %>% 
  left_join(nyc %>% group_by(., borough) %>% summarise(., count_total=n()),
            by='borough') %>% 
  mutate (., pets_ok_perc=round(count/count_total*100,2)) %>% filter(., pets_ok=='Pets Allowed')
ggplot(nyc_pets_ok_perc, aes(borough, pets_ok_perc, label=paste0(pets_ok_perc, "%"))) +
  geom_col(aes(x=reorder(borough,desc(pets_ok_perc))), fill="#5815AD") +
  geom_text(size = 4, color='#FCFBFF',position = position_stack(reverse = T,vjust = 0.5)) +
  xlab("") +
  ylab("pets allowed listings/ total listings") +
  ggsave("./ppt/boro_pets_allowed_perc.png", width = 6, height = 3)

nyc_pet_premium <- nyc %>% group_by(.,borough, pets_ok) %>% summarise(., price = round(median(price))) %>% 
  spread(., pets_ok, price) %>% mutate(., diff = `Pets Allowed`-`Pets Not Allowed`, 
                                       perc = round(100*diff/`Pets Not Allowed`,2))

ggplot(nyc_pet_premium, aes(borough, perc, label=paste0(perc, "%"))) +
  geom_col(aes(x=reorder(borough,perc)), fill="#5815AD") +
  coord_flip()+
  geom_text(size = 4, color='#FCFBFF',position = position_stack(reverse = T,vjust = 0.5)) +
  xlab("") +
  ylab("pets allowed listings/ total listings") +
  ggsave("./ppt/boro_pets_premium.png", width = 4, height = 3)

cats_allowed <- cr_unique %>% group_by(.,neighborhood,cats_allowed) %>% 
  summarise(., price=round(mean(price))) %>% 
  spread(., cats_allowed,price ) %>% rename(., 'Yes'='1','No'='0') %>% 
  mutate(., More = Yes - No, Perc = round(More/No*100), Type = 'Cats Allowed') 

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

nyc_time_range2 <- data %>% filter(., city=="New York") %>% 
  mutate(., time_range2 = ifelse(as.integer(substr(time,1,2))>=8 & as.integer(substr(time,1,2))<=11,"8AM~12PM",
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
#titles <- tm_map(titles, removeNumbers)
# Remove english common stopwords
titles <- tm_map(titles, removeWords, stopwords("english"))
# Remove your own stop word
# specify your stopwords as a character vector
# titles <- tm_map(titles, removeWords, c("blabla1", "blabla2")) 
# Remove punctuations
titles <- tm_map(titles, removePunctuation)
# Eliminate extra white spaces
titles <- tm_map(titles, stripWhitespace)
# Text stemming
# titles <- tm_map(titles, stemDocument)

dtm <- TermDocumentMatrix(titles)
m <- as.matrix(dtm)
v <- sort(rowSums(m),decreasing=TRUE)
d <- data.frame(word = names(v),freq=v)
head(d, 10)

set.seed(1234)
wordcloud(words = d$word, freq = d$freq, min.freq = 100,
          max.words=200, random.order=FALSE, rot.per=0.35, 
          colors=brewer.pal(10, "Spectral"))

