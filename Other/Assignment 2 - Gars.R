library("rvest")
library(stringr)
library("dplyr")

link = "http://www.imdb.com/search/keyword?keywords=based-on-novel&sort=moviemeter,asc&mode=detail&page=3&ref_=kw_nxt&release_date=1960%2C2016"

for(i in 0:99){
  imdb.links=(paste("http://www.imdb.com/search/keyword?keywords=based-on-novel&sort=moviemeter,asc&mode=detail&page=", 0:i, sep = "","&ref_=kw_nxt&release_date=1960%2C2016"))
}
head(imdb.links,20)

#func
scrape_imdb = function(link){
  my.link = read_html(link, encoding = "UTF-8")
  my.link.title = my.link %>% 
    html_nodes(".lister-item-header a") %>% 
    html_text()
  my.link.rating = my.link %>% 
    html_nodes(".ratings-imdb-rating strong") %>% 
    html_text()
  #my.link.name = my.link %>% 
  #  html_nodes(".name a") %>% 
  #  html_text()
  #my.link.transaction = my.link %>% 
  #  html_nodes(".transaction a") %>% 
  #  html_text()
  #my.link.views = my.link %>% 
  #  html_nodes(".overview .views") %>% 
  #  html_text()
    return(cbind( my.link.title, my.link.rating))
}

#Loop data to a list
my.imdb.data = list() # initialize empty list
for (i in imdb.links[1:2]){
  print(paste("processing", i, sep = " "))
  my.imdb.data[[i]] = scrape_imdb(i)
  # waiting one second between hits
  Sys.sleep(1)
  cat(" done!\n")
}

#converting into a data frame
library("plyr")
df.imdb = ldply(my.imdb.data)


#data cleaning
df.imdb$my.link.amount_text = str_extract(df.imdb$my.link.amount_text,"[0-9]+")
df.imdb$my.link.views = str_extract(df.imdb$my.link.views,"[0,0-9,9]+")

df.imdb=data.frame(page=df.imdb$.id, title=df.imdb$my.link.title, amount=df.imdb$my.link.amount_text, name=df.imdb$my.link.name, transaction=df.imdb$my.link.transaction, views=df.imdb$my.link.views, city=df.imdb$my.link.city, date=df.imdb$my.link.date)

library("dplyr")
library("lubridate")

df.imdb$amount = as.numeric(str_replace_all(df.imdb$amount, pattern = "," , replacement = ""))
df.imdb$views = as.numeric(df.imdb$views)
df.imdb$city1 = str_extract(df.imdb$city,"[A-z]*")

df.imdb$reg = str_extract(df.imdb$city,", [A-z]+ [A-z]+")
df.imdb$reg1 = str_extract(df.imdb$city,", [A-z]+")
df.imdb$reg=gsub(",","", df.imdb$reg)
df.imdb$reg1=gsub(",","", df.imdb$reg1)

df.imdb$reg2[is.na(df.imdb$reg)]=df.imdb$reg1[is.na(df.imdb$reg)]
df.imdb$reg2[is.na(df.imdb$reg2)]=df.imdb$reg[is.na(df.imdb$reg2)]
df.imdb$reg=df.imdb$reg2

class(df.imdb$amount)
df.imdb$antal=1

# stat data for cities
df.imdb.reg = df.imdb %>%
  group_by(reg) %>%
  summarise(antal = sum(antal), median_amount = median(amount), mean_views = mean(views) ) %>%
  arrange(desc(median_amount)) %>%
  data.frame

# stat data for name
df.imdb.name = df.imdb %>%
  group_by(name) %>%
  summarise(antal = sum(antal), median_amount = median(amount), mean_views = mean(views) ) %>%
  arrange(desc(median_amount), city) %>%
  data.frame

# stat data for transaction
df.imdb.transaction = df.imdb %>%
  group_by(transaction) %>%
  summarise(antal = sum(antal), median_amount = median(amount), mean_views = mean(views) ) %>%
  arrange(desc(median_amount), city) %>%
  data.frame

library("ggplot2")