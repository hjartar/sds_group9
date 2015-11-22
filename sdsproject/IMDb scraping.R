library("rvest")
library(stringr)
library("dplyr")

#Generating links to loop through all movies "based-on-novel".
for(i in 0:611){
  imdb.links=(paste("http://www.imdb.com/search/keyword?keywords=based-on-novel&sort=moviemeter,asc&mode=detail&page=", 0:i, sep = "","&ref_=kw_nxt"))
}
head(imdb.links,20)


#Function to collect links to every movie on the list
scrape_imdb = function(link){
  my.link = read_html(link, encoding = "UTF-8")
  my.link.title = my.link %>% 
    html_nodes(".lister-item-header a") %>% 
    html_text()
  my.link.link = my.link %>% 
    html_nodes(".lister-item-header a") %>% 
    html_attr("href")
    return(cbind( my.link.title,my.link.link))
}

#Loop data to a list
my.imdb.data = list() # initialize empty list
for (i in imdb.links[1:10]){
  print(paste("processing", i, sep = " "))
  my.imdb.data[[i]] = scrape_imdb(i)
  # waiting one second between hits
  Sys.sleep(1)
  cat(" done!\n")
}

#converting into a data frame
library("plyr")
df.imdb = ldply(my.imdb.data)

#Creating list of links
imdb.secondary.links=(paste("http://www.imdb.com", sep = "", df.imdb$my.link.link))
head(imdb.secondary.links,20)



