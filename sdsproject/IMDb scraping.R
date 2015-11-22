library("rvest")
library(stringr)
library("dplyr")

#Generating links to loop through all movies "based-on-novel".
for(i in 0:611){
  imdb.links=(paste("http://www.imdb.com/search/keyword?keywords=based-on-novel&sort=moviemeter,asc&mode=detail&page=", 0:i, sep = "","&ref_=kw_nxt"))
}
head(imdb.links,20)


#Function to collect links to every movie on the list
getlinks_imdb = function(link){
  my.link = read_html(link, encoding = "UTF-8")
  my.link.link = my.link %>% 
    html_nodes(".lister-item-header a") %>% 
    html_attr("href")
    return(cbind(my.link.link))
}

#Loop data to a list
my.imdb.linkdata = list() # initialize empty list
for (i in imdb.links[1:3]){
  print(paste("processing", i, sep = " "))
  my.imdb.linkdata[[i]] = getlinks_imdb(i)
  # waiting one second between hits
  Sys.sleep(1)
  cat(" done!\n")
}

#converting into a data frame
library("plyr")
df.imdb = ldply(my.imdb.linkdata)

#Creating list of links
imdb.secondary.links=(paste("http://www.imdb.com", sep = "", df.imdb$my.link.link))
head(imdb.secondary.links,20)


# - - - - - - - - - - - - - - - -- - - - - -- - - - -- - - - - - 


#Function to collect links to every movie on the list
scrape_imdb = function(link2){
  my.link2 = read_html(link2, encoding = "UTF-8")
  imdb.title = my.link2 %>% 
    html_nodes(".header .itemprop") %>% 
    html_text()
  imdb.rating = my.link2 %>% 
    html_nodes("strong span") %>% 
    html_text()
  imdb.year = my.link2 %>% 
    html_nodes(".header a") %>% 
    html_text()
  imdb.runtime = my.link2 %>% 
    html_nodes("#overview-top time") %>% 
    html_text()
  imdb.numberofratings = my.link2 %>% 
    html_nodes(".star-box-details a:nth-child(3) span") %>% 
    html_text()
  imdb.metascore = my.link2 %>% 
    html_nodes(".star-box-details a:nth-child(4)") %>% 
    html_text()
  imdb.budget = my.link2 %>% 
    html_nodes("#titleDetails :nth-child(13)") %>% 
    html_text()
  return(cbind(imdb.title, imdb.rating, imdb.year, imdb.runtime,imdb.numberofratings, imdb.metascore, imdb.budget))
}

#Loop data to a list
my.imdb.data = list() # initialize empty list
for (i in imdb.secondary.links[1:150]){
  print(paste("processing", i, sep = " "))
  my.imdb.data[[i]] = scrape_imdb(i)
  # waiting one second between hits
  #Sys.sleep(1)
 # cat(" done!\n")
}

df.imdb.data = ldply(my.imdb.data)

