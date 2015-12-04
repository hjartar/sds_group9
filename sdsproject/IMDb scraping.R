library("rvest")
library("stringr")
library("dplyr")

#---------------------------------------------------------------------------------
#---------------------------------------------------------------------------------

#Generating links to loop through all movies "based-on-novel".
for(i in 0:3){
  imdb.links=(paste("http://www.imdb.com/search/keyword?keywords=based-on-novel&sort=moviemeter,asc&mode=detail&page=", 0:i, sep = "","&ref_=kw_nxt"))
}
head(imdb.links)


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
for (i in imdb.links[1:100]){
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
head(imdb.secondary.links)

#---------------------------------------------------------------------------------
#---------------------------------------------------------------------------------


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
  imdb.leadactor = my.link2 %>% 
    html_nodes(":nth-child(10) a:nth-child(2) .itemprop") %>% 
    html_text()
  return(cbind(imdb.title, imdb.rating, imdb.year, imdb.runtime,imdb.numberofratings, imdb.metascore, imdb.leadactor))
}

#Loop data to a list
my.imdb.data = list() # initialize empty list
for (i in imdb.secondary.links[1:100]){
  print(paste("processing", i, sep = " "))
  my.imdb.data[[i]] = scrape_imdb(i)
  # waiting one second between hits
  Sys.sleep(1)
  cat(" done!\n")
}

df.imdb.data = ldply(my.imdb.data)

# END OF SCRAPING

#---------------------------------------------------------------------------------
#---------------------------------------------------------------------------------

# BEGINNING OF CLEANING
# Cleaning IMDb data

df.imdb.clean=df.imdb.data

#Correcting runtime variable
df.imdb.clean$imdb.runtime = gsub(" min","", df.imdb.clean$imdb.runtime) #remove trailing characters
df.imdb.clean$imdb.runtime = gsub("(^ +)|( +$)", "", df.imdb.clean$imdb.runtime) #remove trailing blanks
df.imdb.clean$imdb.runtime <- as.numeric(df.imdb.clean$imdb.runtime) #Convert to numeric

#Correcting number of ratins variable:
df.imdb.clean$imdb.numberofratings = gsub(",","", df.imdb.clean$imdb.numberofratings) #remove thousand separator
df.imdb.clean$imdb.numberofratings = as.numeric(df.imdb.clean$imdb.numberofratings) #Convert to numeric

#Correcting meta score variable:
df.imdb.clean$imdb.metascore = gsub("/100","", df.imdb.clean$imdb.metascore) #Change to score out of hundred.
df.imdb.clean$imdb.metascore = as.numeric(df.imdb.clean$imdb.metascore) #Convert to numeric
df.imdb.clean$imdb.metascore = df.imdb.clean$imdb.metascore/10 #Divide by 10 to get at number on the same scale as imdb ratings.


#filter on runtime, to get rid of TV-shows (Assuming TV-shows are no longer than 65 minutes and relevant movies are longer than 65 minutes - Think about possible improvements for this)
df.imdb.clean = filter(df.imdb.clean, df.imdb.clean$imdb.runtime > 65)

class(df.imdb.clean$imdb.numberofratings)

#Remove NA's (And thus hopefully removing TV-series)
df.imdb.clean=na.omit(df.imdb.clean) #THINK ABOUT POSSIBLE PROBLEMS WITH THIS

#Removing irrelevant variables:
df.imdb.clean = subset(df.imdb.clean, select = -.id) # .id-string

#Changing name of the title variable
df.imdb.clean$title=df.imdb.clean$imdb.title

#Selecting only relevant variables
df.imdb.clean=select(df.imdb.clean, -imdb.title)

#---------------------------------------------------------------------------------
#---------------------------------------------------------------------------------

#CLEANING GOODREAD DATA
df.book.clean = df.book

#Changing variable names to something more simple
df.book.clean$title=df.book.clean$my.link.title
df.book.clean$book.author=df.book.clean$my.link.authorname
df.book.clean$book.rating=df.book.clean$my.link.ratingtext

#Selecting only relevant variables
df.book.clean=select(df.book.clean, title, book.author, book.rating)

#Splitting rating variable in two - First part gonna be used for rating, second part for numberofratings  
splitstring=str_split(df.book.clean$book.rating,"—",n=Inf)
head(splitstring)
unique(splitstring)

#unlisting splitstring to dataframe
df <- data.frame(matrix(unlist(splitstring), nrow=1443, byrow=T))

#Converting X1 and X2 to characters:
df$X1 <- as.character(df$X1)
df$X2 <- as.character(df$X2)

#stringmanipulations on the two new variables:
df$X1 = gsub("(\\d\\.\\d+\\s\\w+\\s\\d\\s\\w+\\s)","", df$X1) #Cleaning X1 by Regex
df.book.clean$book.rating=gsub(" avg rating","",df$X1) #Inserting in the df.book.clean data frame

df$X2= gsub(",","", df$X2) #Cleaning X2 by removing commas
df.book.clean$books.numberofratings=gsub(" ratings","",df$X2) #Inserting in the df.book.clean data frame

#---------------------------------------------------------------------------------
#---------------------------------------------------------------------------------

# Getting rid of duplicates (However: The Hunger Games (The Hunger Games, #1) appear twice due to one ekstra rating in one observation)
df.book.unique=unique(df.book.clean)
df.imdb.unique=unique(df.imdb.clean)


# Merging the two data frames by an inner join on title.
df.merged=join(df.book.unique, df.imdb.unique,
     type = "inner")






