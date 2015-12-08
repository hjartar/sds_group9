library("rvest")
library(stringr)
library("dplyr")

link = "https://www.goodreads.com/list/show/2451.I_Saw_the_Movie_Read_the_Book"

for(i in 0:99){
  book.links=(paste("https://www.goodreads.com/list/show/2451.I_Saw_the_Movie_Read_the_Book?page=", 0:i, sep = ""))
}
head(book.links,20)

#func
scrape_bribe = function(link){
  my.link = read_html(link, encoding = "UTF-8")
  my.link.title = my.link %>% 
    html_nodes(".bookTitle span") %>% 
    html_text()
  my.link.ratingtext = my.link %>% 
    html_nodes(".minirating") %>% 
    html_text()
  my.link.authorname = my.link %>% 
    html_nodes(".authorName span") %>% 
    html_text()
  return(cbind( my.link.title, my.link.ratingtext,  my.link.authorname ))
}

#Loop data to a list
my.book.data = list() # initialize empty list
for (i in book.links[1:100]){
  print(paste("processing", i, sep = " "))
  my.book.data[[i]] = scrape_bribe(i)
  # waiting one second between hits
  Sys.sleep(1)
  cat(" done!\n")
}

#converting into a data frame
library("plyr")
df.book = ldply(my.book.data)


df.book$rating= as.numeric(gsub( " .*$", "", df.book$my.link.ratingtext))

save(df.book, file="~/Desktop/books.Rda")

library(ggplot2)
p = ggplot(data = df.book, aes(x = rating)) # data & aesthetics
p = p + geom_density() # add geom
plot(p)
