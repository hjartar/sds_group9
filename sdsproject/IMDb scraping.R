library("rvest")
library("stringr")
library("dplyr")

#---------------------------------------------------------------------------------
#-------SCRAPING OF IMDb DATA-----------------------------------------------------
#---------------------------------------------------------------------------------

#Generating links to loop through all movies "based-on-novel".
for(i in 0:414){
  imdb.links <- (paste("http://www.imdb.com/search/title?at=0&keywords=based-on-novel&sort=moviemeter&start=", (0:i)*50+1, sep = "","&title_type=feature&year=1900,2015"))
}
head(imdb.links)

#Function to collect links to every movie on the list
getlinks_imdb <- function(link){
  my.link <- read_html(link, encoding = "UTF-8")
  my.link.link <- my.link %>% 
    html_nodes(".title") %>% 
    html_node("a") %>% 
    html_attr("href")
  return(cbind(my.link.link))
}

#Loop data to a list
my.imdb.linkdata <- list() # initialize empty list
for (i in imdb.links[1:414]){
  print(paste("processing", i, sep = " "))
  my.imdb.linkdata[[i]] = getlinks_imdb(i)
  # waiting one second between hits
  Sys.sleep(1)
  cat(" done!\n")
}

#converting into a data frame
library("plyr")
df.imdb <- ldply(my.imdb.linkdata)
save(my.imdb.linkdata, file="~/Desktop/my.imdb.linkdata.Rda")

#Creating list of links
imdb.secondary.links=(paste("http://www.imdb.com", sep = "", df.imdb$my.link.link))
head(imdb.secondary.links)

#---------------------------------------------------------------------------------
#---------------------------------------------------------------------------------

#Function to collect links to every movie on the list
scrape_imdb <- function(link2){
  my.link2 <- read_html(link2, encoding = "UTF-8")
  imdb.title <- my.link2 %>% 
    html_nodes(".header .itemprop") %>% 
    html_text()
  imdb.rating <- my.link2 %>% 
    html_nodes("strong span") %>% 
    html_text()
  imdb.year <- my.link2 %>% 
    html_nodes(".header a") %>% 
    html_text()
  imdb.runtime <- my.link2 %>% 
    html_nodes("#overview-top time") %>% 
    html_text()
  imdb.numberofratings <- my.link2 %>% 
    html_nodes(".star-box-details a:nth-child(3) span") %>% 
    html_text()
  imdb.metascore <- my.link2 %>% 
    html_nodes(".star-box-details a:nth-child(4)") %>% 
    html_text()
  imdb.leadactor <- my.link2 %>% 
    html_nodes(":nth-child(10) a:nth-child(2) .itemprop") %>% 
    html_text()
  return(cbind(imdb.title, imdb.rating, imdb.year, imdb.runtime,imdb.numberofratings, imdb.metascore, imdb.leadactor))
}

#Loop data to a list
my.imdb.data <- list() # initialize empty list
for (i in imdb.secondary.links[1:20665]){
  print(paste("processing", i, sep = " "))
  my.imdb.data[[i]] <- scrape_imdb(i)
  # waiting one second between hits
  Sys.sleep(1)
  cat(" done!\n")
}

df.imdb.data <- ldply(my.imdb.data)

#---------------------------------------------------------------------------------
#-------SCRAPING OF GOODREADS DATA-------------------------------------------------
#---------------------------------------------------------------------------------

link <- "https://www.goodreads.com/list/show/2451.I_Saw_the_Movie_Read_the_Book"

for(i in 0:99){
  book.links <- (paste("https://www.goodreads.com/list/show/2451.I_Saw_the_Movie_Read_the_Book?page=", 0:i, sep = ""))
}
head(book.links,20)

#func
scrape_bribe <- function(link){
  my.link = read_html(link, encoding = "UTF-8")
  my.link.title <- my.link %>% 
    html_nodes(".bookTitle span") %>% 
    html_text()
  my.link.ratingtext <- my.link %>% 
    html_nodes(".minirating") %>% 
    html_text()
  my.link.authorname <- my.link %>% 
    html_nodes(".authorName span") %>% 
    html_text()
  return(cbind( my.link.title, my.link.ratingtext,  my.link.authorname ))
}

#Loop data to a list
my.book.data <- list() # initialize empty list
for (i in book.links[1:100]){
  print(paste("processing", i, sep = " "))
  my.book.data[[i]] <- scrape_bribe(i)
  # waiting one second between hits
  Sys.sleep(1)
  cat(" done!\n")
}

#converting into a data frame
library("plyr")
df.book <- ldply(my.book.data)


df.book$rating <- as.numeric(gsub( " .*$", "", df.book$my.link.ratingtext))



# END OF SCRAPING

# BEGINNING OF CLEANING

#---------------------------------------------------------------------------------
#-------CLEANING OF IMDb DATA-----------------------------------------------------
#---------------------------------------------------------------------------------


df.imdb.clean <- df.imdb.data

#Changing name of the title variable
df.imdb.clean$title <- df.imdb.clean$imdb.title

#Correcting runtime variable
df.imdb.clean$imdb.runtime <- gsub(" min","", df.imdb.clean$imdb.runtime) #remove trailing characters
df.imdb.clean$imdb.runtime <- gsub("(^ +)|( +$)", "", df.imdb.clean$imdb.runtime) #remove trailing blanks
df.imdb.clean$imdb.runtime <- as.numeric(df.imdb.clean$imdb.runtime) #Convert to numeric

#Correcting number of ratins variable:
df.imdb.clean$imdb.numberofratings <- gsub(",","", df.imdb.clean$imdb.numberofratings) #remove thousand separator
df.imdb.clean$imdb.numberofratings <- as.numeric(df.imdb.clean$imdb.numberofratings) #Convert to numeric

#Correcting meta score variable:
df.imdb.clean$imdb.metascore <- gsub("/100","", df.imdb.clean$imdb.metascore) #Change to score out of hundred.
df.imdb.clean$imdb.metascore <- as.numeric(df.imdb.clean$imdb.metascore) #Convert to numeric
df.imdb.clean$imdb.metascore <- df.imdb.clean$imdb.metascore/10 #Divide by 10 to get at number on the same scale as imdb ratings.

#Remove suffix from sequels
df.imdb.clean$title <- gsub("- Part \\d","", df.imdb.clean$title) 
df.imdb.clean$title <- gsub(": Part \\d","", df.imdb.clean$title) 
df.imdb.clean$title <- gsub(", Part \\d","", df.imdb.clean$title) 
df.imdb.clean$title <- gsub("- Part [A-z]*","", df.imdb.clean$title) 
df.imdb.clean$title <- gsub(": Part [A-z]*","", df.imdb.clean$title) 


class(df.imdb.clean$imdb.numberofratings)

#Removing irrelevant variables:
df.imdb.clean <- subset(df.imdb.clean, select = -.id) # .id-string

#Selecting only relevant variables
df.imdb.clean <- select(df.imdb.clean, -imdb.title)

#---------------------------------------------------------------------------------
#---------------------------------------------------------------------------------

#CLEANING GOODREAD DATA
df.book.clean <- df.book

#Changing variable names to something more simple
df.book.clean$title <- df.book.clean$my.link.title
df.book.clean$book.author <- df.book.clean$my.link.authorname
df.book.clean$book.rating <- df.book.clean$my.link.ratingtext

#Selecting only relevant variables
df.book.clean <- select(df.book.clean, title, book.author, book.rating)

#Splitting rating variable in two - First part gonna be used for rating, second part for numberofratings  
splitstring <- str_split(df.book.clean$book.rating,"—",n=Inf)


#unlisting splitstring to dataframe
df <- data.frame(matrix(unlist(splitstring), nrow=dim(df.book.clean)[1], byrow=T))

#Converting X1 and X2 to characters:
df$X1 <- as.character(df$X1)
df$X2 <- as.character(df$X2)

#stringmanipulations on the two new variables:
df$X1 <- gsub("(\\d\\.\\d+\\s\\w+\\s\\d\\s\\w+\\s)","", df$X1) #Cleaning X1 by Regex
df.book.clean$book.rating <- gsub(" avg rating","",df$X1) #Inserting in the df.book.clean data frame

df$X2 <- gsub(",","", df$X2) #Cleaning X2 by removing commas
df.book.clean$books.numberofratings=gsub(" ratings","",df$X2) #Inserting in the df.book.clean data frame
summary(dfplot)
#---------------------------------------------------------------------------------
#---------------------------------------------------------------------------------

# Getting rid of duplicates (However: The Hunger Games (The Hunger Games, #1) appear twice due to one ekstra rating in one observation)
df.book.unique <- unique(df.book.clean)
df.imdb.unique <- unique(df.imdb.clean)
#Remove parenthesised comment in title stating order in a book series. 
df.book.unique$title <- gsub("\\s*\\([^\\)]+\\)","",as.character(df.book.unique$title))

# Merging the two data frames by an inner join on title.
df.merged <- join(df.book.unique, df.imdb.unique,
               type = "inner")

#---------------------------------------------------------------------------------
#-------PLOT RATINGS-----------------------------------------------------------------
#---------------------------------------------------------------------------------

# Remove empty observations
dfplot <- df.merged %>%
  filter(!is.na(imdb.rating)) %>%
  filter(!is.na(book.rating))  %>%
  data.frame

dfplot$imdb.rating <- as.character(dfplot$imdb.rating)

dfplot$imdb.rating <- as.numeric(dfplot$imdb.rating)
dfplot$book.rating <- as.numeric(dfplot$book.rating)
dfplot$books.numberofratings <- as.numeric(dfplot$books.numberofratings)


# Create standardized and naive variables
bookshit=2 #Minimum of Goodreads
imdbshit=1.5 #Minimum of IMDb
  
summary(dfplot)

summary(dfplot)
dfplot <- mutate(dfplot,
                 booknorm = (book.rating - bookshit)/(max(book.rating)-bookshit),
                 movienorm = (imdb.rating - imdbshit)/(max(imdb.rating)-imdbshit),
                 disnorm= booknorm-movienorm,
                 booknaive = book.rating*2,
                 disnaive = booknaive-imdb.rating)
dfplot <- mutate(dfplot,
                 disnaivesign  =sign(dfplot$disnaive),
                 disnormsign =sign(dfplot$disnorm))

dfplot$disnaivesign <- as.factor(dfplot$disnaivesign)
dfplot$disnormsign <- as.factor(dfplot$disnormsign)

# Share of books rated better than movies, under naive comparison
table(sign(dfplot$disnaive))[3]/dim(dfplot)[1]
#0.8929385 


# Share of books rated better than movies, under standardized comparisaon
table(sign(dfplot$disnorm))[2]/dim(dfplot)[1]
# 0.4738041 

library(ggplot2)
# Plot of naive comparison: Goodreads rating is multiplied by 2
naive <- ggplot(dfplot, aes(x=booknaive, y=imdb.rating, group=disnaivesign, colour=disnaivesign))
naive <- naive +  geom_jitter(aes(), alpha=0.5)+ geom_abline(intercept = 0, slope = 1, linetype = 2)+ 
  theme(legend.position="bottom") +
  scale_y_continuous(name = "IMDb Rating" , limits = c(4,10)) + scale_x_continuous(name= "Goodreads rating * 2", limits = c(4,10)) + # remove outliers
  scale_colour_manual(name ="Difference",
                    breaks = c(-1,0,1),
                    labels = c("Movie is better", "Same rating", "Book is better"),
                    values = c("blue","green", "red"))+ 
  geom_abline(intercept = 0, slope = 1, linetype = 2, color="black")
naive 
table(dfplot$disnaivesign)
ggsave(plot = naive, 
       file = "~/Desktop/naive.png",
       height = 6, width = 6)

# Linear regression of standardised ratings
olsslope <- lm(dfplot$movienorm~ dfplot$booknorm)$coefficients[2] # Slope
olsint <- lm(dfplot$moviestand ~ dfplot$bookstand)$coefficients[1] # Intercept

lm(dfplot$movienorm~ dfplot$booknorm)
t.test(dfplot$movienorm , dfplot$booknorm)

# Plot of standardised comparison
summary(dfplot)
norm <- ggplot(dfplot, aes(x=booknorm, y=movienorm))
norm <- norm +  geom_jitter(aes(colour = disnormsign), alpha=0.5)+ 
  scale_y_continuous(limits = c(0,1.01)) + scale_x_continuous(limits = c(0,1.01)) + # remove outliers
  stat_smooth(method=lm) + #OLS 
  theme(legend.position="bottom") +
  #stat_smooth(method=loess) + #LOESS+
  scale_colour_manual(name ="Difference",
                      breaks = c(-1,0,1),
                      labels = c("Movie is better", "Same rating", "Book is better"),
                      values = c("blue","green", "red"))+ 
  geom_abline(intercept = 0, slope = 1, linetype = 2, color="black") #+#45 degree line
#geom_abline(intercept = olsint, slope = olsslope, linetype = 1) # ols with outliers
norm
ggsave(plot = norm, 
       file = "~/Desktop/norm.png",
       height = 6, width = 6)


# Compare number of ratings

ratings <- ggplot(dfplot, aes(y=book.rating, x=books.numberofratings))
ratings <- ratings +  geom_jitter()+ stat_smooth(method=lm) +  ggtitle("Goodreads ratings versus \nnumber of votes")+
  scale_x_log10(name="Log of number of votes")  +  scale_y_continuous(name= "Goodreads Rating") 
ratings

ratings2 <- ggplot(dfplot, aes(y=imdb.rating, x=imdb.numberofratings))
ratings2 <- ratings2 +  geom_jitter()+ ggtitle("IMDb ratings versus \nnumber of votes")+
  scale_x_log10(name="Log of number of votes") + stat_smooth(method=lm) +  scale_y_continuous(name= "IMDb Rating") 
ratings2

ggsave(plot = ratings2, 
       file = "~/Desktop/ratingsimdb.png",
       height = 6, width = 8)
ggsave(plot = ratings, 
       file = "~/Desktop/ratingsbooks.png",
       height = 6, width = 8)
# Plot the distributions of the naive goodreads and imdb ratings

library(readr)


#Naive bell curves

df.bell <- select(dfplot, title, imdb.rating, booknaive)
summary(df.bell)

df.bell <- melt(df.bell, id.var="title")
df.bell <- rename(df.bell, c("value"="Rating"))

bell <- ggplot(df.bell, aes(x = Rating, group = variable, fill = variable)) + geom_density(alpha=0.3) +
  scale_fill_manual(name="Site",values=c("red","blue"), labels=c("IMDb","Goodreads * 2"))+theme(legend.position="bottom")
bell



#Normalised bell curves
df.bell <- select(dfplot, title, movienorm, booknorm)
df.bell <- melt(df.bell, id.var="title")
df.bell <- rename(df.bell, c("value"="Rating"))

bell <- ggplot(df.bell, aes(x = Rating, group = variable, fill = variable))  + geom_density(alpha=0.3)+
  scale_fill_manual(name="Site",values=c("red","blue"), labels=c("IMDb","Goodreads * 2")) +theme(legend.position="bottom")
bell

ggsave(plot = bell, 
       file = "~/Desktop/ratingnorm.png",
       height = 6, width = 6)

bell <- ggplot(dfplot, aes(x = disstand))
bell + geom_density(alpha=0.5)


wilcox.test(dfplot$booknaive,dfplot$imdb.rating, paired=TRUE)

wilcox.test(dfplot$booknorm,dfplot$movienorm, paired=TRUE)

summary(dfplot)
shapiro.test(dfplot$booknaive)
shapiro.test(dfplot$imdb.rating)

save(dfplot, file="~/Desktop/dfplot.Rda")

