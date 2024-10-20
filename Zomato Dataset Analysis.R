
#                      Zomato Dataset Analysis

# Business Problem: Where should you eat! What you should eat! Will it be good!  
# These are the questions always on mind not just when you are travelling but 
# also in day to day life. No one wants a bad experience. Everyone wants to 
# eat the most popular dish of a place and at the best restaurant and at a 
# reasonable rate. But HOW??  
  

# Include libraries ----
install.packages("wordcloud")
install.packages("RColorBrewer")
install.packages("wordcloud2")
install.packages("tm")
install.packages("tidyverse")
install.packages("descr")
install.packages("magrittr")
install.packages("highcharter")
install.packages("gridExtra")


library(wordcloud)
library(RColorBrewer)
library(wordcloud2)
library(tm)
library(gridExtra)
library(tidyverse)
library(descr)
library(magrittr)
library(highcharter)

#
# Read file ----
zomato <- read.csv('E:/NMIMS/zomato.csv')
#
# Dimension of data ----
cat("\n dimension of data = ", dim(zomato))
## After execution we get to know that the data set has 8652 observations and
## 21 variables
#
# View data ----
cat("Viewing the data")
cat("\n Viewing Top 5 rows of data")
head(zomato, 5)
cat("\n Viewing last 5 rows of data")
tail(zomato, 5)

#
# Structure of the data ----
cat("\n structure of the data \n")
str(zomato)
# After execution, we get to know that structure of the data set is data.frame
# with variables having a mix of int,chr and num data types. 
#
# Summary of the data ----
cat("\n Summary of data = ") 
summary(zomato)
# It shows the summary of all the numeric columns in the data set
#
# Total Null values in each column---- 
cat("\n Total number of missing 'NA' values in each column \n")
colSums(is.na(zomato))
# After execution, we get to know that there are no null values in the data. 
#
#
# In which cities are these restaurants present in ?  ----
zomato %>%
select(Restaurant.ID,City) %>%
unique() %>%
group_by(City) %>%
summarise(n=n()) %>%
ggplot(aes(x=reorder(City,n),y=n))+geom_bar(stat='identity',fill='#8b0000')+
coord_flip() + 
labs(title="City-wise distribution in India", x="City", y ="No of outlets") + 
theme(plot.title = element_text(size = 10, face = "bold"))
# A lot of restaurants in the dataset are located in Delhi, NCR, Noida. 
# There are a high chances that the dataset is biased
#
# Is the data biased? ---- 
zomato %>%
  group_by(City) %>%
  summarise(Pct=(n()/nrow(zomato))*100) %>%
  top_n(n = 10, wt = Pct) %>%
  arrange(-Pct)
#
# Nearly 85% of the data belongs to restaurants in NCR region
# NCR Data ----
zomato <- zomato[zomato$City %in% c("New Delhi", "Gurgaon",
                                    "Noida","Faridabad"),]
zomato$City <- factor(zomato$City)
# 
# Data Seperation city-wise
# Restaurant distribution of processed data ----
zomato %>%
  select(Restaurant.ID,City) %>%
  unique() %>%
  group_by(City) %>%
  summarise(n=n()) %>%
  ggplot(aes(x=reorder(City,n),y=n))+geom_bar(stat='identity',fill='#6A8B41')+
  coord_flip() + 
  labs(title="City-wise distribution of restaurants in NCR", 
       x="City", y ="No of outlets") + 
  theme(plot.title = element_text(size = 12, face = "bold"))
#
# What is the average cost for two? ----
# the average cost for two is mainly between 0-2000 
ggplot(data = zomato, aes(x=Average.Cost.for.two)) + 
       geom_histogram(bins=75,fill='#6A8B41')
###
# Descriptive statistics on Cost for 2 ----
descr(zomato$Average.Cost.for.two)
#
# price range distribution ----
##price ranges are: 1 is 0-500
##2 is 500-1000
##3 is 1000-1500
##4 is 1500+
ggplot(data = zomato, aes(factor(Price.range), fill=factor(Price.range))) + 
  geom_bar(fill = "#800000") + 
  ggtitle("Price Range Distribution") + 
  theme(plot.title = element_text(hjust = 0.5)) +
  xlab('Price Range') + ylab('Number of Restaurants') +
  guides(fill=guide_legend(title="Price Category Legend"))

#
# some FAQ's for services asked by customers ----

##Yes/No Stats about Indian Restaurants
p1 <- ggplot(data = zomato) + 
  geom_bar(aes(x=factor(Has.Online.delivery)), fill=c('#800000','#6A8B41')) + 
  xlab('Has Online Delivery') +ylab('No of Restaurants') 

p2 <- ggplot(data = zomato) + 
  geom_bar(aes(x=factor(Has.Table.booking)),fill=c('#800000','#6A8B41')) + 
  xlab('Has Table Booking') + ylab('No of Restaurants') 

gridExtra::grid.arrange(p1,p2,ncol=2,
                        nrow=1,top="Yes/No Stats About Indian Restaurants")

## 2200 restaurants have online delivery
## 1000 restaurants have table booking

#
# How many restaurants have Online Delivery in NCR? ----
zomato %>%
  select(Restaurant.ID,Has.Online.delivery,City) %>%
  unique() %>%
  group_by(City,Has.Online.delivery) %>%
  summarise(n=n()) %>%
  ungroup() %>%
  rename(`Online Delivery Service`=Has.Online.delivery) %>%
  #
  ggplot(aes(x=reorder(City,n),y=n,fill=`Online Delivery Service`))+
  geom_bar(stat='identity',width = 0.5,position = 'dodge')+
  labs(x='City',y='Number of Restaurants')+coord_flip()+
  theme(plot.title = element_text(size = 10, face = "bold"))
#
# Seperating the data for cities of NCR ----
zomato_delhi <- zomato[zomato$City %in% c("New Delhi"),]
zomato_gurgaon <- zomato[zomato$City %in% c("Gurgaon"),]
zomato_noida <- zomato[zomato$City %in% c("Noida"),]
zomato_faridabad <- zomato[zomato$City %in% c("Faridabad"),]

#
# City wise popular cuisines ----

# Graph for delhi
zomato_delhi$Cuisines <- factor(zomato_delhi$Cuisines)
zomato_delhi_data <- zomato_delhi %>%
  select(Restaurant.Name,City,Aggregate.rating,Cuisines)%>%
  group_by(Cuisines) %>%
  summarise(Count = n()) %>% 
  top_n(9)

cuisine_delhi_plot <- ggplot(zomato_delhi_data,aes(x = reorder(Cuisines,Count), 
                                              fill = Count, 
                                              y = Count, label = Count)) +
  coord_flip() + labs(x = "", y = "", title = "Top 9 Cuisines of New Delhi") + 
  theme_minimal() + geom_bar(stat = "identity", width = 0.5) +
  scale_fill_gradient(low = "#FC3F3F", high = "#800000")

cuisine_delhi_plot

# Graph for gurgaon  
zomato_gurgaon$Cuisines <- factor(zomato_gurgaon$Cuisines)
zomato_gurgaon_data <- zomato_gurgaon %>%
  select(Restaurant.Name,City,Aggregate.rating,Cuisines)%>%
  group_by(Cuisines) %>%
  summarise(Count = n()) %>% 
  top_n(9)

cuisine_gurgaon_plot <- ggplot(zomato_gurgaon_data,
                               aes(x = reorder(Cuisines,Count), fill = Count, 
                                   y = Count, label = Count)) +
  coord_flip() + labs(x = "", y = "", title = "Top 9 Cuisines of Gurgaon") + 
  theme_minimal() + geom_bar(stat = "identity", width = 0.5) +
  scale_fill_gradient(low = "#FC3F3F", high = "#800000")

cuisine_gurgaon_plot

# Graph for noida  
zomato_noida$Cuisines <- factor(zomato_noida$Cuisines)
zomato_noida_data <- zomato_noida %>%
  select(Restaurant.Name,City,Aggregate.rating,Cuisines)%>%
  group_by(Cuisines) %>%
  summarise(Count = n()) %>% 
  top_n(9)

cuisine_noida_plot <- ggplot(zomato_noida_data,aes(x = reorder(Cuisines,Count), 
                                                   fill = Count, 
                                                   y = Count, label = Count)) +
  coord_flip() + labs(x = "", y = "", title = "Top 9 Cuisines of Noida") + 
  theme_minimal() + geom_bar(stat = "identity", width = 0.5) +
  scale_fill_gradient(low = "#FC3F3F", high = "#800000")

cuisine_noida_plot

# graph for faridabad  
zomato_faridabad$Cuisines <- factor(zomato_faridabad$Cuisines)
zomato_faridabad_data <- zomato_faridabad %>%
  select(Restaurant.Name,City,Aggregate.rating,Cuisines)%>%
  group_by(Cuisines) %>%
  summarise(Count = n()) %>% 
  top_n(9)

cuisine_faridabad_plot <- ggplot(zomato_faridabad_data,
                          aes(x = reorder(Cuisines,Count), 
                          fill = Count, y = Count, label = Count)) +
  coord_flip() + labs(x = "", y = "", title = "Top 9 Cuisines of Faridabad") + 
  theme_minimal() + geom_bar(stat = "identity", width = 0.5) +
  scale_fill_gradient(low = "#FC3F3F", high = "#800000")

cuisine_faridabad_plot

#
# City wise average cost for 2  ----

# For Delhi
avg_cost_delhi <- zomato_delhi %>%
  group_by(Average.Cost.for.two) %>%
  summarise(Count = n()) %>% 
  mutate(Ratio = Count/sum(Count))

avg_cost_plot_delhi <- ggplot(avg_cost_delhi,aes(x = Average.Cost.for.two, 
                                                 fill = Average.Cost.for.two, 
                                                 y = Ratio, label = Count)) + 
  geom_bar(stat="identity")+theme_minimal() + 
  labs(x = "", y = "", title = "Avg Cost Distribution of New Delhi") +
  scale_fill_gradient(low = "#FC3F3F", high = "#800000")

avg_cost_plot_delhi

# For faridabad
avg_cost_faridabad <- zomato_faridabad %>%
  group_by(Average.Cost.for.two) %>%
  summarise(Count = n()) %>% 
  mutate(Ratio = Count/sum(Count))

avg_cost_plot_faridabad <- ggplot(avg_cost_faridabad,
                                  aes(x = Average.Cost.for.two, 
                                      fill = Average.Cost.for.two, 
                                      y = Ratio, label = Count)) + 
  geom_bar(stat="identity")+theme_minimal() + 
  labs(x = "", y = "", title = "Avg Cost Distribution of Faridabad") +
  scale_fill_gradient(low = "#FC3F3F", high = "#800000")

avg_cost_plot_faridabad

# For gurgaon
avg_cost_gurgaon <- zomato_gurgaon %>%
  group_by(Average.Cost.for.two) %>%
  summarise(Count = n()) %>% 
  mutate(Ratio = Count/sum(Count))

avg_cost_plot_gurgaon <- ggplot(avg_cost_gurgaon,
                                aes(x = Average.Cost.for.two, 
                                    fill = Average.Cost.for.two, 
                                    y = Ratio, label = Count)) + 
  geom_bar(stat="identity")+theme_minimal() + 
  labs(x = "", y = "", title = "Avg Cost Distribution of Gurgaon") +
  scale_fill_gradient(low = "#FC3F3F", high = "#800000")

avg_cost_plot_gurgaon

# For noida
avg_cost_noida <- zomato_noida %>%
  group_by(Average.Cost.for.two) %>%
  summarise(Count = n()) %>% 
  mutate(Ratio = Count/sum(Count))

avg_cost_plot_noida <- ggplot(avg_cost_noida,aes(x = Average.Cost.for.two, 
                                                 fill = Average.Cost.for.two, 
                                                 y = Ratio, label = Count)) + 
  geom_bar(stat="identity")+theme_minimal() + 
  labs(x = "", y = "", title = "Avg Cost Distribution of Noida") +
  scale_fill_gradient(low = "#FC3F3F", high = "#800000")

avg_cost_plot_noida

#
# Number of restaurants based on ratings ----

Rating <- as.numeric(zomato$Aggregate.rating,na.rm=TRUE)
Rating <- data.frame(Rating)
Rating %>% 
  group_by(Rating) %>%
  count() %>% 
  ggplot(aes(x=Rating,y=n)) + 
  geom_bar(stat="identity",fill="#800000")+
  theme_minimal()+geom_text(aes(label=n),size=3,vjust=-2.5)


#
# Does average cost impact table booking? ----
##price ranges are: 1 is 0-500
##2 is 500-1000
##3 is 1000-1500
##4 is 1500+
qplot(Has.Table.booking,Average.Cost.for.two,data=zomato, 
      facets=~Price.range, main="Table Booking v/s Price range") + 
  geom_boxplot(aes(fill = Has.Table.booking)) + 
  scale_fill_manual(values=c("#800000", 
                             "#6A8B41")) + theme(legend.position="none")

# For different price ranges, it shows the classification between average costs
# of restaurants who have table booking and restaurants who don't. 
# So it can be seen that for higher price range, the average cost is higher 
# that expected for both types of restaurants who has table booking and 
# those who don't. 

#
# Cost Vs Ratings ----
boxplot(Aggregate.rating~Average.Cost.for.two, data=zomato,
        main='Cost Vs Rating', ylab='Ratings', xlab='Average Cost')
#The restaurants in price range 1 (average cost < 500) have wide range of 
# ratings when compared with the restaurants with higher price range. The 
# costly restaurants have ratings when compared with others. 
# As price increases the good rating is given. Although the outliers in higher
# price range show that there are people who believe that costly restaurants
# are not always value for money.
#
# Votes Vs Rating ----
ggplot(data=zomato,
  aes(y=Aggregate.rating, x=Votes, group=1)) + geom_point()

# Even though the aggregate ratings are got from the sum of ratings given by a 
# group of customers, even in that case ratings does not increase based on the 
# no of votes. It increases based on the ratings given by the reviewers. So 
# rating does not depend on the no of votes
#
# Does average cost impact aggregate rating? ----
ggplot(data = zomato,aes(x=Average.Cost.for.two, y=Aggregate.rating,
                         color=Rating.color,size=Average.Cost.for.two)) + 
  geom_point(alpha=0.5)

# Not necessarily every high avg cost for two has a good rating.
# Top 5 localities with most number of votes ----
d=aggregate(Votes~Locality,zomato,sum)
samp1=d %>% arrange(desc(Votes)) %>% head(5)

ggplot(data = samp1,aes(x=Locality,y=Votes,fill=Locality))+
  geom_bar(stat = 'identity')+
  xlab('Locality')+
  ylab('Total Votes')+
  coord_flip()
  ggtitle('Top 5 Localities with most number of Votes')
#

# What are the most popular cuisines in NCR?(treemap) ----
  cuisines <- zomato %>%
    select(Cuisines) %>%
    mutate(Cuisines=strsplit(as.character(Cuisines), ", ")) %>%
    unnest(Cuisines) %>%
    group_by(Cuisines) %>%
    summarise(Count = n()) %>%
    arrange(desc(Count))%>%head(20)
  #
  hchart(cuisines, "treemap", hcaes(x = Cuisines, value = Count, color = Count))
  #
# Cuisine wordcloud ----
  
  zomato$Cuisines2 <- (sapply(zomato$Cuisines,
                              gsub,pattern = "\\,",replacement = " "))
  cuisine <- Corpus(VectorSource(zomato$Cuisines2))
  cuisine_dtm <- DocumentTermMatrix(cuisine)
  cuisine_freq <- colSums(as.matrix(cuisine_dtm))
  freq <- sort(colSums(as.matrix(cuisine_dtm)), decreasing=TRUE) 
  cuisine_wf <- data.frame(word = names(cuisine_freq), freq=cuisine_freq)
  
  pal2 <- brewer.pal(8,"Dark2")
  wordcloud(cuisine_wf$word,cuisine_wf$freq,random.order=FALSE,
            rot.per =.15, colors=pal2,scale=c(4,.9))
#