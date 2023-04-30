library(tidyverse)
library(reshape2)

dforig <- read_csv("file.csv")
df <- dforig
View(df)
glimpse(df)

options(scipen=999)


##################################
##Top hashtags
##################################




##### clean hashtag column
df$hashtags <- substr(df$hashtags, 2, nchar(df$hashtags)-1) #ie remove extra brackets on either end
df$hashtags[which(df$hashtags=="")] <- "nohashtag" #add nohashtag identifier 
glimpse(df)



##### separate hashtags from one  column
foo <- data.frame(do.call('rbind', strsplit(as.character(df$hashtags),', ',fixed=TRUE)))
glimpse(foo)

df2 <- cbind(df[, "id"], foo)
glimpse(df2)

df3 <- melt(df2, id.var = "id")
#df3 <- subset(df3, value!="nohashtag") #if we want to remove nohashtags
glimpse(df3)




##### remove duplicates
df3$concat <- paste(df3$id, df3$value)
df4 <- df3[!duplicated(df3$concat),]
glimpse(df4)



##### final summary
Hashtags <- df4 %>% count(value)
glimpse(Hashtags)
View(Hashtags)




##### plot on top hashtags
Tophashtags <- Hashtags %>% subset(n>500 & value!="nohashtag") 

Tophashtags %>% 
  ggplot(aes(x=n, y=reorder(value, n))) +
  geom_col() + 
  ylab("top hashtags") +
  xlab("number of tweets") +
  scale_x_continuous(breaks=seq(0, 23000, 5000)) + 
  theme_minimal()





##################################
##Node Edges by hashtag
##################################

####make complete dataframe
glimpse(df4)
View(df4)

#include account, date of tweet, mention, reply to
finaldf <- merge(x = df4, y = (df %>% select(id, date, user_id, mentions, reply_to)), by = "id", all.x = TRUE)

glimpse(finaldf)




#NODE CALCULATION - accounts tweeting the hashtag by date

finaldf$datehash <- paste(finaldf$date, finaldf$value)
nodes <- aggregate(data=finaldf, user_id ~ datehash, function(x) length(unique(x)))
glimpse(nodes)


#EDGES CALCULATION - mentions or replies

  
  
  
  
####final dataframe for plots - HASHBYDATE
  
HASHBYDATE <- finaldf  %>% group_by(date, value, datehash) %>% tally() 
HASHBYDATE <- merge(HASHBYDATE, nodes, by="datehash", all=TRUE) #add nodes
glimpse(HASHBYDATE)




##################################
##Node number plots
##################################


nodeplot <- HASHBYDATE %>% subset(value == "'covidvaccine'" | 
                                    value == "'russianvaccine'" |
                                    value == "'covid19india'" |
                                    value == "'trump'" |
                                    value == "'billgates'")


#nodes by Day
nodeplot %>% 
  ggplot(aes(x = date, y = user_id, color = value)) +
  geom_line() + 
  ylab("Nodes") +
  xlab("Date") +
  theme_minimal() + 
  facet_wrap(~value) +
  ylim(0,100)



#####SCRATCH###########

#df4 <- with(df3, table(id, value))
#glimpse(df4)
