######################### PDS HW 3 ##################################
## Author: Crystal Caragine
## Date: 2/26/2020
## Class: Political Data Science
## Saved As: PDS_HW3_Caragine


library(ggplot2)
library(dplyr)
library(tidyr)
library(fivethirtyeight) 
library(tidyverse) 
install.packages('tm') 
library(tm) 
#install.packages('lubridate') 
library(lubridate) 
#install.packages('wordcloud') 
library(wordcloud)
library(qdap)
library(stopwords)
#install.packages("RColorBrewer")
library(RColorBrewer)
#install.packages("wordcloud2")
library(wordcloud2)
library(tidytext)

##################### Question 1 
# Downloading/ subsetting data
primaryPolls<-read.csv('https://jmontgomery.github.io/PDS/Datasets/president_primary_polls_feb2020.csv', stringsAsFactors = F)
primaryPolls$start_date<-as.Date(primaryPolls$start_date, "%m/%d/%Y")
primaryPollssuper<-primaryPolls[primaryPolls$state %in% c("Alabama", "Arkansas", "California", "Colorado", "Maine", "Massachusetts", "Minnesota", 
                                                                "North Carolina", "Oklahoma", "Tennessee", "Texas", "Utah", "Vermont", "Virginia"), ]
primaryPollssuper<-primaryPollssuper[primaryPollssuper$candidate_name%in%c("Amy Klobuchar", "Bernard Sanders", "Elizabeth Warren", "Joseph R. Biden Jr.", "Michael Bloomberg", "Pete Buttigieg", "Tom Steyer"),]

# creating plot
ggplot(data=primaryPollssuper)+
  geom_point(mapping = aes(x=start_date, y=pct, color=candidate_name, 
                            linetype=candidate_name)) +
  facet_wrap(~ state, nrow=5) +
  ggtitle("Super Tuesday") +
  labs(y="Percent", x = "Date") +
  labs(colour = "Candidate") +
  theme_minimal()


######################## Question 2
# Subsetting data
POLL <-primaryPolls %>%
  filter(candidate_name %in% c("Amy Klobuchar", "Bernard Sanders", "Elizabeth Warren", 
                               "Joseph R. Biden Jr.", "Michael Bloomberg", "Pete Buttigieg"))

# creating state/candidate dyad
WIDEPOLL <- pivot_wider(POLL, id_cols=candidate_name, names_from=c(state), values_from=pct)

NEWPOLL <- POLL %>%
               group_by(candidate_name, state) %>%
               summarise(average_candidate=mean(pct), count=n())
object.size(NEWPOLL)
object.size(POLL)
# the original data set is much larger



######################## Question 3

polls <- read_csv('https://jmontgomery.github.io/PDS/Datasets/president_primary_polls_feb2020.csv') 
Endorsements <- endorsements_2020

# Renaming endorsee
Endorsements <- rename(Endorsements, candidate_name = endorsee)

# Creating a tibble
Endorsements <- as_tibble(Endorsements)

# Subsetting data
POLLS <- polls %>% 
  select(candidate_name, sample_size, start_date, party, pct) %>%
  filter(candidate_name %in% c("Amy Klobuchar", "Bernard Sanders","Elizabeth Warren", "Joseph R. Biden Jr.", "Michael Bloomberg", "Pete Buttigieg"))

# Comparing data
table(POLLS$candidate_name)
table(Endorsements$candidate_name)

POLLS$candidate_name <- recode(POLLS$candidate_name, "Bernard Sanders"="Bernie Sanders", "Joseph R. Biden Jr."="Joe Biden")

table(POLLS$candidate_name)

# Merging the data sets
MPOLLS <- inner_join(POLLS, Endorsements, by="candidate_name")
table(MPOLLS$candidate_name)

# Number of endorsements
basicPolls %>%
  group_by(candidate_name, state) %>%
  summarise(average_candidate=mean(pct), count=n())

number <- MPOLLS %>%
              group_by(candidate_name) %>%
              summarise(count=n())

# Plotting 
p<-ggplot(data=df, aes(x=dose, y=len)) +
  geom_bar(stat="identity")

p <- ggplot(data=number, aes(x=candidate_name, y=count, fill=candidate_name)) +
         geom_bar(stat="identity")

# Altering 
pdf("C:/Users/cmcar/OneDrive/Documents/GitHub/PS3/numberbar.pdf")
p + theme_dark()
dev.off()

# Improving 
pdf("C:/Users/cmcar/OneDrive/Documents/GitHub/PS3/numberbarbetter.pdf")
p + theme_classic() + 
  ggtitle("Endorsements") +
  labs(y="Number of Endorsements", x = "Candidate") +
  coord_flip() +
  theme(legend.position = "none")
dev.off()

###################### Question 4
TWEETS <- read_csv('https://politicaldatascience.com/PDS/Datasets/trump_tweets.csv')

# Date and Time
hours <- format(as.POSIXct(strptime(TWEETS$created_at,"%m/%d/%Y %H:%M",tz="")) ,format = "%H:%M")
dates <- format(as.POSIXct(strptime(TWEETS$created_at,"%m/%d/%Y %H:%M",tz="")) ,format = "%m/%d/%Y")

dates <- mdy(dates)
class(dates)
dates[c(which.min(dates), which.max(dates))]
# this data ranges from 1/1/2014 to 2/14/2020

# removing retweets
NOTWEETS <- filter(TWEETS, is_retweet==FALSE)

TWEETS.POP <- arrange(NOTWEETS, desc(retweet_count))
head(TWEETS.POP$retweet_count)
head(TWEETS.POP$text)

TWEETS.RE <- arrange(NOTWEETS, desc(favorite_count))
head(TWEETS.RE$favorite_count)
head(TWEETS.RE$text)

# Removing extra stuff 
TWEETS$textrm <- trimws(TWEETS$text)
TWEETS$textrm <- removeNumbers(TWEETS$textrm)
TWEETS$textrm <- removePunctuation(TWEETS$textrm)
TWEETS$textrm <- tolower(TWEETS$textrm)
words <- stopwords()
TWEETS$textrm <- removeWords(TWEETS$textrm, words)
words2 <- c('people', 'new','want','one','even','must','need','done','back','just','going', 'know', 'can','said','like','many','like','realdonaldtrump')
TWEETS$textrm <- removeWords(TWEETS$textrm, words2)

# Creating wordcloud
textdata<-Corpus(VectorSource(TWEETS$textrm))

dtm <- TermDocumentMatrix(textdata) 
matrix <- as.matrix(dtm) 
words <- sort(rowSums(matrix),decreasing=TRUE) 
df <- data.frame(word = names(words),freq=words)

tweets_words <-  TWEETS %>%
  unnest_tokens(word, textrm)
words <- tweets_words %>% group_by(word) %>% tally()



wordcloud(words = words$word, freq=words$n, min.freq = 3, max.words=50, colors=brewer.pal("random-dark"))

wordcloud2(data=TWEETS$textrm, size=1.6, color='random-dark')



head(TWEETS$textrm)




