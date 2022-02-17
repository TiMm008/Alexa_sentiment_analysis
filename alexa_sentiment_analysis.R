rm(list= ls())
# Packages needed --------------------------------------------------------------
install.packages("tidyverse")
install.packages("readr")
install.packages("RColorBrewer")
install.packages("tidytext")
install.packages("wordcloud")
install.packages("stringr")
install.packages("stringi")
install.packages("XML")
install.packages("RCurl")
install.packages("httr")
install.packages("tm")
install.packages("SnowballC")
install.packages("dplyr")
install.packages("sentimentr")
#Library -----------------------------------------------------------------------
library("tidyverse")
library("readr")
library("RColorBrewer")
library("tidytext")
library("wordcloud")
library("stringr")
library("stringi")
library("XML")
library("RCurl")
library("httr")
library("tm")
library("SnowballC")
library("dplyr")
library(sentimentr)
library(rtweet)
library(ggplot2)
library(data.table)
library(magrittr)
library(quanteda)
library(quanteda.textstats) # loads function textstat_frequency to name space

# Import dataset
reviews <- read_tsv("amazon_alexa.tsv")

# Load the data as a corpus
alexa_reviews <- VectorSource(reviews$verified_reviews)
reviews_corpus <- VCorpus(alexa_reviews)
#cleaning
clean_reviews <- tm_map(reviews_corpus, content_transformer(tolower))
clean_reviews <- tm_map(clean_reviews, removeNumbers)
clean_reviews <- tm_map(clean_reviews, removeWords, stopwords("en"))
clean_reviews <- tm_map(clean_reviews, removeWords, c("Alexa", "alexa", "Amazon", "amazon"))
clean_reviews <- tm_map(clean_reviews, removePunctuation)
clean_reviews <- tm_map(clean_reviews, stripWhitespace)

# Deleate teh duplicates
reviews<- reviews%>%
  distinct()

# Building a term-document matrix
clean_reviews_dtm <- TermDocumentMatrix(clean_reviews)
clean_reviews_m <- as.matrix(clean_reviews_dtm)
#sorting the words by the most frequent words
frequent_words <- rowSums(clean_reviews_m) %>%
  sort(decreasing = TRUE)
#the barchart of the 15 most frequent words
barplot(frequent_words[1:15], las = 2, col="lightblue", main ="Top 15 most frequent words",
        ylab = "Word frequencies")


# Set as a data table
setDT(reviews)

# Bar cahrt of the general rating
reviews %>%
  count(rating) %>%
  ggplot(aes(x = factor(rating), y = n, fill = factor(rating))) +
  geom_col(color = "black") +
  geom_text(aes(label = n), position = position_stack(vjust = 0.5)) +
  scale_fill_brewer("Blues") +
  labs(x = "Alexa Rating", y = "Number of Ratings") +
  ggtitle("Bar Chart of Alexa Ratings") +
  theme(legend.position = "none", plot.title = element_text(hjust = 0.5, face = "bold"))


# Sentiment analyisis --------------------------------------------------
require(sentimentr)

# Generate a table with the sentiment score for each review.
sentiment_by_review = 
  reviews$verified_reviews %>% get_sentences %>% sentiment_by()

# Add these scores to the table of all reviews
# Make sure that the columns of sentiment_by_review have not already been added to reviews
reviews[,colnames(sentiment_by_review) :=NULL]
# then add them to reviews
reviews_new = cbind(reviews,sentiment_by_review)
view(reviews_new)

# 10 most negatively charged tweets
negative_reviews <- reviews_new[,.(rating, verified_reviews,  word_count, ave_sentiment)][order(ave_sentiment)] %>% head(10)
view(negative_reviews)
summary(reviews_new$word_count)

# 10 most positively charged tweets
positive_reviews <- reviews_new[,.(rating, verified_reviews, word_count, ave_sentiment)][order(-ave_sentiment)] %>% head(10)
view(positive_reviews)

# Average sentiment overall:
hist(reviews_new$ave_sentiment, main = "Average sentiment per review", xlab = "Average sentiment", col = "light blue")
mean(reviews_new$ave_sentiment)

# The 10 most frequent negative terms
require(data.table)
require(magrittr)
require(sentimentr)

reviews_new[,list(verified_reviews),] %>% 
  get_sentences() %>%              # get sentences
  extract_sentiment_terms() %>%    # extract negative terms
  .[,negative] %>%                 # select the negative colum
  unlist %>%                       # unlist
  table  %>%                       # create freq table
  sort(decreasing = TRUE) %>% 
  head(10) %>% 
  as.data.frame.table

# The 10 most frequent positive terms:
reviews_new[,.(verified_reviews),] %>% 
  get_sentences() %>%              # get sentences
  extract_sentiment_terms() %>%    # extract negative terms
  .[,positive] %>%                 # select the negative colum
  unlist %>%                       # unlist
  table  %>%                       # create freq table
  sort(decreasing = TRUE) %>% 
  head(10) %>% 
  as.data.frame.table

# Investigation of the word alarm
# Some reviews mentioning the alarm
reviews_new$verified_reviews[sample(which(str_detect(reviews_new$verified_reviews, pattern = "alarm")), size = 5)]

# Most frequent words in the negative reviews mentionning the alarm
neg_rating_reviews <- reviews_new %>%
  filter(rating <= 2) %>%
  unnest_tokens(word, verified_reviews) %>%
  inner_join(get_sentiments(lexicon = "bing")) %>%
  filter(sentiment == "negative") %>%
  count(word)
wordcloud(neg_rating_reviews$word, neg_rating_reviews$n)



# Brouillon
# Corelation between rating and average sentiment
cor.test(reviews$rating, reviews$ave_sentiment)
summary(reviews_new)
as.Date(reviews$date, "%d-%b-%Y")


