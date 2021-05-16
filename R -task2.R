writeLines('PATH="${RTOOLS40_HOME}\\usr\\bin;${PATH}"', con = "~/.Renviron")
install.packages("magick", verbose=TRUE)
install.packages("RWeka")
install.packages("ggplot2")
install.packages("twitteR")
install.packages("graphics")
install.packages("purrr")
install.packages("stringr")
install.packages("tm")
install.packages("syuzhet")
install.packages("tmap")
install.packages("dplyr")
install.packages("magrittr") # package installations are only needed the first time you use it

library(dplyr)
library(tmap)
library(twitteR)
library(graphics)
library(purrr)
library(stringr) 
library(tm)
library(syuzhet)
library(ggplot2)
library(readr)        # reads in CSV
library(ggplot2)      # plot library
library(tidyverse)    # for data manipulation
library(gridExtra)    # multiple plots in 1
library(magick)       # attach dope image for visual
library(scales)       # show the colors
library(ggrepel)      # for graph repel (labels)
library(repr)         # resize graphs
library(hexbin)       # for hive scatter
library(naniar)       # to check for missing data
library(lubridate)    # for date and time
library(tm)
library(wordcloud)    # beautiful wordclouds
library(wordcloud2)
library(tidytext)     # text preprocessing
library(textdata)     # text preprocessing
library(reshape2)
library(knitr)
library(grid)
library(igraph)
library(ggraph)
library(ggsci)
library(devtools)
library(circlize)
library(radarchart)
library(stringr)
library(sjmisc)
library(magick)
library(htmlwidgets)
library(VIM)          # missing values visual
library(colorspace)   # maybe for wordcloud
library(RWeka)
library(textmineR)
library(magrittr) # needs to be run every time you start R and want to use %>%

#reading data
tweets<- read_csv("TWITTER.CSV",
                  col_types = cols(user_name = col_character(),
                                   user_location = col_character(),
                                   user_description = col_character(),
                                   user_created = col_datetime(format = ""),
                                   user_followers = col_double(),
                                   user_friends = col_double(),
                                   user_favourites = col_double(),
                                   user_verified = col_logical(),
                                   date = col_datetime(format = ""),
                                   text = col_character(),
                                   hashtags = col_character(),
                                   source = col_character(),
                                   is_retweet = col_logical()))


tweets %>% head
afinn <- read_csv("Afinn.csv",
                  col_types = cols(word = col_character(), value = col_double()))
nrc <- read_csv("nrc.csv",
                col_types = cols(word = col_character(), sentiment = col_character()))
bing <- read_csv("bing.csv",
                 col_types = cols(word = col_character(), sentiment = col_character()))

# data preprocessing
cleanCorpus <- function(text){
  # punctuation, whitespace, lowercase, numbers
  text.tmp <- tm_map(text, removePunctuation)
  text.tmp <- tm_map(text.tmp, stripWhitespace)
  text.tmp <- tm_map(text.tmp, content_transformer(tolower))
  text.tmp <- tm_map(text.tmp, removeNumbers)
  
# removes stopwords
  stopwords_remove <- c(stopwords("en"), c("whats","youll","hes","theres","ive","we","im",
                                           "will","can","cant","dont","youve","us",
                                           "youre","weve","theyre","thats","whats","didnt","i"))
  text.tmp <- tm_map(text.tmp, removeWords, stopwords_remove)
  
  return(text.tmp)
}


#tokenize the data

unnested_tweets <- tweets %>% 
  mutate(text = as.character(tweets$text)) %>% 
  unnest_tokens(word, text)




install.packages("ndtv")

# Libraries
library(igraph)
library(ggraph)
library(network)
library(sna)
library(visNetwork)
library(threejs)
library(networkD3)
library(ndtv)
library(qgraph)
library(splitstackshape)


install.packages("igraph")
library(igraph)
g1 <-graph(c("covid","help","slow","spread",,
             "wear","mask",
             "family","invest","health","insurance",
             "sooner","selfreporting","symptoms","daily"))

plot(g1,
     vertex.color="yellow",
     vert.size= 50,
     edge.color= 'grey')

options(repr.plot.width=20, repr.plot.height=20)

install.packages("reshape2") 
library(reshape2)
install.packages("wordcloud1") 
library(wordcloud1)
#wordcloud2


unnest_tweets %>% 
  inner_join(bing, by="word") %>%
  count(word, sentiment, sort=T) %>% 
  acast(word ~ sentiment, value.var = "n", fill=0) %>% 
  
  # wordcloud
  comparison.cloud( max.words = 300, title.size = 2,
                   scale = c(3,.5))


           
tweets_location <- tweets %>%
  # convert to lower case
  mutate(user_location = tolower(user_location)) %>%
 