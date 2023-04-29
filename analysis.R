library(tidyverse)
library(rsample)
library(tidymodels)
library(tidytext)
library(topicmodels)
library(tm)
library(ggplot2)
# setwd("~/Desktop/LDA_topicmodel")

#Dataset:
# https://www.kaggle.com/datasets/jrobischon/wikipedia-movie-plots

#read in the data from a CSV file
wiki_movies_plots <- read.csv("wiki_movie_plots_deduped.csv")
male_names <- tolower(read.delim("male.txt",header = FALSE)$V1)
female_names <- tolower(read.delim("female.txt",header = FALSE)$V1)
wiki_movies_plots <- wiki_movies_plots[1:4000,]
# wiki_movies_plots <- wiki_movies_plots %>% filter(Genre == "western")
# View structure of data
str(wiki_movies_plots)
#view first few rows of data
head(wiki_movies_plots)

stop_words <- tidytext::stop_words
wiki_movies_plots$Plot <- tolower(wiki_movies_plots$Plot)
wiki_movies_plots$Plot <- removeWords(wiki_movies_plots$Plot,c(stop_words$word,"love","tells"))
wiki_movies_plots$Plot <- removeWords(wiki_movies_plots$Plot,c(male_names))
wiki_movies_plots$Plot <- removeWords(wiki_movies_plots$Plot,c(female_names[1:3000]))
# 

# set a seed so that the output of the model is predictable
corpus <- Corpus(VectorSource(wiki_movies_plots$Plot))
tdm <- DocumentTermMatrix(corpus)
ap_lda <- topicmodels::LDA(tdm, k = 12, control = list(seed = 1234))
ap_lda
#beta table - per topic - per word propabilities
ap_topics <- tidy(ap_lda, matrix = "beta")
ap_topics

ap_top_terms <- ap_topics %>%
  group_by(topic) %>%
  slice_max(beta, n = 10) %>% 
  ungroup() %>%
  arrange(topic, -beta)


ap_top_terms %>%
  mutate(term = reorder_within(term, beta, topic)) %>%
  ggplot(aes(beta, term, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  scale_y_reordered()
ap_top_terms