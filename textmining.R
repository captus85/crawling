library(httr)
library(rvest)
library(tidyverse)
library(qdap)
library(tm)
library(wordcloud)
library(plotrix)
library(ggplot2)
library(ggthemes)
library(RWeka)


result_df <- as_tibble()

for (i in 100:124) {
  tar <- paste0("https://www.sciencedirect.com/journal/journal-of-psychiatric-research/vol/", i, "/suppl/C")
  
  jpr <- read_html(tar) %>%
    html_nodes("span.js-article-title") %>%
    html_text() %>% as_tibble()
  
  jpr$id <- i
  
  result_df <- bind_rows(jpr, result_df)
}

result_df2 <-as_tibble()

for (i in 108:132) {
  tar <- paste0("https://www.sciencedirect.com/journal/journal-of-psychosomatic-research/vol/", i,"/suppl/C")
  
  jpr <- read_html(tar) %>%
    html_nodes("span.js-article-title") %>%
    html_text() %>% as_tibble()
  
  jpr$id <- i
  
  result_df2 <- bind_rows(jpr, result_df2)
}

term_count <- freq_terms(result_df)
plot(term_count)

jpr_title <- result_df$value
title_source <- VectorSource(jpr_title)
title_corpus <- VCorpus(title_source)
title_corpus[[1]][1]
title_corpus[[1]][2]



# Alter the function code to match the instructions
clean_corpus <- function(corpus) {
  # Remove punctuation
  corpus <- tm_map(corpus, removePunctuation)
  # Transform to lower case
  corpus <- tm_map(corpus, content_transformer(tolower))
  # Add more stopwords
  corpus <- tm_map(corpus, removeWords, words = c(stopwords("en"), "coffee", "mug"))
  # Strip whitespace
  corpus <- tm_map(corpus, stripWhitespace)
  return(corpus)
}

title_corpus_clean <- clean_corpus(title_corpus)

title_DTM <- DocumentTermMatrix(title_corpus_clean)
title_DTM_m <- as.matrix(title_DTM)

dim(title_DTM_m)


title_TDM <- TermDocumentMatrix(title_corpus_clean)
title_TDM_m <- as.matrix(title_TDM)

dim(title_TDM_m)

term_frequency <- rowSums(title_TDM_m)
term_frequency <- sort(term_frequency, decreasing = TRUE)
term_frequency[1:10]
barplot(term_frequency[1:10], col = "tan", las = 2)


# Create frequency
frequency <- freq_terms(
  result_df$value, 
  top = 10, 
  at.least = 3, 
  stopwords = "Top200Words"
)

# Make a frequency barchart
plot(frequency)


#wordcloud
term_frequency[1:10]

terms_vec <- names(term_frequency)

wordcloud(terms_vec, term_frequency,
          max.words = 50, colors= "red")

#Stop words and word clouds
stops <- c(stopwords(kind = "en"), "disorders", "disorder", "patients", "symptoms")

cleaned_title_corpus <- tm_map(title_corpus , removeWords, stops)

sorted_term_frequency <- sort(term_frequency, decreasing = T)

terms_vec <- names(sorted_term_frequency)
wordcloud(terms_vec, sorted_term_frequency,
          max.word = 50, colors = "red")


#find common words
all_title <- paste(result_df$value, collapse = " ")
all_title2 <- paste(result_df2$value, collapse = " ")

all_result <- c(all_title, all_title2)

all_result <- VectorSource(all_result)
all_corpus <- VCorpus(all_result)

all_clean <- clean_corpus(all_corpus)
all_tdm <- TermDocumentMatrix(all_clean)
all_m <- as.matrix(all_tdm)
commonality.cloud(all_m, max.words = 100, colors = "steelblue1")

#dissimilar words
str(all_tdm)
colnames(all_tdm) <- c("psychosomatic_research", "psychosomatic_medicine")
all_m <- as.matrix(all_tdm)
comparison.cloud(all_m, colors = c("orange", "blue"), max.words = 50)


#polarized tag cloud, pyramid.plot

top25_df <- all_m %>%
  as_data_frame(rownames = "word") %>%
  filter_all(all_vars(.>0)) %>%
  mutate(diff = psychosomatic_research - psychosomatic_medicine) %>%
  top_n(25, wt= diff) %>%
  arrange(desc(diff))

pyramid.plot(
  top25_df$psychosomatic_research,
  top25_df$psychosomatic_medicine,
  labels = top25_df$word,
  top.labels = c("psychosomatic_research", "word", "psychosomatic_medicine"),
  main = "word in common",
  unit = NULL,
  gap = 8
)


#word networds
word_associate(result_df$value, match.string = "disorder", 
               stopwords = c(stopwords(kind = "en"), "patients", "symptoms"),
               network.plot = TRUE, cloud.colors = c("gray85", "darkred"))

title(main = "Barista Coffee Tweet Associations") # Add title

#dendrogram
dim(title_TDM)

tdm1 <- removeSparseTerms(title_TDM, sparse = 0.95)

tdm1_m <- as.matrix(tdm1)
tdm1_m <- dist(tdm1_m)
hc <- hclust(tdm1_m)
plot(hc)

#word association
associations <- findAssocs(title_TDM, "disorder", 0.2)

association_df <- list_vect2df(associations, col2 = "word", col3 = "score")

ggplot(association_df, aes(score, word)) + 
  geom_point(size = 3) + 
  theme_gdocs()

#changing n-grams
tokenizer <- function(x) {
  NGramTokenizer(x, Weka_control(min = 2, max = 2))
}

unigram_dtm <- DocumentTermMatrix(title_corpus)
bigram_dtm <- DocumentTermMatrix(title_corpus,
                                 control = list(tokenize = tokenizer))
unigram_dtm
bigram_dtm

bigram_dtm_m<- as.matrix(bigram_dtm)

freq <- colSums(bigram_dtm_m)
bi_words <- names(freq)

str_subset(bi_words, "disorder")

wordcloud(bi_words, freq, max.words = 20)
