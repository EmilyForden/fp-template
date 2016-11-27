#Load libraries
library(tidyverse)
library(lubridate)
library(stringr)
library(tidytext)
library(broom)
library(scales)

theme_set(theme_bw())

get_sentiments("nrc")

library(gutenbergr)

titles <- c("The History of Rome, Books 01 to 08", "The History of Rome, Books 09 to 26",
            "The History of Rome, Books 27 to 36", "The History of Rome, Books 37 to the End
with the Epitomes and Fragments of the Lost Books")

books <- gutenberg_works(title %in% titles) %>%
  gutenberg_download(meta_fields = "title")

library(tidytext)
library(stringr)

by_chapter <- books %>%
  group_by(title) %>%
  mutate(chapter = cumsum(str_detect(text, regex("^chapter ", ignore_case = TRUE)))) %>%
  ungroup() %>%
  filter(chapter > 0)

by_chapter_word <- by_chapter %>%
  unite(title_chapter, title, chapter) %>%
  unnest_tokens(word, text)

word_counts <- by_chapter_word %>%
  anti_join(stop_words) %>%
  count(title_chapter, word, sort = TRUE) %>%
  ungroup()

word_counts

tail(word_counts)

chapters_dtm <- word_counts %>%
  cast_dtm(title_chapter, word, n)

chapters_dtm

get_sentiments("nrc") %>%
  count(sentiment)


tidy_books <- austen_books() %>%
  group_by(book) %>%
  mutate(linenumber = row_number(),
         chapter = cumsum(str_detect(text, regex("^chapter [\\divxlc]", 
                                                 ignore_case = TRUE)))) %>%
  ungroup() %>%
  unnest_tokens(word, text)

Livysentiment <- books %>%
  inner_join(get_sentiments("nrc")) %>%
  count(book, index = linenumber %/% 80, sentiment) %>%
  spread(sentiment, n, fill = 0) %>%
  mutate(sentiment = positive - negative)