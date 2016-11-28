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

#topic modeling
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

chapters_dtm <- word_counts %>%
  cast_dtm(title_chapter, word, n)

chapters_dtm

library(topicmodels)
chapters_lda <- LDA(chapters_dtm, k = 4, control = list(seed = 1234))
chapters_lda

library(tidytext)

chapters_lda_td <- tidy(chapters_lda)
chapters_lda_td

top_terms <- chapters_lda_td %>%
  group_by(topic) %>%
  top_n(5, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)
top_terms

top_terms %>%
  mutate(term = reorder(term, beta)) %>%
  ggplot(aes(term, beta, fill = factor(topic))) +
  geom_bar(alpha = 0.8, stat = "identity", show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  coord_flip()

chapters_lda_gamma <- tidy(chapters_lda, matrix = "gamma")
chapters_lda_gamma

chapters_lda_gamma <- chapters_lda_gamma %>%
  separate(document, c("title", "chapter"), sep = "_", convert = TRUE)
chapters_lda_gamma

ggplot(chapters_lda_gamma, aes(gamma, fill = factor(topic))) +
  geom_histogram() +
  facet_wrap(~ title, nrow = 2)
#Ok, this makes it look like only the first and last section of the books are loading. I need to fix this. 
chapter_classifications <- chapters_lda_gamma %>%
  group_by(title, chapter) %>%
  top_n(1, gamma) %>%
  ungroup() %>%
  arrange(gamma)

chapter_classifications

book_topics <- chapter_classifications %>%
  count(title, topic) %>%
  top_n(1, n) %>%
  ungroup() %>%
  transmute(consensus = title, topic)

book_topics

chapter_classifications %>%
  inner_join(book_topics, by = "topic") %>%
  count(title, consensus) %>%
  knitr::kable()

assignments <- augment(chapters_lda, data = chapters_dtm)
assignments

assignments <- assignments %>%
  separate(document, c("title", "chapter"), sep = "_", convert = TRUE) %>%
  inner_join(book_topics, by = c(".topic" = "topic"))

assignments %>%
  count(title, consensus, wt = count) %>%
  spread(consensus, n, fill = 0) %>%
  knitr::kable()

wrong_words <- assignments %>%
  filter(title != consensus)

wrong_words

wrong_words %>%
  count(title, consensus, term, wt = count) %>%
  ungroup() %>%
  arrange(desc(n))

word_counts %>%
  filter(word == "flopson")

#sentiment analysis 

titles <- c("The History of Rome, Books 01 to 08", "The History of Rome, Books 09 to 26",
            "The History of Rome, Books 27 to 36", "The History of Rome, Books 37 to the End
            with the Epitomes and Fragments of the Lost Books")

books <- gutenberg_works(title %in% titles) %>%
  gutenberg_download(meta_fields = "title")


get_sentiments("nrc") %>%
  count(sentiment)


tidy_books <- books() %>%
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