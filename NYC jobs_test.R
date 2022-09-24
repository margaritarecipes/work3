#change change
library(tidytext)
library(dplyr)
library(tidyverse)
library(tibble)
library(ggplot2)
library(stopwords) 
library(readtext)
library(htm2txt)

stopword <- as_tibble(stopwords::stopwords("en")) 
stopword <- rename(stopword, word=value)

listhtml <- list.files(path="/Users/yukikofuruya/Desktop/NYC", pattern=".html", all.files=TRUE,
           full.names=TRUE)

data.frame(gettxt(listhtml[1]))

name <- data.frame(job=c("data.curator","data.scientist","research.data.analyst","sr.research.analyst"))

#a <- strsplit(listhtml,'/')
#for (b in a) {
#   sapply(a[i],tail,1)
#   print(a)
#}

#listhtml[1]
#sapply(a,tail,1)
#a[[1]][6]
#sapply(a[i],tail,1)

txt_dc <- gettxt(listhtml[1])
txt_ds <- gettxt(listhtml[2])
txt_rda <- gettxt(listhtml[3])
txt_srra <- gettxt(listhtml[4])

df <- data.frame(text=rbind(txt_dc,txt_ds,txt_rda,txt_srra))
#text_df <- bind_cols(name,df)
str(df)
text_df <- cbind(name,df)

words_df <- text_df %>%
  unnest_tokens(word, text)

words_df_clean <- anti_join(words_df, stopword, by = 'word')

words_df_clean <- words_df_clean %>%
  group_by(job) %>%
  count(word, sort = TRUE) %>%
  mutate(total = sum(n),
         rank = row_number(),
         'frequency' = n/total)

words_df_clean %>%
  group_by(job) %>%
  top_n(20) %>%
  ggplot(aes(x = reorder(word, n), y=n)) +
  geom_bar(stat = "identity", alpha = .3, show.legend = FALSE) +
  coord_flip() + labs(y='word',x='n') +
  facet_wrap(~ job)

job_tf_idf <- words_df_clean %>%
  bind_tf_idf(word, job, n)

job_tf_idf %>%
  select(-total) %>%
  arrange(desc(tf_idf))

library(forcats)

job_tf_idf %>%
  group_by(job) %>%
  slice_max(tf_idf, n = 10) %>%
  ungroup() %>%
  ggplot(aes(tf_idf, fct_reorder(word, tf_idf), fill = job)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~job, ncol = 4, scales = "free") +
  labs(x = "tf-idf", y = NULL)

#BIGRAM
text_bigram <- text_df %>%
  group_by(job) %>%
  unnest_tokens(bigram, text, token = "ngrams", n = 2)

job_bigram_tf_idf <- text_bigram %>%
  group_by(job) %>%
  separate(bigram, c("word1", "word2"), sep = " ") %>%
  filter(!word1 %in% stop_words$word,
         !word2 %in% stop_words$word) %>%
  unite("bigram", c(word1, word2), sep = " ") %>%
  count(bigram, sort = TRUE) %>%
  mutate(total = sum(n),
         rank = row_number(),
         'frequency' = n/total) %>%
  bind_tf_idf(bigram, job, n) %>%
  arrange(desc(tf_idf))

job_bigram_tf_idf %>%
  group_by(job) %>%
  slice_max(tf_idf, n = 7) %>%
  ungroup() %>%
  ggplot(aes(tf_idf, fct_reorder(bigram, tf_idf), fill = job)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~job, ncol = 4, scales = "free") +
  labs(x="tf_idf", y = NULL)


#data scientist job  
bigram <- 
  job_bigram_tf_idf %>%
  filter(job=="data.scientist") %>%
  slice_max(tf_idf, n = 5) %>%
  ggplot(aes(tf_idf, fct_reorder(bigram, tf_idf))) +
  geom_col(show.legend = FALSE) +
  labs(x='tf_idf', y=NULL) 

word <- 
  job_tf_idf %>%
  filter(job == "data.scientist") %>%
  slice_max(tf_idf, n = 10) %>%
  ungroup() %>%
  ggplot(aes(tf_idf, fct_reorder(word, tf_idf))) +
  geom_col(show.legend = FALSE) +
  labs(x = "tf-idf", y = NULL)

NYCjob_words_df <- 
  job_tf_idf %>%
  filter(job == "data.scientist") %>%
  mutate(id=1, text=word)

##MY TEXTS
txt2 <- readtext("/Users/yukikofuruya/OneDrive/100 Job/2022/2022-09-21 NYC data scientist Cover Letter.docx")$text
#txt2 <- readtext("/Users/yukikofuruya/OneDrive/100 Job/2022/2022-09-17-Yukiko Furuya-CV.docx")$text
text_df2 <- data.frame(job="data.scientist", text=txt2)

words_df2 <- text_df2 %>%
  unnest_tokens(word, text)


words_df_clean2 <- anti_join(words_df2, stopword, by = 'word')

words_df_clean2 <- words_df_clean2 %>%
  count(word, sort = TRUE) %>%
  mutate(total = sum(n),
         rank = row_number(),
         'frequency' = n/total)


similarity_df <- left_join(NYCjob_words_df %>%
                             select(word,n,total,rank)
                           , words_df_clean2 %>% 
                             select(word,n),by= 'word')





list <- c("rda","sra","ds","dc")

# create a list with a specific length 
plot_lst <- vector("list", length = 4)

for (i in 1:4) {
  g <- ggplot(data = , aes(x = hp, y = wt)) +
    geom_point()
  plot_lst[[i]] <- g
}

# Combine all plots
cowplot::plot_grid(plotlist = plot_lst, nrow = 4)