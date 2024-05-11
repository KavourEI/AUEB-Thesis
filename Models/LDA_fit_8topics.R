#### 1. Loading the required libraries for our goal ####
library(tidyverse)    # Documentation: https://www.tidyverse.org
library(tm)    # Documentation: https://cran.r-project.org/web/packages/tm/index.html
library(caret)    # Documentation: https://cran.r-project.org/web/packages/caret/caret.pdf
library(quanteda)    # Documentation: https://cran.r-project.org/web/packages/quanteda/quanteda.pdf
library(topicmodels)    # Documentation: https://cran.r-project.org/web/packages/topicmodels/topicmodels.pdf
library(LDAvis)    # Documentation: https://cran.r-project.org/web/packages/LDAvis/LDAvis.pdf
library(RSQLite)    # Documentation: https://cran.r-project.org/web/packages/RSQLite/RSQLite.pdf
library(ggplot2)    # Documentation: https://cran.r-project.org/web/packages/ggplot2/ggplot2.pdf
library(dplyr)    # Documentation: https://cran.r-project.org/web/packages/dplyr/dplyr.pdf
library(ldatuning)    # Documentation: https://cran.r-project.org/web/packages/ldatuning/ldatuning.pdf
library(tidytext)    # Documentation: https://cran.r-project.org/web/packages/tidytext/tidytext.pdf

#### 2. Custom function ####
get_max_column <- function(row_names, row){
  row_names[which.max(row)]
}

svd_tsne <- function(x) tsne::tsne(svd(x)$u)

#### 3. Loading the extracted data ####
load('/Users/themiskavour/Documents/AUEB_Thesis/AUEB_Thesis_Comp/abstracts.RData')
#Total_Abstracts <- X.1st + X.2nd + X.3rd + X.4th
load('/Users/themiskavour/Documents/AUEB_Thesis/AUEB_Thesis_Comp/titles_1.RData')
Total_Titles <- X.1st + X.2nd

abstract.df1 %>% 
  select(everything()) %>% 
  summarise_all(funs(sum(is.na(.))))
abstracts.all <- rbind(abstract.df1, # Biography and Crime
                      abstract.df2, # Mystery and American-History
                      abstract.df3, # Humor and Fantasy
                      abstract.df4) # Metaphysical-visionary-fiction and Poetry
sum(nchar(abstracts.all$Abstract)==0)   # There are some cells with empty abstracts, is that true?
a.all.filtered <- abstracts.all[nchar(abstracts.all$Abstract) > 0,]

abstracts <- a.all.filtered$Abstract
a.corpus <- corpus(abstracts)   # Converting text to corpus

a.corpus.tokens <- tokens(a.corpus, remove_punct = TRUE, remove_numbers = TRUE)
a.corpus.tokens <- tokens_tolower(a.corpus.tokens)
a.corpus.tokens <- tokens_remove(a.corpus.tokens, stopwords("english"))
a.corpus.tokens <- tokens_wordstem(a.corpus.tokens, language = 'english')

a.dfm <- dfm(a.corpus.tokens)
a.dfm.new <- convert(a.dfm, to = 'topicmodels')

result <- FindTopicsNumber(
  a.dfm.new,
  topics = seq(from = 2, to = 10, by = 1),
  metrics = c("Griffiths2004", "CaoJuan2009", "Arun2010", "Deveaud2014"),
  method = "Gibbs",
  control = list(seed = 1234),
  #mc.cores = 2L,
  #verbose = TRUE
)
FindTopicsNumber_plot(result)
result

#### 5. Fitting LDA Models ####

##### 5.1 Alpha = 0.9 #####
lda.k8_0.9 <- LDA(a.dfm.new, k = 8, method = 'Gibbs', control = list(alpha = 0.9, seed = 1234))

max_col_0.9 <- apply(posterior(lda.k8_0.9)$topics,1, function(row) get_max_column(colnames(posterior(lda.k8_0.9)$topics),row))

df_results_0.9 <- data.frame(a.all.filtered$Book.Id, a.all.filtered$Genre, max_col_0.9)
colnames(df_results_0.9) <- c('Book.ID', 'True.Genre', 'Pred.Genre')

df_results_0.9 %>% 
  select(Pred.Genre, True.Genre) %>% 
  group_by(Pred.Genre, True.Genre) %>% 
  summarise(count = n()) %>% 
  pivot_wider(names_from = Pred.Genre, values_from = count, values_fill = 0) %>%
  View()


for(i in c(1:nrow(df_results_0.9))){
  if(df_results_0.9[i,]$Pred.Genre == "1"){
    df_results_0.9[i,]$Pred.Genre = 'american-history'
  } else if(df_results_0.9[i,]$Pred.Genre == "2"){
    df_results_0.9[i,]$Pred.Genre = 'crime'
  } else if(df_results_0.9[i,]$Pred.Genre == "3"){
    df_results_0.9[i,]$Pred.Genre = 'poetry'
  } else if(df_results_0.9[i,]$Pred.Genre == "4"){
    df_results_0.9[i,]$Pred.Genre = 'biography'
  } else if(df_results_0.9[i,]$Pred.Genre == "5"){
    df_results_0.9[i,]$Pred.Genre = 'mystery'
  } else if(df_results_0.9[i,]$Pred.Genre == "6"){
    df_results_0.9[i,]$Pred.Genre = 'metaphysical-visionary-fiction'
  } else if(df_results_0.9[i,]$Pred.Genre == "7"){
    df_results_0.9[i,]$Pred.Genre = 'fantasy'
  } else if(df_results_0.9[i,]$Pred.Genre == "8"){
    df_results_0.9[i,]$Pred.Genre = 'humor'
  }
}

df_results_0.9$True.Genre <- as.factor(df_results_0.9$True.Genre)
df_results_0.9$Pred.Genre <- as.factor(df_results_0.9$Pred.Genre)

# Confusion Matrix Results
conf_matrix_0.9 <- caret::confusionMatrix(df_results_0.9$True.Genre, df_results_0.9$Pred.Genre)
print(conf_matrix_0.9) # 0.583

# Barplots
books_topics_0.9 <- tidy(lda.k8_0.9, matrix = 'beta')

top_terms.8_0.9 <- books_topics_0.9 %>%
  group_by(topic) %>%
  slice_max(beta, n = 10) %>% # <--- Top 20 terms per topic
  ungroup() %>%
  arrange(topic, -beta)

top_terms.8_0.9 %>%
  mutate(term = reorder_within(term, beta, topic)) %>%
  ggplot(aes(beta, term, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  scale_y_reordered()

# WordCloud
test.1_0.9 <- subset(top_terms.8_0.9, topic == 1)[order(-subset(top_terms.8_0.9, topic == 1)$beta), ]
test.2_0.9 <- subset(top_terms.8_0.9, topic == 2)[order(-subset(top_terms.8_0.9, topic == 2)$beta), ]
test.3_0.9 <- subset(top_terms.8_0.9, topic == 3)[order(-subset(top_terms.8_0.9, topic == 3)$beta), ]
test.4_0.9 <- subset(top_terms.8_0.9, topic == 4)[order(-subset(top_terms.8_0.9, topic == 4)$beta), ]
test.5_0.9 <- subset(top_terms.8_0.9, topic == 5)[order(-subset(top_terms.8_0.9, topic == 5)$beta), ]
test.6_0.9 <- subset(top_terms.8_0.9, topic == 6)[order(-subset(top_terms.8_0.9, topic == 6)$beta), ]
test.7_0.9 <- subset(top_terms.8_0.9, topic == 7)[order(-subset(top_terms.8_0.9, topic == 7)$beta), ]
test.8_0.9 <- subset(top_terms.8_0.9, topic == 8)[order(-subset(top_terms.8_0.9, topic == 8)$beta), ]

w1_0.9 <- wordcloud2::wordcloud2(data = test.1_0.9[,2:3], size = 0.7, shape = 'circle')
w1_0.9
w2_0.9 <- wordcloud2::wordcloud2(data = test.2_0.9[,2:3], size = 0.7, shape = "circle")
w2_0.9
w3_0.9 <- wordcloud2::wordcloud2(data = test.3_0.9[,2:3], size = 0.7, shape = 'circle')
w3_0.9
w4_0.9 <- wordcloud2::wordcloud2(data = test.4_0.9[,2:3], size = 0.7, shape = "circle")
w4_0.9
w5_0.9 <- wordcloud2::wordcloud2(data = test.1_0.9[,2:3], size = 0.7, shape = 'circle')
w5_0.9
w6_0.9 <- wordcloud2::wordcloud2(data = test.2_0.9[,2:3], size = 0.7, shape = "circle")
w6_0.9
w7_0.9 <- wordcloud2::wordcloud2(data = test.3_0.9[,2:3], size = 0.7, shape = 'circle')
w7_0.9
w8_0.9 <- wordcloud2::wordcloud2(data = test.4_0.9[,2:3], size = 0.7, shape = "circle")
w8_0.9


##### 5.2 Alpha = 0.5 #####
lda.k8_0.5 <- LDA(a.dfm.new, k = 8, method = 'Gibbs', control = list(alpha = 0.5, seed = 1234))

max_col_0.5 <- apply(posterior(lda.k8_0.5)$topics,1, function(row) get_max_column(colnames(posterior(lda.k8_0.5)$topics),row))

df_results_0.5 <- data.frame(a.all.filtered$Book.Id, a.all.filtered$Genre, max_col_0.5)
colnames(df_results_0.5) <- c('Book.ID', 'True.Genre', 'Pred.Genre')

df_results_0.5 %>% 
  select(Pred.Genre, True.Genre) %>% 
  group_by(Pred.Genre, True.Genre) %>% 
  summarise(count = n()) %>% 
  pivot_wider(names_from = Pred.Genre, values_from = count, values_fill = 0) %>% 
  View()


for(i in c(1:nrow(df_results_0.5))){
  if(df_results_0.5[i,]$Pred.Genre == "1"){
    df_results_0.5[i,]$Pred.Genre = 'american-history'
  } else if(df_results_0.5[i,]$Pred.Genre == "2"){
    df_results_0.5[i,]$Pred.Genre = 'fantasy'
  } else if(df_results_0.5[i,]$Pred.Genre == "3"){
    df_results_0.5[i,]$Pred.Genre = 'poetry'
  } else if(df_results_0.5[i,]$Pred.Genre == "4"){
    df_results_0.5[i,]$Pred.Genre = 'biography'
  } else if(df_results_0.5[i,]$Pred.Genre == "5"){
    df_results_0.5[i,]$Pred.Genre = 'crime'
  } else if(df_results_0.5[i,]$Pred.Genre == "6"){
    df_results_0.5[i,]$Pred.Genre = 'metaphysical-visionary-fiction'
  } else if(df_results_0.5[i,]$Pred.Genre == "7"){
    df_results_0.5[i,]$Pred.Genre = 'mystery'
  } else if(df_results_0.5[i,]$Pred.Genre == "8"){
    df_results_0.5[i,]$Pred.Genre = 'humor'
  }
}

df_results_0.5$True.Genre <- as.factor(df_results_0.5$True.Genre)
df_results_0.5$Pred.Genre <- as.factor(df_results_0.5$Pred.Genre)

# Confusion Matrix Results
conf_matrix_0.5 <- caret::confusionMatrix(df_results_0.5$True.Genre, df_results_0.5$Pred.Genre)
print(conf_matrix_0.5) # 0.5981

# Barplots
books_topics_0.5 <- tidy(lda.k8_0.5, matrix = 'beta')

top_terms.8_0.5 <- books_topics_0.5 %>%
  group_by(topic) %>%
  slice_max(beta, n = 10) %>% # <--- Top 20 terms per topic
  ungroup() %>%
  arrange(topic, -beta)

top_terms.8_0.5 %>%
  mutate(term = reorder_within(term, beta, topic)) %>%
  ggplot(aes(beta, term, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  scale_y_reordered()

# WordCloud
test.1_0.5 <- subset(top_terms.8_0.5, topic == 1)[order(-subset(top_terms.8_0.5, topic == 1)$beta), ]
test.2_0.5 <- subset(top_terms.8_0.5, topic == 2)[order(-subset(top_terms.8_0.5, topic == 2)$beta), ]
test.3_0.5 <- subset(top_terms.8_0.5, topic == 3)[order(-subset(top_terms.8_0.5, topic == 3)$beta), ]
test.4_0.5 <- subset(top_terms.8_0.5, topic == 4)[order(-subset(top_terms.8_0.5, topic == 4)$beta), ]
test.5_0.5 <- subset(top_terms.8_0.5, topic == 5)[order(-subset(top_terms.8_0.5, topic == 5)$beta), ]
test.6_0.5 <- subset(top_terms.8_0.5, topic == 6)[order(-subset(top_terms.8_0.5, topic == 6)$beta), ]
test.7_0.5 <- subset(top_terms.8_0.5, topic == 7)[order(-subset(top_terms.8_0.5, topic == 7)$beta), ]
test.8_0.5 <- subset(top_terms.8_0.5, topic == 8)[order(-subset(top_terms.8_0.5, topic == 8)$beta), ]

w1_0.5 <- wordcloud2::wordcloud2(data = test.1_0.5[,2:3], size = 0.7, shape = 'circle')
w1_0.5
w2_0.5 <- wordcloud2::wordcloud2(data = test.2_0.5[,2:3], size = 0.7, shape = "circle")
w2_0.5
w3_0.5 <- wordcloud2::wordcloud2(data = test.3_0.5[,2:3], size = 0.7, shape = 'circle')
w3_0.5
w4_0.5 <- wordcloud2::wordcloud2(data = test.4_0.5[,2:3], size = 0.7, shape = "circle")
w4_0.5
w5_0.5 <- wordcloud2::wordcloud2(data = test.1_0.5[,2:3], size = 0.7, shape = 'circle')
w5_0.5
w6_0.5 <- wordcloud2::wordcloud2(data = test.2_0.5[,2:3], size = 0.7, shape = "circle")
w6_0.5
w7_0.5 <- wordcloud2::wordcloud2(data = test.3_0.5[,2:3], size = 0.7, shape = 'circle')
w7_0.5
w8_0.5 <- wordcloud2::wordcloud2(data = test.4_0.5[,2:3], size = 0.7, shape = "circle")
w8_0.5

##### 5.3 Alpha = 0.1 #####
lda.k8 <- LDA(a.dfm.new, k = 8, method = 'Gibbs', control = list(alpha = 0.1, seed = 1234))

a.dfm.new <- a.dfm.new[slam::row_sums(a.dfm.new) > 0, ]
phi <- as.matrix(posterior(lda.k8)$terms)
theta <- as.matrix(posterior(lda.k8)$topics)
vocab <- colnames(phi)
doc.length <- slam::row_sums(a.dfm.new)
term.freq <- slam::col_sums(a.dfm.new)[match(vocab, colnames(a.dfm.new))]

json <- createJSON(phi = phi, theta = theta, vocab = vocab, doc.length = doc.length, term.frequency = term.freq,mds.method = svd_tsne,plot.opts = list(xlab="", ylab=""))
serVis(json)

max_col <- apply(posterior(lda.k8)$topics,1, function(row) get_max_column(colnames(posterior(lda.k8)$topics),row))

df_results <- data.frame(a.all.filtered$Book.Id, a.all.filtered$Genre, max_col)
colnames(df_results) <- c('Book.ID', 'True.Genre', 'Pred.Genre')


out.8 <- df_results %>% 
  arrange(a.all.filtered, max_col)
true8 <- out.8$True.Genre
pred8 <- out.8$Pred.Genre
save('out.8', 'true8','pred8', file = '/Users/themiskavour/Documents/AUEB_Thesis/AUEB_Thesis_Comp/results8.RData')

df_results %>% 
  select(Pred.Genre, True.Genre) %>% 
  group_by(Pred.Genre, True.Genre) %>% 
  summarise(count = n()) %>% 
  pivot_wider(names_from = Pred.Genre, values_from = count, values_fill = 0) %>% 
  View()


for(i in c(1:nrow(df_results))){
  if(df_results[i,]$Pred.Genre == "1"){
    df_results[i,]$Pred.Genre = 'american-history'
  } else if(df_results[i,]$Pred.Genre == "2"){
    df_results[i,]$Pred.Genre = 'fantasy'
  } else if(df_results[i,]$Pred.Genre == "3"){
    df_results[i,]$Pred.Genre = 'poetry'
  } else if(df_results[i,]$Pred.Genre == "4"){
    df_results[i,]$Pred.Genre = 'biography'
  } else if(df_results[i,]$Pred.Genre == "5"){
    df_results[i,]$Pred.Genre = 'crime'
  } else if(df_results[i,]$Pred.Genre == "6"){
    df_results[i,]$Pred.Genre = 'metaphysical-visionary-fiction'
  } else if(df_results[i,]$Pred.Genre == "7"){
    df_results[i,]$Pred.Genre = 'mystery'
  } else if(df_results[i,]$Pred.Genre == "8"){
    df_results[i,]$Pred.Genre = 'humor'
  }
}

df_results$True.Genre <- as.factor(df_results$True.Genre)
df_results$Pred.Genre <- as.factor(df_results$Pred.Genre)

# Confusion Matrix Results
conf_matrix <- caret::confusionMatrix(df_results$True.Genre, df_results$Pred.Genre)
print(conf_matrix)

# Barplots
books_topics <- tidy(lda.k8, matrix = 'beta')

top_terms.8 <- books_topics %>%
  group_by(topic) %>%
  slice_max(beta, n = 10) %>% # <--- Top 20 terms per topic
  ungroup() %>%
  arrange(topic, -beta)

top_terms.8$topic2 <- 
  ifelse(top_terms.8$topic == 1, 'American-history',
         ifelse(top_terms.8$topic == 2, 'Fantasy',
                ifelse(top_terms.8$topic==3, 'Poetry',
                       ifelse(top_terms.8$topic==4, 'Biography',
                              ifelse(top_terms.8$topic==5, 'Crime',
                                     ifelse(top_terms.8$topic==6, 'Metaphysical-visionary-fiction',
                                            ifelse(top_terms.8$topic==7, 'Mystery','Humor')))))))


top_terms.8 %>%
  mutate(term = reorder_within(term, beta, topic2)) %>%
  ggplot(aes(beta, term, fill = factor(topic2))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic2, scales = "free") +
  scale_y_reordered()

# WordCloud
test.1 <- subset(top_terms.8, topic == 1)[order(-subset(top_terms.8, topic == 1)$beta), ]
test.2 <- subset(top_terms.8, topic == 2)[order(-subset(top_terms.8, topic == 2)$beta), ]
test.3 <- subset(top_terms.8, topic == 3)[order(-subset(top_terms.8, topic == 3)$beta), ]
test.4 <- subset(top_terms.8, topic == 4)[order(-subset(top_terms.8, topic == 4)$beta), ]
test.5 <- subset(top_terms.8, topic == 5)[order(-subset(top_terms.8, topic == 5)$beta), ]
test.6 <- subset(top_terms.8, topic == 6)[order(-subset(top_terms.8, topic == 6)$beta), ]
test.7 <- subset(top_terms.8, topic == 7)[order(-subset(top_terms.8, topic == 7)$beta), ]
test.8 <- subset(top_terms.8, topic == 8)[order(-subset(top_terms.8, topic == 8)$beta), ]

w1 <- wordcloud2::wordcloud2(data = test.1[,2:3], size = 0.7, shape = 'circle')
w1
w2 <- wordcloud2::wordcloud2(data = test.2[,2:3], size = 0.7, shape = "circle")
w2
w3 <- wordcloud2::wordcloud2(data = test.3[,2:3], size = 0.7, shape = 'circle')
w3
w4 <- wordcloud2::wordcloud2(data = test.4[,2:3], size = 0.7, shape = "circle")
w4
w5 <- wordcloud2::wordcloud2(data = test.1[,2:3], size = 0.7, shape = 'circle')
w5
w6 <- wordcloud2::wordcloud2(data = test.2[,2:3], size = 0.7, shape = "circle")
w6
w7 <- wordcloud2::wordcloud2(data = test.3[,2:3], size = 0.7, shape = 'circle')
w7
w8 <- wordcloud2::wordcloud2(data = test.4[,2:3], size = 0.7, shape = "circle")
w8

##### 5.4 Alpha = 0.01 #####
lda.k8_0.01 <- LDA(a.dfm.new, k = 8, method = 'Gibbs', control = list(alpha = 0.01, seed = 1234))

max_col_0.01 <- apply(posterior(lda.k8_0.01)$topics,1, function(row) get_max_column(colnames(posterior(lda.k8_0.01)$topics),row))

df_results_0.01 <- data.frame(a.all.filtered$Book.Id, a.all.filtered$Genre, max_col_0.01)
colnames(df_results_0.01) <- c('Book.ID', 'True.Genre', 'Pred.Genre')

df_results_0.01 %>% 
  select(Pred.Genre, True.Genre) %>% 
  group_by(Pred.Genre, True.Genre) %>% 
  summarise(count = n()) %>% 
  pivot_wider(names_from = Pred.Genre, values_from = count, values_fill = 0) %>% 
  View()


for(i in c(1:nrow(df_results_0.01))){
  if(df_results_0.01[i,]$Pred.Genre == "1"){
    df_results_0.01[i,]$Pred.Genre = 'american-history'
  } else if(df_results_0.01[i,]$Pred.Genre == "2"){
    df_results_0.01[i,]$Pred.Genre = 'biography'
  } else if(df_results_0.01[i,]$Pred.Genre == "3"){
    df_results_0.01[i,]$Pred.Genre = 'poetry'
  } else if(df_results_0.01[i,]$Pred.Genre == "4"){
    df_results_0.01[i,]$Pred.Genre = 'metaphysical-visionary-fiction'
  } else if(df_results_0.01[i,]$Pred.Genre == "5"){
    df_results_0.01[i,]$Pred.Genre = 'crime'
  } else if(df_results_0.01[i,]$Pred.Genre == "6"){
    df_results_0.01[i,]$Pred.Genre = 'fantasy'
  } else if(df_results_0.01[i,]$Pred.Genre == "7"){
    df_results_0.01[i,]$Pred.Genre = 'mystery'
  } else if(df_results_0.01[i,]$Pred.Genre == "8"){
    df_results_0.01[i,]$Pred.Genre = 'humor'
  }
}

df_results_0.01$True.Genre <- as.factor(df_results_0.01$True.Genre)
df_results_0.01$Pred.Genre <- as.factor(df_results_0.01$Pred.Genre)

# Confusion Matrix Results
conf_matrix_0.01 <- caret::confusionMatrix(df_results_0.01$True.Genre, df_results_0.01$Pred.Genre)
print(conf_matrix_0.01) # 0.5981

# Barplots
books_topics_0.01 <- tidy(lda.k8_0.01, matrix = 'beta')

top_terms.8_0.01 <- books_topics_0.01 %>%
  group_by(topic) %>%
  slice_max(beta, n = 10) %>% # <--- Top 20 terms per topic
  ungroup() %>%
  arrange(topic, -beta)

top_terms.8_0.01 %>%
  mutate(term = reorder_within(term, beta, topic)) %>%
  ggplot(aes(beta, term, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  scale_y_reordered()

# WordCloud
test.1_0.01 <- subset(top_terms.8_0.01, topic == 1)[order(-subset(top_terms.8_0.01, topic == 1)$beta), ]
test.2_0.01 <- subset(top_terms.8_0.01, topic == 2)[order(-subset(top_terms.8_0.01, topic == 2)$beta), ]
test.3_0.01 <- subset(top_terms.8_0.01, topic == 3)[order(-subset(top_terms.8_0.01, topic == 3)$beta), ]
test.4_0.01 <- subset(top_terms.8_0.01, topic == 4)[order(-subset(top_terms.8_0.01, topic == 4)$beta), ]
test.5_0.01 <- subset(top_terms.8_0.01, topic == 5)[order(-subset(top_terms.8_0.01, topic == 5)$beta), ]
test.6_0.01 <- subset(top_terms.8_0.01, topic == 6)[order(-subset(top_terms.8_0.01, topic == 6)$beta), ]
test.7_0.01 <- subset(top_terms.8_0.01, topic == 7)[order(-subset(top_terms.8_0.01, topic == 7)$beta), ]
test.8_0.01 <- subset(top_terms.8_0.01, topic == 8)[order(-subset(top_terms.8_0.01, topic == 8)$beta), ]

w1_0.01 <- wordcloud2::wordcloud2(data = test.1_0.01[,2:3], size = 0.7, shape = 'circle')
w1_0.01
w2_0.01 <- wordcloud2::wordcloud2(data = test.2_0.01[,2:3], size = 0.7, shape = "circle")
w2_0.01
w3_0.01 <- wordcloud2::wordcloud2(data = test.3_0.01[,2:3], size = 0.7, shape = 'circle')
w3_0.01
w4_0.01 <- wordcloud2::wordcloud2(data = test.4_0.01[,2:3], size = 0.7, shape = "circle")
w4_0.01
w5_0.01 <- wordcloud2::wordcloud2(data = test.1_0.01[,2:3], size = 0.7, shape = 'circle')
w5_0.01
w6_0.01 <- wordcloud2::wordcloud2(data = test.2_0.01[,2:3], size = 0.7, shape = "circle")
w6_0.01
w7_0.01 <- wordcloud2::wordcloud2(data = test.3_0.01[,2:3], size = 0.7, shape = 'circle')
w7_0.01
w8_0.01 <- wordcloud2::wordcloud2(data = test.4_0.01[,2:3], size = 0.7, shape = "circle")
w8_0.01

##### 5.5 Alpha = 0.001 #####
lda.k8_0.001 <- LDA(a.dfm.new, k = 8, method = 'Gibbs', control = list(alpha = 0.001, seed = 1234))

max_col_0.001 <- apply(posterior(lda.k8_0.001)$topics,1, function(row) get_max_column(colnames(posterior(lda.k8_0.001)$topics),row))

df_results_0.001 <- data.frame(a.all.filtered$Book.Id, a.all.filtered$Genre, max_col_0.001)
colnames(df_results_0.001) <- c('Book.ID', 'True.Genre', 'Pred.Genre')

df_results_0.001 %>% 
  select(Pred.Genre, True.Genre) %>% 
  group_by(Pred.Genre, True.Genre) %>% 
  summarise(count = n()) %>% 
  pivot_wider(names_from = Pred.Genre, values_from = count, values_fill = 0) %>% 
  View()


for(i in c(1:nrow(df_results_0.001))){
  if(df_results_0.001[i,]$Pred.Genre == "1"){
    df_results_0.001[i,]$Pred.Genre = 'american-history'
  } else if(df_results_0.001[i,]$Pred.Genre == "2"){
    df_results_0.001[i,]$Pred.Genre = 'biography'
  } else if(df_results_0.001[i,]$Pred.Genre == "3"){
    df_results_0.001[i,]$Pred.Genre = 'poetry'
  } else if(df_results_0.001[i,]$Pred.Genre == "4"){
    df_results_0.001[i,]$Pred.Genre = 'crime'
  } else if(df_results_0.001[i,]$Pred.Genre == "5"){
    df_results_0.001[i,]$Pred.Genre = 'metaphysical-visionary-fiction'
  } else if(df_results_0.001[i,]$Pred.Genre == "6"){
    df_results_0.001[i,]$Pred.Genre = 'fantasy'
  } else if(df_results_0.001[i,]$Pred.Genre == "7"){
    df_results_0.001[i,]$Pred.Genre = 'mystery'
  } else if(df_results_0.001[i,]$Pred.Genre == "8"){
    df_results_0.001[i,]$Pred.Genre = 'humor'
  }
}

df_results_0.001$True.Genre <- as.factor(df_results_0.001$True.Genre)
df_results_0.001$Pred.Genre <- as.factor(df_results_0.001$Pred.Genre)

# Confusion Matrix Results
conf_matrix_0.001 <- caret::confusionMatrix(df_results_0.001$True.Genre, df_results_0.001$Pred.Genre)
print(conf_matrix_0.001) # 0.5981

# Barplots
books_topics_0.001 <- tidy(lda.k8_0.001, matrix = 'beta')

top_terms.8_0.001 <- books_topics_0.001 %>%
  group_by(topic) %>%
  slice_max(beta, n = 10) %>% # <--- Top 20 terms per topic
  ungroup() %>%
  arrange(topic, -beta)

top_terms.8_0.001 %>%
  mutate(term = reorder_within(term, beta, topic)) %>%
  ggplot(aes(beta, term, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  scale_y_reordered()

# WordCloud
test.1_0.001 <- subset(top_terms.8_0.001, topic == 1)[order(-subset(top_terms.8_0.001, topic == 1)$beta), ]
test.2_0.001 <- subset(top_terms.8_0.001, topic == 2)[order(-subset(top_terms.8_0.001, topic == 2)$beta), ]
test.3_0.001 <- subset(top_terms.8_0.001, topic == 3)[order(-subset(top_terms.8_0.001, topic == 3)$beta), ]
test.4_0.001 <- subset(top_terms.8_0.001, topic == 4)[order(-subset(top_terms.8_0.001, topic == 4)$beta), ]
test.5_0.001 <- subset(top_terms.8_0.001, topic == 5)[order(-subset(top_terms.8_0.001, topic == 5)$beta), ]
test.6_0.001 <- subset(top_terms.8_0.001, topic == 6)[order(-subset(top_terms.8_0.001, topic == 6)$beta), ]
test.7_0.001 <- subset(top_terms.8_0.001, topic == 7)[order(-subset(top_terms.8_0.001, topic == 7)$beta), ]
test.8_0.001 <- subset(top_terms.8_0.001, topic == 8)[order(-subset(top_terms.8_0.001, topic == 8)$beta), ]

w1_0.001 <- wordcloud2::wordcloud2(data = test.1_0.001[,2:3], size = 0.7, shape = 'circle')
w1_0.001
w2_0.001 <- wordcloud2::wordcloud2(data = test.2_0.001[,2:3], size = 0.7, shape = "circle")
w2_0.001
w3_0.001 <- wordcloud2::wordcloud2(data = test.3_0.001[,2:3], size = 0.7, shape = 'circle')
w3_0.001
w4_0.001 <- wordcloud2::wordcloud2(data = test.4_0.001[,2:3], size = 0.7, shape = "circle")
w4_0.001
w5_0.001 <- wordcloud2::wordcloud2(data = test.1_0.001[,2:3], size = 0.7, shape = 'circle')
w5_0.001
w6_0.001 <- wordcloud2::wordcloud2(data = test.2_0.001[,2:3], size = 0.7, shape = "circle")
w6_0.001
w7_0.001 <- wordcloud2::wordcloud2(data = test.3_0.001[,2:3], size = 0.7, shape = 'circle')
w7_0.001
w8_0.001 <- wordcloud2::wordcloud2(data = test.4_0.001[,2:3], size = 0.7, shape = "circle")
w8_0.001
