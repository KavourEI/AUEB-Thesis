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

#### 4. Preprocess the data ####
set.seed(1234)
trainIndex <- createDataPartition(abstract.df1$Abstract, 
                                  p = 0.7, 
                                  list = FALSE, 
                                  times = 1)
df_train <- abstract.df1[trainIndex,]
df_test <- abstract.df1[-trainIndex,]

abstract.df1 %>% 
  select(everything()) %>% 
  summarise_all(funs(sum(is.na(.))))
length(abstract.df1$Abstract)
sum(nchar(abstract.df1$Abstract)==0)   # There are some cells with empty abstracts, is that true?
a.df1_filtered <- abstract.df1[nchar(abstract.df1$Abstract) > 0,]
a.df1_filtered$unique_id <- seq_along(a.df1_filtered$Abstract)   # Creating a unique ID

abstracts <- a.df1_filtered$Abstract
a.corpus <- corpus(abstracts)   # Converting text to corpus

a.corpus.tokens <- tokens(a.corpus, remove_punct = TRUE, remove_numbers = TRUE)
a.corpus.tokens <- tokens_tolower(a.corpus.tokens)
a.corpus.tokens <- tokens_remove(a.corpus.tokens, stopwords("english"))
a.corpus.tokens <- tokens_wordstem(a.corpus.tokens, language = 'english')

a.dfm <- dfm(a.corpus.tokens)
a.dfm.new <- convert(a.dfm, to = 'topicmodels')

#### 5. Fitting LDA Models ####
# Fitting LDA for alpha = 0.9, 0.5, 0.1, 0.01, 0.001
lda.k2_0.9 <- LDA(a.dfm.new, k = 2, method = 'Gibbs', control = list(alpha = 0.9, seed = 1234))
lda.k2_0.5 <- LDA(a.dfm.new, k = 2, method = 'Gibbs', control = list(alpha = 0.5, seed = 1234))
lda.k2_0.1 <- LDA(a.dfm.new, k = 2, method = 'Gibbs', control = list(alpha = 0.1, seed = 1234))
lda.k2_0.01 <- LDA(a.dfm.new, k = 2, method = 'Gibbs', control = list(alpha = 0.01, seed = 1234))
lda.k2_0.001 <- LDA(a.dfm.new, k = 2, method = 'Gibbs', control = list(alpha = 0.001, seed = 1234))

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

a.dfm.new <- a.dfm.new[slam::row_sums(a.dfm.new) > 0, ]
phi <- as.matrix(posterior(lda.k2_0.1)$terms)
theta <- as.matrix(posterior(lda.k2_0.1)$topics)
vocab <- colnames(phi)
doc.length <- slam::row_sums(a.dfm.new)
term.freq <- slam::col_sums(a.dfm.new)[match(vocab, colnames(a.dfm.new))]

json <- createJSON(phi = phi, theta = theta, vocab = vocab, doc.length = doc.length, term.frequency = term.freq,mds.method = svd_tsne,plot.opts = list(xlab="", ylab=""))
serVis(json)

#### 6. Resutls ####

##### 6.1 Alpha = 0.9 #####

lda.k2_0.9 <- LDA(a.dfm.new, k = 2, method = 'Gibbs', control = list(alpha = 0.9, seed = 1234))

max_col_0.9 <- apply(posterior(lda.k2_0.9)$topics,1, function(row) get_max_column(colnames(posterior(lda.k2_0.9)$topics),row))

df_results_0.9 <- data.frame(a.df1_filtered$Book.Id, a.df1_filtered$Genre, max_col_0.9)
colnames(df_results_0.9) <- c('Book.ID', 'True.Genre', 'Pred.Genre')

df_results_0.9 %>% 
  arrange(a.df1_filtered, max_col_0.9) %>% 
  View()

df_results_0.9$Pred.Genre <- ifelse(df_results_0.9$Pred.Genre == 2,  'biography', 'crime')
df_results_0.9$True.Genre <- as.factor(df_results_0.9$True.Genre)
df_results_0.9$Pred.Genre <- as.factor(df_results_0.9$Pred.Genre)

# Confusion Matrix Results
conf_matrix_0.9 <- confusionMatrix(df_results_0.9$True.Genre, df_results_0.9$Pred.Genre,mode = 'everything')
print(conf_matrix_0.9) # 0.931

# Barplots
books_topics_0.9 <- tidy(lda.k2_0.9, matrix = 'beta')

top_terms.2_0.9 <- books_topics_0.9 %>%
  group_by(topic) %>%
  slice_max(beta, n = 20) %>% # <--- Top 20 terms per topic
  ungroup() %>%
  arrange(topic, -beta)

top_terms.2_0.9 %>%
  mutate(term = reorder_within(term, beta, topic)) %>%
  ggplot(aes(beta, term, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  scale_y_reordered()

# WordCloud
test.1_0.9 <- subset(top_terms.2_0.9, topic == 1)[order(-subset(top_terms.2_0.9, topic == 1)$beta), ]
test.2_0.9 <- subset(top_terms.2_0.9, topic == 2)[order(-subset(top_terms.2_0.9, topic == 2)$beta), ]

w1.2_0.9 <- wordcloud2::wordcloud2(data = test.1_0.9[,2:3], size = 0.7, shape = 'circle')
w1.2_0.9
w2.2_0.9 <- wordcloud2::wordcloud2(data = test.2_0.9[,2:3], size = 0.7, shape = "circle")
w2.2_0.9

##### 6.2 Alpha = 0.5 #####
lda.k2_0.5 <- LDA(a.dfm.new, k = 2, method = 'Gibbs', control = list(alpha = 0.5, seed = 1234))

max_col_0.5 <- apply(posterior(lda.k2_0.5)$topics,1, function(row) get_max_column(colnames(posterior(lda.k2_0.5)$topics),row))

df_results_0.5 <- data.frame(a.df1_filtered$Book.Id, a.df1_filtered$Genre, max_col_0.5)
colnames(df_results_0.5) <- c('Book.ID', 'True.Genre', 'Pred.Genre')

df_results_0.5 %>% 
  arrange(a.df1_filtered, max_col_0.5) %>% 
  View()

df_results_0.5$Pred.Genre <- ifelse(df_results_0.5$Pred.Genre == 2,  'biography', 'crime')
df_results_0.5$True.Genre <- as.factor(df_results_0.5$True.Genre)
df_results_0.5$Pred.Genre <- as.factor(df_results_0.5$Pred.Genre)

# Confusion Matrix Results
conf_matrix_0.5 <- confusionMatrix(df_results_0.5$True.Genre, df_results_0.5$Pred.Genre, mode = 'everything')
print(conf_matrix_0.5) #0.9326

# Barplots
books_topics_0.5 <- tidy(lda.k2_0.5, matrix = 'beta')

top_terms.2_0.5 <- books_topics_0.5 %>%
  group_by(topic) %>%
  slice_max(beta, n = 20) %>% # <--- Top 20 terms per topic
  ungroup() %>%
  arrange(topic, -beta)

top_terms.2_0.5 %>%
  mutate(term = reorder_within(term, beta, topic)) %>%
  ggplot(aes(beta, term, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  scale_y_reordered()

# WordCloud
test.1_0.5 <- subset(top_terms.2_0.5, topic == 1)[order(-subset(top_terms.2_0.5, topic == 1)$beta), ]
test.2_0.5 <- subset(top_terms.2_0.5, topic == 2)[order(-subset(top_terms.2_0.5, topic == 2)$beta), ]

w1.2_0.5 <- wordcloud2::wordcloud2(data = test.1_0.5[,2:3], size = 0.7, shape = 'circle')
w1.2_0.5
w2.2_0.5 <- wordcloud2::wordcloud2(data = test.2_0.5[,2:3], size = 0.7, shape = "circle")
w2.2_0.5


##### 6.3 Alpha = 0.1 #####
lda.k2_0.1 <- LDA(a.dfm.new, k = 2, method = 'Gibbs', control = list(alpha = 0.1, seed = 1234))

max_col_0.1 <- apply(posterior(lda.k2_0.1)$topics,1, function(row) get_max_column(colnames(posterior(lda.k2_0.1)$topics),row))

df_results_0.1 <- data.frame(a.df1_filtered$Book.Id, a.df1_filtered$Genre, max_col_0.1)
colnames(df_results_0.1) <- c('Book.ID', 'True.Genre', 'Pred.Genre')

df_results_0.1 %>% 
  arrange(a.df1_filtered, max_col_0.1) %>% 
  View()

df_results_0.1$Pred.Genre <- ifelse(df_results_0.1$Pred.Genre == 2,  'biography', 'crime')
df_results_0.1$True.Genre <- as.factor(df_results_0.1$True.Genre)
df_results_0.1$Pred.Genre <- as.factor(df_results_0.1$Pred.Genre)

# Confusion Matrix Results
conf_matrix_0.1 <- confusionMatrix(df_results_0.1$True.Genre, df_results_0.1$Pred.Genre, mode = 'everything')
print(conf_matrix_0.1) #0.9232

# Barplots
books_topics_0.1 <- tidy(lda.k2_0.1, matrix = 'beta')

top_terms.2_0.1 <- books_topics_0.1 %>%
  group_by(topic) %>%
  slice_max(beta, n = 20) %>% # <--- Top 20 terms per topic
  ungroup() %>%
  arrange(topic, -beta)

top_terms.2_0.1 %>%
  mutate(term = reorder_within(term, beta, topic)) %>%
  ggplot(aes(beta, term, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  scale_y_reordered()

# WordCloud
test.1_0.1 <- subset(top_terms.2_0.1, topic == 1)[order(-subset(top_terms.2_0.1, topic == 1)$beta), ]
test.2_0.1 <- subset(top_terms.2_0.1, topic == 2)[order(-subset(top_terms.2_0.1, topic == 2)$beta), ]

w1.2_0.1 <- wordcloud2::wordcloud2(data = test.1_0.1[,2:3], size = 0.7, shape = 'circle')
w1.2_0.1
w2.2_0.1 <- wordcloud2::wordcloud2(data = test.2_0.1[,2:3], size = 0.7, shape = "circle")
w2.2_0.1


##### 6.4 Alpha = 0.01 #####
lda.k2_0.01 <- LDA(a.dfm.new, k = 2, method = 'Gibbs', control = list(alpha = 0.01, seed = 1234))

max_col_0.01 <- apply(posterior(lda.k2_0.01)$topics,1, function(row) get_max_column(colnames(posterior(lda.k2_0.01)$topics),row))

df_results_0.01 <- data.frame(a.df1_filtered$Book.Id, a.df1_filtered$Genre, max_col_0.01)
colnames(df_results_0.01) <- c('Book.ID', 'True.Genre', 'Pred.Genre')

df_results_0.01 %>% 
  arrange(a.df1_filtered, max_col_0.01) %>% 
  View()

df_results_0.01$Pred.Genre <- ifelse(df_results_0.01$Pred.Genre == 2,  'biography', 'crime')
df_results_0.01$True.Genre <- as.factor(df_results_0.01$True.Genre)
df_results_0.01$Pred.Genre <- as.factor(df_results_0.01$Pred.Genre)

# Confusion Matrix Results
conf_matrix_0.01 <- confusionMatrix(df_results_0.01$True.Genre, df_results_0.01$Pred.Genre, mode = 'everything')
print(conf_matrix_0.01) #0.9357

# Barplots
books_topics_0.01 <- tidy(lda.k2_0.01, matrix = 'beta')

top_terms.2_0.01 <- books_topics_0.01 %>%
  group_by(topic) %>%
  slice_max(beta, n = 20) %>% # <--- Top 20 terms per topic
  ungroup() %>%
  arrange(topic, -beta)

top_terms.2_0.01 %>%
  mutate(term = reorder_within(term, beta, topic)) %>%
  ggplot(aes(beta, term, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  scale_y_reordered()

# WordCloud
test.1_0.01 <- subset(top_terms.2_0.01, topic == 1)[order(-subset(top_terms.2_0.01, topic == 1)$beta), ]
test.2_0.01 <- subset(top_terms.2_0.01, topic == 2)[order(-subset(top_terms.2_0.01, topic == 2)$beta), ]

w1.2_0.01 <- wordcloud2::wordcloud2(data = test.1_0.01[,2:3], size = 0.7, shape = 'circle')
w1.2_0.01
w2.2_0.01 <- wordcloud2::wordcloud2(data = test.2_0.01[,2:3], size = 0.7, shape = "circle")
w2.2_0.01

##### 6.5 Alpha = 0.001 #####
lda.k2_0.001 <- LDA(a.dfm.new, k = 2, method = 'Gibbs', control = list(alpha = 0.001, seed = 1234))

max_col_0.001 <- apply(posterior(lda.k2_0.001)$topics,1, function(row) get_max_column(colnames(posterior(lda.k2_0.001)$topics),row))

df_results_0.001 <- data.frame(a.df1_filtered$Book.Id, a.df1_filtered$Genre, max_col_0.001)
colnames(df_results_0.001) <- c('Book.ID', 'True.Genre', 'Pred.Genre')

df_results_0.001 %>% 
  arrange(a.df1_filtered, max_col_0.001) %>% 
  View()

df_results_0.001$Pred.Genre <- ifelse(df_results_0.001$Pred.Genre == 2,  'biography', 'crime')
df_results_0.001$True.Genre <- as.factor(df_results_0.001$True.Genre)
df_results_0.001$Pred.Genre <- as.factor(df_results_0.001$Pred.Genre)

# Confusion Matrix Results
conf_matrix_0.001 <- confusionMatrix(df_results_0.001$True.Genre, df_results_0.001$Pred.Genre, mode = 'everything')
print(conf_matrix_0.001) #0.9373

# Barplots

books_topics_0.001 <- tidy(lda.k2_0.001, matrix = 'beta')

top_terms.2_0.001 <- books_topics_0.001 %>%
  group_by(topic) %>%
  slice_max(beta, n = 20) %>% # <--- Top 20 terms per topic
  ungroup() %>%
  arrange(topic, -beta)

top_terms.2_0.001$topic <- ifelse(top_terms.2_0.001$topic == 2,  'Biography', 'Crime')

top_terms.2_0.001 %>%
  mutate(term = reorder_within(term, beta, topic)) %>%
  ggplot(aes(beta, term, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  scale_y_reordered()

top_terms.2_0.001 %>%
  group_by(term) %>%
  filter(all(c("Crime", "Biography") %in% topic)) %>%
  distinct(term)

# WordCloud
test.1_0.001 <- subset(top_terms.2_0.001, topic == "Crime")[order(-subset(top_terms.2_0.001, topic == "Crime")$beta), ]
test.2_0.001 <- subset(top_terms.2_0.001, topic == "Biography")[order(-subset(top_terms.2_0.001, topic == "Biography")$beta), ]

w1.2_0.001 <- wordcloud2::wordcloud2(data = test.1_0.001[,2:3], size = 0.7, shape = 'circle')
w1.2_0.001
w2.2_0.001 <- wordcloud2::wordcloud2(data = test.2_0.001[,2:3], size = 0.7, shape = "circle")
w2.2_0.001
