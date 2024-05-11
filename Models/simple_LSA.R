#### 1. Loading the required libraries for our goal ####
library(lsa)    # Documentation: https://cran.r-project.org/web/packages/lsa/lsa.pdf
library(tm)    # Documentation: https://cran.r-project.org/web/packages/tm/index.html
library(ggplot2)    # Documentation: https://cran.r-project.org/web/packages/ggplot2/ggplot2.pdf

#### 2. Creating sample data ####
sample_documents <- c(
  "The cat sat on the mat",
  "The dog chased the cat",
  "The mouse ran away from the cat and the dog",
  "The cat is afraid of the dog",
  "A rainy day is perfect for staying indoors and reading",
  "I love the smell of fresh coffee in the morning",
  "The early bird catches the worm",
  "The sun sets in the west",
  "A picture is worth a thousand words",
  "Time flies when you're having fun",
  "Actions speak louder than words",
  "The pen is mightier than the sword",
  "Where there's a will, there's a way",
  "Beauty is in the eye of the beholder",
  "When life gives you lemons, make lemonade",
  "Every cloud has a silver lining",
  "All is fair in love and war",
  "You can't judge a book by its cover",
  "Don't count your chickens before they hatch",
  "The grass is always greener on the other side",
  "Better late than never",
  "Two heads are better than one",
  "Out of sight, out of mind",
  "Practice makes perfect",
  "Honesty is the best policy",
  "Laughter is the best medicine",
  "Actions speak louder than words",
  "All good things come to those who wait",
  "An apple a day keeps the doctor away",
  "A stitch in time saves nine",
  "Beggars can't be choosers",
  "Birds of a feather flock together",
  "Blood is thicker than water",
  "The early bird catches the worm",
  "Every cloud has a silver lining",
  "Fools rush in where angels fear to tread",
  "Great minds think alike",
  "Haste makes waste",
  "Ignorance is bliss",
  "It's a small world",
  "It's the thought that counts",
  "Kill two birds with one stone",
  "The more, the merrier",
  "The pot calling the kettle black",
  "There's no place like home",
  "Time heals all wounds",
  "Variety is the spice of life",
  "When in Rome, do as the Romans do",
  "You reap what you sow",
  "Where there's smoke, there's fire"
)
doc_id <- c(1:50)

#### 3. Preprocess Data ####
docs_df <- data.frame(doc_id,sample_documents)

doc_corpus <- Corpus(VectorSource(docs_df$sample_documents))

doc_corpus <- tm_map(doc_corpus, tolower)
doc_corpus <- tm_map(doc_corpus, function(x) removeWords(x, stopwords("english")))
doc_corpus <- tm_map(doc_corpus, removePunctuation)

doc_mat <- as.matrix(TermDocumentMatrix(doc_corpus))

doc_lsa = lsa(doc_mat)
doc_lsa <- as.textmatrix(doc_lsa)

#### 4. Fitting LSA model ####
lsa_model <- lsa(doc_mat, dim =12)

#### 5. Evaluating and Visualizing the results from LSA Model ####
print(lsa_model$sk)    # Print the singular values

plot(lsa_model$sk, type = "b", main = "Scree Plot", xlab = "Singular Value Index", ylab = "Singular Value")    # Scree plot with singular values

topcs <- lsa_model$tk
print(topcs)

max_col_index <- max.col(topcs)
result_mx <- data.frame(words =rownames(topcs), topic = max_col_index)

result_mx <- arrange(result_mx, desc(topic))
result_mx

ggplot(result_mx, aes(x = factor(topic))) +
  geom_bar(fill = "skyblue", color = "black") +
  geom_text(stat = 'count', aes(label = ..count..), vjust = -0.5) +
  labs(title = "Count of Words per Topic Number", x = "Topic Number", y = "Word Count") +
  theme_minimal()
