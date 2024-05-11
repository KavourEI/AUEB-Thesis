#### 1. Importing the libraries required ####

library(rvest)    # Documentation: https://cran.r-project.org/web/packages/rvest/rvest.pdf
library(xml2)    # Documentation: https://cran.r-project.org/web/packages/xml2/xml2.pdf
library(dplyr)    # Documentation: https://dplyr.tidyverse.org
library(stringr)    # Documentation: https://stringr.tidyverse.org

#### 2. Extracting Data ####

##### 2.1 Defining target URL  #####
genre_url <- 'https://thegreatestbooks.org/genres'
html_genre <- read_html(genre_url)

##### 2.2 Defining target xpath to acquire required genres #####
num_genres <- length(html_nodes(html_genre, xpath = '/html/body/div[1]/div/div[4]/div/div/div/a'))
Genres.df <- data.frame(Genres = character(num_genres), Count = character(num_genres))

#### 2.3 Scrapping the defined genres
for (i in 1:num_genres) {
  genre.xpath <- sprintf('/html/body/div[1]/div/div[4]/div/div/div[%s]/a/div/div/h5', i)
  genre <- html_nodes(html_genre, xpath = genre.xpath)
  Genres.df$Genres[i] <- str_trim(xml_text(genre))
  
  count.xpath <- sprintf('/html/body/div[1]/div/div[4]/div/div/div[%s]/a/div/div/p/small', i)
  count <- html_nodes(html_genre, xpath = count.xpath)
  Genres.df$Count[i] <- str_trim(xml_text(count))
}

print(Genres.df)

Genres.df$Count <- as.integer(gsub(' books', '', Genres.df$Count))
print(Genres.df)

#### 3. Exporting the genres in an .RData format ####
save('Genres.df', file = '/Users/themiskavour/Documents/AUEB_Thesis/Thesis Final/Genres.df.RData')
