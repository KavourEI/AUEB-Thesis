#### 1. Importing the necessary libraries to accomplish our goal ####
library(rvest)
library(dplyr)
library(xml2)

#### 2. Custom functions needed #####

last.page.check <- function(custom_url){
  " Checks if the page we are looping though is the last one or not "
  
  page <- read_html(custom_url)

  xpath_expression <- "//a[contains(text(), 'Next')]"
  next_link_exists <- length(xml_find_all(page, xpath_expression)) == 0
  next_link_exists
}

#### 3. Loading the genres to extract books for ####

load('/Users/themiskavour/Documents/AUEB_Thesis/Thesis Final/Genres.df.RData')

Genres.df$Genres.new <- tolower(Genres.df$Genres)
Genres.df.selected <- Genres.df %>% 
  filter(Count > 250 & Count < 350)
Genres.df.selected

Genres.df.selected <- Genres.df.selected %>% 
  mutate(
    Genres.new = case_when(
      Genres == 'American History' ~ 'american-history',
      Genres == 'Speculative Fiction' ~ 'metaphysical-visionary-fiction',
      TRUE ~ tolower(Genres)
    )
  )

first_run <- head(Genres.df.selected,4)
second_run <- tail(Genres.df.selected,3)

book_info.df_1 <- data.frame(Page.Number = integer(),
                           Book.Number = character(),
                           Title = character(),
                           Author = character(),
                           Genre = character(),
                           stringsAsFactors = FALSE)
#### 4. Extracting Book Titles ####
##### 4.1 First Batch #####
Start.1st <- Sys.time()
for(genre in first_run$Genres.new){
  print(genre)
  url <- sprintf('https://thegreatestbooks.org/the-greatest/%s/books',genre)
  url.new <- paste(url, '/page/', 1, sep = '')
  i = 1
  while(last.page.check(url.new) == FALSE){
    print(url.new)
    html.selected <- read_html(url.new)
    
    for(j in (1:25)){
      book.no <- sprintf('/html/body/div[1]/div/div[3]/div[2]/ol/li[%s]/div/div[1]/h4/text()[1]', j)
      temp.bnumber <- html_nodes(read_html(url.new), xpath = book.no)
      
      #book_info.df$Book.Number[i,j] <- str_trim(xml_text(temp.bnumber)) # Book Number
      
      title <- sprintf('/html/body/div[1]/div/div[3]/div[2]/ol/li[%s]/div/div[1]/h4/a[1]', j)
      temp.btitle <- html_nodes(read_html(url.new), xpath = title)
      #book_info.df$Title[i,j] <- str_trim(xml_text(temp.btitle)) # Book Title
      
      author <- sprintf('/html/body/div[1]/div/div[3]/div[2]/ol/li[%s]/div/div[1]/h4/a[2]', j)
      temp.bauthor <- html_nodes(read_html(url.new), xpath = author)
      #book_info.df$Author[i,j] <- str_trim(xml_text(temp.bauthor)) # Book Author
      
      book_info.df_1 <- rbind(book_info.df, data.frame(
        Page.Number = i,
        Book.Number = str_trim(xml_text(temp.bnumber)),
        Title = str_trim(xml_text(temp.btitle)),
        Author = str_trim(xml_text(temp.bauthor)),
        Genre = genre,
        stringsAsFactors = FALSE))
    }
    i = i + 1
    url.new <- paste(url,'/page/', i, sep = '')
  }
  
  xpath_selector <- "/html/body/div[1]/div/div[3]/div[2]/ol/li"
  page <- read_html(url.new)
  matching_elements <- xml_find_all(page, xpath_selector)
  num_elements <- length(list_items)
  
  for(j in (1:num_elements)){
    book.no <- sprintf('/html/body/div[1]/div/div[3]/div[2]/ol/li[%s]/div/div[1]/h4/text()[1]', j)
    temp.bnumber <- html_nodes(read_html(url.new), xpath = book.no)
      #book_info.df$Book.Number[i,j] <- str_trim(xml_text(temp.bnumber)) # Book Number
    title <- sprintf('/html/body/div[1]/div/div[3]/div[2]/ol/li[%s]/div/div[1]/h4/a[1]', j)
    temp.btitle <- html_nodes(read_html(url.new), xpath = title)
    #book_info.df$Title[i,j] <- str_trim(xml_text(temp.btitle)) # Book Title
    author <- sprintf('/html/body/div[1]/div/div[3]/div[2]/ol/li[%s]/div/div[1]/h4/a[2]', j)
    temp.bauthor <- html_nodes(read_html(url.new), xpath = author)
      #book_info.df$Author[i,j] <- str_trim(xml_text(temp.bauthor)) # Book Author
    
    book_info.df_1 <- rbind(book_info.df, data.frame(
      Page.Number = i,
      Book.Number = str_trim(xml_text(temp.bnumber)),
      Title = str_trim(xml_text(temp.btitle)),
      Author = str_trim(xml_text(temp.bauthor)),
      Genre = genre,
      stringsAsFactors = FALSE))
    }
  }
End.1st <- Sys.time()
X.1st <- End.1st-Start.1st

save('book_info.df_1','X.1st', file = '/Users/themiskavour/Documents/AUEB_Thesis/Thesis Final/titles_1.RData')

##### 4.2 Second Batch #####

book_info.df_2 <- data.frame(
  Page.Number = integer(),
  Book.Number = character(),
  Title = character(),
  Author = character(),
  Genre = character(),
  stringsAsFactors = FALSE)

Start.2nd <- Sys.time()
for(genre in second_run$Genres.new){
  print(genre)
  url <- sprintf('https://thegreatestbooks.org/the-greatest/%s/books',genre)
  url.new <- paste(url, '/page/', 1, sep = '')
  i = 1
  while(last.page.check(url.new) == FALSE){
    print(url.new)
    html.selected <- read_html(url.new)
    
    for(j in (1:25)){
      book.no <- sprintf('/html/body/div[1]/div/div[3]/div[2]/ol/li[%s]/div/div[1]/h4/text()[1]', j)
      temp.bnumber <- html_nodes(read_html(url.new), xpath = book.no)
      
      #book_info.df$Book.Number[i,j] <- str_trim(xml_text(temp.bnumber)) # Book Number
      
      title <- sprintf('/html/body/div[1]/div/div[3]/div[2]/ol/li[%s]/div/div[1]/h4/a[1]', j)
      temp.btitle <- html_nodes(read_html(url.new), xpath = title)
      #book_info.df$Title[i,j] <- str_trim(xml_text(temp.btitle)) # Book Title
      
      author <- sprintf('/html/body/div[1]/div/div[3]/div[2]/ol/li[%s]/div/div[1]/h4/a[2]', j)
      temp.bauthor <- html_nodes(read_html(url.new), xpath = author)
      #book_info.df$Author[i,j] <- str_trim(xml_text(temp.bauthor)) # Book Author
      
      book_info.df_2 <- rbind(book_info.df, data.frame(
        Page.Number = i,
        Book.Number = str_trim(xml_text(temp.bnumber)),
        Title = str_trim(xml_text(temp.btitle)),
        Author = str_trim(xml_text(temp.bauthor)),
        Genre = genre,
        stringsAsFactors = FALSE))
    }
    i = i + 1
    url.new <- paste(url,'/page/', i, sep = '')
  }
  
  xpath_selector <- "/html/body/div[1]/div/div[3]/div[2]/ol/li"
  page <- read_html(url.new)
  matching_elements <- xml_find_all(page, xpath_selector)
  num_elements <- length(list_items)
  
  for(j in (1:num_elements)){
    book.no <- sprintf('/html/body/div[1]/div/div[3]/div[2]/ol/li[%s]/div/div[1]/h4/text()[1]', j)
    temp.bnumber <- html_nodes(read_html(url.new), xpath = book.no)
    #book_info.df$Book.Number[i,j] <- str_trim(xml_text(temp.bnumber)) # Book Number
    title <- sprintf('/html/body/div[1]/div/div[3]/div[2]/ol/li[%s]/div/div[1]/h4/a[1]', j)
    temp.btitle <- html_nodes(read_html(url.new), xpath = title)
    #book_info.df$Title[i,j] <- str_trim(xml_text(temp.btitle)) # Book Title
    author <- sprintf('/html/body/div[1]/div/div[3]/div[2]/ol/li[%s]/div/div[1]/h4/a[2]', j)
    temp.bauthor <- html_nodes(read_html(url.new), xpath = author)
    #book_info.df$Author[i,j] <- str_trim(xml_text(temp.bauthor)) # Book Author
    
    book_info.df_2 <- rbind(book_info.df, data.frame(
      Page.Number = i,
      Book.Number = str_trim(xml_text(temp.bnumber)),
      Title = str_trim(xml_text(temp.btitle)),
      Author = str_trim(xml_text(temp.bauthor)),
      Genre = genre,
      stringsAsFactors = FALSE))
  }
}
End.2nd <- Sys.time()
X.2nd <- End.2nd-Start.2nd

save('book_info.df_2','X.2nd', file = '/Users/themiskavour/Documents/AUEB_Thesis/Thesis Final/titles_1.RData')