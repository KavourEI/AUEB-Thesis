#### 1. Importing the necessary libraries to accomplish our goal ####
library(rvest)
library(dplyr)
library(stringr)
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
load("/Users/themiskavour/Documents/AUEB_Thesis/AUEB_Thesis_Comp/Genres.df.RData")

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


first_run <- Genres.df.selected[1:2,]
second_run <- Genres.df.selected[3:4,]
third_run <- Genres.df.selected[5:6,]
fourth_run <- Genres.df.selected[7:8,]

abstract.df1 <- data.frame(Page.Number = integer(),
                           Book.Number = character(),
                           Book.Id = character(),
                           Abstract = character(),
                           Genre = character(),
                           stringsAsFactors = FALSE)

#### 4. Scraping the data #####

##### 4.1 First Batch ##### 
Start.1st <- Sys.time()
for(genre in first_run$Genres.new){
  print(genre)
  url <- sprintf('https://thegreatestbooks.org/the-greatest/%s/books', genre)
  url.new <- paste(url, '/page/', 1, sep = '')
  i = 1
  while(last.page.check(url.new) == FALSE){
    print(url.new)
    html.selected <- read_html(url.new)
    
    for(j in (1:25)){
      book.id <- sprintf('/html/body/div[1]/div/div[3]/div[2]/ol/li[%s]', j)
      
      book_id_value <- html.selected %>%
        html_node(xpath = book.id) %>%
        html_attr("data-book")
      
      #book_info.df$Book.Number[i,j] <- str_trim(xml_text(temp.bnumber)) # Book Number
      
      title <- sprintf('/html/body/div[1]/div/div[3]/div[2]/ol/li[%s]/div/div[1]/h4/a[1]', j)
      temp.btitle <- html_nodes(read_html(url.new), xpath = title)
      #book_info.df$Title[i,j] <- str_trim(xml_text(temp.btitle)) # Book Title
      
      book.no <- sprintf('/html/body/div[1]/div/div[3]/div[2]/ol/li[%s]/div/div[1]/h4/text()[1]', j)
      temp.bnumber <- html_nodes(read_html(url.new), xpath = book.no)
      
      abstract.url <- sprintf('https://thegreatestbooks.org/books/%s',book_id_value)
      abstract.xpath <- '/html/body/div[1]/div/div[1]/div[1]/div[1]/div/p[1]/text()'
      abstract.temp <- html_nodes(read_html(abstract.url), xpath = abstract.xpath)
      
      abstract.df1 <- rbind(abstract.df1,
                            data.frame(
                              Page.Number = i,
                              Book.Number = str_trim(xml_text(temp.bnumber)),
                              Book.Id = book_id_value,
                              Abstract = str_trim(xml_text(abstract.temp)),
                              Genre = genre,
                              stringsAsFactors = FALSE))
    }
    i = i + 1
    url.new <- paste(url,'/page/', i, sep = '')
  }
}
End.1st <- Sys.time()
X.1st <- End.1st - Start.1st

##### 4.2 Second Batch #####
abstract.df2 <- data.frame(Page.Number = integer(),
                           Book.Number = character(),
                           Book.Id = character(),
                           Abstract = character(),
                           Genre = character(),
                           stringsAsFactors = FALSE)

Start.2nd <- Sys.time()
for(genre in second_run$Genres.new){
  print(genre)
  url <- sprintf('https://thegreatestbooks.org/the-greatest/%s/books', genre)
  url.new <- paste(url, '/page/', 1, sep = '')
  i = 1
  while(last.page.check(url.new) == FALSE){
    print(url.new)
    html.selected <- read_html(url.new)
    
    for(j in (1:25)){
      book.id <- sprintf('/html/body/div[1]/div/div[3]/div[2]/ol/li[%s]', j)
      
      book_id_value <- html.selected %>%
        html_node(xpath = book.id) %>%
        html_attr("data-book")
      
      #book_info.df$Book.Number[i,j] <- str_trim(xml_text(temp.bnumber)) # Book Number
      
      title <- sprintf('/html/body/div[1]/div/div[3]/div[2]/ol/li[%s]/div/div[1]/h4/a[1]', j)
      temp.btitle <- html_nodes(read_html(url.new), xpath = title)
      #book_info.df$Title[i,j] <- str_trim(xml_text(temp.btitle)) # Book Title
      
      book.no <- sprintf('/html/body/div[1]/div/div[3]/div[2]/ol/li[%s]/div/div[1]/h4/text()[1]', j)
      temp.bnumber <- html_nodes(read_html(url.new), xpath = book.no)
      
      abstract.url <- sprintf('https://thegreatestbooks.org/books/%s',book_id_value)
      abstract.xpath <- '/html/body/div[1]/div/div[1]/div[1]/div[1]/div/p[1]/text()'
      abstract.temp <- html_nodes(read_html(abstract.url), xpath = abstract.xpath)
      
      abstract.df2 <- rbind(abstract.df2,
                            data.frame(
                              Page.Number = i,
                              Book.Number = str_trim(xml_text(temp.bnumber)),
                              Book.Id = book_id_value,
                              Abstract = str_trim(xml_text(abstract.temp)),
                              Genre = genre,
                              stringsAsFactors = FALSE))
    }
    i = i + 1
    url.new <- paste(url,'/page/', i, sep = '')
  }
}
End.2nd <- Sys.time()
X.2nd <- End.2nd - Start.2nd



abstract.df3 <- data.frame(Page.Number = integer(),
                           Book.Number = character(),
                           Book.Id = character(),
                           Abstract = character(),
                           Genre = character(),
                           stringsAsFactors = FALSE)

##### 4.3 Third Batch #####
Start.3rd <- Sys.time()
for(genre in third_run$Genres.new){
  print(genre)
  url <- sprintf('https://thegreatestbooks.org/the-greatest/%s/books', genre)
  url.new <- paste(url, '/page/', 1, sep = '')
  i = 1
  while(last.page.check(url.new) == FALSE){
    print(url.new)
    html.selected <- read_html(url.new)
    
    for(j in (1:25)){
      book.id <- sprintf('/html/body/div[1]/div/div[3]/div[2]/ol/li[%s]', j)
      
      book_id_value <- html.selected %>%
        html_node(xpath = book.id) %>%
        html_attr("data-book")
      
      #book_info.df$Book.Number[i,j] <- str_trim(xml_text(temp.bnumber)) # Book Number
      
      title <- sprintf('/html/body/div[1]/div/div[3]/div[2]/ol/li[%s]/div/div[1]/h4/a[1]', j)
      temp.btitle <- html_nodes(read_html(url.new), xpath = title)
      #book_info.df$Title[i,j] <- str_trim(xml_text(temp.btitle)) # Book Title
      
      book.no <- sprintf('/html/body/div[1]/div/div[3]/div[2]/ol/li[%s]/div/div[1]/h4/text()[1]', j)
      temp.bnumber <- html_nodes(read_html(url.new), xpath = book.no)
      
      abstract.url <- sprintf('https://thegreatestbooks.org/books/%s',book_id_value)
      abstract.xpath <- '/html/body/div[1]/div/div[1]/div[1]/div[1]/div/p[1]/text()'
      abstract.temp <- html_nodes(read_html(abstract.url), xpath = abstract.xpath)
      
      abstract.df3 <- rbind(abstract.df3,
                            data.frame(
                              Page.Number = i,
                              Book.Number = str_trim(xml_text(temp.bnumber)),
                              Book.Id = book_id_value,
                              Abstract = str_trim(xml_text(abstract.temp)),
                              Genre = genre,
                              stringsAsFactors = FALSE))
    }
    i = i + 1
    url.new <- paste(url,'/page/', i, sep = '')
  }
}
End.3rd <- Sys.time()
X.3rd <- End.3rd - Start.3rd



abstract.df4 <- data.frame(Page.Number = integer(),
                           Book.Number = character(),
                           Book.Id = character(),
                           Abstract = character(),
                           Genre = character(),
                           stringsAsFactors = FALSE)

##### 4.4 Fourth Batch #####
Start.4th <- Sys.time()
for(genre in fourth_run$Genres.new){
  print(genre)
  url <- sprintf('https://thegreatestbooks.org/the-greatest/%s/books', genre)
  url.new <- paste(url, '/page/', 1, sep = '')
  i = 1
  while(last.page.check(url.new) == FALSE){
    print(url.new)
    html.selected <- read_html(url.new)
    
    for(j in (1:25)){
      book.id <- sprintf('/html/body/div[1]/div/div[3]/div[2]/ol/li[%s]', j)
      
      book_id_value <- html.selected %>%
        html_node(xpath = book.id) %>%
        html_attr("data-book")
      
      #book_info.df$Book.Number[i,j] <- str_trim(xml_text(temp.bnumber)) # Book Number
      
      title <- sprintf('/html/body/div[1]/div/div[3]/div[2]/ol/li[%s]/div/div[1]/h4/a[1]', j)
      temp.btitle <- html_nodes(read_html(url.new), xpath = title)
      #book_info.df$Title[i,j] <- str_trim(xml_text(temp.btitle)) # Book Title
      
      book.no <- sprintf('/html/body/div[1]/div/div[3]/div[2]/ol/li[%s]/div/div[1]/h4/text()[1]', j)
      temp.bnumber <- html_nodes(read_html(url.new), xpath = book.no)
      
      abstract.url <- sprintf('https://thegreatestbooks.org/books/%s',book_id_value)
      abstract.xpath <- '/html/body/div[1]/div/div[1]/div[1]/div[1]/div/p[1]/text()'
      abstract.temp <- html_nodes(read_html(abstract.url), xpath = abstract.xpath)
      
      abstract.df4 <- rbind(abstract.df4,
                            data.frame(
                              Page.Number = i,
                              Book.Number = str_trim(xml_text(temp.bnumber)),
                              Book.Id = book_id_value,
                              Abstract = str_trim(xml_text(abstract.temp)),
                              Genre = genre,
                              stringsAsFactors = FALSE))
    }
    i = i + 1
    url.new <- paste(url,'/page/', i, sep = '')
  }
}
End.4th <- Sys.time()
X.4th <- End.4th - Start.4th

View(abstract.df4)

#### 5. Exporting the data as an .RData file ####
save('abstract.df1','X.1st','abstract.df2','X.2nd','abstract.df3','X.3rd','abstract.df4','X.4th', file = '/Users/themiskavour/Documents/AUEB_Thesis/AUEB_Thesis_Comp/abstracts.RData')
