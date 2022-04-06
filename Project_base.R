#library(tidyverse)
#library(RSelenium)
#rD <- rsDriver(browser="chrome", port=4546L, verbose=F)
#remDr <- rD[["client"]]

library(rvest)
library(tidyverse)
library(httr) 
library(curl) 
library(jsonlite)
library(dplyr)
library(ggplot2)

#TESTING THE REVIEWS
amazon_firestick_url = "https://www.amazon.com/fire-tv-stick-lite/product-reviews/B07YNLBS7R/ref=cm_cr_arp_d_paging_btm_next_2?ie=UTF8&reviewerType=all_reviews&pageNumber=1"
#page1 <- amazon_firestick_url %>% read_html() %>% html_nodes('.review-data') %>% html_text()
page1_v2 = amazon_firestick_url %>% read_html() %>% html_nodes("[class='a-size-base review-text review-text-content']") %>% html_text()


amazon_fun = function(product, pg_num){
  amazon_url = paste0("https://www.amazon.com/fire-tv-stick-lite/product-reviews/",product,"/ref=cm_cr_arp_d_paging_btm_next_2?ie=UTF8&reviewerType=all_reviews&pageNumber=",pg_num)
  var1 = read_html(amazon_url) 
  review_title = var1 %>% html_nodes("[class='a-size-base a-link-normal review-title a-color-base review-title-content a-text-bold']") %>% html_text() 
  review_text =  var1 %>% html_nodes("[class='a-size-base review-text review-text-content']") %>% html_text()
  return(tibble(title = review_title, text = review_text, page = pg_num))
}

all_reviews = tibble()
#decide about the page number later
for(i in 1:10){
  review = amazon_fun(product = "B07YNLBS7R", pg_num = i) 
  all_reviews = bind_rows(all_reviews,review)
  Sys.sleep(3)
  }

all_reviews_cleaned = all_reviews

#CLEANING REVIEWS TABLE
all_reviews_cleaned$title = str_replace_all(all_reviews_cleaned$title,"\n ","")
all_reviews_cleaned$title = str_replace_all(all_reviews_cleaned$title,"\n     ","")
all_reviews_cleaned$title = str_replace_all(all_reviews_cleaned$title,"\n","")

all_reviews_cleaned$text = str_replace_all(all_reviews_cleaned$text,"\n ","")
all_reviews_cleaned$text = str_replace_all(all_reviews_cleaned$text,"\n      ","")
all_reviews_cleaned$text = str_replace_all(all_reviews_cleaned$text,"\n","")


#TOKENIZING



#APPLY SENTIMENT ANALYSIS

