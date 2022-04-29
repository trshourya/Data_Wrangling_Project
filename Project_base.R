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
library(tidytext)

#TESTING THE REVIEWS
amazon_firestick_url = "https://www.amazon.com/fire-tv-stick-lite/product-reviews/B07YNLBS7R/ref=cm_cr_arp_d_paging_btm_next_2?ie=UTF8&reviewerType=all_reviews&pageNumber=1"
#page1 <- amazon_firestick_url %>% read_html() %>% html_nodes('.review-data') %>% html_text()
page1_v2 = amazon_firestick_url %>% read_html() %>% html_nodes("[class='a-size-base review-text review-text-content']") %>% html_text()


amazon_fun = function(product, pg_num){
  amazon_url = paste0("https://www.amazon.com/fire-tv-stick-lite/product-reviews/",product,"/ref=cm_cr_arp_d_paging_btm_next_2?ie=UTF8&reviewerType=all_reviews&pageNumber=",pg_num)
  var1 = read_html(amazon_url) 
  review_title = var1 %>% html_nodes("[class='a-size-base a-link-normal review-title a-color-base review-title-content a-text-bold']") %>% html_text() 
  review_text =  var1 %>% html_nodes("[class='a-size-base review-text review-text-content']") %>% html_text()
  review_rating = var1 %>% html_nodes("[data-hook='review-star-rating']") %>% html_text() -> review_star
    
  return(tibble(title = review_title, text = review_text,rating = review_rating, page = pg_num))
}

all_reviews = tibble()
#decide about the page number later
for(i in 1:500){
  review = amazon_fun(product = "B07YNLBS7R", pg_num = i) 
  all_reviews = bind_rows(all_reviews,review)
  #Sys.sleep(1)
  }

all_reviews_cleaned = all_reviews 

#CLEANING REVIEWS TABLE
all_reviews_cleaned$title = str_replace_all(all_reviews_cleaned$title,"\n ","")
all_reviews_cleaned$title = str_replace_all(all_reviews_cleaned$title,"\n     ","")
all_reviews_cleaned$title = str_replace_all(all_reviews_cleaned$title,"\n","")

all_reviews_cleaned$text = str_replace_all(all_reviews_cleaned$text,"\n ","")
all_reviews_cleaned$text = str_replace_all(all_reviews_cleaned$text,"\n      ","")
all_reviews_cleaned$text = str_replace_all(all_reviews_cleaned$text,"\n","")

#ADDING AN INCREMENTAL NUMBER FOR EACH TITLE
all_reviews_cleaned = all_reviews_cleaned  %>% mutate(review_number = row_number())

#TOKENIZING
#page_word_tokens = all_reviews_cleaned %>% group_by(review_number) %>% unnest_tokens(word,text)
#page_word_tokens = page_word_tokens %>% anti_join(stop_words) 

#FOR TOPIC MODELLING or SENTIMENT 
text_word_tokens = all_reviews_cleaned %>% unnest_tokens(word,text) %>% select(review_number,word)
text_word_tokens = text_word_tokens %>% anti_join(stop_words) 
text_word_tokens = text_word_tokens %>% mutate(word = str_extract(word, "[a-z]+")) #to make sure we only have words and not numbers
text_word_tokens = text_word_tokens %>% anti_join(stop_words) 

#CHECKING COMMON WORDS
count_words_text = text_word_tokens %>% count(word, sort = TRUE) 
head(count_words_text,25)

#REMOVING PRODUCT SPECIFIC WORDS - removing words that are product specific or technical
prod_words = c('fire','stick','tv','remote','firestick','amazon','prime','roku','lite','device',
               'app','apps','alexa','netflix')

text_word_tokens[,2] = lapply(text_word_tokens[,2], function(x) replace(x,x %in% prod_words,NA)) #replacing all the product specific words with NA
count_words_text2 = text_word_tokens %>% count(word, sort = TRUE) 

#removing rows with NA
text_word_tokens = text_word_tokens %>% drop_na(word)

#getting the value from afinn
afinn_lex = get_sentiments("afinn")
df_score = text_word_tokens %>% inner_join(afinn_lex)
count_df_score = df_score %>% count(word, sort = TRUE) 

#calculating the score for each review 
df_score2 = df_score %>% group_by(review_number) %>% summarise(sent_score = sum(value)/n() )

#normalizing the score to 1-5 scale
df_score2 = df_score2 %>% mutate(norm_score = ifelse(sent_score==0,1,(0.4*(sent_score-5)) + 5))
#df_score2 = df_score2 %>% mutate(norm_score = ifelse(sent_score==0,1,(4/(max(sent_score)-min(sent_score))*(sent_score-5)) + 5))
df_score2 = df_score2 %>% mutate(norm_score_round = round(norm_score))

#comparing the predicted rating for each review with the actual rating
actual_rating = all_reviews_cleaned[,c(3,5)]
actual_rating$rating = as.numeric(substr(actual_rating$rating,1,3))
df_score_compare = df_score2 %>% inner_join(actual_rating)

cor(df_score_compare$norm_score_round,df_score_compare$rating) #we have to improve the correlation 1) increase n 2) remove outliers 3) try different scaling formula
cor(df_score_compare$norm_score,df_score_compare$rating)


############################################ USING BING LEXICON ####################################################
bing_lex = get_sentiments("bing")
df_bing_score = text_word_tokens %>% inner_join(bing_lex)

df_bing_score2 = df_bing_score %>% group_by(review_number,sentiment) %>% summarize(n = n()) %>% 
                 pivot_wider(names_from = "sentiment", values_from = "n")

df_bing_score2[is.na(df_bing_score2)] = 0
df_bing_score2 = df_bing_score2 %>% mutate(positive_perc = positive/(positive+negative))

#scaling the positive perc to 1-5 scale
df_bing_score3 = df_bing_score2 %>% mutate(norm_score = (4*(positive_perc-1)) + 5)
df_bing_score3 = df_bing_score3 %>% mutate(norm_score_round = round(norm_score))
df_bing_score3 = df_bing_score3 %>% inner_join(actual_rating)

cor(df_bing_score3$norm_score_round,df_bing_score3$rating) #we have to improve the correlation 1) increase n 2) remove outliers 3) try different scaling formula
cor(df_bing_score3$norm_score,df_bing_score3$rating) 





########################## TOPIC MODELLING #############################




#TOKENIZING ON TITLE MAKES MORE SENSE
title_word_tokens = all_reviews_cleaned %>% unnest_tokens(word,title)
title_word_tokens = title_word_tokens %>% anti_join(stop_words) 
title_word_tokens = title_word_tokens %>% mutate(word = str_extract(word, "[a-z]+")) #to make sure we only have words and not numbers

#CHECKING COMMON WORDS
count_words_title = title_word_tokens %>% count(word, sort = TRUE)
head(count_words_title,25)




#TITLE AND TEXT BOTH ARE PRESENT, LETS ADD A WEIGHT TO BOTH LATER
# SEE IF WE CAN ADD WORD CLOUDE __ CHECK SHINY
#arxiv.org
#perform n gram





#APPLY SENTIMENT ANALYSIS

