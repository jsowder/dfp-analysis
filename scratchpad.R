library(tidyverse)
library(magrittr)
library(xml2)
library(rvest)

safe_read <-
  possibly(
  function(url){
    Sys.sleep(3)
    message(paste("Parsing", url))
    read_html(url)
  },
  "404")

dfp_blog <-
  1:30 %>% 
  paste0("https://www.dataforprogress.org/blog?page=", .) %>%
  sapply(safe_read, simplify = F, USE.NAMES = T) %>% 

dfp_posts_base <-
  dfp_blog %>%
  flatten() %>% 
  sapply(xml_attr, "href") %>% 
  paste0("https://www.dataforprogress.org", .) %>% 
