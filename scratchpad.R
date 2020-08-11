library(tidyverse)
library(magrittr)
library(xml2)

dfp_blog <-
  1:30 %>% 
  paste0("https://www.dataforprogress.org/blog?page=", .) %>%
  sapply(read_html, simplify = F, USE.NAMES = T) %>% 
  sapply(xml_find_all, '//a[@class="BlogList-item-title"]', simplify = F)

dfp_posts_base <-
  dfp_blog %>%
  flatten() %>% 
  sapply(xml_attr, "href") %>% 
  paste0("https://www.dataforprogress.org", .) %>% 
  tibble("url" = .)
