library(tidyverse)
library(magrittr)
library(xml2)
library(rvest)

safe_read <-
  possibly(
    function(url){
      Sys.sleep(1)
      message(paste("Parsing", url))
      read_html(url)
    },
    "404")

dfp_blog <-
  1:30 %>% 
  paste0("https://www.dataforprogress.org/blog?page=", .) %>%
  sapply(safe_read, simplify = F, USE.NAMES = T) %>% 
  sapply(html_nodes, xpath = '//a[@class="BlogList-item-title"]', simplify = F)

dfp_posts_base <-
  dfp_blog %>%
  flatten() %>% 
  sapply(xml_attr, "href") %>% 
  paste0("https://www.dataforprogress.org", .) %>% 
  tibble("url" = .) %>% 
  mutate(raw_src = map(url, safe_read))

dfp_posts <-
  dfp_posts_base %>% 
  mutate(
    title = 
      raw_src %>% 
      map_chr(~.x %>% 
                as_xml_document() %>% 
                html_node(".BlogItem-title") %>% 
                html_text()
      ),
    pub_date = 
      raw_src %>% 
      map_chr(~.x %>% 
                as_xml_document() %>% 
                html_node(".Blog-meta-item--date") %>% 
                html_text()
      ),
    authors = 
      raw_src %>% 
      map(~.x %>% 
            as_xml_document() %>% 
            html_node('[data-layout-label="Post Body"]') %>%
            html_node('p') %>% 
            html_nodes('strong') %>% 
            html_text()
      ),
    content = 
      raw_src %>% 
      map_chr(~.x %>% 
            as_xml_document() %>% 
            html_node('[data-layout-label="Post Body"]') %>%
            html_nodes('p') %>%
            tail(-1) %>% 
            html_text() %>% 
            str_subset("^.{55,}$") %>% 
            str_c(collapse = "\n")
      ),
  ) %>% 
  select(-raw_src) %T>%
  glimpse()
