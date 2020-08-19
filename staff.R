# Library ----
library(tidyverse)
library(magrittr)
library(xml2)
library(rvest)
library(tidytext)
library(reshape2)
library(pdftools)

# Import Data ----
safe_read <-
  possibly(
    function(url){
      Sys.sleep(.1)
      message(paste("Parsing", url))
      read_html(url)
    },
    "404")

dfp_staff_base <-
  "https://www.dataforprogress.org/who-we-are" %>%
  safe_read() %>% 
  html_nodes(".summary-item")

dfp_staff <-
  dfp_staff_base %>% 
  enframe('order', 'src') %>% 
  mutate(
    name = 
      src %>% 
      html_node(".summary-title") %>% 
      html_text() %>% 
      str_squish(),
    title = 
      src %>% 
      html_node(".summary-excerpt") %>% 
      html_node("strong") %>% 
      html_text() %>% 
      replace_na("Fellow") %>% 
      str_squish(),
    twitter = 
      src %>% 
      html_node(".summary-excerpt") %>% 
      html_node(xpath = "p[starts-with(., '@')]") %>% 
      html_text(),
    about =
      src %>%
      map_chr(~.x %>% 
                html_node(".summary-excerpt") %>%
                html_nodes(xpath = "p[not(starts-with(., '@')) and string-length(.) > 40]") %>% 
                html_text() %>% 
                paste0(collapse = " ") %>% 
                str_squish()
      ),
  ) %>% 
  select(-order, -src) %T>%
  glimpse()
