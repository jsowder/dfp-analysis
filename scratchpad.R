library(tidyverse)
library(magrittr)
library(xml2)

dfp_blog <-
  1:30 %>% 
  paste0("https://www.dataforprogress.org/blog?page=", .) %>%
  sapply(read_html, simplify = F, USE.NAMES = T)
