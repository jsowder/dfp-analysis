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

dfp_memos <-
  1:10 %>% 
  paste0("https://www.dataforprogress.org/memos?page=", .) %>%
  sapply(safe_read, simplify = F, USE.NAMES = T) %>% 
  sapply(html_nodes, xpath = '//a[@class="BlogList-item-title"]', simplify = F)

dfp_memos_base <-
  dfp_memos %>%
  flatten() %>% 
  sapply(xml_attr, "href") %>% 
  paste0("https://www.dataforprogress.org", .) %>% 
  tibble("url" = .) %>% 
  mutate(raw_src = map(url, safe_read))

dfp_memos_detail <-
  dfp_memos_base %>% 
  filter(raw_src != "404") %>% 
  mutate(
    title = 
      raw_src %>% 
      map_chr(~.x %>% 
                as_xml_document() %>% 
                html_node(".BlogItem-title") %>% 
                html_text() %>% 
                str_remove_all("Memo:|Report:") %>% 
                str_squish()
      ),
    pub_date = 
      raw_src %>% 
      map_chr(~.x %>% 
                as_xml_document() %>% 
                html_node(".Blog-meta-item--date") %>% 
                html_text()
      ) %>% 
      lubridate::mdy(),
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
    memo_pdf = 
      raw_src %>% 
      map_chr(~.x %>% 
                as_xml_document() %>% 
                html_node('[class="sqs-block-button-element--medium sqs-block-button-element"]') %>%
                html_attr('href')
      ),
    img = 
      raw_src %>% 
      map_chr(~.x %>% 
                as_xml_document() %>% 
                html_node('[class="sqs-block-button-element--medium sqs-block-button-element"]') %>%
                html_attr('href')
      ),
  ) %>% 
  select(-raw_src) %T>%
  glimpse()

# listen here jack, how do I open PDF? ----

safe_pdf <-
  possibly(
    function(url){
      Sys.sleep(.01)
      message(paste("Parsing", url))
      pdf_data(url)
    },
    NULL)

dfp_pdf_base <-
  dfp_memos_detail %>% 
  select(memo_pdf) %>% 
  filter(str_detect(memo_pdf, "pdf$")) %>% 
  mutate(contents = 
           memo_pdf %>% 
           map(~ .x %>% 
                 safe_pdf())
  ) %>% 
  unnest(contents) %>%
  group_by(memo_pdf) %>%
  mutate(page = row_number()) %>% 
  unnest(contents) %>% 
  group_by(memo_pdf, y) %>% 
  mutate(line = row_number())

dfp_pdfs <- 
  dfp_pdf_base %>% 
  group_by(memo_pdf) %>% 
  count(height) %>% 
  arrange(desc(n), .by_group=T) %>% 
  slice(1) %>% 
  rename(primary_height = height) %>% 
  select(memo_pdf, primary_height) %>% 
  inner_join(dfp_pdf_base) %>% 
  filter(height == primary_height)

# getting sentimental ----

tidy_pdfs <-
  dfp_pdfs %>% 
  rename(word = text) %>% 
  anti_join(stop_words) %>% 
  inner_join(get_sentiments("afinn")) %>% 
  group_by(memo_pdf) %>% 
  summarise(sentiment = mean(value)) %>% 
  inner_join(dfp_memos_detail) %>% 
  select(title, pub_date, sentiment) %>% 
  arrange(desc(sentiment))

tidy_pdfs %>% 
  filter(
    pub_date > lubridate::today()-365,
    sentiment > 2 | sentiment < -1
    ) %>% 
  ggplot(aes(reorder(title, sentiment), sentiment, fill = sentiment > 0)) +
  geom_bar(stat = "identity", show.legend = F) +
  ylab("Sentiment Score") +
  xlab("") +
  coord_flip() + 
  theme_classic()

tidy_pdfs %>% 
  filter(pub_date > lubridate::today()-365) %>% 
  ggplot(aes(pub_date, sentiment, fill = sentiment > 0)) +
  geom_bar(stat = "identity", show.legend = F) +
  ylab("Sentiment Score") +
  xlab("")

