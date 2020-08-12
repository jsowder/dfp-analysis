# Library ----
library(tidyverse)
library(magrittr)
library(xml2)
library(rvest)
library(tidytext)
library(reshape2)

# Import Data ----
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
  filter(raw_src != "404") %>% 
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
      ) %>% 
      lubridate::mdy(),
    authors = 
      raw_src %>% 
      map(~.x %>% 
            as_xml_document() %>% 
            html_node('[data-layout-label="Post Body"]') %>%
            html_node('p') %>% 
            html_text() %>%
            str_remove_all("^[Bb]y|^Authors:") %>% 
            str_remove_all("\\([^()]*\\)") %>%
            str_remove_all("@[:graph:]*") %>% 
            str_remove_all("^.{62,}$") %>% 
            str_remove_all("Friends,") %>% 
            str_squish() %>% 
            str_split(", | and ")
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

dfp_tidy <-
  dfp_posts %>% 
  unnest_tokens(word, content) %>% 
  anti_join(stop_words)

# Folks, we've got a wordcloud ----
dfp_tidy %>%
  count(word, sort = T) %>% 
  with(wordcloud::wordcloud(word, n, max.words = 100))

# Let's get sentimental ----

# Sentiment over time
stmnt_timeline <-
  dfp_tidy %>%
  filter(lubridate::year(pub_date) > 2018) %>% 
  mutate(`Publication Date` = 
           pub_date %>% 
           lubridate::floor_date(unit = "months")) %>% 
  inner_join(get_sentiments("afinn")) %>% 
  group_by(`Publication Date`) %>% 
  summarise(`Sentiment Score` = mean(value)) %>% 
  arrange(`Publication Date`)

stmnt_timeline %>% 
  ggplot(aes(`Publication Date`, `Sentiment Score`)) +
  geom_col(show.legend = FALSE)

# Sentiment by author
stmnt_authors <-
  dfp_tidy %>%
  inner_join(get_sentiments("afinn")) %>% 
  unnest(authors) %>%
  unnest(authors) %>% 
  mutate(authors =
           authors %>% 
           str_squish()) %>% 
  filter(authors != "",
         authors != "Tufts University",) %>% 
  group_by(authors) %>% 
  summarise(`Sentiment Score` = mean(value),
            post_count = n_distinct(url)
  ) %>% 
  filter(post_count > 10) %>% 
  arrange(desc(`Sentiment Score`)) %>% 
  rename(Author = authors)

stmnt_authors %>% 
  ggplot(aes(reorder(`Author`, `Sentiment Score`), `Sentiment Score`)) +
  geom_col(show.legend = FALSE) +
  coord_flip()

stmnt_authors %>% 
  ggplot(aes(reorder(`Author`, `Sentiment Score`), `Sentiment Score`)) + 
  library(reshape2)

dfp_tidy %>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort = TRUE) %>%
  acast(word ~ sentiment, value.var = "n", fill = 0) %>%
  wordcloud::comparison.cloud(colors = c("maroon", "forestgreen"),
                   max.words = 100)
  
  

