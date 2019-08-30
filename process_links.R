# This reads the links.txt file as an html and extracts every link from it as a vector
mylinks <- read_html(links.txt) %>% 
  html_nodes("a") %>% 
  html_attr("href") %>% 
  unique() %>%
  stringr::str_subset("session")
