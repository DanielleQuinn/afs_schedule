# To use extract_schedule() I need to be able to pull
# all of the links from https://afstws2019.org/detailed-schedule/
# Apparently, javascript is doing something to modify the
# page after loading so it was tricky to automate ...
# I finally resorted to copy/pasting the raw html code
# directly from the website and saving it -_-
# This file can be found at https://github.com/DanielleQuinn/afs_schedule/blob/master/links.txt

# Using this file, extract all of the links to each session
# and tidy for use with extract_schedule()
mylinks <- read_html("C:/Users/Danielle/Desktop/testme.txt") %>% 
  html_nodes("a") %>% 
  html_attr("href") %>% 
  unique() %>%
  stringr::str_subset("session")
