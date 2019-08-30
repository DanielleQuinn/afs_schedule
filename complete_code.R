library(rvest)
library(dplyr)
library(stringr)
library(lubridate)
library(tidyr)

# Function for extracting schedule from html
extract_schedule <- function(page) {
  read_page <- read_html(page)
  type <- read_page %>%
    html_node(xpath = "/html/body/div[2]/div/div[1]/p[1]/span[1]/strong") %>%
    html_text()
  if(!type %in% c("Symposium", "Contributed Paper Session")) { return(NULL)}
  if(type %in% c("Symposium", "Contributed Paper Session")) {
    date_input <- read_page %>%
      html_nodes(xpath = "/html/body/div[2]/div/div[2]/div/span[2]/strong") %>%
      html_text()
    room_input <- read_page %>%
      html_nodes(xpath = "/html/body/div[2]/div/div[1]/p[1]/span[2]") %>%
      html_text() %>%
      gsub("ROOM: ", "", .)
    session_input <- read_page %>%
      html_nodes("h1") %>%
      html_text(trim = TRUE)
    href_rows <- html_nodes(read_page, "table") %>%
      html_nodes( "tr") %>%
      lapply(., function(x) {
        x %>%
          html_nodes( "a") %>%
          html_attr("href")
      })
    df <- read_page %>%
      html_nodes(xpath = "/html/body/div[2]/div/div[1]/table") %>%
      html_table() %>%
      data.frame() %>%
      mutate(href = as.numeric(as.character(gsub("#", "", href_rows)))) %>%
      rename("time" = "X1", "title" = "X2") %>%
      filter(!time == "" & !title == "") %>%
      mutate(date = date_input,
             session = session_input,
             room = room_input, 
             link = ifelse(!is.na(href), paste0(page, href), "")) %>%
      separate(room, into = c("location", "room"), sep = ",") %>%
      select(date, session, location, room, time, title, link)
    return(df)
  }  
}

# from https://stackoverflow.com/users/5778374/nm200
CatchupPause <- function(Secs){
  Sys.sleep(Secs) #pause to let connection work
  closeAllConnections()
  gc()
}

# Process links
## Need a list of all links from detailed schedule; read_html() doesn't import child elements of 
## <div class="sessions list"> so I'm not able to directly extract the links I need
## Gave up, copy/pasted missing elements from raw html into .txt - find better solution! PhantomJS?
page2 <- read_html("C:/Users/Danielle/Desktop/testme.txt")
mylinks <- page2 %>% 
  html_nodes("a") %>% 
  html_attr("href") %>% 
  unique() %>%
  str_subset("session")

# Extract schedule
new_df <- list(NULL)
counter <- 0
starttime <- Sys.time()
for(i in mylinks)
{
  counter <- counter + 1
  print(paste(counter, "of", length(mylinks)))
  if(counter %% 25 == 0) {
    print(paste("elapsed time =", round(as.numeric(difftime(Sys.time(), starttime, unit="sec"))), "seconds"))
    CatchupPause(1)
  }
  new_df[[length(new_df) + 1]] <- extract_schedule(i)
}

# Create data frame of talks
talks <- bind_rows(new_df)

talks <- talks %>%
  separate(time, into = c("hour", "min"), sep = ":") %>%
  separate(min, into = c("min", "am_pm"), sep = 2) %>%
  mutate(hour = as.numeric(hour),
         min = as.numeric(min),
         hour = ifelse(am_pm == "PM", hour + 12, hour),
         date = mdy(talks$date),
         datetime = ymd_hm(paste(date, hour, min, sep="-"))) %>%
  select(date, datetime, location, room, session, title, link)

# Export data frame
write.csv(talks, "afs_talks.csv", row.names = FALSE)
