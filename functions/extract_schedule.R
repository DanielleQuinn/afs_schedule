# Function: extract_schedule(page)
# input: urls of links from AFS149 Detailed Schedule: https://afstws2019.org/detailed-schedule/
## Output: data frame of date, time, location (building), room, session name, talk title, and link to abstract for each talk in session
extract_schedule <- function(page) {
  read_page <- read_html(page)

  # Only extract schedules from pages labelled "Symposium" or
  # "Contributed Paper Session"
  type <- read_page %>%
    html_node(xpath = "/html/body/div[2]/div/div[1]/p[1]/span[1]/strong") %>%
    html_text()
  if(!type %in% c("Symposium", "Contributed Paper Session")) {return(NULL)}

  # Extract date, location, session name 
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
    
    # For each row of the html table
    # extract the embedded link from href
    href_rows <- html_nodes(read_page, "table") %>%
      html_nodes( "tr") %>%
      lapply(., function(x) {
        x %>%
          html_nodes( "a") %>%
          html_attr("href")
      })
    
    # Extract the text from the html table, convert to a
    # data frame, tidy, and add date, session,
    # location, and links
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
      tidyr::separate(room, into = c("location", "room"), sep = ",") %>%
      select(date, session, location, room, time, title, link)
    return(df)
  }  
}
