# Function: order_rooms(x)
# input: table generated from process_talks()
# output: vector of locations organized by building and room number
order_rooms <- function(x) {
  data.frame(names = names(x)[2:ncol(x)]) %>%
    separate(names, into = c("location", "room"), sep = ",", remove = FALSE) %>%
    mutate(room = str_trim(room),
           room_id = str_extract(room, "[aA-zZ]+"), 
           room_number = as.numeric(str_extract(room, "[0-9]+")),
           names = as.character(names)) %>%
    select(names, location, room_id, room_number) %>%
    arrange(location, room_id, room_number) %>%
    pull(names) %>% 
    return()
}
