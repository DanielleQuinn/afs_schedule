# Function: process_talks(date_input)
# requires: data frame called talks, output by build_full_schedule.R
# input: "yyyy-mm-dd"
# output: table
## columns are locations
## rows are time
## cells are talk title and link, formatted to be read by Excel
process_talks <- function(date_input) {
  talks %>%
    filter(date == ymd(date_input)) %>%
    filter(!link == "") %>%
    mutate(use = paste0('=HYPERLINK("', link, '",', '"', title, '")')) %>%
    mutate(loc = paste(location, room, sep = ", ")) %>%
    select(Time = datetime, use, loc) %>%
    tidyr::spread(key = "loc", value = "use", fill = "-") %>%
    mutate(Time = str_sub(Time, str_length(Time) - 7, str_length(Time) - 3)) %>%
    return()
}
