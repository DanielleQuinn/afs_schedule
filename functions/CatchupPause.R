# Sourced from https://stackoverflow.com/users/5778374/nm200
# https://stackoverflow.com/questions/37839566/how-do-i-close-unused-connections-after-read-html-in-r

# Solution for connections timing out when looping through all links
CatchupPause <- function(Secs){
  Sys.sleep(Secs) #pause to let connection work
  closeAllConnections()
  gc()
}
