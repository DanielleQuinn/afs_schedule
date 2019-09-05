# With hundreds of links to loop through, it seemed to
# time out. I found this solution / function on
# StackOverflow and it fixed the problem
# post: https://stackoverflow.com/questions/37839566/how-do-i-close-unused-connections-after-read-html-in-r
# user: https://stackoverflow.com/users/5778374/nm200
CatchupPause <- function(Secs){
  Sys.sleep(Secs) #pause to let connection work
  closeAllConnections()
  gc()
}


