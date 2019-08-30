# For each link extracted from the AFS Detailed Schedule, extract the schedule details

new_df <- list(NULL)
counter <- 0
starttime <- Sys.time()
for(i in mylinks)
{
  counter <- counter + 1
  print(paste(counter, "of", length(mylinks)))
  # After every 25 links processed, print the elapsed time and run CatchupPause
  if(counter %% 25 == 0) {
    print(paste("elapsed time =", round(as.numeric(difftime(Sys.time(), starttime, unit="sec"))), "seconds"))
    CatchupPause(1)
  }
  new_df[[length(new_df) + 1]] <- extract_schedule(i)
}

# Bind data frames from each session
talks <- bind_rows(new_df)
