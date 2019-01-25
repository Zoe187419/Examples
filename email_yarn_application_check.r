# Configs
application_id <- 'application_1545394293353_8270'
email_address <- 'matt.johnson@hrblock.com'
still_running_check <- TRUE
sleep_seconds <- 300

# Define function
send_yarn <- function(){
  
  # Get applications
  app_list <- system('yarn application -list', intern=TRUE)
  
  # Check for application
  done <- TRUE
  details <- ''
  for (a in unlist(app_list)[c(3:length(unlist(app_list)))]){
    row <- trimws(strsplit(a, '\t')[[1]])
    if (row[1] == application_id){
      done <- FALSE
      details <- system(paste('yarn application -status', application_id), intern=TRUE)
      details <- paste(trimws(details), collapse='\n')
    }
  }
  
  # Write body
  body <- paste('Application', application_id, 'is still running.\n\n', details)
  if (done){
    body <- paste('Application', application_id, 'is DONE running.')
  }
  
  # Send email
  if (done | still_running_check){
    body <- paste(body, collapse='\n')
    system(paste('mail -s "Application Check"', email_address, '<<< """', body, '"""'))
  }
}

# Run 
for (t in c(1:10000)){
  print(paste('Running iteration', t))
  send_yarn()
  print(paste('Sleeping for', sleep_seconds/60, 'minutes'))
  Sys.sleep(sleep_seconds)
}