#'Prepares eeg and eyetracking data and performs claassification
#'
#'@examples
#'#First need to set base path for the Brownie package 
#'#that contains a folder with data
#'set.brownie.root("d:\\")
#'@param exp_type name of the experiment's serie (e.g. "e2")
#'@param exp_name name of the concrete experiment (e.g. "01" if your experiment's full name is "e201s001") 

classify <- function(exp_type, exp_name, classifier = NULL)
{

  path.to.files <- sprintf("%s\\%s\\%s", data.path(), exp_type, exp_name)
  exp.meta <- fromJSON(file = sprintf("%s\\meta.json", path.to.files))
  
  name_dat <- brownie.extract.file.names(exp.meta)$dat
  name_edf <- brownie.extract.file.names(exp.meta)$edf
  
  data <-  load_bcidat(sprintf("%s\\%s", path.to.files, name_dat)) 
  ans <- load.one.eye(sprintf("%s\\%s", path.to.files, name_edf))
  
  newsi <- cut.eeg(data)
  
  #extracting events
  
  lines <- ans$events$message
  
  first_sync <- ans$sync_timestamp
  
  fixation.duration <- ans$fixation.duration
  
  fixations.starts <- get.events.timestamps("fixation", lines, fisrt_sync, get_ev_starts = T, fixation.duration)
  
  eyeRate <- ans$samplingRate
  
  eegRate <- as.numeric(gsub("Hz", "", data$parameters$SamplingRate))
  

}