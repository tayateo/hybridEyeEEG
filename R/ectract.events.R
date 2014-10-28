extract.events <- function(filename)
{
  #with this function you can extract existing messages and timestamps 
  #from an edf file
  
  lines <- load.edf(sprintf("%s.edf",filename))
  
  fixation.duration <- as.numeric((str_filter(lines, '.+fixationDuration\":([[:digit:]]+)'))[[1]][[2]])
  
  fixations <- str_filter(lines, '^MSG\\t(\\d+) fixation in region.center.x = ([0-9.]+), region.center.y = ([0-9.]+)')   
  
  #searching for bits, sent from an application
  bits <- str_filter(lines, '^MSG\\t([[:digit:]]+) +\\!CMD.+2') 
  
  #taking timestamps of all fixation ends
  fix_times <- sapply(fixations, function(i) as.numeric(i[[2]]));
  
  #here we are taking the third synchronization bit to use
  #it as the reference point in eeg file
  first_sync <- as.numeric(bits[[3]][[2]])
  
  corrected_times <- fix_times - first_sync - fixation.duration
  # we substract it from each fixation end timestamp
  
  # writeMat(sprintf("%s_corr_times.mat", filename), corrected_times=corrected_times)
  
  l <- list(lines=lines, first_sync=first_sync, corrected_times=corrected_times)
  
  return(l) 
}
