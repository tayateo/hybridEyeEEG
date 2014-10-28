epocherSRfix <- function(edffile, eegfile, t1=-1000, t2=1000)
{
  
  data <- load_bcidat(sprintf("%s.dat",eegfile)) 
  lines <- load.edf(sprintf("%s.edf",edffile))
  
  samplingRate <- as.numeric(gsub('Hz','', data$parameters$SamplingRate))
  
  eyeTrackersRate <- as.numeric((str_filter(lines, '^SAMPLES.+RATE\\t([[:digit:]]+)'))[[1]][[2]])
  
  #searching for bits, sent from an application
  bits <- str_filter(lines, '^MSG\\t([[:digit:]]+) +\\!CMD.+2') 
  first_sync <- as.numeric(bits[[3]][[2]])
  
  timestamps <- str_filter(lines, 'SFIX.+ ([[:digit:]]+)')
  timestamps <- sapply(timestamps, function(i) as.numeric(i[[2]])- first_sync)
  
  count.index <- which(data$signal[,15]!=0)
  count15 <- which(data$signal[count.index,15]==15)
  if (length(count15)>1)
  {
    ref.point <- (count15[length(count15)-1])-1
  }
  else
  {
    ref.point <- (count15[length(count15)])-1
  }
  third.bit <- count.index[ref.point]
  
  newsi <- data$signal[third.bit:length(data$signal[,1]),]
  
  epo <- filt.and.epo(newsi, samplingRate, timestamps)
  
  res <- list(epo, samplingRate = samplingRate)
  
  return(res)
}