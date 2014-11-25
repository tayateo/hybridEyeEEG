new.epocher <- function(edffile,eegfile, t1=-500, t2=1000, ev1, ev2)
{  
  
  type1 = ev1
  
  switch(ev1,
         fixation = {ev1 = 'fixation in region.center.x.+time = +([[:digit:]]+)'},
         btn = {ev1 = '"ClickedToUnlock".+time += ([[:digit:]]+)'},
         ball = {ev1 = '"ballSelect".+time += ([[:digit:]]+)'},
         moveTo = {ev1 = '"ballMove".+time += ([[:digit:]]+)'},
         inBlocked = {ev1 = '"BoardPositionClickedInBlockedMode".+time += ([[:digit:]]+)'},
         block_ball = {ev1 = '"BallClickedInBlockedMode".+time += ([[:digit:]]+)'},
         block_board = {ev1 = '"BoardClickedInBlockedMode".+time += ([[:digit:]]+)'})
  
  
  type2 = ev2
  
  switch(ev2,
         fixation = {ev2 = ' fixation in region.center.x.+time = +([[:digit:]]+)'},
         btn = {ev2 = '"ClickedToUnlock".+time += ([[:digit:]]+)'},
         ball = {ev2 = '"ballSelect".+time += ([[:digit:]]+)'},
         moveTo = {ev2 = '"ballMove".+time += ([[:digit:]]+)'},
         inBlocked = {ev2 = '"BoardPositionClickedInBlockedMode".+time += ([[:digit:]]+)'},
         block_ball = {ev2 = '"BallClickedInBlockedMode".+time += ([[:digit:]]+)'},
         block_board = {ev2 = '"BoardClickedInBlockedMode".+time += ([[:digit:]]+)'})
  
  
  
  data <- load_bcidat(sprintf("%s.dat",eegfile)) 
  ans <- load.one.eye(sprintf("%s.edf",edffile))
  
  samplingRate <- as.numeric(gsub('Hz','', data$parameters$SamplingRate))
  
  eyeTrackersRate <- ans$samplingRate
  
  lines <- ans$events$message
  
  first_sync <- ans$sync_timestamp
  
  fixation.duration <- as.numeric((str_filter(lines, 'fixationDuration.+:([[:digit:]]+)'))[[1]][[2]])
  
  ev1 <- str_filter(lines, ev1)
  ev1.times <- sapply(ev1, function(i) as.numeric(i[[2]]) - first_sync - fixation.duration)
  
  ev2 <- str_filter(lines, ev2)
  ev2.times <- sapply(ev2, function(i) as.numeric(i[[2]]) - first_sync - fixation.duration)
  
  last.ch <- dim(data$signal)[[2]]
  
  count.index <- which(data$signal[,last.ch]!=0)
  count15 <- which(data$signal[count.index,last.ch]==15)
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
  
  epo.ev1 <- filt.and.epo(newsi, samplingRate, ev1.times, t1, t2)
  epo.ev2 <- filt.and.epo(newsi, samplingRate, ev2.times, t1, t2)
  
  chans <- data$parameters$ChannelNames
  
  if(length(chans) == 0)
  {
    channels <- c(5,6,7,8,11,12,14)
  }
  else
  {
    channels <- c(which(chans == "PO3"),
                  which(chans == "PO4"),
                  which(chans == "PO7"),
                  which(chans == "PO8"),
                  which(chans == "HEOG_L"),
                  which(chans == "HEOG_R"),
                  which(chans == "VEOG_B"))
  }
  
  
  res <- list(epo.ev1 = epo.ev1, epo.ev2 = epo.ev2, samplingRate = samplingRate,
              type1 = type1, type2 = type2, channels = channels, fixation.duration = fixation.duration)
  
  return(res)
}