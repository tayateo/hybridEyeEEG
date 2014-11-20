
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
  
  epo.ev1 <- filt.and.epo(newsi, samplingRate, ev1.times, t1, t2)
  epo.ev2 <- filt.and.epo(newsi, samplingRate, ev2.times, t1, t2)
  
  res <- list(epo.ev1 = epo.ev1, epo.ev2 = epo.ev2, samplingRate = samplingRate, type1 = type1, type2 = type2)
  
  return(res)
}

######################plot###################

new.plot.epo <- function(edffile, eegfile, ev1, ev2, t1=-500, t2=1000)
{

  res <- new.epocher(edffile, eegfile, t1, t2, ev1, ev2)
  
  epo.ev1 <- res$epo.ev1
  epo.ev2 <- res$epo.ev2
  
  samplingRate <- res$samplingRate
  
  t = (1:(t2-t1+1) +t1)/samplingRate;
  
  meanEpo1 <- colMeans(aperm(epo.ev1, c(3,1,2)))
  meanEpo2 <- colMeans(aperm(epo.ev2, c(3,1,2)))
  
  df <- data.frame(mode = c(rep(res$type1,nrow(meanEpo1)), rep(res$type2,nrow(meanEpo2))),
                  t=c(t,t),
                  P3 = c(meanEpo1[,5], meanEpo2[,5]),
                  P4 = c(meanEpo1[,6], meanEpo2[,6]),
                  O1 = c(meanEpo1[,7], meanEpo2[,7]),
                  O2 = c(meanEpo1[,8], meanEpo2[,8]),
                  HeOGl = c(meanEpo1[,11], meanEpo2[,11]),
                  HeOGr = c(meanEpo1[,12], meanEpo2[,12]),
                  VUp = c(meanEpo1[,13], meanEpo2[,13]),
                  Vdn = c(meanEpo1[,14], meanEpo2[,14]))
    
 
  plP3 <- plot_ch(df, "P3")  
  plP4 <- plot_ch(df, "P4")  
  plO1 <- plot_ch(df, "O1")  
  plO2 <- plot_ch(df, "O2")
  plHeOGl <- plot_ch(df, "HeOGl", -50, 50)
  plHeOGr <- plot_ch(df, "HeOGr", -50, 50)
  plVUp <- plot_ch(df, "VUp", -50, 50)
  plVdn <- plot_ch(df, "Vdn", -50, 50)
  

  grid.arrange(plP3, plP4, plO1, plO2,
               plHeOGl, plHeOGr, plVUp, plVdn,               
               ncol=2, heights=0.5, main = sprintf('file %s', eegfile),
               sub = sprintf('nEpo %s=%.0f\nnEpo %s=%.0f',
                             levels(df$mode)[[2]], dim(epo.ev1)[[3]],
                             levels(df$mode)[[1]], dim(epo.ev2)[[3]]))
  
  pl <- arrangeGrob(plP3, plP4, plO1, plO2,
                    plHeOGl, plHeOGr, plVUp, plVdn,               
                    ncol=2, heights=0.5, main = sprintf('file %s', eegfile),
                    sub = sprintf('nEpo %s=%.0f\nnEpo %s=%.0f',
                                  levels(df$mode)[[2]], dim(epo.ev1)[[3]],
                                  levels(df$mode)[[1]], dim(epo.ev2)[[3]]))
  ggsave(filename=sprintf('file %s_%s vs %s.jpg', eegfile, res$type1, res$type2), plot=pl)
  
  
}
