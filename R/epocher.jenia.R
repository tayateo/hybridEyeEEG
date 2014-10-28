epocher.jenia <- function(edffile, eegfile, diff, t1=-1000, t2=1000, durToCompare800=1200, prevSacc)
{
  
  data <- load_bcidat(sprintf("%s.dat",eegfile)) 
  lines <- load.edf(sprintf("%s.edf",edffile))
  
  samplingRate <- as.numeric(gsub('Hz','', data$parameters$SamplingRate))
  
  eyeTrackersRate <- as.numeric((str_filter(lines, '^SAMPLES.+RATE\\t([[:digit:]]+)'))[[1]][[2]])
  
  first.sync <- edf.sync(lines)
  
  if (diff == T)
  {
    #biderectional linear identity analysis
    ans <- processFixation(edffile, 800, 1200)
    ans2 <- processFixation(edffile, durToCompare800, 1200)
    
    timestamps <- comp_ans_ans2(ans, ans2)
   
    
    if(prevSacc == TRUE)
    {
      saccs <- getSaccs(lines, timestamps, first.sync, eyeTrackersRate)
      for.eeglab <- data.frame(Latency = c(timestamps/eyeTrackersRate,saccs/eyeTrackersRate),
                               Type = c(rep('fixation_start', length(timestamps)), rep('saccade_start', length(saccs))))
      for.eeglab <- for.eeglab[order(for.eeglab$Latency),]
      
      write.table(for.eeglab, sprintf('fixLatencies%s.ascii', edffile), row.names = F, quote = F)
      
    }
    else
    {
      saccs <- str_filter(lines, 'ESACC.+ [[:digit:]]+\\t([[:digit:]]+)')
      saccs <- sapply(saccs, function(i) as.numeric(i[[2]])- first.sync)
      
      for.eeglab <- data.frame(Latency = c(timestamps/eyeTrackersRate,saccs/eyeTrackersRate),
                               Type = c(rep('fixation_start', length(timestamps)), rep('saccade_start', length(saccs))))
      for.eeglab <- for.eeglab[order(for.eeglab$Latency),]
      
      write.table(for.eeglab, sprintf('fixLatencies%s.ascii', edffile), row.names = F, quote = F)
    }
      
  }
  else
  {
    ans <- processFixation(edffile, 800, 800)
    ans2 <- processFixation(edffile, durToCompare800, 800)
    
    timestamps <- comp_ans_ans2(ans, ans2)
    
        
    if(prevSacc == TRUE)
    {
      saccs <- getSaccs(lines, timestamps, first.sync, eyeTrackersRate)
      for.eeglab <- data.frame(Latency = c(timestamps/eyeTrackersRate,saccs/eyeTrackersRate),
                               Type = c(rep('fixation_start', length(timestamps)), rep('saccade_start', length(saccs))))
      for.eeglab <- for.eeglab[order(for.eeglab$Latency),]
      
      write.table(for.eeglab, sprintf('fixLatencies%s.ascii', edffile), row.names = F, quote = F)
      
    }
    else
    {
      saccs <- str_filter(lines, 'ESACC.+ [[:digit:]]+\\t([[:digit:]]+)')
      saccs <- sapply(saccs, function(i) as.numeric(i[[2]])- first.sync)
      
      for.eeglab <- data.frame(Latency = c(timestamps/eyeTrackersRate,saccs/eyeTrackersRate),
                               Type = c(rep('fixation_start', length(timestamps)), rep('saccade_start', length(saccs))))
      for.eeglab <- for.eeglab[order(for.eeglab$Latency),]
      write.table(for.eeglab, sprintf('fixLatencies%s.ascii', edffile), row.names = F, quote = F)
    }
  }
    
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
  
  epo <- filt.and.epo(newsi, samplingRate, timestamps, t1, t2)
  
  res <- list(epo, samplingRate = samplingRate)
  
  return(res)
}

