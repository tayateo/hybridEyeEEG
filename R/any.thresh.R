any.thresh <- function(edffile, lower_th, higher_th, actualfixDur)
  
{
  
  lines <- load.edf(sprintf("%s.edf",edffile))
  
  cat("\n", edffile, ".EDF", "has been read", "\n")
    
  eyeTrackersRate <- as.numeric((str_filter(lines, '^SAMPLES.+RATE\\t([[:digit:]]+)'))[[1]][[2]])
  
  first.sync <- edf.sync(lines)
  
  lower_th_fixes <- processFixation(edffile, lower_th, actualfixDur)
  
  cat("\n","fixations processed for lower threshold in file ", edffile,".EDF", "\n")
  
  higher_th_fixes <- processFixation(edffile, higher_th, actualfixDur)
  
  cat("\n","fixations processed for higher threshold in file ", edffile, ".EDF","\n")
  
  timestamps <- comp_ans_ans2(lower_th_fixes, higher_th_fixes)
  
  saccs <- str_filter(lines, 'ESACC.+ [[:digit:]]+\\t([[:digit:]]+)')
  saccs <- sapply(saccs, function(i) as.numeric(i[[2]])- first.sync)
  
  saccs <- saccs[saccs>0]
  timestamps <- timestamps[timestamps>0]
  
  for.eeglab <- data.frame(Latency = c(timestamps/eyeTrackersRate,saccs/eyeTrackersRate),
                           Type = c(rep('fixation_start', length(timestamps)), rep('SRsaccadeEND', length(saccs))))
  
  for.eeglab <- for.eeglab[order(for.eeglab$Latency),]
  
  write.table(for.eeglab, sprintf('fixLatencies%s_%s-%s.ascii', edffile, lower_th, higher_th), row.names = F, quote = F)
  
  cat("\n","file ", edffile, ".EDF", " - thresholds comparison done", "\n")
  
  cat("\n",">>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>", "\n")
  
  return(for.eeglab)
}