
check.sync <- function(exp)
{
  time.and.type <- data.frame()
  for (files in exp)
  {
    
    edffile <- files$edffile
    eegfile <- files$eegfile
    
    
    lines <- load.edf(sprintf("%s.edf",edffile))
    eeg <- load_bcidat(sprintf("%s.dat",eegfile)) 
    
    
    samplingRate <- as.numeric(gsub('Hz','', eeg$parameters$SamplingRate))
    
    eyeTrackersRate <- as.numeric((str_filter(lines, '^SAMPLES.+RATE\\t([[:digit:]]+)'))[[1]][[2]])
    
    
    #find sync sign in eeg file
        
    df.eeg <- data.frame(times = which(eeg$signal[,15]!=0),
                         type = eeg$signal[which(eeg$signal[,15]!=0),15],
                         file = rep(eegfile, length(which(eeg$signal[,15]!=0))))
    
    #find sync sign in edf file
    
    sync.points <- str_filter(lines, 'MSG\\t([[:digit:]]+).+CMD.+0x8 +([[:digit:]]+)')
    type <- sapply(sync.points, function(i) (as.numeric(i[[3]])))
    times = sapply(sync.points, function(i) (as.numeric(i[[2]])))
    df.edf <- data.frame(times = times,
                             type = type,
                             file = rep(edffile, length(sync.points)))
    
    #check timing between points in one block
    
    tmp <- subset(df.edf, type == 2 | type == 15)
    sync.delay.edf <- diff(tmp$times)[c(1,2,3,5,6,7)]/eyeTrackersRate*1000
    
    sync.delay.eeg <- (diff(df.eeg$times)[c(1,2,3,5,6,7)])/samplingRate*1000
    
    cat('\n', edffile, '\n' ,sync.delay.edf, '\n', eegfile, '\n', sync.delay.eeg, '\n')
    
    write.table(data.frame(c(edffile, sync.delay.edf), c(eegfile, sync.delay.eeg)),
                file = sprintf('sync.diff%s-%s.asc', edffile, eegfile),
                row.names = F,
                col.names = F,
                quote = F)
    
    time.and.type <- rbind(time.and.type, df.edf, df.eeg)
  }
  
write.table(time.and.type, file = "sync_table.asc", row.names = F, quote = F)
return(time.and.type)

}