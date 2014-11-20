#'Checks synchronization between EDF and DAT files
#'
#'Computes differenece between every two synchronization points in the beggining and in the end of two files
#'
#'@param edffile an .edf file name without an extesion
#'@param eegfile a .dat file name without an extesion
#'@param channel_with_sync - a channel in eeg, containing synchronization timestamps
#'
#'@return
#'a table with all sync points and their timestamps for both files.
#'Also saves the same table to asc file with a corresponding name.

check.sync <- function(edffile, eegfile)
{
    time.and.type <- data.frame()
    
    if(grepl('dat', eegfile, ignore.case = T))
    {
      eeg <- load_bcidat(eegfile) 
    }
    else
    {
      eeg <- load_bcidat(sprintf("%s.dat",eegfile)) 
    }
    
    if(grepl('edf', edffile, ignore.case = T))
    {
      ans <- load.one.eye(edffile)
    }
    else
    {
      ans <- load.one.eye(sprintf("%s.edf",edffile))
    }
    
    
    
    samplingRate <- as.numeric(gsub('Hz','', eeg$parameters$SamplingRate))
    
    eyeTrackersRate <- ans$samplingRate
    
    
    #find sync sign in eeg file
        
    df.eeg <- data.frame(times = which(eeg$signal[,dim(eeg$signal)[[2]]]!=0),
                         type = eeg$signal[which(eeg$signal[,dim(eeg$signal)[[2]]]!=0),dim(eeg$signal)[[2]]],
                         file = rep(eegfile, length(which(eeg$signal[,dim(eeg$signal)[[2]]]!=0))))
    
    #find sync sign in edf file
    
    sync.points <- ans$pulses[-(which(ans$pulses$sent == 0)),]
    sync.points$file = rep(edffile, nrow(sync.points))
    
    #check timing between points in one block
    
    sync.delay.edf <- diff(sync.points$time)[c(1,2,3,5,6,7)]/eyeTrackersRate*1000
    
    sync.delay.eeg <- (diff(df.eeg$times)[c(1,2,3,5,6,7)])/samplingRate*1000
    
    cat('\n', 'Synchronization intervals in edf file: ', edffile, '\n' ,sync.delay.edf, 
        '\n', 'Synchronization intervals in eeg file: ',  eegfile, '\n', sync.delay.eeg, '\n')
    
    df.edf <- data.frame(times = sync.points$time, type = sync.points$sent, file = sync.points$file)
    
    write.table(data.frame(c(edffile, sync.delay.edf), c(eegfile, sync.delay.eeg)),
                file = sprintf('sync.diff%s-%s.asc', edffile, eegfile),
                row.names = F,
                col.names = F,
                quote = F)
    
    time.and.type <- rbind(time.and.type, df.edf, df.eeg[1:8,])
  
  write.table(time.and.type, file = sprintf("sync_table-%s-%s.asc", edffile, eegfile), row.names = F, quote = F)
  
}