#'
#'Cuts eeg signal bt third sync point in the last channel
#'
cut.eeg <- function(data)
  
{
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
  
  return(newsi)
}