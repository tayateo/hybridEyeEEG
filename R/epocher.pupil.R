pupil.epocher <- function(points, lines, str, t1, t2, first_sync, fixation.duration)
{
  #extracting timestamps of all "button press" events from the file
  event <- str_filter(lines, str)
  times <- sapply(event, function(i) as.numeric(i[[2]])- first_sync - fixation.duration);
  
  if(length(times)==0) stop(sprintf('no events of type %s in file', str))
  
  #ceating an empty two-dimensional array that will be filled with pupil diameters
  times<-times[times+t1>=1 & (times+t2 <=nrow(points))]
  
  epo <- array(dim = c(length(times), (fixation.duration-t1+t2+1)))
  
  #here we are taking all pupil diameters from 50 samples before the timestamp
  #of the button press event and 100 samples after it
  for (i in 1:length(times))
  {
    point <- match(times[i],points[,1])
    epo[i,] <- points[(point + t1) : (point + fixation.duration + t2), 2]
  }
  return(epo)
}