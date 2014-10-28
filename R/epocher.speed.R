epocher.speed <- function(string, t1, t2, first_sync, fixation.duration, points, lines, timestamps = F)
{
  #extracting timestamps of all "button press" events from the file
  event <- str_filter(lines, string)
  times <- sapply(event, function(i) as.numeric(i[[2]])- first_sync - fixation.duration);
  
  #ceating an empty two-dimensional array that will be filled with pupil diameters
  
  times <- which(points$time %in% times)
  times <- times[times+t1>=1 & (times+t2 <=nrow(points))]
  
  epo <- array(dim = c(length(times)*2, (fixation.duration-t1+t2+1)))
  
  #here we are taking all pupil diameters from t1 samples before timestamp 
  #and t2 samples after it
  if(timestamps == T)
  {
    for(i in seq(1, length(times)*2,2))
    {
      point <- times[ceiling(i/2)]
      epo[i,] <- points[(point + t1) : (point + fixation.duration + t2), 4]
      epo[i+1,] <- points[(point + t1) : (point + fixation.duration + t2), 1]
    }
  }
  else
  {
    for (i in 1:length(times))
    {
      point <- match(times[i],points[,1])
      epo[i,] <- points[(point + t1) : (point + fixation.duration + t2), 4]
    }
  }

  return(epo)
}