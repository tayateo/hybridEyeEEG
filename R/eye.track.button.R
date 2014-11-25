eye.track.button <- function(filename, t1, t2)
{
  ans <- load.one.eye(sprintf("%s.edf",filename))
  eyeTrackersRate <- ans$samplingRate
  lines <- ans$events$message
  first_sync <- ans$sync_timestamp
  fixation.duration <- as.numeric((str_filter(lines, '.+fixationDuration\":([[:digit:]]+)'))[[1]][[2]])
  
  
  if(eyeTrackersRate == 1000)
  {
    points <- decimate.eye(filename, 2)
    devisor <- 2
  }
  else
  {
    points <- ans$samples
    dvisor <- 1
  }
  
  button.press <- str_filter(lines, '.+"ClickedToUnlock".+time += ([[:digit:]]+)')
  button.times <- sapply(button.press, function(i) (as.numeric(i[[2]])- first_sync - fixation.duration)/devisor)
  
  
  epoL <- t2-t1+1
  xs <- array(dim = c( length(button.times), epoL ) )
  ys <- array(dim = c( length(button.times), epoL ) )
  
  for(i in 1:length(button.times))
  {
    xs[ i, ] <- as.numeric(points$x[ (button.times[i]+t1):(button.times[i]+t2) ])
    ys[ i, ] <- as.numeric(points$y[ (button.times[i]+t1):(button.times[i]+t2) ])
    
  }
  
  meanX <- colMeans(xs, na.rm = T)
  meanY <- colMeans(ys, na.rm = T)
  
  plot(meanX, meanY)
  draw.circle(550+47,495+47,47/2)
  rect(550, 495+90, 550+90, 495)

}



circleFun <- function(center = c(0,0),diameter = 1, npoints = 100){
  r = diameter / 2
  tt <- seq(0,2*pi,length.out = npoints)
  xx <- center[1] + r * cos(tt)
  yy <- center[2] + r * sin(tt)
  return(data.frame(x = xx, y = yy))
}