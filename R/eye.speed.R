#eye.speed

eye.speed <- function(filename, t1 = -1000, t2 = 1000)
{

  # ARGUMENTS:
  # 1. filename is a name of the file you want to process, in quotes and
  #wthout extension: e.g. '23525924'
  #reading lines from converted edf file
  lines <- load.edf(sprintf("%s.edf",filename))
  
  ans <- extract.samples(lines)
  points <- ans[[1]]
  first_sync <- ans[[2]]
  fixation.duration <- ans[[3]]
 
  
  #sqrt( (x(i) - x(i-1))^2 + (y(i) - y(i-1))^2 )
  speed.of.eye <- c(sqrt( (diff(points[,2])^2) + (diff(points[,3])^2) ), 0)
  points$speed <- speed.of.eye
  
  
  #extracting timestamps of all "button press" events from the file
  str.bp <- '^MSG.+"ClickedToUnlock".+time += ([[:digit:]]+)'
  speedAroundButtonPress <- epocher.speed(str.bp, t1, t2, 
                                          first_sync, fixation.duration,
                                          points, lines)
  
  #same for "fixation without intntion" event
  str.ni <- '^MSG.+"BoardPositionClickedInBlockedMode".+time += ([[:digit:]]+)'
  speedNoIntention <- epocher.speed(str.ni, t1, t2, 
                                    first_sync, fixation.duration,
                                    points, lines)
  
  
  #taking mean of all "epochs" (timestamps before and ater each event)
  #and draw them at the same plot
  meanButtonPress <- colMeans(speedAroundButtonPress)
  meanNoIntention <- colMeans(speedNoIntention)
  t <- t1:(fixation.duration+t2)
  
  par(mfrow=c(2,1)) 
  plot(t, meanNoIntention, xlab = 'samples', type = 'l', col='pink', ylab = sprintf('No Intent, epo = %.0f', length(speedNoIntention[ ,1])), main = sprintf('Speed file %s.edf',filename))
  abline(v=0, col = 'green')
  abline(v = fixation.duration, col = 'green')
  plot(t, meanButtonPress, type = 'l', col='blue', xlab = 'samples', ylab = sprintf('Button Pressed, epo = %.0f', length(speedAroundButtonPress[ ,1])))
  abline(v=0, col = 'green')
  abline(v = fixation.duration, col = 'green')
  
}