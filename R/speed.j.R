speed.j <- function(filename, fixation.duration, t1 = -1000, t2 = 1000)
{
  lines <- load.edf(sprintf("%s.edf",filename))
  sRate <- as.numeric((str_filter(lines, '^SAMPLES.+RATE\\t([[:digit:]]+)'))[[1]][[2]])
  bits <- str_filter(lines, '^MSG\\t([[:digit:]]+) +\\!CMD.+2') 
  first_sync <- as.numeric(bits[[3]][[2]])
  
  positions <- grepl('^[[:digit:]]+\\t +[[:digit:]]+\\.[[:digit:]]+\\t +[[:digit:]]+\\.[[:digit:]]+\\t', lines)
  pointsOne <- lines[positions, drop = F]
  pointsOne <- pointsOne[-(1:(grep(first_sync, pointsOne)-1)),drop = F]
  points <- t(sapply(pointsOne, function(x) as.numeric(unlist(strsplit(x, "\t"))[c(1,2,3)]), USE.NAMES = F))
  
  points <- data.frame(time = points[,1], x = points[,2], y = points[,3])
  
  
  speed.of.eye <- c(sqrt( (diff(points[,2])^2) + (diff(points[,3])^2) ), 0)
  points$speed <- speed.of.eye
  
  #set a zero point according to our synchronization timestamp
  points[,1] <- points[,1]- first_sync
  
  str <- '^MSG\\t(\\d+) fixation in region.center.x.+'
  speedAroundFixation<- epocher.speed(str, t1, t2, 
                                          first_sync, fixation.duration,
                                          points, lines)
 
  meanEpo <- colMeans(speedAroundFixation)
  
  t <- t1:(fixation.duration+t2)
  plot(t, meanEpo, xlab = 'samples', type = 'l', col='pink', ylab = sprintf('No Intent, epo = %.0f', length(speedAroundFixation[ ,1])), main = sprintf('Speed file %s.edf',filename))
  abline(v=0, col = 'green')
  abline(v = fixation.duration, col = 'green')
}