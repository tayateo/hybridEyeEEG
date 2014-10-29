decimate.eye <- function(filename, q, old = F, fixation.duration = NULL)
{
  
  #q  - integer factor to downsample by.
  lines <- load.edf(sprintf("%s.edf",filename))
  ans <- extract.samples(lines, old, fixation.duration)
  points <- ans$points
  decimated.points <- data.frame(x = downsample.vector(points$x, q),
                                 y = downsample.vector(points$y, q),
                                 pupil = downsample.vector(points$pupil, q))
  
  #write.table
  
  
}

