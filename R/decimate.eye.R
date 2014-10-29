decimate.eye <- function(filename, q, old = F, fixation.duration = NULL)
{
  
  #q  - integer factor to downsample by.

  ans <- load.one.eye(filename)
  points <- ans$samples
  decimated.points <- data.frame(x = downsample.vector(points$x, q),
                                 y = downsample.vector(points$y, q),
                                 pupil = downsample.vector(points$pupil, q))
  
  #write.table
  
  
}

