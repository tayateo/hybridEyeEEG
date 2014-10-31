decimate.eye <- function(filename, q, old = F, fixation.duration = NULL)
{
  
  #q  - integer factor to downsample by.

  ans <- load.one.eye(filename)
  points <- ans$samples
  decimated.points <- data.frame(x = decimate.with.na(points$x, q),
                                 y = decimate.with.na(points$y, q),
                                 pupil = decimate.with.na(points$pupil, q))
  
  #write.table
  
  
}

