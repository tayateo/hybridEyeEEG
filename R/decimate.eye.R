#' Decimates samples from EDF file
#' 
#' @param filename - name of a file with extension
#' @param q - integer factor to downsample by. E.g. if your file's sampling rate is 1000 Hz and you want to downsample it to 500 Hz, q should equal "2".
#' @param na.code - a value to replace NAs in a table. NAs values in data stand for blinks.
#' 
#' @examples
#' decimate.eye('23566333.edf', 2)
#' 
#' @return
#' Writes a table to a working derictory in ascii format compatible with EEGlab


decimate.eye <- function(filename, q, na.code = "NaN")
{
  
  filename <- sprintf("%s.edf",filename)
  
  #q  - integer factor to downsample by.

  ans <- load.one.eye(filename)
  points <- ans$samples
  
  na.indexies <- which(is.na(points$x))
  points$pupil[na.indexies] <- NA

  
  decimated.points <- data.frame(x = decimate.with.na(points$x, q),
                                 y = decimate.with.na(points$y, q),
                                 pupil = decimate.with.na(points$pupil, q))
    
  if(!is.na(na.code))
  {
    decimated.points[is.na(decimated.points)] <- na.code
  }
  
  #write.table - need to get details about the format
  
  write.table(decimated.points, sprintf('decimated_samples%s.ascii', filename), row.names = F, quote = F)
  
  cat("\n", filename, "decimation done", "\n")
  
  return(decimated.points)
}

