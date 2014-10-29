downsample.vector <- function(vect, q)
{
  v <- data.frame(t = 1:length(vect), vect = vect)
  
  v.withoutNA <- v[complete.cases(v),]
  v.withNA <- v[!complete.cases(v),]
  
  dec.vect <- decimate(v.withoutNA$vect, q)
  
  d <- diff(v.withoutNA$t)
  
  for(i in 1:length(d))
  {
    if(d[i] != 1)
    {
      dec.vect <- append(dec.vect, rep(NA, floor(d[i]/q)), ceiling(i/q))
    }
  }
  
  return(dec.vect)
  
}