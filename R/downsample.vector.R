downsample.vector <- function(vect, q)
{
  v <- data.frame(t = 1:length(vect), vect = vect)
  
  v.withoutNA <- v[complete.cases(v),]
  v.withNA <- v[!complete.cases(v),]
  
  dec.vect <- decimate(v.withoutNA$vect, q)
  

  
}