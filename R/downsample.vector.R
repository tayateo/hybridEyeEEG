downsample.vector <- function(vect, q)
{
  cut.begin <- 0
  
  if(is.na(vect[1]))
  {
    for (i in 1:length(vect))
    {
      if(is.na(vect[i]))
      {
        cut.begin <- cut.begin + 1
      }
      else
      {
        break
      }
    }
  }
  
  if(cut.begin!=0)
  {
    vect <- vect[-cut.begin]
  }
  
  v <- data.frame(t = 1:length(vect), vect = vect)
  
  v.withoutNA <- v[complete.cases(v),]
  v.withNA <- v[!complete.cases(v),]
  
  dec.vect <- decimate(v.withoutNA$vect, q)
  
  
  d <- diff(v.withoutNA$t)
  
  
  
  for(i in 1:length(d))
  {
    if(d[i] != 1)
    {
      if(d[i]==2)
      {
        if (i == 1)
        {
          dec.vect <- append(dec.vect, NA, 0)
        }
        else
        {
          dec.vect <- append(dec.vect, NA, i)
        }
      }
      else
      {
        dec.vect <- append(dec.vect, rep(NA, floor((d[i]-1)/q)), ceiling(i/q))
      }
    }
  }
  
  
  
  ##append NAs to begining or to the end of vector 
  if(is.na(vect[length(vect)]))
  {
    z <- 0
    for (i in length(vect):1)
    {
      if(is.na(vect[i]))
      {
        z <- z + 1
      }
      else
      {
        break
      }
    }
    
    z <- ceiling(z/q)
    
    dec.vect <- append(dec.vect, rep(NA, z), length(dec.vect))
  }
  
  
  return(dec.vect)
  
}