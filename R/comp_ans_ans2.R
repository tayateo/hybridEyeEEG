comp_ans_ans2<- function(ans, ans2)
{
  `%nin%` <- Negate(`%in%`)
  
  timestamps <- numeric()
  
  for (i in 1:nrow(ans))
  {
    interv <- (ans$start_fix[i]-100):(ans$start_fix[i]+100)
    if ( all(interv %nin% ans2$start_fix) )
    {
      timestamps[i] <- ans$start_fix[i]
    }
  }
  
  timestamps <- na.omit(timestamps)
  
  return(timestamps)
}