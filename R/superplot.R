superplot <- function(filename, str, t1 = -2000, t2 = 2000)
{
  
#   str might be 
#   'fixation'
#   'btn'
#   'ball'
#   'moveTo'
#   'inBlocked'
#   'inBlocked-ball'
#   'inBlocked - btn'
  
  
  ans <- load.one.eye(sprintf("%s.edf",filename))
  
  points <- ans$samples
  lines <- ans$events$message
  sync_point <- ans$sync_timestamp
  fixation.duration <- as.numeric((str_filter(lines, 'fixationDuration.+:([[:digit:]]+)'))[[1]][[2]])
  
  
  speed.of.eye <- c(sqrt( (diff(points[,2])^2) + (diff(points[,3])^2) ), 0)
  points$speed <- speed.of.eye
  write.table(points, file = sprintf("file%s.csv", filename), row.names = F)
  
  
  type = str
  
  switch(str,
         fixation = {str = 'fixation in region.center.x.+time = +([[:digit:]]+)'},
         btn = {str = '"ClickedToUnlock".+time += ([[:digit:]]+)'},
         ball = {str = '"ballSelect".+time += ([[:digit:]]+)'},
         moveTo = {str = '"ballMove".+time += ([[:digit:]]+)'},
         inBlocked = {str = '"BoardPositionClickedInBlockedMode".+time += ([[:digit:]]+)'},
         block_ball = {str = '"BallClickedInBlockedMode".+time += ([[:digit:]]+)'},
         block_board = {str = '"BoardClickedInBlockedMode".+time += ([[:digit:]]+)'})

  speedAroundFixation <- (epocher.speed(str, t1, t2, 
                                      fixation.duration,
                                      points, lines, timestamps = TRUE, 
                                      sync_point))
  
  if(is.null(speedAroundFixation))
  {
    return
  }
  else
  {
    speedAroundFixation <- t( speedAroundFixation )
  }
  
  
  
  t <- t1:(fixation.duration+t2)
  
  df <- as.data.frame(speedAroundFixation)

  
  endSac <- str_filter(lines, 'ESACC.+ [[:digit:]]+\\t([[:digit:]]+)')
  endSac.times <- sapply(endSac, function(i) (as.numeric(i[[2]])- first_sync))
  
  steps <- 10
  l <- list()
  
  x <- rep(NA, nrow(df))
  
  if ((ncol(df)/2)==0)
  {
    return()
  }
  
  if((ncol(df)/2)<steps)
  {
    cols.left <- steps - (ncol(df)/2)

    for (i in 1:(cols.left*2))
    {
      df <- cbind(df, x)
    }
  }
  
  if(ncol(df)%%steps!=0)
  {
   
    for(i in 1:(steps - ncol(df)%%steps))
    {
      df <- cbind(df, x)
    }
  }
    
  df$t <- t
  
  for (i in seq(1, (ncol(df)-1), 2))
  {
     k <- ceiling(i/2) %% steps
     
     if(k == 0) k = steps
     
     if(k == 1)
     {
        l[[k]] = first.line(df, i, endSac.times, t1, fixation.duration)
     }
     else
     {
        l[[k]] = one.line(df, i, endSac.times, t1, fixation.duration)
     }
     
          
     if (k==steps)
     {
       pl <- arrangeGrob(l[[10]], l[[9]], l[[8]], l[[7]], l[[6]],
                   l[[5]], l[[4]], l[[3]], l[[2]], l[[1]], ncol = 1 ,
                   main = textGrob(sprintf('%s-%.0f\ntype %s', filename, ceiling(i/2)/10, type),
                   gp=gpar(fontsize=15)))
       ggsave(filename=sprintf('file%s-%s-%.0f.jpg', filename, type, ceiling(i/2)/10), plot=pl)
     }
   }
}