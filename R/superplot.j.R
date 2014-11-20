superplot.j <- function(filename, fixation.duration, t1 = -2000, t2 = 2000)
{
    
  lines <- load.edf(sprintf("%s.edf",filename))
  
  ans <- extract.samples(lines, old = TRUE, fixation.duration)
  points <- ans$points[,c(1,2,3)]
  first_sync <- ans$first_sync
  fixation.duration <- ans$fixation.duration
  
  
  speed.of.eye <- c(sqrt( (diff(points[,2])^2) + (diff(points[,3])^2) ), 0)
  points$speed <- speed.of.eye
  write.table(points, file = sprintf("file%s.csv", filename), row.names = F)
  
    
  str= '^MSG\\t([[:digit:]]+).+fixation in region.center.x'

  speedAroundFixation<- t(epocher.speed(str, t1, t2, 
                                        fixation.duration,
                                        points, lines, timestamps = TRUE, first_sync))
  
  
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
                        main = textGrob(sprintf('%s-%.0f', filename, ceiling(i/2)/10),
                                        gp=gpar(fontsize=15)))
      ggsave(filename=sprintf('file%s-%.0f.jpg', filename, ceiling(i/2)/10), plot=pl)
    }
  }
}