speed.superposition <- function(filename, t1 = -2000, t2 = 2000)
{
  
  lines <- load.edf(sprintf("%s.edf",filename))
    
  ans <- extract.samples(lines)
  points <- ans[[1]][,c(1,2,3)]
  first_sync <- ans[[2]]
  fixation.duration <- ans[[3]]
  
  
  
  speed.of.eye <- c(sqrt( (diff(points[,2])^2) + (diff(points[,3])^2) ), 0)
  points$speed <- speed.of.eye
  
  
  str <- '^MSG.+ fixation in region.center.x.+time = +([[:digit:]]+)'
  speedAroundFixation<- t(epocher.speed(str, t1, t2, 
                                      first_sync, fixation.duration,
                                      points, lines, timestamps = TRUE))
    
  
  t <- t1:(fixation.duration+t2)
  
  df <- data.frame(t = t, speed = as.vector(speedAroundFixation[, 1:ncol(speedAroundFixation)]),
                   nClicks = rep(1:dim(speedAroundFixation)[[2]], each=dim(speedAroundFixation)[[1]]))
  
  #split df to small data.frames nrow = 20
  
  n <- 20
  k <- 1
  for (i in 1:((as.integer(dim(speedAroundFixation)[[2]]/n))+1))
  {
    
    if (i == ((as.integer(dim(speedAroundFixation)[[2]]/n))+1))
    {
      
      nn <- dim(speedAroundFixation)[[2]]%%n
      
      nextEndingRow <- nrow(df)
      
      dfmod <- df[(k+1):nextEndingRow, ]
      
      for (ii in (((i*n)-n)+1):(nextEndingRow/dim(speedAroundFixation)[[1]]))
      {
        dfmod$speed[dfmod$nClicks==ii] <- dfmod$speed[dfmod$nClicks==ii]+(10*ii)
      }
      
    }
    else
    {
      nn <- n
      nextEndingRow <- (n*dim(speedAroundFixation)[[1]])*i
      
      if (k == 1) dfmod <- df[k:nextEndingRow, ] else  dfmod <- df[(k+1):nextEndingRow, ]
      
      for (ii in ((i*n)-n):(i*n))
      {
        if(ii==0)ii=1
        
        dfmod$speed[dfmod$nClicks==ii] <- dfmod$speed[dfmod$nClicks==ii]+(10*ii)
        
      }
    }
                    
    pl <- ggplot(dfmod, aes(x=t, y=speed, group = nClicks))+
      geom_line()+
      theme(legend.position="none")+
      labs(title = sprintf('file %s.edf\nN clicks = %.0f', filename, nn))+
      geom_vline(xintercept = c(0, fixation.duration), size = 1, color = "hotpink4")+
      theme(axis.ticks.y = element_blank(), axis.text.y = element_blank())+
      ylab("")
        
    if (i<10)
    {
      ggsave(filename=sprintf('file%sNo0%s.jpg', filename, i), plot=pl)
      
    }
    else
    {
      ggsave(filename=sprintf('file%sNo%s.jpg', filename, i), plot=pl)
      
    }
    
    k <- nextEndingRow

  }
  
}