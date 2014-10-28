first.line <- function(df, z, endSac.times, t1, fixation.duration)
{
  
  #find saccade starts on the line 
  saccs <- endSac.times[endSac.times %in% df[ , z+1]]
  saccs <- which( (df[ , z+1] %in% saccs) == TRUE )
  saccs <-df$t[saccs]
  
  if(length(saccs)==0)
  {
    dfe <- data.frame()
    line <- ggplot(dfe)+
      geom_point()+
      xlim(min(df$t), max(df$t))+
      ylim(-1,50) +
      theme_bw() +
      theme(axis.text.y=element_blank(),
            axis.ticks.y=element_blank(),
            plot.margin = rep(unit(0,"null"),4),
            panel.margin = unit(0,"null"),
            axis.ticks.length = unit(0,"null"),
            axis.ticks.margin = unit(0,"null"))
    
  }
  else
  {
    line <- ggplot(df, aes(x=t, y=df[ , z]), environment = environment() )+
      geom_line(size = 0.5)+
      annotate('text', x = t1-100, y = 1, label = sprintf('%.1f s', (df[abs(t1), z+1])/1000), size = 2)+
      annotate('point', x = saccs, y = -0.5, colour = "blue") +
      geom_vline(xintercept = c(0, fixation.duration), size = 1, color = "hotpink4")+
      labs(x=NULL, y=NULL)+
      ylim(-1,50) +
      theme_bw() +
      theme(axis.text.y=element_blank(),
            axis.ticks.y=element_blank(),
            plot.margin = rep(unit(0,"null"),4),
            panel.margin = unit(0,"null"),
            axis.ticks.length = unit(0,"null"),
            axis.ticks.margin = unit(0,"null"))  
  }
  
  return(line)
}