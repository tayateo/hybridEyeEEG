plot.average.epo <- function(epo.ev1,epo.ev2, res, t1=-500, t2=1000, expname)
{

  
  samplingRate <- res$samplingRate
  
  t = (1:(t2-t1+1) +t1)/samplingRate
  
  
  
  
  meanEpo1 <- colMeans(aperm(epo.ev1, c(3,1,2)))
  meanEpo2 <- colMeans(aperm(epo.ev2, c(3,1,2)))
  
  df <- data.frame(mode = c(rep(res$type1,nrow(meanEpo1)),
                            rep(res$type2,nrow(meanEpo2)),
                            rep("diff", nrow(meanEpo1))),
                   t=c(t,t,t),
                   P0z = c(meanEpo1[,res$channels[1]], meanEpo2[,res$channels[1]],
                           meanEpo1[,res$channels[1]] - meanEpo2[,res$channels[1]]),
                   
                   Oz = c(meanEpo1[,res$channels[2]], meanEpo2[,res$channels[2]],
                          meanEpo1[,res$channels[2]] - meanEpo2[,res$channels[2]]),
                   
                   PO7 = c(meanEpo1[,res$channels[3]], meanEpo2[,res$channels[3]],
                           meanEpo1[,res$channels[3]] - meanEpo2[,res$channels[3]]),
                   
                   PO8 = c(meanEpo1[,res$channels[4]], meanEpo2[,res$channels[4]],
                           meanEpo1[,res$channels[4]] - meanEpo2[,res$channels[4]]),
                   
                   HeOGl = c(meanEpo1[,res$channels[5]], meanEpo2[,res$channels[5]],
                             meanEpo1[,res$channels[5]] - meanEpo2[,res$channels[5]]),
                   
                   Vdn = c(meanEpo1[,res$channels[7]], meanEpo2[,res$channels[7]],
                           meanEpo1[,res$channels[7]] - meanEpo2[,res$channels[7]]))
  

  
  plP0z <- plot_ch(df, "P0z")  
  plOz <- plot_ch(df, "Oz")  
  plPO7 <- plot_ch(df, "PO7")  
  plPO8 <- plot_ch(df, "PO8")
  plHeOGl <- plot_ch(df, "HeOGl", -100, 100)
  plVdn <- plot_ch(df, "Vdn", -60, 60)
  
  
  grid.arrange(plP0z, plOz, plPO7, plPO8,
               plHeOGl, plVdn,               
               ncol=2, heights=0.5, main = sprintf('file %s\nfix.dur = %s ms', expname, res$fixation.duration),
               sub = sprintf('nEpo %s=%.0f\nnEpo %s=%.0f',
                             levels(df$mode)[[2]], dim(epo.ev1)[[3]],
                             levels(df$mode)[[1]], dim(epo.ev2)[[3]]))
  
  pl <- arrangeGrob(plP0z, plOz, plPO7, plPO8,
                    plHeOGl, plVdn,               
                    ncol=2, heights=0.5, main = sprintf('file %s', expname),
                    sub = sprintf('nEpo %s=%.0f\nnEpo %s=%.0f',
                                  levels(df$mode)[[2]], dim(epo.ev1)[[3]],
                                  levels(df$mode)[[1]], dim(epo.ev2)[[3]]))
  ggsave(filename=sprintf('file %s_%s vs %s.jpg', expname, res$type1, res$type2), plot=pl)
}