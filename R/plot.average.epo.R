plot.average.epo <- function(epo.ev1,epo.ev2, res, t1=-500, t2=1000, expname)
{

  
  samplingRate <- res$samplingRate
  
  t = (1:(t2-t1+1) +t1)/samplingRate
  
  
  
  
  meanEpo1 <- colMeans(aperm(epo.ev1, c(3,1,2)))
  meanEpo2 <- colMeans(aperm(epo.ev2, c(3,1,2)))
  
  df <- data.frame(mode = c(rep(res$type1,nrow(meanEpo1)), rep(res$type2,nrow(meanEpo2))),
                   t=c(t,t),
                   P3 = c(meanEpo1[,res$channels[1]], meanEpo2[,res$channels[1]]),
                   P4 = c(meanEpo1[,res$channels[2]], meanEpo2[,res$channels[2]]),
                   O1 = c(meanEpo1[,res$channels[3]], meanEpo2[,res$channels[3]]),
                   O2 = c(meanEpo1[,res$channels[4]], meanEpo2[,res$channels[4]]),
                   HeOGl = c(meanEpo1[,res$channels[5]], meanEpo2[,res$channels[5]]),
                   Vdn = c(meanEpo1[,res$channels[7]], meanEpo2[,res$channels[7]]))
  
  
  plP3 <- plot_ch(df, "P3")  
  plP4 <- plot_ch(df, "P4")  
  plO1 <- plot_ch(df, "O1")  
  plO2 <- plot_ch(df, "O2")
  plHeOGl <- plot_ch(df, "HeOGl", -100, 100)
  plVdn <- plot_ch(df, "Vdn", -60, 60)
  
  
  grid.arrange(plP3, plP4, plO1, plO2,
               plHeOGl, plVdn,               
               ncol=2, heights=0.5, main = sprintf('file %s\nfix.dur = %s ms', expname, res$fixation.duration),
               sub = sprintf('nEpo %s=%.0f\nnEpo %s=%.0f',
                             levels(df$mode)[[2]], dim(epo.ev1)[[3]],
                             levels(df$mode)[[1]], dim(epo.ev2)[[3]]))
  
  pl <- arrangeGrob(plP3, plP4, plO1, plO2,
                    plHeOGl, plVdn,               
                    ncol=2, heights=0.5, main = sprintf('file %s', expname),
                    sub = sprintf('nEpo %s=%.0f\nnEpo %s=%.0f',
                                  levels(df$mode)[[2]], dim(epo.ev1)[[3]],
                                  levels(df$mode)[[1]], dim(epo.ev2)[[3]]))
  ggsave(filename=sprintf('file %s_%s vs %s.jpg', expname, res$type1, res$type2), plot=pl)
}