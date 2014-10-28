compare.800.1200 <- function(edffile800, eegfile800, edffile1200, eegfile1200, t1, t2, durToCompare800, prevSacc)
{  
  
  ans1 <- meta(edffile800, eegfile800, diff = FALSE, t1, t2, durToCompare800, prevSacc)
  ans2 <- meta(edffile1200, eegfile1200, diff = TRUE, t1, t2, durToCompare800, prevSacc)
  
  meanEpo1 <- ans1[[1]]
  meanEpo2 <- ans2[[1]]
  nEpo1 <- ans1[[2]]
  nEpo2 <- ans2[[2]]
  
  samplingRate <- 500
  t = (1:(t2-t1+1) +t1)/samplingRate
  
  
  df <- data.frame(mode = c(rep("actualFixDur=800",nrow(meanEpo1)), rep("actualFixDur=1200",nrow(meanEpo2))),
                   t=c(t,t),
                   P3 = c(meanEpo1[,5], meanEpo2[,5]),
                   P4 = c(meanEpo1[,6], meanEpo2[,6]),
                   O1 = c(meanEpo1[,7], meanEpo2[,7]),
                   O2 = c(meanEpo1[,8], meanEpo2[,8]),
                   HeOGl = c(meanEpo1[,11], meanEpo2[,11]),
                   HeOGr = c(meanEpo1[,12], meanEpo2[,12]),
                   VUp = c(meanEpo1[,13], meanEpo2[,13]),
                   Vdn = c(meanEpo1[,14], meanEpo2[,14]))
  
  
  plP3 <- plot_ch(df, "P3", -15, 15)  
  plP4 <- plot_ch(df, "P4", -15, 15)  
  plO1 <- plot_ch(df, "O1", -15, 15)  
  plO2 <- plot_ch(df, "O2", -15, 15)
  plHeOGl <- plot_ch(df, "HeOGl", -30, 30)
  plHeOGr <- plot_ch(df, "HeOGr", -30, 30)
  plVUp <- plot_ch(df, "VUp", -30, 30)
  plVdn <- plot_ch(df, "Vdn", -30, 30) 
  
  grid.arrange(plP3, plP4, plO1, plO2,
               plHeOGl, plHeOGr, plVUp, plVdn, 
               ncol=2, heights=0.5, main = gsub('S001.+','',eegfile800[[1]]),
               sub = sprintf('nEpo(actualFixDur=800)=%.0f\nnEpo(actualFixDur=1200)=%.0f', nEpo1, nEpo2))
  
  pl <- arrangeGrob(plP3, plP4, plO1, plO2,
                    plHeOGl, plHeOGr, plVUp, plVdn, 
                    ncol=2, heights=0.5, main = gsub('S001.+','',eegfile800[[1]]),
                    sub = sprintf('nEpo(actualFixDur=800)=%.0f\nnEpo(actualFixDur=1200)=%.0f', nEpo1, nEpo2))
  ggsave(filename=sprintf('plot%s.jpg', gsub('S001.+','',eegfile800[[1]])), plot=pl)


  return(df)
}
