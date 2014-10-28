plot_SR <- function(edffile800, eegfile800, edffile1200, eegfile1200, t1, t2)
{
  

  setwd("D:/tmp/crowd")
  
  ans <- meta.SR(edffile800, eegfile800, t1, t2)
  
  meanEpo <- ans[[1]]
  nEpo <- ans[[2]]
  
  samplingRate <- 500
  t = (1:(t2-t1+1) +t1)/samplingRate
  
  
  df <- data.frame(mode = c(rep("SRfix",nrow(meanEpo))),
                   t=t,
                   P3 = c(meanEpo[,5]),
                   P4 = c(meanEpo[,6]),
                   O1 = c(meanEpo[,7]),
                   O2 = c(meanEpo[,8]),
                   HeOGl = c(meanEpo[,11]),
                   HeOGr = c(meanEpo[,12]),
                   VUp = c(meanEpo[,13]),
                   Vdn = c(meanEpo[,14]))
  
  
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
               sub = sprintf('nEpo=%.0f', nEpo))
  
  
  return(df)
}