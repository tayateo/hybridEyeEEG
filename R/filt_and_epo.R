filt.and.epo <- function(signal, samplingRate, timestamps, t1 = -1000, t2 = 1000)
{
  
  #for butter
#   f1 = 0.1  
#   f2 = 70
#   filtorder = 2
#   
#   bp <- butter(filtorder, c(f1/samplingRate*2, f2/samplingRate*2 ), 'pass')
#   notch <- butter(filtorder, c(49/samplingRate*2, 51/samplingRate*2), 'stop')
#   
#   for(i in 1:ncol(signal))
#   {
#     signal[i, ] <- (filtfilt(bp, signal[i, ]))
#     signal[i, ] <- (filtfilt(notch,signal[i, ]))
#   }
  
  k <- floor(timestamps/1000*samplingRate)
  
  epoL <- t2-t1+1
  
  
  k<-k[k+t1>=1 & (k+t2 <=nrow(signal))]
  
  epo <-array(dim = c(epoL, ncol(signal),length(k)))
  
  for(i in 1:length(k))
  {
    epo[ , , i] <- signal[ (k[i]+t1):(k[i]+t2), ]
  }
  
  return(epo)
  
}