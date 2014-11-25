new.plot.epo <- function(edffile, eegfile, ev1, ev2, t1=-500, t2=1000)
{

  res <- new.epocher(edffile, eegfile, t1, t2, ev1, ev2)
  
  epo.ev1 <- res$epo.ev1
  epo.ev2 <- res$epo.ev2
  
  samplingRate <- res$samplingRate
  
  t = (1:(t2-t1+1) +t1)/samplingRate;
  
  expname <- gsub('S001R[[:digit:]]+', '', eegfile)
  
  plot.average.epo(epo.ev1, epo.ev2,
                   res, t1, t2, expname)
  
  
}
