meta <- function(exp, type1 = 'btn', type2 = 'ball', t1=-500, t2=1000)
{
  
  res <- new.epocher(exp[[1]]$edffile, exp[[1]]$eegfile, t1, t2, type1, type2)
  
  result.ev1 <- res$epo.ev1
  result.ev2 <- res$epo.ev2
  
  for (files in exp[-1])
  {
    res <- new.epocher(files$edffile, files$eegfile, t1, t2, type1, type2)
    result.ev1 <- abind(res$epo.ev1, result.ev1, along = 3)
    result.ev2 <- abind(res$epo.ev2, result.ev2, along = 3)
  }  
 
  expname <- gsub('S001R[[:digit:]]+', '', exp[[1]]$eegfile)
  
  plot.average.epo(result.ev1, result.ev2,
                   res, t1, t2, expname)
  
  
}
  

