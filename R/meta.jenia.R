meta <- function(edffile, eegfile, diff, t1, t2, durToCompare800, prevSacc)
{
  #here we count epochs for all files and bind them together to make an average epoch
  
  res <- list()
  
  cl <- makeCluster(2)
  registerDoParallel(cl)
  
  if(length(edffile)==1)
  {
    res <- list(epocher.jenia(edffile, eegfile,  diff, t1, t2, durToCompare800, prevSacc)[[1]])
  }
  else
  {  
    res <- foreach(i=1:length(edffile), .combine=c) %dopar% {
      library(hybridEyeEEG, quietly = T)
      list(epocher.jenia(edffile[i], eegfile[i],  diff, t1, t2, durToCompare800, prevSacc)[[1]])
    }
  }
  
  stopCluster(cl)
  
  
  all.epos <- abind(res, along = 3)
  
  
  meanEpo <- colMeans(aperm(all.epos, c(3,1,2)))
  nEpos <- dim(all.epos)[3]
  
  ans <- list(meanEpo <- meanEpo, nEpos <- nEpos)
  
  return(ans)
  
}