meta <- function(exp)
{
  
  result.ev1 <- array(dim = c(1501,62,1))
  result.ev2 <- array(dim = c(1501,62,1))
  i <- 1
  
  for (files in exp)
  {
    res <- new.epocher(files$edffile, files$eegfile, t1=-500, t2=1000, 'btn', 'ball')
    result.ev1 <- abind(res$epo.ev1, result.ev1, along = 3)
    result.ev2 <- abind(res$epo.ev2, result.ev2, along = 3)
  }  
  
}
  