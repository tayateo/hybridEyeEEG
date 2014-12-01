brownie.extract.file.names <- function(exp.meta)
{
  l <- list()
  
  l$dat <- sprintf("%s.dat", exp.meta$"valid files"[[1]]$name_dat)
  l$edf <- sprintf("%s.edf", exp.meta$"valid files"[[1]]$name_edf)
  
  return(l)
}