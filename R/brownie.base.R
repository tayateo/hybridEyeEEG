global <- new.env()

set.brownie.root <- function(path)
{
  global$brownie.root <- path
}

data.path <- function()
{
  normalizePath(paste(global$brownie.root, "\\data", sep = ''))
}