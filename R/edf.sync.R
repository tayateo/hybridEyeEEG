edf.sync <- function(lines)
{
  bits <- str_filter(lines, '^MSG\\t([[:digit:]]+) +\\!CMD.+2') 
  first_sync <- as.numeric(bits[[3]][[2]])
  return(first_sync)
}
