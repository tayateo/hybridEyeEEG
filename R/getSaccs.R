getSaccs <- function(lines, timestamps, first_sync,eyeTrackersRate)
{
  endSac <- str_filter(lines, 'ESACC.+ [[:digit:]]+\\t([[:digit:]]+)')
  endSac.times <- sapply(endSac, function(i) as.numeric(i[[2]])- first_sync)
  df <- data.frame(Lat = c(timestamps, endSac.times), Type = c(rep("fixStart", length(timestamps)),
                                                               rep("saccEnd", length(endSac.times))))
  df <- df[order(df$Lat),]
  
  prevSaccEnd <- df[(which(df[,2]== "fixStart"))-1,]
  
  timestamps <- prevSaccEnd$Lat
  
  return(timestamps)
}