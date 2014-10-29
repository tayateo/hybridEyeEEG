get.samples <- function(listik, old = FALSE, fixation.duration = NULL)
{  
  
  if(old==F)
  {
    fixation.duration <- as.numeric((str_filter(listik$events$message, '.+fixationDuration\":([[:digit:]]+)'))[[1]][[2]])
  }
  
  points <- listik$samples
  
  
  endblinks <- str_filter(lines, '^EBLINK.+[[:digit:]]+.+\\t([[:digit:]]+)\\t')
  endblinks <- sapply(endblinks, function(i) (as.numeric(i[[2]])))
  
  blinks <- find.blinks(lines, first_sync)
  
  points <- rbind(points, blinks)
  points <- points[order(points$time),]
  
  #set a zero point according to our synchronization timestamp
  points[,1] <- points[,1]- first_sync
  
  ans <- list(points = points, first_sync = first_sync, fixation.duration = fixation.duration)
  
  return(ans)
}

defineEmptyRows <- function()
{
  p <- which(diff(points$time)!=1 )
  starts <- points$time[p]+1
  ends <- points$time[p+1]
}

find.blinks <- function(lines, first_sync)
{
  blinks <- grepl('^[[:digit:]]+\\t +[[:punct:]]+\\t +[[:punct:]]+\\t', lines)
  blinks <- lines[blinks, drop = F]
  blinks <- suppressWarnings(t(sapply(blinks, function(x) as.numeric(unlist(strsplit(x, "\t"))[c(1,2,3)]), USE.NAMES = F)))
  blinks <- blinks[(which(blinks[,1]>=first_sync)),]
  blinks <- data.frame(time = blinks[,1], x = blinks[,2], y = blinks[,3], pupil = rep(NA, nrow(blinks)))
  return(blinks)
}