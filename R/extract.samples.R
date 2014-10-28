extract.samples <- function(lines, old = FALSE, fixation.duration = NULL)
{
  first_sync <- edf.sync(lines)
  
  if(old==F)
  {
    fixation.duration <- as.numeric((str_filter(lines, '.+fixationDuration\":([[:digit:]]+)'))[[1]][[2]])
  }
  
  positions <- grepl('^[[:digit:]]+\\t +-*[[:digit:]]+\\.-*[[:digit:]]+\\t +-*[[:digit:]]+\\.-*[[:digit:]]+\\t', lines)
  pointsOne <- lines[positions, drop = F]
  
  
  endblinks <- str_filter(lines, '^EBLINK.+[[:digit:]]+.+\\t([[:digit:]]+)\\t')
  endblinks <- sapply(endblinks, function(i) (as.numeric(i[[2]])))
  
  if(length(grep(first_sync, pointsOne))==0)
  {
    first_sync_after_blink <- (endblinks[endblinks > first_sync])[[1]]
    pointsOne <- pointsOne[-(1:(grep(first_sync_after_blink+1, pointsOne)-1)),drop = F]
  }
  else
  {
    pointsOne <- pointsOne[-(1:(grep(first_sync, pointsOne)-1)),drop = F]
  }
  
  
  points <- t(sapply(pointsOne, function(x) as.numeric(unlist(strsplit(x, "\t"))[c(1,2,3,4)]), USE.NAMES = F))
  points <- data.frame(time = points[,1], x = points[,2], y = points[,3], pupil = points[,4])
  
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