processFixation <- function(filename, bufSize, actualFixDur)
{
  #this function recovers the algorythm of processing fixations 
  #used online in eyeLines
  #   1. filename -  edf or asc
  #   2. bufSize - arbitrary fixation duration in ms
  #   3. targetSize - fixRegion size (was 55 in Jenia experiment)
  #   4. delayBetweenFixations - delay between two fixations
  #   5. actualFixDur - fixation duartion that really was in an experiment
  #   6. sRate - sampling rate of eyetracker (usually 1000)
  lines <- load.edf(sprintf("%s.edf",filename))
  #taking the third synchronization bit as a referent point
  sRate <- as.numeric((str_filter(lines, '^SAMPLES.+RATE\\t([[:digit:]]+)'))[[1]][[2]])
  
  delayBetweenFixations <- 1000
  targetSize <- 55
  
  bits <- str_filter(lines, '^MSG\\t([[:digit:]]+) +\\!CMD.+2') 
  first_sync <- as.numeric(bits[[3]][[2]])
  
  #now we need to find the actual begining of processing
  #the first sample that came into fixation buffer
  #now there is a special message in edf files fro this purpose
  #but if your file was written befor 6th September 2014
  #we use another technic, though it is not robust enough:
  #finding first fixation end timestamp, and then substract 
  #fixation length from it
  if(sum(grepl('first element of the vector.+',lines[1:10000]))!=0)
  {
    first.element.timestamp <- as.numeric(str_filter(lines[1:10000], 'first element of the vector ([[:digit:]]+)')[[1]][[2]])
  }
  else
  {
    first.element.timestamp <- as.numeric(
      str_filter(
        lines[1:10000], '^MSG\\t([[:digit:]]+).+fixation in region.center.x.+')[[1]][[2]]
    )-actualFixDur
  }
  
  #creating matrix of timestamps, x and y for further processing
  positions <- grepl('^[[:digit:]]+\\t +[[:digit:]]+\\.[[:digit:]]+\\t +[[:digit:]]+\\.[[:digit:]]+\\t', lines)
  pointsOne <- lines[positions, drop = F]
  pointsOne <- pointsOne[-(1:(grep(first.element.timestamp, pointsOne)-1)),drop = F]
  
  points <- t(sapply(pointsOne, function(x) as.numeric(unlist(strsplit(x, "\t"))[1:3]), USE.NAMES = F))
  
  #creating cycling buffer to process fixations  
  bufX <- numeric(length = bufSize)
  bufY <- numeric(length = bufSize)
  answer <- data.frame(start_fix = 0, maxX = 0, minX = 0, maxY = 0, minY = 0, medianX = 0, medianY = 0)
  
  for (i in 1:nrow(points))
  {
    bufPointer = i%%bufSize
    if (bufPointer==0) bufPointer = bufSize
    bufX[bufPointer] = points[i,2]
    bufY[bufPointer] = points[i,3]
    
    
    if(max(bufX)-min(bufX)<=targetSize 
       && max(bufY)-min(bufY)<=targetSize)
    {
      possible.start.fix <- points[i-(bufSize-1),1]-first_sync
      
      if(answer[nrow(answer), 1]+delayBetweenFixations<=possible.start.fix)
      {
        
        if(!(median(bufX)<=answer$maxX[nrow(answer)]
             && median(bufX)>=answer$minX[nrow(answer)]
             && median(bufY)<=answer$maxY[nrow(answer)]
             && median(bufY)>=answer$minY[nrow(answer)]
        ))
        {
          answer[nrow(answer)+1,]<- c(possible.start.fix, max(bufX),
                                      min(bufX),max(bufY),min(bufY),
                                      median(bufX),median(bufY))
        }
        
      }
    } 
  }
  
#   for.eeglab <- data.frame(Latency = answer[,1]/sRate, Type = rep('fixation_start', nrow(answer)))[-1,]
#   write.table(for.eeglab, sprintf('fixLatencies%s.ascii', filename), row.names = F, quote = F)
  answer <- answer[-1,]
  return(answer)
}
