extract.actions <- function(filename, new.block.diff = T)
  
  #sSearchRegion - to foem interval  sSearchRegion +/- saccade latency to search in, in seconds
{
  #with this function you can extract existing messages and timestamps 
  #from an edf file
  
  ans <- load.one.eye(sprintf("%s.edf",filename))
  
  lines <- ans$events$message
  
  sRate <- ans$samplingRate
  
  fixation.duration <- as.numeric((str_filter(lines, '"fixationDuration\":([[:digit:]]+)'))[[1]][[2]])
  quick.fixation.duration <- as.numeric((str_filter(lines, '"quickFixationDuration\":([[:digit:]]+)'))[[1]][[2]])
  
  blockedMove <- str_filter(lines, '.+"blockedMove".+time += ([[:digit:]]+)')
  button.press <- str_filter(lines, '.+"ClickedToUnlock".+time += ([[:digit:]]+)')
  ball.choose <- str_filter(lines, '.+"ballSelect".+time += ([[:digit:]]+)')
  fix.event <- str_filter(lines, '^fixation in region.+time += ([[:digit:]]+)')
  random.block.starts <- str_filter(lines, '.+"random_block_starts".+time += ([[:digit:]]+)')
  random.block.ends <- str_filter(lines, '.+"random_block_ends".+time += ([[:digit:]]+)')
  quick.click <- str_filter(lines, '^quick fixation in region.+time += ([[:digit:]]+)')
  do.click <- str_filter(lines, '^received click.+time += ([[:digit:]]+)')
  
 

  ball.move <-  str_filter(lines, '.+"ballMove".+time += ([[:digit:]]+)')
  ReachedMaximumMovesQuantity <- str_filter(lines, '.+"ReachedMaximumMovesQuantity".+time += ([[:digit:]]+)')
  
  first_sync <- ans$sync_timestamp
  
  #taking timestamps of all events ends
  #here we are taking the third synchronization bit to use
  #it as the reference point in eeg file
  
  quick.click <- extr.num(quick.click, first_sync, quick.fixation.duration, sRate)
  do.click <- unlist(sapply(do.click, function(i) (as.numeric(i[[2]])- first_sync)/sRate))
    
  button.times <- extr.num(button.press, first_sync, fixation.duration, sRate)
  ball.times <- extr.num(ball.choose , first_sync, fixation.duration, sRate)
  fix.times <- extr.num(fix.event , first_sync, fixation.duration, sRate)
  ball.move.times <- extr.num(ball.move , first_sync, fixation.duration, sRate)
  max.reached.times <- unlist(sapply(ReachedMaximumMovesQuantity, function(i) (as.numeric(i[[2]])- first_sync)/sRate))
  blockedMove.times <- extr.num(blockedMove , first_sync, fixation.duration, sRate)
  
  random.block.starts <- sapply(random.block.starts, function(i) (as.numeric(i[[2]])- first_sync)/sRate)
  random.block.ends <- sapply(random.block.ends, function(i) (as.numeric(i[[2]])- first_sync)/sRate)
  
  #SR events
  SRfix.times <- ans$SRstartFix/sRate
  endSac.times <- ans$SRendSacc/sRate
  startSac.times <- ans$SRstartSacc/sRate
  
  
  
  
  for.eeglab <- data.frame(Latency = c(button.times,ball.times,
                                       ball.move.times, max.reached.times, quick.click,
                                       do.click),
                           Type = c(rep("msgbuttonPressed", length(button.times)), 
                                    rep("msgballChosen", length(ball.times)),
                                    rep("msgBallMoved", length(ball.move.times)),
                                    rep("ReachedMax", length(max.reached.times)),
                                    rep("quick.click", length(quick.click)),
                                    rep("do.click", length(do.click))))
  
  for.eeglab <- for.eeglab[order(for.eeglab$Latency),]
  for.eeglab$Type <- sapply(for.eeglab$Type, as.character)
  for (i in 1:(nrow(for.eeglab)-1))
  {
    if (for.eeglab$Type[i] == "msgballChosen" 
        && for.eeglab$Type[i+1] == "msgballChosen" )
    {
      for.eeglab$Type[i] = "errorBallChosen" 
    }
  }
 
  if(new.block.diff==F)
  {
    BoardPositionClickedInBlockedMode <- str_filter(lines, '.+"BoardPositionClickedInBlockedMode".+time += ([[:digit:]]+)')
    clicked.in.blocked.mode <- extr.num(BoardPositionClickedInBlockedMode , first_sync, fixation.duration, sRate)
    if(length(clicked.in.blocked.mode)!=0)
    {
      for.eeglab <- rbind(for.eeglab, data.frame(Latency = clicked.in.blocked.mode, Type = rep("msgClickedInBlockMode", length(clicked.in.blocked.mode))))
    }
  }
  else
  {
    ball_in_blocked <- str_filter(lines, '+"BallClickedInBlockedMode".+time += ([[:digit:]]+)')
    board_in_blocked <- str_filter(lines, '+"BoardClickedInBlockedMode".+time += ([[:digit:]]+)')
    ball_in_blocked <- extr.num( ball_in_blocked , first_sync, fixation.duration, sRate)
    board_in_blocked <- extr.num( board_in_blocked , first_sync, fixation.duration, sRate)
    if(length(ball_in_blocked)!=0)
    {
      for.eeglab <- rbind(for.eeglab, data.frame(Latency = ball_in_blocked, Type = rep("msgBallClickedInBlockedMode", length(ball_in_blocked))))  
    }
    else if(length(board_in_blocked)!=0)
    {
      for.eeglab <- rbind(for.eeglab, data.frame(Latency = board_in_blocked, Type = rep("msgBoardClickedInBlockedMode", length(board_in_blocked))))
    }
  }
  
  if(length(blockedMove.times)!=0)
  {
    for.eeglab <- rbind(for.eeglab, data.frame(Latency = blockedMove.times, Type = rep("msgImpossibleMove", length(blockedMove.times))))
  }
  
  if(length(random.block.starts)!=0)
  {
    for.eeglab <- rbind(for.eeglab, data.frame(Latency = random.block.starts, Type = rep("random.block.starts", length(random.block.starts))))
  }
  if(length(random.block.ends)!=0)
  {
    for.eeglab <- rbind(for.eeglab, data.frame(Latency = random.block.ends, Type = rep("random.block.ends", length(random.block.ends))))
  }

  
  for.eeglab = rbind(for.eeglab, data.frame(Latency = c(SRfix.times, endSac.times, fix.times, startSac.times),
                     Type = c(rep("SRfix", length(SRfix.times)),
                     rep("SRSaccEnd", length(endSac.times)),
                     rep("FixationStart", length(fix.times)),
                     rep("SRsaccStart", length(startSac.times)))))

  for.eeglab <- for.eeglab[order(for.eeglab$Latency),]
  
  for.eeglab <- subset(for.eeglab, Latency >=0)

  #for.eeglab <- extract.saccades(for.eeglab, 0.1)
  
  return(for.eeglab) 
}

exportActionsToMatlab <- function(filename, new.block.diff=T){
  
  dir <- dirname(filename)
  name <- tools::file_path_sans_ext(basename(filename))
  
  
  for.eeglab <- extract.actions(paste0(dir,'/',name), new.block.diff)
  write.table(for.eeglab, sprintf('%s/eventsLatencies%s.ascii', dir, name), row.names = F, quote = F)
}

chooseSacc <- function()
{
  fixes <- subset(temp, Type == "FixationStart")$Latency
  
  needSaccend <- numeric()
  for (i in 1:length(fixes))
  {
    prevSaccEnd <- temp[(which(temp$Latency == fixes[i]))-1,1]
    nextSaccEnd <- temp[(which(temp$Latency == fixes[i]))+1,1]
    diffPrev <- abs(fixes[i] - prevSaccEnd)
    diffNext <- abs(fixes[i] - nextSaccEnd)
    if(diffPrev<diffNext)
    {
      needSaccend[i] <- prevSaccEnd
      print("prev")
    }
    else
    {
      needSaccend[i] <- nextSaccEnd
      print("next")
    }
    
    return(needSaccend)
  }
  
}

extract.saccades <- function(for.eeglab, dur)
{
  saccs <- unlist(subset(for.eeglab, Type == "SRSaccEnd", select = "Latency"), use.names = F)
  msgs <-  unlist(subset(for.eeglab,  Type!="SRfix" & Type != "FixationStart" & Type != "ReachedMax" & Type != "SRSaccEnd", select = "Latency"), use.names = F)
  selected <- numeric()
  
  for (i in 1:length(saccs))
  {
    interval <- seq(saccs[i]-dur,saccs[i]+dur, 0.001)
    if(sum(msgs %in% interval))
    {
      selected[i] <- saccs[i]
    }
  }
  selected <- selected[!is.na(selected)]
  
  for.eeglab <- rbind(for.eeglab, data.frame(Latency = selected, Type = rep("selectedSacc", length(selected))))
  for.eeglab <- for.eeglab[order(for.eeglab$Latency),]
  
  return(for.eeglab)
}

mark.saccades <- function(for.eeglab)
{
  #mark saccades in accordance with ongoing msg
  temp2 <- subset(for.eeglab, Type!="SRSaccEnd" & Type!="SRfix" & Type != "FixationStart" & Type != "ReachedMax")
  tempRest <- subset(for.eeglab, Type=="SRSaccEnd" | Type == "SRfix" | Type == "FixationStart" | Type == "ReachedMax")
  names <- levels(temp2$Type)[temp2$Type]
  
  for(i in (which(names=="selectedSacc")))
  {
    if(grepl('msg', names[i+1]))
    {
      names[i] <- sprintf("%sBefore%s", names[i], gsub("msg", "", names[i+1]))
    }
  }
  temp2$Type = names
  
  for.eeglab <- rbind(temp2, tempRest)
  for.eeglab <- for.eeglab[order(for.eeglab$Latency),]
}

extr.num <- function(extr, first_sync, fixation.duration, sRate)
{
  ans <- sapply(extr, function(i) (as.numeric(i[[2]])- first_sync - fixation.duration)/sRate);
  unlist(ans)
}