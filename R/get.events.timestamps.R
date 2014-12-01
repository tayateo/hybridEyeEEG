get.events.timestamps <- function(ev, lines, fisrt_sync, get_ev_starts = FALSE, fixation.duration = NULL)
{
    
  switch(ev,
         fixation = {ev = 'fixation in region.center.x.+time = +([[:digit:]]+)'},
         btn = {ev = '"ClickedToUnlock".+time += ([[:digit:]]+)'},
         ball = {ev = '"ballSelect".+time += ([[:digit:]]+)'},
         moveTo = {ev = '"ballMove".+time += ([[:digit:]]+)'},
         inBlocked = {ev = '"BoardPositionClickedInBlockedMode".+time += ([[:digit:]]+)'},
         block_ball = {ev = '"BallClickedInBlockedMode".+time += ([[:digit:]]+)'},
         block_board = {ev = '"BoardClickedInBlockedMode".+time += ([[:digit:]]+)'})
  
  events <- str_filter(lines, ev)
  
  times <- sapply(events, function(i) (as.numeric(i[[2]]) - first_sync))
  
  if(get_ev_starts == T)
  {
    times <- times - fixation.duration
  }
  
  return(times)
  
}