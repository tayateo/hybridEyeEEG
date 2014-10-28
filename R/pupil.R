#'Draws averaged epochs for pupil area
#'
#'@param filename name of your edf file without extension
#'@param ev1 event for first plot on figure, should be in quotes
#'@param ev2 event for second plot on figure, should be in quotes 
#'
#'List of possible events to use:
#'
#'fixation - fixation, detected by Eyelines algorythm
#'
#'btn - user pressed the button to unclock the game
#'
#'ball - user selected a ball
#'
#'moveTo - user selected a cell where he wants to move
#'
#'inBlocked - board/ball clicked when the game is in blocked mode (valid only for experiments e202, e203)
#'
#'block_ball - ball clicked when the game is in blocked mode (valid only for experiments e204 and later)
#'
#'block_board - board clicked when the game is in blocked mode (valid only for experiments e204 and later)
#'
#'@param t1 start of epoch according to the start of the event
#'@param t2 end of epoch according to the start of the event
#'
#'@examples
#'pupil.size('23537434', 'btn', 'block_board', -800, 800)  

pupil.size <- function(filename, ev1, ev2, t1 = -1000, t2 = 1000)
{

  type1 = ev1
  
  switch(ev1,
         fixation = {ev1 = '^MSG.+ fixation in region.center.x.+time = +([[:digit:]]+)'},
         btn = {ev1 = '^MSG.+"ClickedToUnlock".+time += ([[:digit:]]+)'},
         ball = {ev1 = '^MSG.+"ballSelect".+time += ([[:digit:]]+)'},
         moveTo = {ev1 = '^MSG.+"ballMove".+time += ([[:digit:]]+)'},
         inBlocked = {ev1 = '^MSG.+"BoardPositionClickedInBlockedMode".+time += ([[:digit:]]+)'},
         block_ball = {ev1 = '^MSG.+"BallClickedInBlockedMode".+time += ([[:digit:]]+)'},
         block_board = {ev1 = '^MSG.+"BoardClickedInBlockedMode".+time += ([[:digit:]]+)'})
  
  
  type2 = ev2
  
  switch(ev2,
         fixation = {ev2 = '^MSG.+ fixation in region.center.x.+time = +([[:digit:]]+)'},
         btn = {ev2 = '^MSG.+"ClickedToUnlock".+time += ([[:digit:]]+)'},
         ball = {ev2 = '^MSG.+"ballSelect".+time += ([[:digit:]]+)'},
         moveTo = {ev2 = '^MSG.+"ballMove".+time += ([[:digit:]]+)'},
         inBlocked = {ev2 = '^MSG.+"BoardPositionClickedInBlockedMode".+time += ([[:digit:]]+)'},
         block_ball = {ev2 = '^MSG.+"BallClickedInBlockedMode".+time += ([[:digit:]]+)'},
         block_board = {ev2 = '^MSG.+"BoardClickedInBlockedMode".+time += ([[:digit:]]+)'})
  
  
  lines <- load.edf(sprintf("%s.edf",filename))
  
  ans <- extract.samples(lines)
  points <- ans[[1]][, c(1,2)]
  first_sync <- ans[[2]]
  fixation.duration <- ans[[3]]
  
  
  #taking mean of all "epochs" (timestamps before and ater each event)
  #and draw them at the same plot
  
  pupilAroundEv1 <- pupil.epocher(points, lines, ev1,
                                   t1,t2, first_sync,
                                   fixation.duration)
  
  pupilAroundEv2 <- pupil.epocher(points, lines, ev2,
                                          t1,t2, first_sync,
                                          fixation.duration)
  
  meanEv1 <- colMeans(pupilAroundEv1, na.rm = T)
  meanEv2 <- colMeans(pupilAroundEv2, na.rm = T)
  t <- t1:(fixation.duration+t2)
  
  par(mfrow=c(2,1)) 
  plot(t, meanEv1, xlab = 'samples', type = 'l', col='pink', ylab = sprintf('%s', type1),
       main = sprintf('file %s.edf',filename))
  abline(v=0, col = 'green')
  abline(v = fixation.duration, col = 'green')
  plot(t, meanEv2, type = 'l', col='blue', xlab = 'samples', ylab = sprintf('%s', type2))
  abline(v=0, col = 'green')
  abline(v = fixation.duration, col = 'green')
  
}