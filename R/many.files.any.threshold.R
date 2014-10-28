#' Ascii for eeglab with comparison for many files simultaniously
#' 
#' Creates ascii file for eeglab with fixation's starts 
#' that were proceessed in offline simulation and possibly compared with two fixation thresholds
#' also puts all saccades ends in ascii file
#' 
#' If you need to compare only one file use arb.thresh function
#' 
#' @param actualFixDur fixation threshold that was used in experiment
#' @param lowerThresh  lower threshold to use in comparison
#' @param higherThresh higher threshold to use in experiment
#' @param files list of files to be processed
#' @examples
#' many.files.any.threshold(800, 500, 1000, j.files800)
#' 
#' #this will take all files where threshold was 800 and compare each of them 
#' #as if the threshold was between 500 and 1000
#' 
many.files.any.threshold <- function(actualFixDur, lowerThresh, higherThresh, files)
{
  for (file in files)
  {
    any.thresh(file, lowerThresh, higherThresh, actualFixDur)
  }
}