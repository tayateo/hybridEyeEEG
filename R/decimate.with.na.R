#'Decimates a vector, that contains NAs 
#'
#'Based on decimate function from the "signal" package.
#'By default, an order 8 Chebyshev type I filter is used or a 30-point FIR filter if ftype is 'fir'. Note that q must be an integer for this rate change method.
#'Makes use of the filtfilt function with all its limitations.
#'
#'@param x signal to be decimated, may contain NAs.
#'@param q integer factor to downsample by.
#'@param n filter order used in the downsampling.
#'@param ftype filter type, "iir" or "fir"
#'
#'@examples
#'decimate.with.na(x, q)
#'
#'@author Nuzhdin Urii, Anastasia Fedorova

decimate.with.na <- function(vect, q, n = if (ftype == "iir") 8 else 30, ftype = "iir")
{
  
  if (q != round(q)) 
    stop("decimate only works with integer q.")
  
  na.indexies <- which(is.na(vect))
  not.na.indexies <- which(!is.na(vect))
  
  x <- vect[!is.na(vect)]
  
  fir <- ftype == "fir"
  if (fir) {
    b <- fir1(n, 1/q)
    y <- fftfilt(b, x)
  }
  else {
    y <- filtfilt(cheby1(n, 0.05, 0.8/q), x)
  }
  
  new.vect <- numeric()
  new.vect[na.indexies] <- rep(NA, length(na.indexies))
  new.vect[not.na.indexies] <- y
  
  new.vect[seq(1, length(new.vect), by = q)]
}

