load.edf <- function(filename)
{
  ascfile <- sprintf("%sasc",gsub("edf","",filename,ignore.case = T))
  
  #convert edf to asc if it was not done yet
  if (!file.exists(ascfile))
  {
    
    #here you may need to put your own path to edf converter file in brackets after shQuote
    #won't work on 32bit systems
    suppressWarnings(system(sprintf("%s %s",
                                    shQuote("edfconverter/Example/edf2asc"),
                                    filename), 
                            ignore.stdout = T, ignore.stderr = T))
  }
  
  #read asc file with readLines functionstr
  lines <- readLines(ascfile)
}