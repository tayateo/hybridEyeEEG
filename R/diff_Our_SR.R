diffOur_vs_SR <- function(filename)
{
  
  sRate <- as.numeric((str_filter(load.edf(sprintf("%s.edf",filename)), '^SAMPLES.+RATE\\t([[:digit:]]+)'))[[1]][[2]])
  ans <- extract.actions(filename)
  ans$Latency <- ans$Latency*sRate
  
  ans <- subset(ans, Type == "SRfix" | Type == "FixationStart")
    
  ourFixStarts <- ans[which(ans[,2]=="FixationStart"),1]
  SRfixStarts <- ans[(which(ans[,2]=="FixationStart"))-1,1]
  
  d.f <- data.frame(latencies = c(ourFixStarts, SRfixStarts),
                          type = c(rep("our", length(ourFixStarts)),
                                   rep("SR", length(SRfixStarts))))
  
  d.f <- d.f[order(d.f$latencies),]
  
  diffFix <- numeric(length = length(SRfixStarts))
  for (i in 1:length(SRfixStarts))
  {
    diffFix[i] <- ourFixStarts[i]-SRfixStarts[i]  
  }
  
  hist(diffFix, xlab = "milliseconds", main = sprintf("Difference between SR and Our fixations\nin file %s.edf", filename))

  return(diffFix)

}