str_filter <- function(list, regexp)
{  
  short <- list[str_detect(list, regexp)]
  str_match_all(short, regexp)
}