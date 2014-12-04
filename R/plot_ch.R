plot_ch <- function(df, name, ylimMin = -20, ylimMax = 20, diff = F)
{
  
  pl <- ggplot(df, aes_string(x="t", y=name, colour="mode"))+
    geom_line()+
    theme_classic()+
    ylim(c(ylimMin,ylimMax))+
    theme(legend.position="top")+
    geom_vline(xintercept = 0, size = 1, color = "hotpink4")+
    labs(x = "", y = name)+
    scale_x_continuous(expand = c(0,0))
  
  return(pl)
}