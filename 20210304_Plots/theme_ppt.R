


theme_ppt <- function(legend=TRUE){
  theme_bw() +  
    theme(plot.title = element_text(size = 20),  
          strip.text = element_text(size=18),  
          axis.text = element_text(size=15),  
          axis.title = element_text(size=18),  
          legend.position=ifelse(legend,'bottom','none'),  
          legend.text=element_text(size=15),  
          legend.title=element_text(size=18)) 
  
  
}
