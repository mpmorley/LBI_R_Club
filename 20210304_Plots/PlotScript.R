############################################################################
#  2021 R club MPM Patchwork and ggplot2 for PPT and AI
# 
#
############################################################################
library(ggsci)
library(patchwork)
library(tidyverse)

plotHt <- 5.75
plotWd <- 9


ggplot(iris, aes(x=Sepal.Length,y=Sepal.Width,color=Species)) + 
  geom_point()

ggplot(iris, aes(x=Sepal.Length,y=Sepal.Width,color=Species)) + 
  geom_point() + 
  scale_color_aaas() +
  theme_bw() +  
  theme(plot.title = element_text(size = 20),  
        strip.text = element_text(size=18),  
        axis.text = element_text(size=15),  
        axis.title = element_text(size=18),  
        legend.position="bottom",  
        legend.text=element_text(size=15),  
        legend.title=element_text(size=18)) 





  
  

# Make a Function ---------------------------------------------------------

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



ggplot(iris, aes(x=Sepal.Length,y=Sepal.Width,color=Species)) + 
  geom_point() + 
  scale_color_aaas() +
  theme_ppt()

ggplot(iris, aes(x=Sepal.Length,y=Sepal.Width,color=Species)) + 
  geom_point() +
  scale_color_aaas() +
  theme_ppt(legend = F)



#We can also set the theme for all the plots. 

plot1 <- ggplot(iris, aes(x=Sepal.Length,y=Sepal.Width,color=Species)) + 
  scale_color_aaas() +
  geom_point() + 
  theme_ppt()

plot1

ggsave("plot1.png", plot, height=plotHt, width=plotWd)

#or with ggsave it save the current plot if we don't give it one. 

ggsave("plot1.png", height=plotHt, width=plotWd)


# Make a 2nd slide with 2 plots with patchwork  -------------------------------------------
#Notice I'm not applying the theme yet or aaas color 

plot2 <- ggplot(iris, aes(y=Sepal.Length,x=Species,color=Species)) + 
  geom_jitter(width = .2) 
plot2


plot3 <- ggplot(iris, aes(y=Sepal.Length,x=Species)) + 
  geom_boxplot(alpha=0) + 
  geom_jitter(aes(color=Species),width = .2) 
plot3

str(plot2)
#Let's add 2 plot together using patchwork. 

plot2 + plot3

#Let's save it as a patchwork object now, pw is patchwork not ggplot2

pw <- plot2 + plot3

#Let's see what kind of object pw is?
str(pw)

#Let's add some annotation 

pw + plot_annotation(tag_levels='A') 


pw + plot_annotation(tag_levels='1') 


pw + plot_annotation(tag_levels='I') 



#Let's collect the legends into one. 
pw + plot_layout(guides='collect') 


#Let's add our theme to all the plots, we do with the 
# Patchwork & operator. 

pw <- pw + plot_annotation(tag_levels='A') + plot_layout(guides='collect') &
  scale_color_aaas() &
  theme_ppt() 
pw
ggsave("plot2.png",pw, height=plotHt, width=plotWd)



# Save Plots for AI -------------------------------------------------------

#Let's be naive and just just size
plot3 + theme_bw()
ggsave("plot3_2x2.png", height=2, width=2)

#More res?
plot3 + theme_bw()
ggsave("plot3_2x2_hires.png", height=2, width=2,dpi=600)

#We have to change point size and also relative font size
ggplot(iris, aes(y=Sepal.Length,x=Species)) + 
  geom_boxplot(alpha=0) + 
  geom_jitter(size=.25,aes(color=Species),width = .2) + 
  scale_color_aaas() +
  theme_bw(9)
ggsave("plot3_2x2_ptsize.png", height=2, width=2)


# For most plots we really need vector based files, such as PDF or SVG


ggplot(iris, aes(y=Sepal.Length,x=Species)) + 
  geom_boxplot(alpha=0) + 
  geom_jitter(size=.25,aes(color=Species),width = .2) + 
  scale_color_aaas() +
  theme_bw(9)

ggsave("plot3_2x2_ptsize.pdf", height=2, width=2)


#Another trick is to save twice as large and resize in AI, also have to now bump up hte base size 
plot3 + theme_bw(14)
ggsave("plot3_4x4.pdf", height=4, width=4)

#Finally get rid of the legend
plot3 + theme_bw(14) + theme(legend.position = 'none')
ggsave("plot3_4x4_nolegend.pdf", height=4, width=4)



