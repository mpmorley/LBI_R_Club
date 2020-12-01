library(tidyverse)
library(patchwork)
install.packages('ggsci')
install.packages("wesanderson")
library(ggsci)
library(wesanderson)


#How to write a function 

concat <- function(x,text='CC'){
  paste(x,text,sep='_')
}

concat(2)
concat(c(1,2))
concat('Red',text='color')

#Lists. Like vectors but more flexible. An element can be anything. 
# Text, numeric, data frame or another list. 

df <- data.frame(colA=1:10,colB=rep('A',10))

#Create a list 

a.list <- list("Blue", 10, c(21,32,11), TRUE,df)
a.list

# Using a single [] we create a list slice
a.list[1]


# Using double [[]] is the reference, this is the same as single [] in a vector
a.list[[1]]


#Created a named list

b.list <- list(color="Blue", id=10, summary=c(21,32,11), afib=TRUE,data=df)
b.list

b.list['color']
b.list[['color']]


#Retrieve the names of the list
names(b.list)

## Add to out list

b.list$age <- 40
b.list[['gender']] <- 'F'

names(b.list)


#Loop over lists
d.list <- list('purple','green','red')

#Using a for loop, this works only for list of 3 elements 
for(i in 1:3){
  print(concat(d.list[[i]]))
}

#We can modify it so it takes any size list
for(i in 1:length(d.list)){
  print(concat(d.list[[i]]))
}

#But what if we wanted to save the results to a new list, How ugly! 
newlist <- list()
for(i in 1:length(d.list)){
  newlist[[i]] <- print(concat(d.list[[i]]))
}

newlist

# Loop over list the R way 

newlist <- lapply(d.list,concat)
newlist

## Or the tidyverse way, the map family of functions are in the purrr library

newlist <- map(d.list,concat)
newlist

# We can add the extra arguments to the function
newlist <- map(d.list,concat,text='boo')
newlist

#We don't have to create the function, we can use an anonymous function

newlist <- map(d.list,function(x,text='goo')
  paste(x,text,sep='_')
  
  )
newlist

## Now with numbers 
c.list <- list(c(runif(10,min=1,max=25)),runif(10,min=1,max=25),runif(10,min=1,max=25))
c.list
#using base R
lapply(c.list,mean)
#Using purrr library from tidyverse
map(c.list,mean)


#Back to ggplot! 

#read in data, make sure you alter this to your path
data <- read_csv('20201201_list_ggplot2/data.csv')



#lets make some basic plots. 

ggplot(data,aes(x=etiology,y=BMI)) + geom_point()

#Let's jitter the points, so they do not lie on top of each other
ggplot(data,aes(x=etiology,y=BMI)) + geom_jitter(position=position_jitter(0.2))

#Boxplot
ggplot(data,aes(x=etiology,y=BMI)) + geom_boxplot()
#Points and Boxplots, much better
ggplot(data,aes(x=etiology,y=BMI)) + 
  geom_boxplot() + 
  geom_jitter(position=position_jitter(0.2))

#using fill 
ggplot(data,aes(x=etiology,y=BMI,fill=sex)) + 
  geom_boxplot() + 
  geom_jitter(position=position_jitter(0.2))
#using color
ggplot(data,aes(x=etiology,y=BMI,color=sex)) + 
  geom_boxplot() + 
  geom_jitter(position=position_jitter(0.2))
#using both, but we have an issue with jitter, the points are mixed
ggplot(data,aes(x=etiology,y=BMI,color=sex,fill=sex)) + 
  geom_boxplot() + 
  geom_jitter(position=position_jitter(0.2))
#We can dodge the points so the appear witht he proper box, but not jittered
ggplot(data,aes(x=etiology,y=BMI,color=sex,fill=sex)) + 
  geom_boxplot() + 
  geom_jitter(position=position_dodge(0.2))

#Putting it all together with jitterdodge
ggplot(data,aes(x=etiology,y=BMI,color=sex)) + 
  geom_boxplot() + 
  geom_jitter(position=position_jitterdodge(0.2))


#Let's make our own theme. 

myTheme <- theme_bw() + theme(
  legend.position='bottom',
  panel.grid.major = element_blank(), 
  panel.grid.minor = element_blank(),
  panel.border = element_blank(),
  axis.line = element_line(colour = "black")
)



ggplot(data,aes(x=etiology,y=BMI,color=sex)) + 
  geom_boxplot() + 
  geom_jitter(position=position_jitterdodge(0.2)) +
  myTheme
  



ggplot(data,aes(x=etiology,y=BMI,color=sex)) + 
  geom_boxplot() + 
  geom_jitter(position=position_jitterdodge(0.2)) +
  myTheme + 
  scale_color_aaas()

ggplot(data,aes(x=etiology,y=BMI,color=sex)) + 
  geom_boxplot() + 
  geom_jitter(position=position_jitterdodge(0.2)) +
  myTheme + 
  scale_color_manual(values = wes_palette("Royal1"))

ggplot(data,aes(x=etiology,y=BMI,color=sex)) + 
  geom_boxplot() + 
  geom_jitter(position=position_jitterdodge(0.2)) +
  myTheme + 
  scale_color_manual(values = wes_palette("GrandBudapest1"))


#### let's make a figure, back to lists!

plots.list <- list()
plots.list$A <- ggplot(data,aes(x=etiology,y=BMI,color=sex)) + 
  geom_boxplot() + 
  geom_jitter(position=position_jitterdodge(0.2)) +
  scale_color_aaas()

plots.list$B <- ggplot(data,aes(x=etiology,y=Gene1,color=Afib)) + 
  geom_boxplot() + 
  geom_jitter(position=position_jitterdodge(0.2)) +
  scale_color_manual(values = wes_palette("GrandBudapest1"))

plots.list$C <- ggplot(data,aes(x=ht_in,y=wt_kg,color=sex)) + 
  geom_point() +
  scale_color_aaas()

wrap_plots(plots.list)

#We can know apply theme each plot. 

plots.list <- map(plots.list, function(p)
  p+myTheme
  )

wrap_plots(plots.list)

#Add some annotation 
wrap_plots(plots.list) + plot_annotation(tag_levels = 'A')

#With patchwork we can spefic layouts using hte "+","/". 

plots.list$A + (plots.list$B/plots.list$C) + 
  plot_annotation(tag_levels = 'A')








