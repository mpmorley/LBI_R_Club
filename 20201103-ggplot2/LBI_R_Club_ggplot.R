#LBI R club meeting 11/03
#Apoorva Babu

#Creating Basic plots using ggplot2

#Install ggplot2 library
install.packages(c("ggplot2","dplyr","tidyr"))

#Load libraries
library("ggplot2")
library("dplyr")
library("tidyr")

#Read input data
df = read.csv("~/Desktop/Testdata.csv")


################# Bar-plot #######################

#Basic
ggplot(data=df[1:5,], aes(x=sample_name, y=age)) + geom_bar(stat = "identity")
#add color
ggplot(data=df[1:5,], aes(x=sample_name, y=age)) + geom_bar(stat = "identity", fill="steelblue")
#colored by category
ggplot(data=df[1:5,], aes(x=sample_name, y=age, fill=gender)) + geom_bar(stat = "identity")
#Horizontal bar-plot
ggplot(data=df[1:5,], aes(x=sample_name, y=age, fill =gender)) + geom_bar(stat = "identity") + coord_flip()
#Stacked Barplot
ggplot(data=df[1:10,], aes(x=gender, y=height, fill=race)) + geom_bar(stat = "identity",position=position_dodge())

#Factorize columns
ggplot(data=df, aes(x=etiology, y=age, fill=etiology)) + geom_bar(stat = "identity")
df$etiology = factor(df$etiology, levels=c("NF","DCM","HCM","PPCM"))

#Themes and labels
ggplot(data=df[1:5,], aes(x=sample_name, y=age, fill=gender)) + geom_bar(stat = "identity") + theme_minimal()
ggplot(data=df[1:5,], aes(x=sample_name, y=age, fill=gender)) + geom_bar(stat = "identity") + theme_dark()
ggplot(data=df[1:5,], aes(x=sample_name, y=age, fill=gender)) + geom_bar(stat = "identity") + theme(legend.position="top")

gg=ggplot(data=df[1:5,], aes(x=sample_name, y=age, fill=gender)) + geom_bar(stat = "identity") + theme_minimal()

gg+scale_color_manual(values=c("#C0C0C0", "#E69F00"))
gg+scale_fill_brewer(palette="Dark2")

gg + theme(
  plot.title = element_text(size=25),
  legend.position = c(.95, .95),
  legend.justification = c("right", "top"),
  legend.box.just = "right",
  legend.margin = margin(6, 6, 6, 6),
  legend.box.background = element_rect(),
  legend.box.margin = margin(6, 6, 6, 6)
) + labs(title="Boxplot",x="Sample",y="Age")

################## Dot-plot ##################
ggplot(df, aes(x=Library.Pool,y=TIN.median.)) +geom_dotplot(binaxis='y',dotsize=0.5)

#Scatter-plot
 ggplot(df, aes(x=height,y=weight)) +geom_point()
 
 ds <- df %>% group_by(Library.Pool) %>% summarise(mean = mean(weight))
 ggplot(df, aes(Library.Pool, weight)) +
   geom_point() +
   geom_point(data = ds, aes(y = mean), colour = 'red', size = 3)
 
#Box-plot
ggplot(df, aes(tissue_source,age,color=gender)) + geom_boxplot()

#Violin plot
ggplot(df, aes(x=Library.Pool,y=TIN.median.,fill=race)) +geom_violin(trim = FALSE) + facet_grid(rows=vars(race), cols=vars(gender)) + theme(axis.text.x = element_text(angle = 90) )

#Histogram
ggplot(df, aes(x=weight)) + geom_histogram(fill="blue") 
ggplot(df, aes(x=weight,color=gender)) + geom_histogram(fill="white") 


#Density plot
hist= ggplot(df, aes(x=weight, color=gender, fill=gender)) +
  geom_histogram(aes(y=..density..), position="identity")+
  geom_density(alpha=0.5)

#Faceting
ggplot(df, aes(x=weight,color=gender)) + geom_histogram(fill="white") + facet_grid(cols=vars(gender))
ggplot(df, aes(x=weight,color=gender)) + geom_histogram(fill="white") + facet_grid(.~gender)

#Saving high resolution plots
 gg %>% ggsave(file="example_plot.png",path="~/Desktop/",device="png",width = 3,height =5 ,units="in", dpi =300 )
