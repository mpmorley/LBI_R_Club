#LBI R club meeting 04/01
#Apoorva Babu

#Creating 3D plots using plotly

#Install plotly from CRAN or Github 
install.packages("plotly")
devtools::install_github("ropensci/plotly")

#Load libraries
library(ggplot2)
library("plotly")

#Read input data
df = read.csv("20210401_plotly/testdata.csv")

#Basic barplot
ggplot(data=df[1:5,], aes(x=sample_name, y=age)) + geom_bar(stat = "identity", fill="steelblue")

#barplot using plotly
plot_ly(x=df$sample_name[1:5], y=df$age[1:5], type="bar") 

plot_ly(data= df[1:5,], x=~sample_name, y=~age, type="bar") 

df2=df[1:10,]
#Grouped Barplot
fig <- plot_ly(data= df2, x=~sample_name, y=~age, type="bar", name="age") 
fig <- fig %>% add_trace(y = ~weight,name="weight")
fig <- fig %>% layout(yaxis = list(title = 'Count'), barmode = 'group')

#Stacked barplot
df2$RIN2 = 10-df2$RIN
fig = plot_ly(data= df2, x=~sample_name, y=~RIN, type="bar") 
fig <- fig %>% add_trace(y = ~RIN2)
fig <- fig %>% layout(yaxis = list(title = 'Count'), barmode = 'stack')

#Scatter plot
plot_ly(df, x = ~height, y = ~weight,color = ~gender, size = ~age, type= "scatter")

#Customizing text on hover
plot_ly(df, x = ~height, y = ~weight,color = ~gender, size = ~age, type= "scatter", text=~paste("Sample: ", sample_name, '<br>Age:', age))

#Adding text
plot_ly(df2, x = ~height, y = ~weight, type = 'scatter',
        mode = 'text', text = ~sample_name, textposition = 'top right',
        textfont = list(color = toRGB("grey50"), size = 14))

plot_ly(df2, x = ~height, y = ~weight, type = 'scatter',text = ~sample_name) %>%
       add_text(textfont = list(color = toRGB("grey50"), size = 14), textposition = "top right") %>%
       layout(showlegend = FALSE)

#Adding custom colors
pal <- c("red", "blue")
pal <- setNames(pal, c("Male", "Female"))
plot_ly(df, x = ~height, y = ~weight,color = ~gender, size = ~age,colors=pal, type= "scatter")

#Box and whiskers plot
plot_ly(df,x=~etiology, y = ~age,color = ~etiology, type = "box")

#heatmap
data <- read.csv("20201201_list_ggplot2/data.csv")
data = data %>% column_to_rownames(var = "id") %>% select(Gene1:Gene10) 
plot_ly(z=as.matrix(data), type = "heatmap")

plot_ly(y= colnames(data),x= rownames(data),z=as.matrix(t(data)), type = "heatmap",colors = colorRamp(c("white", "blue")))

#using plotly on ggplot
gg=ggplot(data=df[1:5,], aes(x=sample_name, y=age, fill=gender)) + geom_bar(stat = "identity") + theme_minimal()
gg=gg+scale_fill_manual(values=c("#C0C0C0", "#E69F00"))
ggplotly(gg)

#Faceting
gg=ggplot(df, aes(x=weight,y=height,color=gender)) + geom_point() + facet_grid(cols=vars(gender))
ggplotly(gg)

#Themes 
gg2=gg +theme_dark()
ggplotly(gg2)

#Saving plots as html
p=plot_ly(df, x = ~height, y = ~weight,color = ~gender, size = ~age, text=~paste("Sample: ", sample_name, '<br>Age:', age))
htmlwidgets::saveWidget(p, file = "~/Desktop/image.html")

#Static images saved using orca.



