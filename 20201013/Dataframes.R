
#loading some libraries. 
library(tidyverse)
library(lubridate) 

#############################################################
#   20201013 MPM R club Data Frame Basics
#
#
#
############################################################




#Very basic matrix intro
m1 <- matrix(1:10,nrow=5)
m1
#Matrix anre index like matrix[row,column]
#First row
m1[1,]
#first col
m1[,1]

#a single element
m1[5,2]

#We can add row or col names. 

colnames(m1) <- c('GeneA','GeneB')
rownames(m1) <- paste('sample',1:5,sep='')
m1

m1['sample1',]
m1[,'GeneA']

#Can perform arithmetic on all elements of matrix
m1+10
m1*2


m2 <- m1*2
m1*m2

#get the size of the matrix
dim(m1)


# R has built in function to sum by row or Cols
rowSums(m1)
colSums(m1)

#Can perform comparisons 
m1 > 5
sum(m1>5)

#mix comparisons and rowSum/colSum to caluate % of samples greater than a value

colSums(m1>4)/dim(m1)[1]*100



summary(m1)

##################################################################
#
#  Data Frames
#


# Data Frames. Similar to matrx but each colum can be a different data type

id <- paste('PENNID',1:50,sep='')
sex <- sample(c('F','M'),50,replace = TRUE)
Afib <- sample(c(T,F),50,replace = TRUE)
etiology <- c(rep('DCM',10),rep('HCM',15),rep('NF',25))
NPPA <-runif(50,min=2,max=10)
MYH7<-runif(50,min=5,max=20)


df <- data.frame(
                 'sex'=sex,
                 'Afib'=Afib,
                 'etiology'=etiology,
                 'NPPA'=NPPA,
                 "MYH7"=MYH7,
                 row.names = id
                 )

df
#Use summary to get basic stats on the data.frame
summary(df)

df$sex <- factor(df$sex)
df$etiology <- factor(df$etiology)

summary(df)


#Multipe ways to access cols, rows and "cells". 

#Column accessors just like matrix

df[,1]
df[,2:3]

#or by column name 

df$NPPA

df[,c('NPPA','MYH7')]


#We can select rows by names. 

df[c('PENNID39','PENNID39'),]

df[c('PENNID39','PENNID39'),'NPPA']


#How to subset a data frame, eventually we do it the dplyr way. 

#let's build first with a comparison, these returns a logical vector
#for each row. 

df$Afib == F

#We can then use that to select the rows we want. 


df[df$Afib==F,]


#Quick peak at the dpyr way. 

filter(df,Afib==F)


#Sometimes it's helpful to know what data struture we have. 


is.data.frame(df)
is.data.frame(df$NPPA)
is.vector(df$NPPA)


###############################################
#             tibble 
#   This is the tidyverse version of data.frame
# 

tb <- as_tibble(df)

#Major difference is no more rownames!!. But we lost our IDs!! 

#We cna fix it. 

df <- rownames_to_column(df,'id')


tb <- as_tibble(df)
tb




####################
#   read in data 
#######################
d.base <- read.csv('testData.csv')
d.base

summary(d.base)

d.tidy <- read_csv('testData.csv')

d.tidy

#Let's fix the date, we use lubridate function parse_date_time

d.tidy$DOB <-  parse_date_time(d.tidy$DOB, orders = c('mdy', 'dmy'))



####################################################
#   Write out the results
#
####################################################

#Using Base R 
write.csv(x=df,file='TestDf_base.csv')
#Using Tidy R 
write_csv(x=df,path='TestDf_tidy.csv')




