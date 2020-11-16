library(ggplot2)
library(dplyr)
library(tidyr)
library(Hmisc)
library(readr)
require(tidyverse)
library(tidyverse)
library(tidyr)

##Load in Data

TACMIF <- read_csv("Documents/Mouse TAC MI/TACMIF.csv")

##Sample Generate HR
##This section exists to show how I generate single graphs for singular data

HR <- ggplot(TACMIF, aes(x=Type, y=HR)) +
  geom_dotplot(binaxis='y', stackdir='center') +ylim(0, 700)

HR + stat_summary(fun.data=mean_sdl, fun.args = list(mult=1),
                  geom="errorbar", color="red", width=0.2) +
  stat_summary(fun.y=mean, geom="point", color="red")

##Change Name Ordering
TACMIF$Type = factor(TACMIF$Type, levels=c("Sham-Het","HET","Sham-WT","WT"))

##Longdata Attempt to build consecutive graphs
##This is my attempt to generate multiple graphs using the the Longdata Method

longheart <- pivot_longer(TACMIF,
                          cols=HW:Tibia,
                          names_to = 'Measurement',
                          values_to = 'Output'
                          
)

##Change Name Ordering
longheart$Type = factor(longheart$Type, levels=c("Sham-Het","HET","Sham-WT","WT"))

##Test Cumulative Graph adapted from R club's example
ggplot(longheart,aes(x=Type,y=Output,fill=Type)) + geom_boxplot() + facet_grid(~Measurement)

##Cumulative Data I actually want to generate
longhaul <- ggplot(longheart, aes(x=Type, y=Output)) +
  geom_dotplot(binaxis='y', stackdir='center') +ylim(0, 5000) + facet_grid(~Measurement)


longhaul + stat_summary(fun.data=mean_sdl, fun.args = list(mult=1),
                        geom="errorbar", color="red", width=0.2) +
  stat_summary(fun.y=mean, geom="point", color="red")

########Irrelevant Testdata
########This Testdata is completely irrelevant to my code. It exists because I needed to troubleshoot my code using the R examples from R club

urlfile="https://raw.githubusercontent.com/mpmorley/LBI_R_Club/master/20201027/TestData.csv"
df<-read_csv(url(urlfile))

urlfile="https://raw.githubusercontent.com/mpmorley/LBI_R_Club/master/20201027/geneData.csv"
genes<-read_csv(url(urlfile))

merge.inner <- inner_join(df,genes,by=c('id'='sampid'))
dim(merge.inner)

tmp <- pivot_longer(merge.inner,
                    cols=Gene1:Gene10
)

merge.long <- pivot_longer(merge.inner,
                           cols=Gene1:Gene10,
                           names_to='gene',
                           values_to='UMI'
)

summ <- merge.long %>%
  group_by(etiology,gene) %>%
  summarise(mean=mean(UMI),
            sd=sd(UMI),
            min=min(UMI),
            max=max(UMI)
  )


ggplot(merge.long,aes(x=etiology,y=UMI,fill=etiology)) + geom_boxplot() + facet_grid(~gene)


