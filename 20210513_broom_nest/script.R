library(tidyverse)
library(broom)

#Read in some data 
meta <- read_csv('20210513_broom_nest/testData.csv')
genes <- read_csv('20210513_broom_nest/geneData.csv')



df <- inner_join(meta,genes,by=c('id'='sampid')) %>%
  mutate(etiology=factor(etiology),
         gender=factor(gender)
         )




#Let's do a t.test, the ugly way, don't do this! 

t.test(df$Gene1[df$gender=='F'], df$Gene1[df$gender=='M'])

#Use the ~ instead, which means 'describe by'

t.result <- t.test(Gene1~gender,data = df)

#We can view the results. 
t.result

#What is result type, it's a list. 
str(t.result)

#We can then deference the elements. 

t.result[[3]]

t.result$p.value


#We can use tidy from the broom package

tidy(t.result)


#We can tidy up other functions as well.. 

wil.result <- wilcox.test(Gene1~gender,data = df)
wil.result

tidy(wil.result)


#Linear regression 

lm1.result <- lm(Gene1~gender,data = df)
lm1.result
summary(lm1.result)

tidy(lm1.result)


#more complex models 

lm2.result <- lm(Gene1~etiology+wt_kg+gender,data = df)

summary(lm2.result)
tidy(lm2.result)


### Now what if we wanted to test all genes. 

#brute force way 
rbind(
tidy(lm(Gene1~etiology+wt_kg+gender,data = df)),
tidy(lm(Gene2~etiology+wt_kg+gender,data = df)),
tidy(lm(Gene3~etiology+wt_kg+gender,data = df))
)

#Problem? 

rbind(
  cbind(data.frame(gene='gene1'),tidy(lm(Gene1~etiology+wt_kg+gender,data = df))),
  cbind(data.frame(gene='gene2'),tidy(lm(Gene3~etiology+wt_kg+gender,data = df))),
  cbind(data.frame(gene='gene3'),tidy(lm(Gene3~etiology+wt_kg+gender,data = df)))
)



#For loop?

genenames <- colnames(df)[8:ncol(df)]
genenames
#create a Df to store values 
res.df <- data.frame()

for(g in genenames){
  f <- as.formula(paste0(g,'~etiology+wt_kg+gender'))
  res.df <- rbind(res.df,cbind(data.frame(gene=g),tidy(lm(f,data=df))))
}



#tidyverse method 

# First we need to make data longer. 

df %>% pivot_longer(names_to = 'gene',values_to='signal',cols = Gene1:Gene10)



## Let's break this problem as if we had one gene. 


df.tmp <- df %>% pivot_longer(names_to = 'gene',values_to='signal',cols = Gene1:Gene10) %>%
  filter(gene=='Gene1')




#I no longer need to know the name of the gene. 
tidy(lm(signal~etiology+wt_kg+gender,data=df.tmp))


#What would be nice is a set of tibbles for each gene, so we first need to group the data by gene. 

df %>% pivot_longer(names_to = 'gene',values_to='signal',cols = Gene1:Gene10) %>%
  group_by(gene)

## Now we can use the nest function to create tibbles based on a grouping variable, in this case gene


df %>% pivot_longer(names_to = 'gene',values_to='signal',cols = Gene1:Gene10) %>%
  group_by(gene) %>%
  nest() 

#The only wrinkle is the data col type is a list, so each 'cell' in the table is a list of tibbles, though in our case only one tibble
#how do we deal with lists? We use map. How do we create a new column we use mutate. 


df %>% pivot_longer(names_to = 'gene',values_to='signal',cols = Gene1:Gene10) %>%
  group_by(gene) %>%
  nest() %>%
  mutate(lm1=map(data,~ tidy(lm(signal~etiology+wt_kg+gender,data=.x)))
         )

#We can now unest the lm1 column 

df %>% pivot_longer(names_to = 'gene',values_to='signal',cols = Gene1:Gene10) %>%
  group_by(gene) %>%
  nest() %>%
  mutate(lm1=map(data,~ tidy(lm(signal~etiology+wt_kg+gender,data=.x)))) %>%
  unnest(lm1)



# We can clean up just the rows and columns we want. 


df %>% pivot_longer(names_to = 'gene',values_to='signal',cols = Gene1:Gene10) %>%
  group_by(gene) %>%
  nest() %>%
  mutate(lm1=map(data,~ tidy(lm(signal~etiology+wt_kg+gender,data=.x)))) %>%
  unnest(lm1) %>%
  select(-data) %>%
  filter(term=='etiologyNF')


tmp <- 1

map(tmp, ~mean(.x))


#We can use this make plots as well.. Some 

#Let's make a quick plot function. 


plots <- df %>% pivot_longer(names_to = 'gene',values_to='signal',cols = Gene1:Gene10) %>%
  group_by(gene) %>%
  nest() %>%
  mutate(plot=map2(data,gene, ~ggplot(data=.x,aes(x=etiology,y=signal)) + 
                     geom_violin(aes(fill=etiology)) +
                     geom_jitter(width=.1) + theme_bw()  + 
                     theme(panel.grid.major = element_blank(),
                           panel.grid.minor = element_blank(),
                           axis.title.x = element_blank(),
                           legend.position = 'none'
                     ) + 
                     ggtitle(.y))
  )


plots
  

plots$plot[1]

plots

#The ..1 is the first column 

plots %>% select(gene,plot) %>%
  pwalk(~ggsave(filename=paste0('20210513_broom_nest/',..1,'.png'),plot = ..2,width=6,height=5))















































