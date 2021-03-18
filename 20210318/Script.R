library(tidyverse)




data <- data.frame(sample= paste0('sample',1:n),
                   gender=sample(c('F','M'),100,replace = T),
                   status=c(rep('Donor',40),rep('COPD',20), rep("IPF", 40)),
                   age = runif(20,min=40,80),
                   pop=c(rep('CEU',35),rep('GBR',9), rep("CHB", 11),rep("CHS",4),rep('FIN',6),rep('ASW',7),rep('YRI',18),rep('LWK',10)),
                   RGN1 = rnorm(n, mean = 12),
                   RGN2 = rnorm(n, mean = 6),
                   YFG1 = rnorm(n, mean = 10),
                   ASP1 = rnorm(n, mean = 8),
                   ASP3 = rnorm(n, mean = 14)
) 


summary(data)


#Let's make the data "longer" 

data.lg <- pivot_longer(data,cols=RGN1:ASP3,names_to = 'gene',values_to = 'expression')


#Make the chars 
data.lg %>% mutate(status = as.factor(status))


data.lg <- data.lg %>% mutate_at(c('status','pop','gender'),as.factor)


ggplot(data.lg,aes(x=gene,y=expression)) + geom_boxplot() + theme_bw()


#Let's arrange the manually. 




data.lg %>% mutate(gene=fct_reorder(gene, expression)) %>%
  ggplot(aes(x=gene,y=expression)) + geom_boxplot() + theme_bw()


#To change to descending.. Can use the param desc=TRUE or fct_reorder2

data.lg %>% mutate(gene=fct_reorder(gene, expression,.desc=TRUE)) %>%
  ggplot(aes(x=gene,y=expression)) + geom_boxplot() + theme_bw()


#manul reorder. 


fct_relevel(f, "a", after = 2)
data.lg %>% mutate(gene=fct_relevel(gene, "RGN1","RGN2" )) %>%
  ggplot(aes(x=gene,y=expression)) + geom_boxplot() + theme_bw()


fct_relevel(f, "a", after = 2)
data.lg %>% mutate(gene=fct_relevel(gene, "YFG1",after=1)) %>%
  ggplot(aes(x=gene,y=expression)) + geom_boxplot() + theme_bw()





#Plot by pop 


data.lg %>% mutate(gene=fct_reorder(gene, expression,.desc=TRUE)) %>%
  ggplot(aes(x=gene,y=expression,color=pop)) + geom_boxplot() + theme_bw()


#Let's look at our data, 
data.lg %>% mutate(pop = fct_lump(pop, n = 4)) %>% count(pop)


data.lg %>% mutate(pop = fct_lump(pop, n = 3)) %>%
  ggplot(aes(x=gene,y=expression,color=pop)) + geom_boxplot() + theme_bw() 
  

#Cobine the two orders.. 

data.lg %>% mutate(gene=fct_reorder(gene, expression),
                   pop = fct_lump(pop, n = 3)) %>%
  ggplot(aes(x=gene,y=expression,color=pop)) + geom_boxplot() + theme_bw()




#We can also collpaspe the pops... 

data.lg %>% mutate(pop = fct_collapse(pop,
                              EUR = c('CEU','FIN','GBR'),
                              AFR = c('YRI','LWK','ASW'),
                              ASN = c('CHB','CHS')) ) %>% count(pop)




data.lg %>% mutate(pop = fct_collapse(pop,
                                      EUR = c('CEU','FIN','GBR'),
                                      AFR = c('YRI','LWK','ASW'),
                                      ASN = c('CHB','CHS')),
                   gene=fct_reorder(gene, expression),
                   ) %>%
ggplot(aes(x=gene,y=expression,color=pop)) + geom_boxplot() + theme_bw()















