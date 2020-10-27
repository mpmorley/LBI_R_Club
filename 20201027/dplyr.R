require(tidyverse)
library(lubridate)


df <- read_csv('20201027/testData.csv')


# Select: Subset variable (columns) ------------------------------------------------------------------
#single column
select(df,NPPA)
#Range of cols
select(df,id:etiology)
#remove a col
select(df,-DOB)

select(df,starts_with('N'))



# filter: Subset observations (rows) ------------------------------------------------------------------

filter(df,sex=='F')

filter(df,sex=='F' & Afib==TRUE)


# Pipes -------------------------------------------------------------------
# %>% is called a pipe 
#Pipes send the output of one function as the first argument to the next function. 
df %>% select(sex,NPPA) %>%
  filter(sex=='F')


# Mutate ------------------------------------------------------------------
df <- df %>% 
  mutate(DOB=parse_date_time(DOB, orders = c('mdy', 'dmy')),
         BMI=
         )

df %>%
  mutate(age = today()-as.Date(DOB))

#Let's calculate BMI, first we need to convert inches to meters. 

df <- df %>% 
  mutate(ht_m = ht_in*0.0254
  )

df <- df %>% 
  mutate(BMI = wt_kg/(ht_m^2)
  )




df %>% 
  filter(DOB>=as.Date("1980-01-01"))

# ArrangeS Sort column by observations ------------------------------------------------------------------



# sumnmarize --------------------------------------------------------------

df %>% summarise(mean(NPPA),sd(NPPA))


# group_by ----------------------------------------------------------------

df %>% group_by(etiology) %>%
  summarise(mean(NPPA),sd(NPPA))

df %>% group_by(etiology,Afib) %>%
  summarise(mean(NPPA),sd(NPPA))

df %>% group_by(sex,etiology,Afib) %>%
  summarise(mean(NPPA),sd(NPPA))



# join --------------------------------------------------------------------

#Open a 2nd file and "merge' results. 


genes <- read_csv('20201027/geneData.csv')

dim(genes)
dim(df)

df %>% filter(id %in% genes$sampid)
#We can use the "not" in front and retirn the not matching
df %>% filter(!id %in% genes$sampid)

#using join, there's multiple flavors of join.. We'll look at inner_join and left/right join 


merge.inner <- inner_join(df,genes,by=c('id'='sampid'))
dim(merge.inner)
merge.left <- left_join(df,genes,by=c('id'='sampid'))
dim(merge.left)


# long data ---------------------------------------------------------------
pivot_longer(merge.inner,
             Gene1:Gene10
)

merge.long <- pivot_longer(merge.inner,
             Gene1:Gene10,
             names_to='gene',
             values_to='UMI'
)



merge.long %>% 
  group_by(etiology,gene) %>%
  summarise(mean=mean(UMI),
            sd=sd(UMI),
            min(UMI),
            max(UMI)
            )





