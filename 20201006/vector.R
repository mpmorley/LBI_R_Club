# Multiples ways to create a vector ---------------------------------------

c(1,2,3,4,5)
1:5
a<-10:15


# how to select or remove elements.  
a[1]
a[6]
a[-2]


#Seq function to creat sequences

seq(0,10,by=2)
seq(1,10,by=2)


rep(c(1:5))
rep(c(1:5),3)


#Creating vector with random numbers

runif(10,min=1,max=25)

set.seed(100)
runif(10,min=1,max=25)
runif(10,min=1,max=25)
set.seed(100)
runif(10,min=1,max=25)

r50 <-runif(50,min=1,max=25)
#Use the summary function to get some details
summary(r50)
mean(r50)
#Other numeric functions for a vector. 
min(r50)
max(r50)
sd(r50)



#Vectors can store numerical, character, logical,dates

c1 <- c('DCM','DCM')
c2 <- c('HCM','HCM','HCM')
c3 <- c(c1,c2)
c3
  
  
groups <- c(rep('DCM',10),rep('HCM',15),rep('NF',20),rep('nonfail',5))
groups

#Create a random vector of chars, I'll explain the factor function later. 

groups.a <- sample(c('DCM','HCM','NF'),50,replace=T)
summary(factor(groups.a))
groups.b<-sample(groups,50,replace=T, prob = c(.2,.3,.5))
summary(factor(groups.b))

        

#Create a logical vector         

logical <- c(T,F,F,T,T)
logical

#What if we tried to sum a char vector
sum(groups)

#What happens when we do a logical?
sum(logical)

#We can make logical vectors with a comparison
r50 > 5
r50 >= 5
r50 == 5
#Then 
sum(r50>10)

sum(r50 > mean(r50))

#Can we do the same for chars? yes
groups=="HCM"

sum(groups=='HCM')

#Selecting elements from a char vector. 

groups[2:5]
groups[groups=='HCM']

#Using the 'or' and 'and' operator. 
groups[groups=='HCM' | groups=='DCM']
groups[groups=='HCM' & groups=='DCM']

#The 'in' operator 

groups[groups %in% c('HCM','DCM')]

#The not operator 
groups[groups != 'NF']


#Let's use a comparisons to fix the nonfail label

groups[groups=='nonfail']

groups[groups=='nonfail'] <- 'NF'

sum(groups=='nonfail')



#We can randomly sample a vector as well. 
set.seed(10)
sample(groups,15)
set.seed(10)
sample(groups,15)


#Waht can't we do,Cannot mix data types. 
C(1,2,'DCM',F)

#But if we make the numbers chars we can. 
c('1','2','DCM','F')



# Factors ----------------------------------------------------------------


groups.factor <- factor(groups)
groups.factor

as.numeric(groups)
as.numeric(groups.factor)

plot(groups.factor,r50)

#reorder the factor to have the order we want on the x sxis
groups.factor <- factor(groups,levels = c('NF','HCM','DCM'))
groups.factor
plot(groups.factor,r50)


t.test(r50[groups.factor=='NF'],r50[groups.factor=='HCM'],var.equal = T)


# NAs ---------------------------------------------------------------------
r50.na <- r50
r50.na[c(3,8,10,12)] <- NA
r50.na

summary(r50.na)
mean(r50.na)

r50.nona <- na.omit(r50.na)
mean(r50.nona)

mean(r50.na,na.rm = T)

