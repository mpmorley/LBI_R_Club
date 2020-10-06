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
r50 <-runif(50,min=1,max=25)


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
  
  
c4 <- c(rep('DCM',10),rep('HCM',15),rep('NF',20),rep('nonfail',5))
c4

#Create a random vector of chars, I'll explain the factor function later. 
groups <- c('DCM','HCM','NF')
c4.a <- sample(groups,50,replace=T)
summary(factor(c4.a))
c4.b<-sample(groups,50,replace=T, prob = c(.2,.3,.5))
summary(factor(c4.b))

        

#Create a logical vector         

d <- c(T,F,F,T,T)
d

#What if we tried to sum a char vector
sum(c)

#What happens when we do a boolean?
sum(d)

#We can make boolean vecotrs with a comparison
b > 5
b >= 5
b == 5
#Then 
sum(r50>5)

sum(r50 > mean(r50))

#Can we do the same for chars? yes
c4=="HCM"

sum(c4=='HCM')

c4[2:5]

c4[c4=='HCM']

#Using the 'or' and 'and' operator. 
c4[c4=='HCM' | c4=='DCM']
c4[c4=='HCM' & c4=='DCM']

#The 'in' operator 

c4[c4 %in% c('HCM','DCM')]

#The not operator 
c4[c4 != 'NF']


#Let's use a comparisons to fix the nonfail label

c4[c4=='nonfail']

c4[c4=='nonfail'] <- 'NF'

sum(c4=='nonfail')



#We can rendonlyt sample a vector as well. 
set.seed(10)
sample(c4,15)
set.seed(10)
sample(c4,15)


#Cannot mix data types. 
C(1,2,'DCM')

#But if we make the numbers chars we can. 
c('1','2','DCM')




# Factors ----------------------------------------------------------------


c5 <- factor(c4)
c5

as.numeric(c4)
as.numeric(c5)

plot(c5,r50)

#reorder the factor to have the order we want on the x sxis
c5 <- factor(c4,levels = c('NF','HCM','DCM'))
c5
plot(c5,r50)

t.test(r50[c5=='NF'],r50[c5=='HCM'],var.equal = T)


# NAs ---------------------------------------------------------------------
r50.na <- r50
r50.na[c(3,8,10,12)] <- NA
r50.na

summary(r50.na)
mean(r50.na)

r50.nona <- na.omit(r50.na)
mean(r50.nona)

mean(r50.na,na.rm = T)

