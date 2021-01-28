library(tidyverse)
library(devtools)
install_github("jokergoo/ComplexHeatmap")
library(ComplexHeatmap)
library(circlize)
################################################################
#   20210128 MPM Script for using ComplexHeatmap R package
#   See more at https://jokergoo.github.io/ComplexHeatmap-reference/book/index.html
#
#
#
################################################################


set.seed(20)
#Let's create some test data. ComplexHeatmap uses a matrix as input
nums <- runif(200,min=10,max=16)
m1 <- matrix(nums,ncol=10)
dim(m1)

#Let's create some col/row names, For gene expression is typical to have 
# genes as rows and samples as cols, I use the dim function here so if I chnage the 
# size of my matrix I don't have to recode the number of cols or rows. 


rownames(m1) <- paste0('YFG',1:dim(m1)[1])
colnames(m1) <- paste0('donor',1:dim(m1)[2])


#Let's make a simple heatmap. 
?Heatmap 
Heatmap(m1)

#Give the scale a name

Heatmap(m1,name='Gene Expression')


#Remove col clustering

Heatmap(m1,name='Gene Expression',
        cluster_columns = FALSE 
        )

#Remove row clusters
Heatmap(m1,name='Gene Expression',
        cluster_rows = FALSE 
        )

# we can hide the denodrogram but still cluster

Heatmap(m1,name='Gene Expression',
        cluster_columns = FALSE,
        show_row_dend = FALSE
        )


#Let's add some more genes to m1, but much lower expressed

#Another way to create a matrix 
n=200
ncol=10
nrow=n/ncol
m2 <-matrix(runif(n,min=2,max=10),
            ncol=ncol, 
            dimnames=list(paste0('RGN',1:nrow),
                          paste0('donor',1:ncol)
                          )
            )
m2
#Let's plot these new genes. 
Heatmap(m2)


#Now combine both gene matrices using rbind function

m3 <- rbind(m1,m2)

Heatmap(m3)


#Let's change one gene expression to be very high

m3['YFG20',] <- m3['YFG20',]+15

#and one sample to be very high as well.

m3[,'donor5'] <- m3[,'donor5']+15


# Scaling -----------------------------------------------------------------

#Plot the heatmap, we'll turn off clustering in order to keep the rows and  cols 
#the same for each plot
Heatmap(m3,
        cluster_rows =F,
        cluster_columns = F
        )


#The scale function will scale each column (samples in our case) to have a mean =0 and sd=1. ie Z-score

m3.scale.col <- scale(m3)


Heatmap(m3.scale.col,
        cluster_rows =F,
        cluster_columns = F
)

#If we want to scale the rows (genes) we need to transpose the matrix first and then transpose
# it back. 

m3.scale.row <- t(scale(t(m3)))


Heatmap(m3.scale.row,
        cluster_rows =F,
        cluster_columns = F
)

m3.scale.both <- scale(m3.scale.row)

Heatmap(m3.scale.both,
        cluster_rows =F,
        cluster_columns = F
)

Heatmap(m3.scale.both,
        cluster_rows =T,
        cluster_columns = T
)


#Let's add some new samples to matrix with altered gene expression, we'll use the m1 and m2 matrices 

m.tmp <- rbind(m1-5,m2+10)
#We need to change the the sample names since there the same. 
colnames(m.tmp)

colnames(m.tmp) <- gsub(pattern = 'donor', 'COPD',colnames(m.tmp))
colnames(m.tmp)          

m4 <- cbind(rbind(m1,m2),m.tmp)

#I'm going to randomize the gene order, I'm only doing this show the effect of clustering more. 
m4 <- m4[sample(1:dim(m4)[1]),]



Heatmap(m4,
        cluster_rows =F,
        cluster_columns = F
)

Heatmap(m4,
        cluster_rows =T,
        cluster_columns = T
)


#scale the rows(genes) and replot

m4.scale <- t(scale(t(m4)))
Heatmap(m4.scale,
        name='Expression',
        cluster_rows =T,
        cluster_columns = T
)

#Can change colors, ComplexHeatmap does require a bit extra work 
# to change the color scale. Need to use colorRamp2 function from the 
# circlize library. 

col1 = colorRamp2(c(-2,0,2), c('skyblue1', "grey20","yellow"))


Heatmap(m4.scale,
        name='Expression',
        cluster_rows =T,
        cluster_columns = T,
        col = col1
)

#Change the breaks to a narrow interval. 
col2 = colorRamp2(c(-1,0,1), c('skyblue1', "grey20","yellow"))


Heatmap(m4.scale,
        name='Expression',
        cluster_rows =T,
        cluster_columns = T,
        col = col2
)


# Heatmap Annotation  -----------------------------------------------------



#Let's create some annotation data.frame. 
#WARNING!!! This data.frame has to be in the ORDER as the columns in the matrix


anno <- data.frame(sample=colnames(m4),
                   gender=sample(c('F','M'),20,replace = T),
                   status=c(rep('Donor',10),rep('COPD',10)),
                   age = runif(20,min=40,80)
                   )
anno

#Check to see if the anno is in the same order as matrix

all.equal(anno$sample,colnames(m4))



# We need create a "named" vector for the colors key for the Heatmap annotation
gendercols = c('F'='red2','M'='black')
statuscols = c('Donor'='#720DDB','COPD'='#F68D24')

?HeatmapAnnotation
ha = HeatmapAnnotation(
  gender = anno$gender,
  status = anno$status,
  col = list(gender = gendercols,
             status=statuscols
  ))


Heatmap(m4.scale,
        name='Expression',
        cluster_rows =T,
        cluster_columns = F,
        col = col1,
        top_annotation = ha
)
Heatmap(m4.scale,
        name='Expression',
        cluster_rows =T,
        cluster_columns = F,
        col = col1,
        show_row_names = F,
        top_annotation = ha
)



# Add Callouts  -----------------------------------------------------------

genenames <- rownames(m4)
genenames

index <- which(genenames %in% c('RGN10','YFG7','YFG3','RGN1','RGN11'))


ra = rowAnnotation(gene = anno_mark(at = index, labels =genenames[index] ))

Heatmap(m4.scale,
        name='Expression',
        cluster_rows =T,
        cluster_columns = F,
        col = col1,
        show_column_dend = F,
        show_row_names = F,
        top_annotation = ha,
        right_annotation = ra
)



# Split Heatmap -----------------------------------------------------------

#We can use the column_split to create split the heatmpa into subsets, here split by gender

Heatmap(m4.scale,
        name='Expression',
        cluster_rows =T,
        cluster_columns = F,
        col = col1,
        show_column_dend = F,
        column_split = anno$gender,
        show_row_names = F,
        top_annotation = ha,
        right_annotation = ra
)
#We split by status and also remove the column labels

Heatmap(m4.scale,
        name='Expression',
        cluster_rows =T,
        cluster_columns = F,
        col = col1,
        show_column_dend = F,
        column_split = anno$status,
        show_row_names = F,
        show_column_names = F,
        top_annotation = ha,
        right_annotation = ra
)

#We can contionous variables to the heatmap annotation as well, we'll use age
# we'll also remove the legend for status but leave in gender
ha = HeatmapAnnotation(
  gender = anno$gender,
  status = anno$status,
  age=anno_barplot(anno$age),
  col = list(gender = gendercols,
             status=statuscols
  ),
  show_legend = c(TRUE,FALSE)
  )


Heatmap(m4.scale,
        name='Expression',
        cluster_rows =T,
        cluster_columns = F,
        col = col1,
        show_column_dend = F,
        column_split = anno$status,
        show_row_names = F,
        show_column_names = F,
        top_annotation = ha,
        right_annotation = ra
)


# Kmeans Clustering -------------------------------------------------------
# We can "cut" the row(gene) into K groups using K means cluster, here we do 2 groups. 

Heatmap(m4.scale,
        name='Expression',
        cluster_rows =T,
        cluster_columns = F,
        col = col1,
        show_column_dend = F,
        column_split = anno$status,
        show_row_names = F,
        row_km=2,
        top_annotation = ha,
        right_annotation = ra
)

# Here we do 6 groups 
Heatmap(m4.scale,
        name='Expression',
        cluster_rows =T,
        cluster_columns = F,
        col = col1,
        show_column_dend = F,
        column_split = anno$status,
        show_row_names = F,
        row_km=6,
        top_annotation = ha,
        right_annotation = ra
)

#Create a simple gene annotaion data.frame. 
#THIS FILE HAS TO BE IN THE SAME ORDER AS THE MATRIX!!!!!!
geneAnno <- data.frame(gene=genenames, 
                       GO=sample(c('WNT Signaling','Apoptosis','Cell Cycle'),40,replace = T,prob=c(.5,.3,.2))
                       )
#We can check this. 
all.equal(geneAnno$gene,rownames(m4))



geneAnno

Heatmap(m4.scale,
        name='Expression',
        cluster_rows =T,
        cluster_columns = F,
        col = col1,
        show_column_dend = F,
        column_split = anno$status,
        row_split = geneAnno$GO,
        show_row_names = F,
        top_annotation = ha,
        right_annotation = ra
)





# From an Eset ------------------------------------------------------------


if (!requireNamespace("BiocManager", quietly = TRUE))
  install.packages("BiocManager")



BiocManager::install("lungExpression")
library(lungExpression)
data("stanford")

stanford

anno <- pData(stanford)
anno <- anno %>% mutate(stage_label = ifelse(stage=='NA','unknown',paste0('Stage',stage))) 
anno$stage_label <- factor(anno$stage_label, levels=c('Stage1','Stage2','Stage3','Stage4','unknown'))



names<-sample(featureNames(stanford),50)
mat <- exprs(stanford)[names,]


Heatmap(mat,
        column_split = anno$stage_label,
        cluster_columns = F)




