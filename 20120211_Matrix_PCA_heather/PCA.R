library(tidyverse)
library(matrixStats)
library(factoextra)
library(FactoMineR)
library(patchwork)
#Read in PhenoData

PhenoDF <- read_csv(file = 'hg_geneex_metadata.csv') %>%
  mutate(mort30 = as.factor(as.character(mort30)),
         endotype=as.factor(as.character(endotype)),
         pt = as.character(pt)
         )





#Read in expression data
exprsDF <- read_csv('degs_normalized_day0.csv')

#Since the data has the probeid,we need to subset the data.frame and convert it a matrix
mat <- as.matrix(exprsDF[,2:ncol(exprsDF)])
rownames(mat) <- exprsDF$X1

head(mat)


#Don't have the same number, we need to fix, we have an extra sample in mat. 
ncol(mat) == nrow(PhenoDF)




# Colnames in mat do not match any field in, so well remove *Day0.cel" and 
#leave the patient ID

colnames(mat) <- gsub('_Day0.CEL','',colnames(mat))

# Now we can make a matrix with same patients as the phenodata, 
#recall that I make the pt as a char when I imported

#Find which pt is missing
colnames(mat)[!colnames(mat) %in% PhenoDF$pt]

mat <- mat[,PhenoDF$pt]

#Check to see if the pt in PhenpDF is in the same order as the cols in mat
all.equal(colnames(mat),PhenoDF$pt)


#Compute the some expression(rma) stats of each gene in a
# new dataframe

geneStats <-data.frame(probesetid=rownames(mat),
                       mean=rowMeans2(mat),
                       var=rowVars(mat),
                       sd=rowSds(mat),
                       min=rowMins(mat),
                       max=rowMaxs(mat),
                       mad=rowMads(mat)
                       )


ggplot(geneStats,aes(x=mean))+geom_density()
ggplot(geneStats,aes(x=var))+geom_histogram()


#Get a gene list for PCA, top 3000 variables genes with an avg RMA > 3

probesetid.PCA <- geneStats %>% 
  filter(mean >3) %>%
  top_n(3000,var) %>%
  pull(probesetid)


#Make a PCA plot 

mat.pca <- mat[probesetid.PCA,]


#Need to transpose Matrix, need to have the Varibles(genes) as cols
res.pca <- PCA(t(mat.pca),  graph = FALSE)

fviz_screeplot(res.pca, addlabels = TRUE, ylim = c(0, 50))
fviz_pca_ind(res.pca)

colorpal <- wesanderson::wes_palette('Darjeeling1',type = 'discrete')


fviz_pca_ind(res.pca,
             label = "none", # hide individual labels
             habillage = PhenoDF$mort30, # color by groups
             palette = colorpal,
             addEllipses = TRUE # Concentration ellipses
)
fviz_pca_ind(res.pca,
             label = "none", # hide individual labels
             habillage = as.factor(PhenoDF$race), # color by groups
             palette = colorpal,
             addEllipses = TRUE # Concentration ellipses
)




plots <- map(1:4,function(n){
  
  fviz_pca_ind(res.pca,
               axes = c(n,n+1),
               label = "none", # hide individual labels
               habillage = as.factor(PhenoDF$mort30), # color by groups
               palette = colorpal,
               addEllipses = TRUE # Concentration ellipses
  )
})


wrap_plots(plots)
  

# Extra SVA ---------------------------------------------------------------


library(sva)


svaBatchCor <- function(dat, mmi, mm0,n.sv=NULL){
  dat <- as.matrix(dat)
  Y <- t(dat)
  #library(sva)
  if(is.null(n.sv))   n.sv <- num.sv(dat,mmi,method="leek")
  o <- svaseq(dat,mmi,mm0,n.sv=n.sv)
  W <- o$sv
  alpha <- solve(t(W) %*% W) %*% t(W) %*% Y
  o$corrected <- t(Y - W %*% alpha)
  return(o)
}


mod = model.matrix(~mort30, data=PhenoDF)
mod0 = model.matrix(~1,data=PhenoDF)

n.sv = num.sv(mat,mod,method="leek")

svobj = sva(mat,mod,mod0,n.sv=n.sv)

sva <- svaBatchCor(mat,mod,mod0,n.sv=3)
sva.pca <- PCA(t(sva$corrected),graph=FALSE)


fviz_pca_ind(sva.pca,
             label = "none", # hide individual labels
             habillage = as.factor(PhenoDF$mort30), # color by groups
             palette = c("#00AFBB", "#E7B800", "#FC4E07"),
             addEllipses = TRUE # Concentration ellipses
)

