#### Refer to this webpage ###
#http://www.sthda.com/english/articles/31-principal-component-methods-in-r-practical-guide/112-pca-principal-component-analysis-essentials/
# A short youtube tutorial: https://www.youtube.com/watch?v=FgakZw6K1QQ
# Goal:
# 1. Dimensionality reduction
# 2. Maximize the variance

### install packages
install.packages("factoextra")
install.packages("FactoMineR")
library("factoextra")
library(FactoMineR)


### Load the data
load("heart.RData")

### perform PCA
res.pca = PCA(df, ncp = 5, graph = FALSE)
print(res.pca)

### check the result 
## eigenvalues/variances (dim= min_size(sample, feature))
eig.val <- get_eigenvalue(res.pca)
head(eig.val)

## how many dimensions to choose?
# scree plot
fviz_eig(res.pca, addlabels = TRUE, ylim = c(0, 35))

## graph of variables
var <- get_pca_var(res.pca)
var
#coordinates
head(var$coord)
#correlation between variables and dimensions
head(var$cor) 
fviz_pca_var(res.pca, col.var = "black") 

# Contribution of variables
head(var$contrib)

## quality of representation
head(var$cos2, 4)
library("corrplot")
corrplot(var$cos2, is.corr=FALSE)

## contribution of variables
# Contributions of variables to PC1
fviz_contrib(res.pca, choice = "var", axes = 1, top = 10)
# Contributions of variables to PC2
fviz_contrib(res.pca, choice = "var", axes = 2, top = 10)

### Graph of individuals
## Extract the results for individuals
ind <- get_pca_ind(res.pca)
ind
labels = c(rep("NF", 10), rep("DCM", 10), rep("TTNtv", 10))
labels = factor(labels, levels = c("NF", "DCM", "TTNtv"))
df$labels = labels
#p1 = fviz_pca_ind(m_active.pca, geom.ind = "point", col.ind = m_active$Group, palette = c("#000000", "#0000ff", "#ff0000"), addEllipses = TRUE, legend.title = "Groups")
fviz_pca_ind(res.pca,
             geom.ind = "point", 
             col.ind = df$labels,
             palette = c("#000000", "#0000ff", "#ff0000"),
             addEllipses = TRUE, # Concentration ellipses,
             legend.title = "Group"
)

