
if (!requireNamespace("BiocManager", quietly = TRUE))
  install.packages("BiocManager")
BiocManager::install("tidyverse")
library(tidyverse)
BiocManager::install("stringr")


library(readxl)
rawdata <- read_excel("data/rawdata.xlsx")
View(rawdata)
head(rawdata)
class(rawdata)

library(dplyr)
adult <- select(rawdata, SYMBOL, contains("adult")) 
adult <- rename(adult, "AT1_1" = "Hopx-GFP_adult_1", "AT1_2" = "Hopx-GFP_adult_2", "AT2_1" = "Sftpc-GFP_adult_1", "AT2_2" = "Sftpc-GFP_adult_2")


head(adult)
class(adult)

-----------------

# standardization TMM (edgeR), RLE (DESeq/DESeq2)

# standardization with edgeR
if (!requireNamespace("BiocManager", quietly = TRUE))
  install.packages("BiocManager")
BiocManager::install("edgeR")

library(edgeR)

count <- as.matrix(adult[,2:5])
vec <- as.vector(adult$SYMBOL)
rownames(count) <- vec

# make group object to recognize each group
group <- factor(c("A", "A", "B", "B"))

# Design "full model" matrix
design <- model.matrix(~ group)
design


# analyze using edgeR
# edgeR stores data in a simple list-based data object called a DGElist

res <- DGEList(counts = count, group = group)
res <- calcNormFactors(res, method="TMM")  # calculate normalization factors
# normalizes for RNA composition by determining a set of scaling factors for the library sizes that minimize the log-fold changes between samples
res$samples

res <- estimateDisp(res, design) # estimate dispersions using the designed matrix
res <- estimateCommonDisp(res)
res <- estimateTagwiseDisp(res)

norm_counts.table <- t(t(res$pseudo.counts)*(res$samples$norm.factors))


# DEG analysis using edgeR
fit <- glmFit(res, design)
lrt <- glmLRT(fit, coef = 2)
topTags(lrt)

topTags(lrt, 100, adjust.method="BH", sort.by="PValue")

deg_lrt <- as.data.frame(topTags(lrt, n = nrow(count)))

min_lfc = 1
deg = decideTestsDGE(lrt, adjust.method="BH", p.value=0.01, lfc=min_lfc)
de_tags = rownames(lrt)[as.logical(de)]


--------

# DEG detection by using limma
library(limma)

count <- norm_counts.table

# create design matrix
data.cl <- c(rep(1, 2), rep(2, 2))
design <- model.matrix(~ as.factor(data.cl))

# estimate the fold changes and standard errors by fitting a linear model for each gene
fit <- lmFit(count, design)

# apply empirical Bayes smoothing to the standard errors.
out <- eBayes(fit)

# p.value <- out$p.value[,ncol(design)]
# q.value <- p.adjust(p.value, method="BH")
# ranking <- rank(p.value)

results <- topTable(out,coef=colnames(design)[ncol(design)], adjust="BH",number= nrow(count))

write.table(results, "results.txt", sep="\t", append=F, quote=F, row.names=F)


# dataframe into matrix
# count <- as.matrix(adult[,2:5])
# vec <- as.vector(adult$SYMBOL)
# rownames(count) <- vec


---------

# Genes upregulated in AT1
AT1highexp <- filter(results, logFC < -1 & adj.P.Val < 0.05)
dim (AT1highexp)

write.table(AT1highexp, "AT1high.txt", sep="\t", append=F, quote=F, row.names=F)

# check AT1 markers
filter (AT1highexp, ID %in% c("Ager", "Aqp5", "Pdpn"))

------------

# Install clusterprofiler from BioConductor. 

if (!requireNamespace("BiocManager", quietly = TRUE))
  install.packages("BiocManager")

BiocManager::install("clusterProfiler")
BiocManager::install("enrichplot")
install.packages("ggnewscale")
BiocManager::install("pathview")
BiocManager::install("org.Mm.eg.db")
BiocManager::install("org.Hs.eg.db")

library(tidyverse)
library(clusterProfiler)
library(org.Mm.eg.db)
library(org.Hs.eg.db)
# problem?  https://support.bioconductor.org/p/9136239/

options(connectionObserver = NULL)
library(org.Mm.eg.db)

gene.df <- bitr(AT1highexp$ID, fromType = "SYMBOL", toType = c("ENSEMBL", "ENTREZID"), OrgDb = "org.Mm.eg.db")

keytypes(org.Mm.eg.db)

genes <- AT1highexp$ID
genes <- pull(AT1highexp, ID)

ego <-  enrichGO(
    gene      = genes,
    universe      = adult$SYMBOL,
    OrgDb         = org.Mm.eg.db,
    keyType = 'SYMBOL',
    ont           = "BP",
    pAdjustMethod = "BH",
    pvalueCutoff  = 0.01,
    qvalueCutoff  = 0.05,
    readable=F
  )


ego.result <- ego@result
write_csv(ego.result, file='GO_results.csv')

write.table(ego.result, "GO_results.txt", sep="\t", append=F, quote=F, row.names=F)



library(enrichplot)
library(patchwork)
#Barplot and dotplots are very common ways to show GO data
barplot(ego, showCategory=20)


-----------

# Gene Set Enrichment

dataGSEA <- dplyr::select(rawdata, ENTREZID, contains("adult")) 
dataGSEA <- dplyr::rename(dataGSEA, "AT1_1" = "Hopx-GFP_adult_1", "AT1_2" = "Hopx-GFP_adult_2", "AT2_1" = "Sftpc-GFP_adult_1", "AT2_2" = "Sftpc-GFP_adult_2")
dataGSEA <- dplyr::filter(dataGSEA, dataGSEA$ENTREZID != "NA")

FC <- log2(rowMeans(dataGSEA[, c(2,3)])) - log2(rowMeans(dataGSEA[, c(4,5)]))

geneList <- setNames(FC,dataGSEA$ENTREZID)
geneList = sort(geneList, decreasing = TRUE)


gse <- gseGO (geneList     =  geneList,
              OrgDb        = org.Mm.eg.db::org.Mm.eg.db,
              ont          = "BP",
              nPerm        = 1000,
              minGSSize    = 100,
              maxGSSize    = 500,
              pvalueCutoff = 0.05,
              verbose      = FALSE)


gse <- gseGO (geneList     =  geneList,
              OrgDb        = org.Mm.eg.db::org.Mm.eg.db,
              ont          = "BP",
              minGSSize    = 100,
              maxGSSize    = 500,
              pvalueCutoff = 0.05,
              verbose      = FALSE)

gse.result <- gse@result
write.table(gse@result, "gseresult.txt", sep="\t", append=F, quote=F, row.names=F)

gseaplot(gse, geneSetID = 77, by = "runningScore", title = gse$Description[77])
gseaplot2(gse, geneSetID = 77, title = gse$Description[77])


----------

# KEGG

kk<-  enrichKEGG(
    gene      = gene.df$ENTREZID,
    universe      = rawdata$ENTREZID,
    organism     = 'mmu',
    pvalueCutoff = 0.05,
    )

kk.result <- kk@result
write_csv(kk.result,file='KEGG_results.csv')

#Plot the kegg results

browseKEGG(kk, kk.result$ID[1])

#Plot using pathview 
if (!requireNamespace("BiocManager", quietly = TRUE))
  install.packages("BiocManager")

BiocManager::install("pathview")

library(pathview)


pathview(gene.data  = geneList,
         pathway.id =  "mmu04510",
         species    = "mmu",
         limit      = list(gene=max(abs(geneList)), cpd=1))



BiocManager::install("EnrichmentBrowser")

------------
 
# if interested in genes included in the specific KEGG pathway
http://www.gsea-msigdb.org/gsea/login.jsp

library(readr)
kegg_genes <- read_delim("Rclub/c2.cp.kegg.v7.4.symbols.gmt.txt", "\t", escape_double = FALSE, col_names = FALSE, trim_ws = TRUE)

library(dplyr)
focal_adhesion <- filter (kegg_genes, X1 == "KEGG_FOCAL_ADHESION")

library(tidyverse)
library(stringr)

focal_adhesion <- str_to_title(focal_adhesion)
focal_adhesion <- focal_adhesion [3:length(focal_adhesion)]

---------------
# Heatmap using gplots
library(gplots)
focal_adhesion_exp <- filter (adult, SYMBOL %in% focal_adhesion)

m2b2g = colorRampPalette(c("blue","white", "red"))

focal_adhesion_exp_matrix <- as.matrix(focal_adhesion_exp[,2:5])
vec <- as.vector(focal_adhesion_exp$SYMBOL)
rownames(focal_adhesion_exp_matrix) <- vec
focal_adhesion_exp_matrix = log2(focal_adhesion_exp_matrix)

par(mfrow=c(1,1), ps=5)
heatmap.2(data.matrix(focal_adhesion_exp_matrix), col=m2b2g(1024),scale="row", key=TRUE, keysize=0.75, dendrogram="row", Colv=FALSE, Rowv=TRUE, symkey=FALSE, density.info="none", trace="none", cexCol=1.5, cexRow=1.5, margin=c(5,7), main="focal_adhesion") 

------------
# use ComplexHeatmap
library(tidyverse)
library(devtools)
library(ComplexHeatmap)
library(circlize)

focal.scale <- t(scale(t(focal_adhesion_exp_matrix)))
Heatmap(focal.scale,
        name='Expression',
        cluster_rows =T,
        cluster_columns = T
)


