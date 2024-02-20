pacman::p_load(caret, factoextra, cluster, dplyr, homals, seriation, plyr, corrr, ggcorrplot, FactoMineR, paran, ggdendro, ggplot2)
### Principle Component Analysis -----
xxx.pca1<-prcomp(corr_matrix, center=FALSE, scale.=FALSE)
summary(xxx.pca1) 
# visusalisation of quality
fviz_eig(xxx.pca1, addlabels = T)  # eigenvalues on y-axis
fviz_pca_var(xxx.pca1, col.var="steelblue")# 

# Kaiser Criterion
# table of eigenvalues
eig.val<-get_eigenvalue(xxx.pca1)
eig.val

# paran(scaled_ca_df_final, iterations=1000, quietly=FALSE,
#       status=FALSE, all=TRUE, cfa=FALSE, graph=TRUE,
#       color=TRUE, col=c("black","red","blue"),
#       lty=c(1,2,3), lwd=1, legend=TRUE, file="",
#       width=640, height=640, grdevice="png", seed=0, mat=NA, n=NA)

var<-get_pca_var(xxx.pca1)
a<-fviz_contrib(xxx.pca1, "var", axes=1, xtickslab.rt=90) # default angle=45°
plot(a,main = "Variables percentage contribution of first Principal Components")

var<-get_pca_var(xxx.pca1)
a<-fviz_contrib(xxx.pca1, "var", axes=2, xtickslab.rt=90) # default angle=45°
plot(a,main = "Variables percentage contribution of second Principal Components")

var<-get_pca_var(xxx.pca1)
a<-fviz_contrib(xxx.pca1, "var", axes=3, xtickslab.rt=90) # default angle=45°
plot(a,main = "Variables percentage contribution of third Principal Components")

var<-get_pca_var(xxx.pca1)
a<-fviz_contrib(xxx.pca1, "var", axes=4, xtickslab.rt=90) # default angle=45°
plot(a,main = "Variables percentage contribution of fourth Principal Components")

var<-get_pca_var(xxx.pca1)
a<-fviz_contrib(xxx.pca1, "var", axes=5, xtickslab.rt=90) # default angle=45°
plot(a,main = "Variables percentage contribution of fifth Principal Components")

var<-get_pca_var(xxx.pca1)
a<-fviz_contrib(xxx.pca1, "var", axes=6, xtickslab.rt=90) # default angle=45°
plot(a,main = "Variables percentage contribution of sixth Principal Components")

var<-get_pca_var(xxx.pca1)
a<-fviz_contrib(xxx.pca1, "var", axes=7, xtickslab.rt=90) # default angle=45°
plot(a,main = "Variables percentage contribution of seventh Principal Components")

var<-get_pca_var(xxx.pca1)
a<-fviz_contrib(xxx.pca1, "var", axes=8, xtickslab.rt=90) # default angle=45°
plot(a,main = "Variables percentage contribution of eighth Principal Components")

var<-get_pca_var(xxx.pca1)
a<-fviz_contrib(xxx.pca1, "var", axes=9, xtickslab.rt=90) # default angle=45°
plot(a,main = "Variables percentage contribution of ninth Principal Components")

var<-get_pca_var(xxx.pca1)
a<-fviz_contrib(xxx.pca1, "var", axes=10, xtickslab.rt=90) # default angle=45°
plot(a,main = "Variables percentage contribution of tenth Principal Components")

xxx.pca1<-prcomp(corr_matrix, center=FALSE, scale.=FALSE, rank. = 10) # stats::
results <- xxx.pca1$x

# Cluster tendency
clustend <- get_clust_tendency(scaled_ca_df_final, 100, seed = 123)
# Hopkins statistic
clustend$hopkins_stat 
rm(clustend)
### Elbow method (look at the knee)
# Elbow method for kmeans
fviz_nbclust(results, FUNcluster=kmeans) 

# compute gap statistic for kmeans
fviz_nbclust(results, FUNcluster=kmeans, method="gap_stat", k.max = 10)+ theme_classic()

# Distance metrics - Euclidean
km1<-eclust(results, "kmeans", hc_metric="eucliden",k=10)
km2<-eclust(results, "kmeans", hc_metric="manhattan",k=10)

fviz_silhouette(km1)
fviz_silhouette(km2) 
### Elbow method (look at the knee)
# Elbow method for kmediod
# clustering with triangle graphics
fviz_nbclust(results, FUNcluster=cluster::pam)

# compute gap statistic
fviz_nbclust(results, FUNcluster=cluster::pam, method="gap_stat")+ theme_classic()

pam1<-eclust(results, "pam", k=2) # factoextra::
# pam1<-eclust(scaled_ca_df_final, "pam", k=10) # factoextra::

fviz_silhouette(pam1)

# Hieracichal clustering
dm<-dist(results) 
hc<-hclust(dm, method="complete") # simple dendrogram
ggdendrogram(hc, rotate = TRUE, theme_dendro = FALSE)