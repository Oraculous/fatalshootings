pacman::p_load(FactoMineR, paran, ggdendro, ggplot2, Gifi, factoextra)

# Categorical Principle Component Analysis
fitnorm <- princals(ca_df_final, levels = "nominal")  ## nominal PCA
fitnorm
summary(fitnorm)

# plot(fitnorm, plot.type = "transplot")

results <- fitnorm$scoremat


fviz_nbclust(results, FUNcluster=kmeans) 

# compute gap statistic for kmeans
fviz_nbclust(results, FUNcluster=kmeans, method="gap_stat", k.max = 7)+ theme_classic()

# Distance metrics - Euclidean
km1<-eclust(results, "kmeans", hc_metric="eucliden",k=3)
km2<-eclust(results, "kmeans", hc_metric="manhattan",k=3)

fviz_silhouette(km1)
fviz_silhouette(km2) 

fviz_nbclust(results, FUNcluster=cluster::pam)

# compute gap statistic
fviz_nbclust(results, FUNcluster=cluster::pam, method="gap_stat")+ theme_classic()

pam1<-eclust(results, "pam", k=4) # factoextra::
# pam1<-eclust(scaled_ca_df_final, "pam", k=10) # factoextra::

fviz_silhouette(pam1)

# Hieracichal clustering
dm<-dist(results) 
hc<-hclust(dm, method="complete") # simple dendrogram
ggdendrogram(hc, rotate = TRUE, theme_dendro = FALSE)

