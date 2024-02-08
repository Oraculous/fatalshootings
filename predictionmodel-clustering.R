pacman::p_unload(data.world, dwapi, dplyr, stringr, readr, naniar, data.table, 
                 ggplot2, cowplot)
pacman::p_load(caret, factoextra, cluster, dplyr, homals, seriation, plyr, 
               corrr, ggcorrplot, FactoMineR, paran, ggdendro, ggplot2)

set.seed(123)

# Lets treat the variables again for before proceeding with the cluster analysis
# To recap we have 4 levels of factors for gender
table(df$gender)

# Eight levels for race
table(df$race)

# Fifty-Two for state
table(df$state)

# Seven for highest level of force
table(df$highest.level.of.force)

# Three for armed/unarmed
table(df$`armed/unarmed`)

# Fourteen for alleged weapon
table(df$alleged.weapon)

# Three for fleeing or not fleeing
table(df$fleeing.or.not.fleeing)

# And three for foreknowledge of mental illness
table(df$foreknowledge.of.mental.illness)

# By looking at the above we can classify some of these variables into an ordinal scale
# Namely highest.level.of.force, armed/unarmed, fleeing.or.not.fleeing, and foreknowledge.of.mental.illness
# Lets see how we can convert these variables into ordered numeric's
df$highest.level.of.force_score <- revalue(df$highest.level.of.force,
                                            c("Gunshot"="1", "Vehicle"="1", "Tasered"="1", "Less-than-lethal force"="2", 
                                              "Medical emergency"="3", "Undetermined"="3", "Other" = "3"))

df$highest.level.of.force_score <- as.numeric(as.character(df$highest.level.of.force_score))


df$armed.or.unarmed_score <- revalue(df$`armed/unarmed`,
                                           c("Armed"="1", "Uncertain"="2", "Unarmed"="3"))

df$armed.or.unarmed_score <- as.numeric(as.character(df$armed.or.unarmed_score))


df$fleeing.or.not.fleeing_score <- revalue(df$fleeing.or.not.fleeing,
                                     c("Yes"="1", "Uncertain"="2", "No"="3"))

df$fleeing.or.not.fleeing_score <- as.numeric(as.character(df$fleeing.or.not.fleeing_score))

df$foreknowledge.of.mental.illness_score <- revalue(df$foreknowledge.of.mental.illness,
                                           c("Yes"="1", "Uncertain"="2", "No"="3"))

df$foreknowledge.of.mental.illness_score <- as.numeric(as.character(df$foreknowledge.of.mental.illness_score))

drop <- c("highest.level.of.force", "armed/unarmed", "fleeing.or.not.fleeing", "foreknowledge.of.mental.illness", "date.of.injury", "state")
ca_df = df[,!(names(df) %in% drop)]
colSums(is.na(ca_df))
rm(drop)

# df.ca <- df %>% subset(select = -c(state, date.of.injury))
 
# One-hot encoding
dummy <- dummyVars(~ gender + race + alleged.weapon + 
                     aggressive.physical.movement, data=ca_df)
newdata <- data.frame(predict(dummy, newdata = ca_df))
ca_df_final <- cbind(newdata, select(ca_df, -c("gender", "race", "alleged.weapon", "aggressive.physical.movement")))
scaled_ca_df_final <- scale(ca_df_final)
rm(dummy, newdata, new_df, ca_df)

# PCA
colSums(is.na(scaled_ca_df_final))
corr_matrix <- cor(scaled_ca_df_final)
ggcorrplot(corr_matrix)  

# ------ 
# Grouping - Males
ca_df_males <- ca_df_final[ -c(1,3,4,13:36) ] # Drop gender = Females, Transgender & Unkown, Aggressive Physical Movement & Alleged Weapon due to collineaerity with other variables
scaled_ca_df_final <- scale(ca_df_males)
corr_matrix <- cor(scaled_ca_df_final)
rm(df)

xxx.pca1<-prcomp(corr_matrix, center=FALSE, scale.=FALSE)
summary(xxx.pca1) 
# visusalisation of quality
fviz_eig(xxx.pca1, addlabels = T)  # eigenvalues on y-axis
fviz_pca_var(xxx.pca1, col.var="steelblue")# 

# Kaiser Criterion
# table of eigenvalues
eig.val<-get_eigenvalue(xxx.pca1)
eig.val

paran(scaled_ca_df_final, iterations=10000, quietly=FALSE,
      status=FALSE, all=TRUE, cfa=FALSE, graph=TRUE,
      color=TRUE, col=c("black","red","blue"),
      lty=c(1,2,3), lwd=1, legend=TRUE, file="",
      width=640, height=640, grdevice="png", seed=0, mat=NA, n=NA)

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

pam1<-eclust(results, "pam", k=3) # factoextra::
# pam1<-eclust(scaled_ca_df_final, "pam", k=10) # factoextra::

fviz_silhouette(pam1)

# Hieracichal clustering
dm<-dist(results) 
hc<-hclust(dm, method="complete") # simple dendrogram
ggdendrogram(hc, rotate = TRUE, theme_dendro = FALSE)

# -----
# Grouping - Every other gender
ca_df_other <- ca_df_final[ -c(2,13:36) ] # Drop gender = Males, Aggressive Physical Movement & Alleged Weapon due to collineaerity with other variables
scaled_ca_df_final <- scale(ca_df_other)
corr_matrix <- cor(scaled_ca_df_final)
rm(df)

xxx.pca2<-prcomp(corr_matrix, center=FALSE, scale.=FALSE)
summary(xxx.pca2) 
# visusalisation of quality
fviz_eig(xxx.pca2, addlabels = T)  # eigenvalues on y-axis
fviz_pca_var(xxx.pca2, col.var="steelblue")# 

# Kaiser Criterion
# table of eigenvalues
eig.val<-get_eigenvalue(xxx.pca2)
eig.val

# paran(scaled_ca_df_final, iterations=10000, quietly=FALSE,
#       status=FALSE, all=TRUE, cfa=FALSE, graph=TRUE,
#       color=TRUE, col=c("black","red","blue"),
#       lty=c(1,2,3), lwd=1, legend=TRUE, file="",
#       width=640, height=640, grdevice="png", seed=0, mat=NA, n=NA)

var<-get_pca_var(xxx.pca2)
a<-fviz_contrib(xxx.pca2, "var", axes=1, xtickslab.rt=90) # default angle=45°
plot(a,main = "Variables percentage contribution of first Principal Components")

var<-get_pca_var(xxx.pca2)
a<-fviz_contrib(xxx.pca2, "var", axes=2, xtickslab.rt=90) # default angle=45°
plot(a,main = "Variables percentage contribution of second Principal Components")

var<-get_pca_var(xxx.pca2)
a<-fviz_contrib(xxx.pca2, "var", axes=3, xtickslab.rt=90) # default angle=45°
plot(a,main = "Variables percentage contribution of third Principal Components")

var<-get_pca_var(xxx.pca2)
a<-fviz_contrib(xxx.pca2, "var", axes=4, xtickslab.rt=90) # default angle=45°
plot(a,main = "Variables percentage contribution of fourth Principal Components")

var<-get_pca_var(xxx.pca2)
a<-fviz_contrib(xxx.pca2, "var", axes=5, xtickslab.rt=90) # default angle=45°
plot(a,main = "Variables percentage contribution of fifth Principal Components")

var<-get_pca_var(xxx.pca2)
a<-fviz_contrib(xxx.pca2, "var", axes=6, xtickslab.rt=90) # default angle=45°
plot(a,main = "Variables percentage contribution of sixth Principal Components")

var<-get_pca_var(xxx.pca2)
a<-fviz_contrib(xxx.pca2, "var", axes=7, xtickslab.rt=90) # default angle=45°
plot(a,main = "Variables percentage contribution of seventh Principal Components")

var<-get_pca_var(xxx.pca2)
a<-fviz_contrib(xxx.pca2, "var", axes=8, xtickslab.rt=90) # default angle=45°
plot(a,main = "Variables percentage contribution of eighth Principal Components")

var<-get_pca_var(xxx.pca2)
a<-fviz_contrib(xxx.pca2, "var", axes=9, xtickslab.rt=90) # default angle=45°
plot(a,main = "Variables percentage contribution of ninth Principal Components")

var<-get_pca_var(xxx.pca2)
a<-fviz_contrib(xxx.pca2, "var", axes=10, xtickslab.rt=90) # default angle=45°
plot(a,main = "Variables percentage contribution of tenth Principal Components")

xxx.pca2<-prcomp(corr_matrix, center=FALSE, scale.=FALSE, rank. = 10) # stats::
results <- xxx.pca2$x

# # Cluster tendency
# clustend <- get_clust_tendency(scaled_ca_df_final, 100, seed = 123)
# # Hopkins statistic
# clustend$hopkins_stat 
# rm(clustend)
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

pam1<-eclust(results, "pam", k=3) # factoextra::
# pam1<-eclust(scaled_ca_df_final, "pam", k=10) # factoextra::

fviz_silhouette(pam1)

# Hieracichal clustering
dm<-dist(results) 
hc<-hclust(dm, method="complete") # simple dendrogram
ggdendrogram(hc, rotate = TRUE, theme_dendro = FALSE)

