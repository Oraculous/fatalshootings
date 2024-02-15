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


df$fleeing.or.not.fleeing[df$fleeing.or.not.fleeing == "Motorcycle"] <- 'Yes'

df$highest.level.of.force_score <- revalue(df$highest.level.of.force,
                                            c("Gunshot"="2", "Vehicle"="2", "Tasered"="2", "Less-than-lethal force"="1", 
                                              "Medical emergency"="0", "Undetermined"="0", "Other" = "0"))

df$highest.level.of.force_score <- as.numeric(as.character(df$highest.level.of.force_score))


df$armed.or.unarmed_score <- revalue(df$`armed/unarmed`,
                                           c("Armed"="2", "Uncertain"="1", "Unarmed"="0"))

df$armed.or.unarmed_score <- as.numeric(as.character(df$armed.or.unarmed_score))


df$fleeing.or.not.fleeing_score <- revalue(df$fleeing.or.not.fleeing,
                                     c("Yes"="2", "Uncertain"="1", "No"="0"))

df$fleeing.or.not.fleeing_score <- as.numeric(as.character(df$fleeing.or.not.fleeing_score))

df$foreknowledge.of.mental.illness_score <- revalue(df$foreknowledge.of.mental.illness,
                                           c("Yes"="2", "Uncertain"="1", "No"="0"))

df$foreknowledge.of.mental.illness_score <- as.numeric(as.character(df$foreknowledge.of.mental.illness_score))

df$gender <- as.numeric(as.character(df$gender))

df$gender <- revalue(df$gender, c("Male"="0", "Female"="1"))

df$gender <- as.numeric(as.character(df$gender))

drop <- c("highest.level.of.force", "armed/unarmed", "fleeing.or.not.fleeing", "foreknowledge.of.mental.illness", "date.of.injury", "state", "age")
ca_df = df[,!(names(df) %in% drop)]
colSums(is.na(ca_df))
rm(drop)

# df.ca <- df %>% subset(select = -c(state, date.of.injury))
 
# One-hot encoding
dummy <- dummyVars(~ race + alleged.weapon + 
                     aggressive.physical.movement, data=ca_df)
newdata <- data.frame(predict(dummy, newdata = ca_df))
ca_df_one <- cbind(newdata, select(ca_df, -c("race", "alleged.weapon", "aggressive.physical.movement")))
scaled_ca_df_final <- scale(ca_df_one)
# rm(dummy, newdata, new_df, ca_df)

colSums(is.na(scaled_ca_df_final))
corr_matrix <- cor(scaled_ca_df_final)
ggcorrplot(corr_matrix)  

ca_df_final <- ca_df_one[ -c(14:23) ]
ca_df_final[sapply(ca_df_final, is.numeric)] <- lapply(ca_df_final[sapply(ca_df_final, is.numeric)], as.factor)
str(ca_df_final)

# apply MCA
mca1 = MCA(ca_df_final, graph = FALSE)
results <- mca1$var$eta2
# table of eigenvalues
mca1$eig

# column coordinates
head(mca1$var$coord)

# number of categories per variable
cats = apply(ca_df_final, 2, function(x) nlevels(as.factor(x)))
cats
# data frames for ggplot
mca1_vars_df = data.frame(mca1$var$coord, Variable = rep(names(cats), 
                                                         cats))
mca1_obs_df = data.frame(mca1$ind$coord)

# plot of variable categories
ggplot(data = mca1_vars_df, aes(x = Dim.1, y = Dim.2, label = rownames(mca1_vars_df))) + 
  geom_hline(yintercept = 0, colour = "gray70") + geom_vline(xintercept = 0, 
                                                             colour = "gray70") + geom_text(aes(colour = Variable)) + ggtitle("MCA plot of variables using R package FactoMineR")

# MCA plot of observations and categories
ggplot(data = mca1_obs_df, aes(x = Dim.1, y = Dim.2)) + geom_hline(yintercept = 0, 
                                                                   colour = "gray70") + geom_vline(xintercept = 0, colour = "gray70") + geom_point(colour = "gray50", 
                                                                                                                                                   alpha = 0.7) + geom_density2d(colour = "gray80") + geom_text(data = mca1_vars_df, 
                                                                                                                                                                                                                aes(x = Dim.1, y = Dim.2, label = rownames(mca1_vars_df), colour = Variable)) + 
  ggtitle("MCA plot of variables using R package FactoMineR") + scale_colour_discrete(name = "Variable")

# default biplot in FactoMineR
plot(mca1)

fviz_nbclust(results, FUNcluster=kmeans) 

# compute gap statistic for kmeans
fviz_nbclust(results, FUNcluster=kmeans, method="gap_stat", k.max = 10)+ theme_classic()

# Distance metrics - Euclidean
km1<-eclust(results, "kmeans", hc_metric="eucliden",k=9)
km2<-eclust(results, "kmeans", hc_metric="manhattan",k=9)

fviz_silhouette(km1)
fviz_silhouette(km2) 

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


# ------ 
# Grouping - Males
# ca_df_males <- ca_df_final[ -c(10:23) ] # Drop gender = Other, Aggressive Physical Movement & Alleged Weapon due to its similarity with other variables
# scaled_ca_df_final <- scale(ca_df_final)
# corr_matrix <- cor(scaled_ca_df_final)
# ggcorrplot(corr_matrix)  
# rm(df)

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

# Grouping - Every other gender ----- 
# 

# 
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

