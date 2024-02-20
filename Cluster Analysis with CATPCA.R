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

# Categorical Principle Component Analysis












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
