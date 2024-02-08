# Load 
pacman::p_load(data.world, dwapi, dplyr, stringr, readr, naniar, data.table, 
               ggplot2, cowplot, outliers)

# Fatal Encounters 
# Founded site by D. Brian and other stakeholders who collect analyse and impute data to create a public dataset recording fatal police shooting with the United States of America.
# loading new data - Data Cleaning

new_df <- read_csv("FATAL ENCOUNTERS DOT ORG SPREADSHEET (See Read me tab) - Form Responses.csv")
summary(new_df)

# upload new dataset into data.world
dwapi::upload_data_frame(data_frame = new_df, owner_id = 'x8x',dataset_id = 'healthanalysis', file_name = 'fe_dataset_uncleaned.csv')

# The dataset contains 31,498 records as opposed to 8,000 records on the previous dataset
pct_miss_var(new_df)
gg_miss_var(new_df) # lets get rid of the variables that are blank

drop <- c("...34","...33", "Unique ID formula", "URL Temp", "Description Temp", 
          "UID Temporary", "Name Temporary", "URL of image (PLS NO HOTLINKS)", 
          "Race with imputations", "Imputation probability", "Unique identifier (redundant)", 
          "Supporting document link", "Dispositions/Exclusions INTERNAL USE, NOT FOR ANALYSIS", 
          "Unique ID", "Name", "Longitude", "Latitude", "Full Address", "Location of death (county)", 
          "Location of death (zip code)", "Location of death (city)", "Location of injury (address)" , 
          "Agency or agencies involved", "Brief description", "Intended use of force (Developing)")
df = new_df[,!(names(new_df) %in% drop)]
rm(drop)

str(df)

# Rename col names
setnames(df, old = c("Age",                                                           
                    "Gender",                                                         
                    "Race",                                                           
                    "Date of injury resulting in death (month/day/year)",            
                    "State",                                                          
                    "Highest level of force",                                         
                    "Armed/Unarmed",                                                  
                    "Alleged weapon",                                                 
                    "Aggressive physical movement",                                   
                    "Fleeing/Not fleeing",                             
                    "Foreknowledge of mental illness? INTERNAL USE, NOT FOR ANALYSIS"), 
         new = c('age','gender', 'race', 'date.of.injury', 'state', 'highest.level.of.force', 'armed/unarmed',
                 'alleged.weapon', 'aggressive.physical.movement', 'fleeing.or.not.fleeing',
                 'foreknowledge.of.mental.illness'))

str(df)

df$alleged.weapon = str_extract(df$alleged.weapon, '([^/]+)')
df$aggressive.physical.movement = str_extract(df$aggressive.physical.movement, '([^/.]+)')
df$fleeing.or.not.fleeing = str_extract(df$fleeing.or.not.fleeing, '([^/.]+)')
df$highest.level.of.force = str_extract(df$highest.level.of.force, '([^/]+)')

df$gender[is.na(df$gender)] <- 'Unknown'
df$state[is.na(df$state)] <- 'Unknown'

df$race[df$race == "european-American/White"] <- "European-American/White"
df$race[df$race == "European-American/European-American/White"] <- "European-American/White"
df$race[df$race == "African-American/Black African-American/Black Not imputed"] <- "African-American/Black"
df$race[df$race == "Christopher Anthony Alexander"] <- NA
df$race[is.na(df$race)] <- 'Uncertain'

df$`armed/unarmed`[is.na(df$`armed/unarmed`)] <- 'Uncertain'
df$`armed/unarmed`[df$`armed/unarmed` == "Duplicate of 9914"] <- 'Uncertain'
df$`armed/unarmed`[df$`armed/unarmed` == "Duplicate of 13419?"] <- 'Uncertain'
df$`armed/unarmed`[df$`armed/unarmed` == "None"] <- 'Unarmed'
df$`armed/unarmed`[df$`armed/unarmed` == "Arrmed"] <- 'Armed'
df$`armed/unarmed`[df$`armed/unarmed` == "Gunshot"] <- 'Armed'
df$`armed/unarmed`[is.na(df$`armed/unarmed`)] <- 'Uncertain'
df$`armed/unarmed`[df$`armed/unarmed` == "Duplicate of 13457?"] <- 'Uncertain'

df$alleged.weapon[df$alleged.weapon == "Blunt Object"] <- 'Blunt object'
df$alleged.weapon[df$alleged.weapon == "Duplicate of 9914"] <- 'Uncertain'
df$alleged.weapon[df$alleged.weapon == "Duplicate of 13419?"] <- 'Uncertain'
df$alleged.weapon[df$alleged.weapon == "Edged wapon"] <- 'Edged weapon'
df$alleged.weapon[df$alleged.weapon == "Edged Weapon"] <- 'Edged weapon'
df$alleged.weapon[df$alleged.weapon == "Fiream"] <- 'Firearm'
df$alleged.weapon[df$alleged.weapon == "Stun Gun"] <- 'Stun gun'
df$alleged.weapon[df$alleged.weapon == "Duplicate of 13457?"] <- 'Uncertain'
df$alleged.weapon[is.na(df$alleged.weapon)] <- 'Uncertain'

df$aggressive.physical.movement[df$aggressive.physical.movement == "Reached to waist"] <- 'Reached for waist'
df$aggressive.physical.movement[df$aggressive.physical.movement == "Police allege vehicular assault"] <- 'None'
df$aggressive.physical.movement[df$aggressive.physical.movement == "Duplicate of 13419?"] <- 'Uncertain'
df$aggressive.physical.movement[df$aggressive.physical.movement == "Duplicate of 13457?"] <- 'Uncertain'
df$aggressive.physical.movement[df$aggressive.physical.movement == "Duplicate of 9914"] <- 'Uncertain'
df$aggressive.physical.movement[df$aggressive.physical.movement == "Sudden Threatening Movement"] <- 'Sudden threatening movement'
df$aggressive.physical.movement[df$aggressive.physical.movement == "Advanced toward(s) officers"] <- 'Advanced on officer(s)'
df$aggressive.physical.movement[df$aggressive.physical.movement == "Advanced toward officer(s)"] <- 'Advanced on officer(s)'
df$aggressive.physical.movement[df$aggressive.physical.movement == "Brandishing weapon"] <- 'Brandished weapon'
df$aggressive.physical.movement[df$aggressive.physical.movement == "Edged weapon"] <- 'Used weapon'
df$aggressive.physical.movement[df$aggressive.physical.movement == "Self-inflicted Injury"] <- 'Self-inflicted injury'
df$aggressive.physical.movement[df$aggressive.physical.movement == "Advanced upon officer(s)"] <- 'Advanced on officer(s)'
df$aggressive.physical.movement[df$aggressive.physical.movement == "Used Weapon"] <- 'Used weapon'
df$aggressive.physical.movement[is.na(df$aggressive.physical.movement)] <- 'Uncertain'

df$fleeing.or.not.fleeing[df$fleeing.or.not.fleeing == "Motorcycle"] <- 'Yes'
df$fleeing.or.not.fleeing[df$fleeing.or.not.fleeing == "Foot"] <- 'Yes'
df$fleeing.or.not.fleeing[df$fleeing.or.not.fleeing == "Firearm"] <- 'Uncertain'
df$fleeing.or.not.fleeing[df$fleeing.or.not.fleeing == "Duplicate of 13419?"] <- 'Uncertain'
df$fleeing.or.not.fleeing[df$fleeing.or.not.fleeing == "Duplicate of 9914"] <- 'Uncertain'
df$fleeing.or.not.fleeing[df$fleeing.or.not.fleeing == "Duplicate of 13457?"] <- 'Uncertain'
df$fleeing.or.not.fleeing[df$fleeing.or.not.fleeing == "Vehicle"] <- 'Yes'
df$fleeing.or.not.fleeing[df$fleeing.or.not.fleeing == "Not Fleeing"] <- 'No'
df$fleeing.or.not.fleeing[df$fleeing.or.not.fleeing == "Fleeing"] <- 'Yes'
df$fleeing.or.not.fleeing[df$fleeing.or.not.fleeing == "Fleeling"] <- 'Yes'
df$fleeing.or.not.fleeing[df$fleeing.or.not.fleeing == "Not fleeing"] <- 'No'
df$fleeing.or.not.fleeing[df$fleeing.or.not.fleeing == "None"] <- 'Uncertain'
df$fleeing.or.not.fleeing[is.na(df$fleeing.or.not.fleeing)] <- 'Uncertain'

df$foreknowledge.of.mental.illness[is.na(df$foreknowledge.of.mental.illness)] <- 'Unknown'
df$foreknowledge.of.mental.illness[df$foreknowledge.of.mental.illness == "Drug or alcohol use"] <- 'Yes'
df$foreknowledge.of.mental.illness[df$foreknowledge.of.mental.illness == "Unknown"] <- 'Uncertain'

df$highest.level.of.force[df$highest.level.of.force == "Restrain"] <- 'Other'
df$highest.level.of.force[df$highest.level.of.force == "Asphyxiation"] <- 'Other'
df$highest.level.of.force[is.na(df$highest.level.of.force)] <- 'Undetermined'
df$highest.level.of.force[df$highest.level.of.force == "Chemical agent"] <- 'Other'
df$highest.level.of.force[df$highest.level.of.force == "Burned"] <- 'Other'
df$highest.level.of.force[df$highest.level.of.force == "Stabbed"] <- 'Other'
df$highest.level.of.force[df$highest.level.of.force == "Drowned"] <- 'Other'
df$highest.level.of.force[df$highest.level.of.force == "Beaten"] <- 'Other'
df$highest.level.of.force[df$highest.level.of.force == "Drug overdose"] <- 'Other'
df$highest.level.of.force[df$highest.level.of.force == "Fell from a height"] <- 'Other'
df$highest.level.of.force[df$highest.level.of.force == "Asphyxiated"] <- 'Other'


# Gender
ggplot(df, aes(x=reorder(gender, gender, function(x)-length(x)))) +
  geom_bar(fill='red') +  labs(x='Gender')

# Race
ggplot(df, aes(x=reorder(race, race, function(x)-length(x)))) +
  geom_bar(fill='blue') +  labs(x='Race') +
  coord_flip() 

# State
ggplot(df, aes(x=reorder(state, state, function(x)-length(x)))) +
  geom_bar(fill='blue') +  labs(x='State') +
  coord_flip()

# Armed/Unarmed
ggplot(df, aes(x=reorder(`armed/unarmed`, `armed/unarmed`, function(x)-length(x)))) +
  geom_bar(fill='blue') +  labs(x='Armed/Unarmed') +
  coord_flip()

# Aggressive physical movement
ggplot(df, aes(x=reorder(aggressive.physical.movement, 
                         aggressive.physical.movement, 
                         function(x)-length(x)))) +
  geom_bar(fill='blue') +  labs(x='Aggressive Physical Movement') +
  coord_flip()

# Fleeing or not fleeing
ggplot(df, aes(x=reorder(fleeing.or.not.fleeing, 
                         fleeing.or.not.fleeing, function(x)-length(x)))) +
  geom_bar(fill='blue') +  labs(x='Fleeing or Not Fleeing') +
  coord_flip()


ggplot(df, aes(x=reorder(foreknowledge.of.mental.illness, foreknowledge.of.mental.illness, function(x)-length(x)))) +
  geom_bar(fill='blue') +  labs(x='Foreknowledge of Mental Illness') +
  coord_flip()

# Age
ggplot(df, aes(age)) +
  geom_histogram(fill = "#0099F8") +
  ggtitle("Variable distribution") +
  theme(plot.title = element_text(size = 18))

value_imputed <- data.frame(
  original = df$age,
  imputed_zero = replace(df$age, is.na(df$age), 0),
  imputed_mean = replace(df$age, is.na(df$age), mean(df$age, na.rm = TRUE)),
  imputed_median = replace(df$age, is.na(df$age), median(df$age, na.rm = TRUE))
)
value_imputed

h1 <- ggplot(value_imputed, aes(x = original)) +
  geom_histogram(fill = "#ad1538", position = "identity") +
  ggtitle("Original distribution")
h2 <- ggplot(value_imputed, aes(x = imputed_zero)) +
  geom_histogram(fill = "#15ad4f", position = "identity") +
  ggtitle("Zero-imputed distribution") 
h3 <- ggplot(value_imputed, aes(x = imputed_mean)) +
  geom_histogram(fill = "#1543ad", position = "identity") +
  ggtitle("Mean-imputed distribution") 
h4 <- ggplot(value_imputed, aes(x = imputed_median)) +
  geom_histogram(fill = "#ad8415", position = "identity") +
  ggtitle("Median-imputed distribution") 
plot_grid(h1, h2, h3, h4, nrow = 2, ncol = 2)

# Based on the above I decided to impute the mean instead of the median for the NA values
df$age[is.na(df$age)] <- mean(df$age, na.rm = T)
# Now lets take a look at the outlier's. 
ggplot(df) +
  aes(x = "", y = age) +
  geom_boxplot(fill = "#0c4c8a") +
  theme_minimal()

summary(df$age) # the minimum age appears to be 0.08 which cant be right
age_sub <- new_df %>% subset(Age <= 7 | Age >= 100, select = c("Age","Brief description"))
age_sub<- age_sub[order(age_sub$Age),]
View(age_sub) # Comparing the descriptions from the original data set with the age it makes sense that there floating ages are legitimate. As some these individuals were infants.
# So no further cleaning will be done to this variable

rm(h1,h2,h3,h4,value_imputed, age_sub)

# highest level of force
ggplot(df, aes(x=reorder(highest.level.of.force, highest.level.of.force, function(x)-length(x)))) +
  geom_bar(fill='magenta') +  labs(x='Highest Level of Force') +
  coord_flip()

gg_miss_var(df)
pct_miss_var(df) 

# change dtypes
df$date.of.injury<- as.Date(df$date.of.injury , format = "%m/%d/%y")
cols <- c('gender', 'race', 'state', 'highest.level.of.force', 'armed/unarmed',
          'alleged.weapon', 'aggressive.physical.movement', 'fleeing.or.not.fleeing',
          'foreknowledge.of.mental.illness')
df[cols] <- lapply(df[cols], factor)
rm(cols)

summary(df)

dwapi::upload_data_frame(data_frame = df, owner_id = 'x8x',dataset_id = 'healthanalysis', file_name = 'fe_dataset_cleaned.csv')

