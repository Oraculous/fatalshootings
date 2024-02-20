# Fatal Encounters
## Introduction

D. Brian Burghart along with a multi-disciplinary team at the University of Southern California worked on a large-scale project to create an impartial, comprehensive, and searchable national database of people killed during their interactions with the police in the United States of America.
## Executive Summary

This repo consists of the data cleaning, analysis and inference of a dataset obtained from Fatal Encounters. It consists of 31,498 deceased individuals due to encounters with the police in the United States of America between 2019-12-31 and 2020-12-31.

THe purpose of this analysis is to test different types of dimensionaliy reduction techniques namely,
1. Principal Component Analysis (PCA)
2. Mulitple Correspondence Analysis (MCA)
3. Categorical Principal Component Analysis (CATPCA)

To identify groups within the dataset.


## Data Preparation

### Data Cleaning

94.44% of the dataset contained null values. Looking more into the variables themselves most of these null values are a result of redundant variables. Hence, the following were dropped for the sake of dimensionality that is covered later in the analysis. 

"...34","...33", "Unique ID formula", "URL Temp", "Description Temp",  "UID Temporary", "Name Temporary", "URL of image (PLS NO HOTLINKS)", "Race with imputations", "Imputation probability", "Unique identifier (redundant)", "Supporting document link", "Dispositions/Exclusions INTERNAL USE, NOT FOR ANALYSIS", "Unique ID", "Name", "Longitude", "Latitude", "Full Address", "Location of death (county)", "Location of death (zip code)", "Location of death (city)", "Location of injury (address)" , "Agency or agencies involved", "Brief description", "Intended use of force (Developing)"
 
<div align="center"><img width="451" alt="image" src="https://github.com/Oraculous/fatalshootings/assets/77309239/b55f25de-81d5-4936-91e7-b54453a77c75"></div>

Furthermore, it was also observed that due to the nature of the types of variables almost all are nominal categorical variables with multiple levels. However, some of these variables could be factorized and be put on the ordinal scale and the levels themselves. 

That being dealing with the nulls values within these levels were the next hurdle. To be respectful to the amount of effort and time it has taken to gather the data. This analysis did not remove any null values but rather changed the nature of the null values themselves. 

###	Target Encoding Nominal Variables & Dealing with Null Values

It was observed that some variables contained as high as 268 and low as 4 levels. In order for the unsupervised machine learning model to work without having to compute large dimensions after one-hot encoding, the option to target encode these variables were taken.

Let’s take an example, below is a sampled list of alleged weapons that were on the deceased individual. The way how the types of weapons are either classified as either edged weapons or firearms were most helpful to reduce the levels, but the null values were instead encoded as “Uncertain”, since it is neither the absence nor presence of a weapon itself but the inability to have a clear indication of either. Such was the methodology used to tone down the levels for each of the other nominal variables. The full visualizations of the target encoded variables are present in the appendix. 


<div align="center"><img width="248" alt="image" src="https://github.com/Oraculous/fatalshootings/assets/77309239/bc249cb8-9d3c-4d87-81c8-f8c76e85ba79"></div>



Moving on to the only numerical variable in this dataset, age, when observing the variables’ distribution, it is evident that it is slightly skewed to the right.

<div align="center"><img width="468" alt="image" src="https://github.com/Oraculous/fatalshootings/assets/77309239/61d6c962-bb27-4844-a4c0-e3d72002b2f8"></div>


When one thinks about the age of a person one would think it would be a whole number but, in this case, it is best left as ratio data. Why? Because context matters regarding these individuals. When looking at the granularity of the data in the opposing tail ends of the distribution – the youngest, 0.08 years old, had the following description, 

“Timothy Gilles sped away from St. Cloud police and crashing into a vehicle. The crash killed 30-year-old Timothy Casanova of Starbuck. His passenger, 22-year-old Megan Thomsen of Glenwood, was pregnant and gave birth after the crash to a baby that died days after his emergency delivery. Thomsen was also injured. He was sentenced to 432 months in prison.”
And the oldest, 107 years old, had the following description,

“Police arrived at a residence in response to reports of a disturbance. 107 y.o. Monroe Isadore allegedly confronted them with a handgun and retreated into a bedroom, firing on them when they attempted to enter. SWAT officers arrived as backup and, after failed negotiations, released gas into the room and broke down the door. Isadore fired on them as they entered, and they returned fire, killing him.”
But what of the missing values, before removing or imputing these variables let’s look at how the distribution is affected if we were to make these changes. 

<div align="center"><img width="468" alt="image" src="https://github.com/Oraculous/fatalshootings/assets/77309239/31a8d7e0-2619-40aa-a2e2-f3dc6720962c"></div>


The closest to the original distribution is noted to be imputing the mean over the median and this was what was chosen going forward into the analysis.

Looking at the distribution of the gender type it is evident that Males make up the most compared to Other, Transgenders, and Unknowns. In this case, these groups were grouped as Others. 

<div align="center"><img width="468" alt="image" src="https://github.com/Oraculous/fatalshootings/assets/77309239/ecd5eceb-cd7d-4fc1-9121-f42d322fa854"></div>


## Unsupervised Machine Learning

### Data Preprocessing
 
The main issue that this project will face when cluster analysis is conducted is the number of samples present in the dataset. For the analysis to effectively work the option of dimensionality reduction is a must to the one-hot encoding that will be done on some of these variables. However, since a majority of these variables contains a level “Uncertain” or “Unknown” it is difficult to classify these as ordinal so it was decided to be kept as categorical but nominal in scale. The tables below go into detail on how these levels were decided. 


<div align="center"><img width="494" alt="image" src="https://github.com/Oraculous/fatalshootings/assets/77309239/2f9c2778-5ee2-4aee-ac3c-63bc8aa55619"></div>
The rest of the variables were one-hot encoded. 
> [!NOTE]
> The reason why "0" isn't used as a level when converting the text into numerical is mainly due to how one of the dimensionality reduction technique i.e. Categorical Principal Correspondence Analysis considers "0" values as Null values. 

