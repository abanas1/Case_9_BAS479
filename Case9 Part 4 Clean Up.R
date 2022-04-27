
## POPULATION CSV ------------------------------
path <- "C:\\Users\\abiba\\OneDrive\\Documents\\Spring 2022\\BAS 479 - Allen\\Case_9_BAS479" #When Using, Set file path to your folder

install.packages("dlookr")
install.packages("visdat")
install.packages("plotly")
install.packages("missRanger")
install.packages("tidyverse")
install.packages("LaplacesDemon")
library(dlookr)
library(visdat)      
library(plotly)      
library(missRanger)
library(tidyverse)
library(regclass)


Case9Population <- read.csv("9.8 Population v1.9.csv")


## replacing homeownr NA's with the value 1 (per Allen in class Tuesday 4/26) :
Case9Population$homeownr <- ifelse(Case9Population$homeownr %in% NA, 1, ifelse(Case9Population$homeownr <= 0, 0, 1))


## imputing Final Age missing values 
  # using same Random Forest imputation used with sample data

#missRanger Random Forest Imputation :
Case9Population_Imputed <- missRanger(
  Case9Population,
  formula = . ~ . - FinalAge,
  num.trees = 75,
  seed = 479
)

#Original Population Data Set Histogram
hist(Case9Population$FinalAge, breaks = 100)

#Rounding values
Case9Population_Imputed$FinalAge <- round(Case9Population_Imputed$FinalAge, digits = 0)

#Imputed Population Data Histogram
hist(Case9Population_Imputed$FinalAge, breaks = 100)

#Combined Density Plot of Original vs. Imputer
plot(density(Case9Population_Imputed$FinalAge), col = "red")
lines(density(Case9Population$FinalAge[!is.na(Case9Population$FinalAge)]), col = "blue")
  # red is imputed, blue is original


## imputing missing values in the AgeCode column :
 # replacing NA's with the most common code ("W")
table(Case9Population_Imputed$AgeCode)

Case9Population_Imputed$AgeCode[Case9Population_Imputed$AgeCode == ""] <- "W"

table(Case9Population_Imputed$AgeCode) # No more blank Age Codes





## Finished imputed file with changes ::
path <- "C:\\Users\\abiba\\OneDrive\\Documents\\Spring 2022\\BAS 479 - Allen\\Case_9_BAS479" #When Using, Set file path to your folder
write.csv(Case9Population_Imputed, file.path(path, "Case9Population_Imputed.csv"), row.names = FALSE)










