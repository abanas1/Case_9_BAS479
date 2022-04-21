

View(Case9Data)
# Hi here is our data

#binning WebUse column
Case9Data$WebUseBin <- ifelse(Case9Data$WebUse %in% NA, 0, ifelse(Case9Data$WebUse == 0, 0, 1))
Case9Data$canordersBin <- ifelse(Case9Data$canorders == 0, 0, 1)
Case9Data$ProdCatB_Bin <- ifelse(Case9Data$ProdCatB %in% NA, 0, ifelse(Case9Data$ProdCatB <= 100, 1, 2))
Case9Data$ProdCatC_Bin <- ifelse(Case9Data$ProdCatC %in% NA, 0, ifelse(Case9Data$ProdCatC<=100,1,2))
Case9Data$ProdCatD_Bin <- ifelse(Case9Data$ProdCatD %in% NA, 0, ifelse(Case9Data$ProdCatD<=100,1, 2))
Case9Data$ProdCatE_Bin <- ifelse(Case9Data$ProdCatE %in% NA, 0, ifelse(Case9Data$ProdCatE<=100,1, 2))
#Exporting CSV File
path <- "C:\\Users\\jakob\\Documents\\GitHub\\Case_9_BAS479" #When Using, Set file path to your folder
write.csv(Case9Data, file.path(path, "Case9DataBinned.csv"), row.names = FALSE)

###############################################
#Finding Outliers
###############################################

#Finding the z-score of every row in NumOrder
SD <- sd(Case9Data$NumOrder)
Dmean <- mean(Case9Data$NumOrder)
NumOrderZ <- (Case9Data$NumOrder - Dmean)/SD

#Saving the row numbers of all rows with a NumOrder Z-Score greater than 10,
#because only the most extreme outliers should be excluded
OutliersNumOrder <- which(NumOrderZ > 10 | NumOrderZ < -3) #48 outliers found in column

#Calling database without rows that were determined to have a NumOrder that is an extreme outlier 
Case9Data[-OutliersNumOrder,]

###############################################
#Checking for Multi-collinearity
###############################################
library(corrplot)
#Creating a subset of data with purely continuous variables
continuous <- Case9Data[, c("Age", "Age1", "Age2", "Age3", "canorders", "Code1", "Code2", "CustomOrders", "HHI", "items", "LastMail", "monthfrstord", "monthlastord", "NumOrder", "offord_12", "Q1", "Q2", "Q3", "Q4", "retitem_12", "WebUse")]
#Taking another subset so that only complete cases are shown
cc <- continuous[complete.cases(continuous),]
#Creating correlation plot to determine worst multi-collinearity
corrplot(cor(cc), type = 'lower')








##########################################################
#    MILESTONE 3
##########################################################

#binning variables
Case9Data$LastMailBins <- ifelse(Case9Data$LastMail %in% NA, 6, 
                                 ifelse(Case9Data$LastMail <= 6, 1, 
                                        ifelse(Case9Data$LastMail >6 & Case9Data$LastMail<= 12, 2, 
                                               ifelse(Case9Data$LastMail > 12 & Case9Data$LastMail <= 18, 3, 
                                                      ifelse(Case9Data$LastMail>18 &Case9Data$LastMail <= 24, 4, 5)))))

Case9Data$monthfrstordBins <- ifelse(Case9Data$monthfrstord %in% NA, 6, 
                                 ifelse(Case9Data$monthfrstord <= 6, 1, 
                                        ifelse(Case9Data$monthfrstord >6 & Case9Data$monthfrstord<= 12, 2, 
                                               ifelse(Case9Data$monthfrstord > 12 & Case9Data$monthfrstord <= 18, 3, 
                                                      ifelse(Case9Data$monthfrstord>18 &Case9Data$monthfrstord <= 24, 4, 5)))))

Case9Data$monthlastordBin <- ifelse(Case9Data$monthlastord %in% NA, 6, 
                               ifelse(Case9Data$monthlastord <= 6, 1, 
                                      ifelse(Case9Data$monthlastord <= 12, 2, 
                                             ifelse(Case9Data$monthlastord <= 18, 3, 
                                                    ifelse(Case9Data$monthlastord <= 24, 4, 
                                                           ifelse(Case9Data$monthlastord <= 30, 5, 
                                                                  Case9Data$monthlastord))))))


Case9Data <- read.csv("9.13 C9 Random Sample v1.92.csv", stringsAsFactors = TRUE)

Case9Data$WebUseBin <- ifelse(Case9Data$WebUse %in% NA, 0, ifelse(Case9Data$WebUse == 0, 0, 1))
Case9Data$canordersBin <- ifelse(Case9Data$canorders == 0, 0, 1)
Case9Data$ProdCatB_Bin <- ifelse(Case9Data$ProdCatB %in% NA, 0, ifelse(Case9Data$ProdCatB <= 100, 1, 2))
Case9Data$ProdCatC_Bin <- ifelse(Case9Data$ProdCatC %in% NA, 0, ifelse(Case9Data$ProdCatC<=100,1,2))
Case9Data$ProdCatD_Bin <- ifelse(Case9Data$ProdCatD %in% NA, 0, ifelse(Case9Data$ProdCatD<=100,1, 2))
Case9Data$ProdCatE_Bin <- ifelse(Case9Data$ProdCatE %in% NA, 0, ifelse(Case9Data$ProdCatE<=100,1, 2))
#Exporting CSV File


###############################################
#Finding Outliers
###############################################

#Finding the z-score of every row in NumOrder
SD <- sd(Case9Data$NumOrder)
Dmean <- mean(Case9Data$NumOrder)
NumOrderZ <- (Case9Data$NumOrder - Dmean)/SD

#Saving the row numbers of all rows with a NumOrder Z-Score greater than 10,
#because only the most extreme outliers should be excluded
OutliersNumOrder <- which(NumOrderZ > 10 | NumOrderZ < -3) #48 outliers found in column

#Calling database without rows that were determined to have a NumOrder that is an extreme outlier 
Case9Data[-OutliersNumOrder,]

###############################################
#Checking for Multi-collinearity
###############################################
library(corrplot)
#Creating a subset of data with purely continuous variables
continuous <- Case9Data[, c("Age", "Age1", "Age2", "Age3", "canorders", "Code1", "Code2", "CustomOrders", "HHI", "items", "LastMail", "monthfrstord", "monthlastord", "NumOrder", "offord_12", "Q1", "Q2", "Q3", "Q4", "retitem_12", "WebUse")]
#Taking another subset so that only complete cases are shown
cc <- continuous[complete.cases(continuous),]
#Creating correlation plot to determine worst multi-collinearity
corrplot(cor(cc), type = 'lower')


##########################################################
#    MILESTONE 3
##########################################################

#binning variables
Case9Data$LastMailBins <- ifelse(Case9Data$LastMail %in% NA, 6, 
                                 ifelse(Case9Data$LastMail <= 6, 1, 
                                        ifelse(Case9Data$LastMail >6 & Case9Data$LastMail<= 12, 2, 
                                               ifelse(Case9Data$LastMail > 12 & Case9Data$LastMail <= 18, 3, 
                                                      ifelse(Case9Data$LastMail>18 &Case9Data$LastMail <= 24, 4, 5)))))

Case9Data$monthfrstordBin <- ifelse(Case9Data$monthfrstord %in% NA, 6, 
                                     ifelse(Case9Data$monthfrstord <= 6, 1, 
                                            ifelse(Case9Data$monthfrstord >6 & Case9Data$monthfrstord<= 12, 2, 
                                                   ifelse(Case9Data$monthfrstord > 12 & Case9Data$monthfrstord <= 18, 3, 
                                                          ifelse(Case9Data$monthfrstord>18 &Case9Data$monthfrstord <= 24, 4, 5)))))

Case9Data$monthlastordBin <- ifelse(Case9Data$monthlastord %in% NA, 6, 
                                    ifelse(Case9Data$monthlastord <= 6, 1, 
                                           ifelse(Case9Data$monthlastord <= 12, 2, 
                                                  ifelse(Case9Data$monthlastord <= 18, 3, 
                                                         ifelse(Case9Data$monthlastord <= 24, 4, 
                                                                ifelse(Case9Data$monthlastord <= 30, 5, 
                                                                       Case9Data$monthlastord))))))

##########################################
#ALL CODE FOR MILESTONE 3
##########################################

#Case9Data <- read.csv("Case9DataBinnedV2.csv", stringsAsFactors = TRUE)

#Excluding original variables that have been binned
Case9Data <- subset(Case9Data, select = -c(canorders, ProdCatB, ProdCatC, ProdCatD, ProdCatE, WebUse) )
#Excluding more original variables that have been binned
Case9Data = subset(Case9Data, select = -c(LastMail, monthfrstord, monthlastord) )

#Excluding the rows with ZLabels specified in cleaning workflow
Case9Data2 <- subset(Case9Data, !(ZLabel %in% c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,32,33,35,36,37,38,39,40,41,42,43,44,45,46,47,48,41182)))
Case9Data3 <- subset(Case9Data2, !(ZLabel %in% c(20069,10031,56477,95000,21624,12476,53592,21624,28370,12476,76806,63697,93039,92680,44048)))

#Imputing FinalAge Variable using Age Variable

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

#missRanger Random Forest Imputation
Case9Data3Imputed <- missRanger(
  Case9Data3,
  formula = . ~ . - Age,
  num.trees = 75,
  seed = 479
)
#Original Data Set Histogram
hist(Case9Data3$Age, breaks = 100)
#Imputed Data Histogram
hist(Case9Data3Imputed$Age, breaks = 100)

Case9Data3Imputed$FinalAge <- round(Case9Data3Imputed$FinalAge, digits = 0)

#Combined Density Plot of Original vs. Imputer
plot(density(Case9Data3Imputed$Age), col = "red")
lines(density(Case9Data3$Age[!is.na(Case9Data3$Age)]), col = "blue")


colnames(Case9Data3Imputed)[colnames(Case9Data3Imputed)
                   %in% c("FinalAge")] <- c("Age")

#Writing CSV to Path
path <- "C:\\Users\\jakob\\Documents\\GitHub\\Case_9_BAS479" #When Using, Set file path to your folder
write.csv(Case9Data, file.path(path, "Case9DataBinned.csv"), row.names = FALSE)

path <- "C:\\Users\\brayd\\Desktop\\BAS 479\\Case 9\\Phase 3"
write.csv(Case9Data3Imputed, file.path(path, "Case9DataBinnedV3.csv"), row.names = FALSE)

#Using Random Forest to predict response variable (DO NOT RUN - TAKES DOZENS OF MINUTES)
library(randomForest)
library(caret)
forestgrid <- expand.grid(mtry=c(5))
set.seed(479)
FOREST <- train(Resp ~.,
               data = Case9Data3Imputed,
               method = "rf",
               ntree = 20,
               tuneGrid = forestgrid)
FOREST

Pred <- predict(FOREST, Case9Data3Imputed)
hist(Pred, breaks = 10000)
length(Pred[which(Pred > 0.2)])



