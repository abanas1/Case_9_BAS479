

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
