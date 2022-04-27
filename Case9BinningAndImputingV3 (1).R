Case9Data <- Case9DataBinned



Case9Data$LastMailBin <- ifelse(Case9Data$LastMail %in% NA, 6, 
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


path <- "C:\\Users\\abiba\\OneDrive\\Documents\\Spring 2022\\BAS 479 - Allen\\Case_9_BAS479" #When Using, Set file path to your folder
write.csv(Case9Data, file.path(path, "Case9Data.csv"), row.names = FALSE)

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
path <- "C:\\Users\\abiba\\OneDrive\\Documents\\Spring 2022\\BAS 479 - Allen\\Case_9_BAS479" #When Using, Set file path to your folder
write.csv(Case9Data, file.path(path, "Case9DataBinned.csv"), row.names = FALSE)

path <- "C:\\Users\\abiba\\OneDrive\\Documents\\Spring 2022\\BAS 479 - Allen\\Case_9_BAS479" #When Using, Set file path to your folder
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
