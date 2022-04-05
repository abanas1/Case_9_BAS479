

View(Case9Data)
# Hi here is our data

#binning WebUse column
Case9Data$WebUseBin <- ifelse(Case9Data$WebUse %in% NA, 0, ifelse(Case9Data$WebUse == 0, 0, 1))
Case9Data$canordersBin <- ifelse(Case9Data$canorders == 0, 0, 1)
Case9Data$PrdoCatD_Bin <- ifelse(Case9Data$ProdCatD %in% NA, 0, ifelse(Case9Data$ProdCatD<=100,1, 2))
Case9Data$ProdCatE_Bin <- ifelse(Case9Data$ProdCatE %in% NA, 0, ifelse(Case9Data$ProdCatE<=100,1, 2))
