

View(Case9Data)
# Hi here is our data

#binning WebUse column
Case9Data$WebUseBin <- ifelse(Case9Data$WebUse %in% NA, 0, ifelse(Case9Data$WebUse == 0, 0, 1))
