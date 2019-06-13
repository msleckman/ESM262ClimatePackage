#Test that stress days per year are less than or equal to 365 days and more than or equal to 0 days a year
testthat::test_that("Testing that stress days fall within the possible days per yer range", {
  #Create new 1 line sample dataset (also a good test that other sample datasets perform correctly)
  trees <- as.data.frame(matrix(c("TREE",
                                  "Genus Species",
                                  "Trees",
                                  "None",
                                  1,
                                  100,
                                  1000),
                                ncol=7, byrow=FALSE), stringsAsFactors = FALSE)
  colnames(trees)<-c("Species","Scientific", "Common", "DroughtTolerance",
                     "StressDays", "PrecipMin", "PrecipMax")
  trees$StressDays<-as.numeric(trees$StressDays)
  trees$PrecipMin<-as.numeric(trees$PrecipMin)
  trees$PrecipMax<-as.numeric(trees$PrecipMax)
  #Run function on climate and species datasets
  res<-Calculate_TempStressIndex(clim, trees)
  #Perform tests on each unique value
  for (i in 1:nrow(res)){
    testthat::expect_true(res$stressdays[i]<=365)
    testthat::expect_true(res$stressdays[i]>=0)
    }#end for loop
  })#end test that


