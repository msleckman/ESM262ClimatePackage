#Test that the calculate monthly precipitation function works correctly by returning the correct value for a certain year
testthat::test_that("Test that CalculateMonthlyPrecip returns the correct values", {
#Subset the data for one year
clim_2 <- clim %>% dplyr::filter(year < 1945)
#Perform test_that evaluations on a subset (not full) clim dataset
testthat::expect_true(CalculateMonthlyPrecip(clim_2, full_clim_dataset = F)$avge_precipitation_month[8,]==0)
testthat::expect_true(CalculateMonthlyPrecip(clim_2, full_clim_dataset = F)$avge_precipitation_month[12,]>=3.37)

})

