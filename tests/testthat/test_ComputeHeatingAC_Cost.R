#Test that the compute heating and AC cost functions returns the correct values for a different test dataset
test_that("Test that the ComputeHeatingAC_Cost functions performs calculations correctly and returns correct values",{
#Create climate dataset for testing
  clim_test_data =
  as.data.frame(
    cbind(
      date = c("01/01/19",
               "02/01/19",
               "03/01/19",
               "04/01/19",
               "04/01/19"),
      tmin  = c(1,5,8,9,4),
      tmax  = c(10,12,17,18,10),
      rain  = rep(1, times = 5),
      year  = rep(1, times = 5),
      month = rep(1, times = 5),
      day   = rep(1, times = 5),
      wy = rep(1942, times = 5)))
clim_test_data <- clim_test_data %>% dplyr::mutate_if(is.factor,as.numeric)
#Perform test_that evaluations
testthat::expect_that(ComputeHeatingAC_Cost(clim_test_data)$mean_heating_cost, equals(15.92))
testthat::expect_that(ComputeHeatingAC_Cost(clim_test_data)$mean_AC_cost, equals(0))

})


