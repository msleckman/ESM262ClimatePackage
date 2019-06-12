test_that("ComputeHeatingAC_Cost works",{

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

testthat::expect_that(ComputeHeatingAC_Cost(clim_test_data)$meanHeating_cost, equals(16))
testthat::expect_that(ComputeHeatingAC_Cost(clim_test_data)$meanAC_cost, equals(0))

})


