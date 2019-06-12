
library(ggplot2)
library(ggiraph)

source("R/CalculateMonthlyPrecip.R")
source('R/ComputeHeatingAC_Cost.R')


clim_test_data =
  as.data.frame(
    cbind(
      date = c("01/01/19",
               "02/01/19",
               "03/01/19",
               "04/01/19",
               "04/01/19"),
      tmin  = runif(min = -3, max = 8, n = 5),
      tmax  = runif(min = 8, max = 18, n = 5),
      rain  = rep(1, times = 5),
      year  = rep(1, times = 5),
      month = rep(1, times = 5),
      day   = rep(1, times = 5),
      wy = rep(1942, times = 5)))

)

clim_test_data <- clim_test_data %<>% mutate_if(is.factor,as.numeric)

clim_test_data<-sapply(clim_test_data, as.numeric)
str(clim_test_data)


expect_that(ComputeHeatingAC_Cost(clim_test_data)$meanHeating_cost, equals(15.55))


aircond_price = ifelse(~tmin >= 14, (10 + abs(14 - ~average_temp * ~price_cooling_per_degree
str(clim_test_data)


