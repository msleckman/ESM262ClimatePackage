test_that("Calculate Monthly Precip", {
#dataset
clim_2 <- clim %>% dplyr::filter(year < 1945)
testthat::expect_true(CalculateMonthlyPrecip(clim_2, full_clim_dataset = F)$avge_precipitation_month[8,]==0)
testthat::expect_true(CalculateMonthlyPrecip(clim_2, full_clim_dataset = F)$avge_precipitation_month[12,]>=3.37)

})

