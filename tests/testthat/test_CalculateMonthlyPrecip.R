
test_that("Calculate Monthly Precip", {
#dataset
clim_2 <- clim %>% filter(year < 1945)
#
CalculateMonthlyPrecip(clim_2, full_clim_dataset = F)$avge_precipitation_month[8,]


expect_true(CalculateMonthlyPrecip(clim_2, full_clim_dataset = F)$avge_precipitation_month[8,], equals(0))
expect_true(CalculateMonthlyPrecip(clim_2, full_clim_dataset = F)$avge_precipitation_month[12,], equals(3.37774643))

})

