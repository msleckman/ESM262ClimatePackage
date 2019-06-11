#' Function 5: Impact
#' @title
#' @description something about air conditioning and temperature (min or max or mean)
#' @param
#' @param
#' @param
#' @author Sofie McComb & Margaux Sleckman
#' @example
#' @return
#'


library(dplyr)
library(tidyverse)

heating_airtemp_function <- function(climatedata,
                                     price_heating_degree = 0.8, price_cooling_per_degree = 0.8,
                                     airconditioning = T, heating = T){

## $10 is the baseline if you turn on your heater  on or air conditioning
## We assume heater turns on when the tmax is below or equal to 8 degrees C.
## We assume airconditioning turns on if the tmin is greater or equal to 14 degees C.
## Therefore, baseline temp for air conditioning == 8 C
## Baseline temp for air conditioning == 14 C
## With every degree increase above or below the baseline temp, the cost goes up 0.8 cents. So, you will spend $10.8 if the average outdoor temp is 15 degrees.


clim_cost <- climatedata %>%
  mutate(average_temp = (tmin+tmax)/2) %>%
  mutate(aircond_price = ifelse(tmin >= 14, (10 + abs(14 - average_temp * price_cooling_per_degree)), 0)) %>%
  mutate(heating_price = ifelse(tmax <= 8, (10 + abs(8 - average_temp * price_heating_degree)), 0))


AC_heating_per_month <- clim_cost %>%
  group_by(date, year, month) %>%
  summarise(average_monthly_AC_cost = mean(aircond_price),
                                           average_monthly_heating_cost = mean(heating_price))


plot <- ggplot(AC_heating_per_month)+
  geom_point(aes(x=month,  y = average_monthly_AC_cost), col="blue", alpha = 0.3)+
  geom_point(aes(x=month,  y = average_monthly_heating_cost), col = "red", alpha = 0.3)+
  theme_classic()+
  xlab("Month")+
  ylab("average cost of temperature regulation")+
  labs(title = "Datapoints of Monthly temperature regulation cost between 1942-2015"
       # subtitle = sprintf(price_heating_degree, price_cooling_per_degree)
       )

# install.packages("plotly")
# library(plotly)
#
# gg <- ggplotly(plot)
#
# gg <- style(gg, line = list(color = 'gold'), hoverinfo = "date")
#
# gg

return(list(AC_heating_per_month, plot))
}


heating_airtemp_function(clim)
