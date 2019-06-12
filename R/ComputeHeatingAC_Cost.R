#' Heating and Cooling Cost Function
#'
#' @description
#' This function takes climate data with daily minimum and maximum temperatures and returns the
#' daily cost of air conditions and heating.
#'
#' @details
#' The equation used to calculate the heating and cooling cost goes as the following: We assume the
#' user will turn on heating
#'  if the temperature maximum is 8 degrees C. We assume the user will turn on AC if the temperatu
#'  e minimum is 14 degrees.
#'  Simply turning on the AC or the heater costs $10, and every additional degree above the
#'  baseline (8 C or 14 C) costs a
#'  specified price_per_degree. The default is 0.8, but this can be change by manipulating the
#'  appropriate param.
#'  This means that with every degree increase above or below the baseline temp, the cost goes up
#'  .8 cents.
#'  So, you will spend $10.8 if the average outdoor temp is 15 degrees.
#'
#' @param climatedata This is where you put in a climate dataset.
#' @param price_heating_per_degree This is the cost of every incremental increasing in heating due #' to the difference in actual heating with the baseline.
#' @param price_cooling_per_degree Same as above, but for cooling.
#' @param interactive_plot_selection If T, the function returns a plot of the cost datapoints. The
#' AC costs are in blue, the heating costs are in red.
#' #' @return List with the following items
#' \describe{
#' \item{Dataframe}{Data frame of date, month, year, and the cost of heating and cooling for that
#' day}
#' \item{meanHeating_cost}{1 number representing the mean heating cost}
#' \item{meanAC_cost}{1 number representing the mean AC cost}
#' \item{Plot}{Plot of costs of heating and cooling across months}
#' }
#' @author Sofie McComb & Margaux Sleckman
#'

ComputeHeatingAC_Cost = function(climatedata,
                                 price_heating_per_degree = 0.8, price_cooling_per_degree = 0.8,
                                 interactive_plot_selection = F){

clim_cost <- climatedata %>%
    dplyr::mutate(average_temp = ((tmin+tmax)/2)) %>%
    dplyr::mutate(aircond_price = ifelse(tmin >= 14, (10 + abs(14 - average_temp * price_cooling_per_degree)), 0)) %>%
    dplyr::mutate(heating_price = ifelse(tmax <= 8, (10 + abs(8 - average_temp * price_heating_per_degree)), 0))

  AC_heating_per_month <- clim_cost %>%
    dplyr::group_by(date, year, month) %>%
    dplyr::summarise(average_monthly_AC_cost = mean(aircond_price),
                     average_monthly_heating_cost = mean(heating_price))


  meanHeating_cost <- mean(AC_heating_per_month$average_monthly_heating_cost)
  meanAC_cost <- mean(AC_heating_per_month$average_monthly_AC_cost)

  AC_heating_per_month_for_plot <- subset(AC_heating_per_month, average_monthly_AC_cost != 0 |
                                            average_monthly_heating_cost != 0)

  plot <- ggplot2::ggplot(AC_heating_per_month_for_plot)+
    ggiraph::geom_point_interactive(ggplot2::aes(x=month,  y = average_monthly_AC_cost,
                                                 tooltip = date), col="blue", alpha = 0.3)+
    ggiraph::geom_point_interactive(ggplot2::aes(x=month,  y = average_monthly_heating_cost, tooltip = date), col = "red", alpha = 0.3)+
    ggplot2::theme_classic()+
    ggplot2::xlab("Month")+
    ggplot2::ylab("average cost of temperature regulation ($)")+
    ggplot2::labs(title = "Datapoints of Monthly temperature regulation cost\n between 1942-2015"
         # subtitle = sprintf(price_heating_degree, price_cooling_per_degree)
    )
  # scale_x_discrete(labels=c("2.5" = "Feb", "5.0" = "Apr",
  # "7.5" = "July", "10.0" = "Oct", "12.5" = "Dec"))
  # plot

  interactive_plot <- ggiraph::girafe(code = print(plot))




  if(interactive_plot_selection == T){

    return(list(Dataframe = AC_heating_per_month,
                meanHeating_cost = meanHeating_cost,
                meanAC_cost = meanAC_cost,
           Plot = interactive_plot
                ))

  }

  else{

    return(list(Dataframe = AC_heating_per_month,
                meanHeating_cost = meanHeating_cost,
                meanAC_cost = meanAC_cost
           ))

  }

}

