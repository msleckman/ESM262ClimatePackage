#' Heating and Cooling Cost Function
#'
#' @description
#' This function takes historical climate data (clim) with daily minimum and maximum temperatures #' and returns a summary of the daily cost of air conditioning (AC) and heating. The function
#' the average monthly cost of heating and AC, the mean daily cost of AC and Heating, and an
#' optional interactive graph of daily heating and AC costs across the months of the year. The
#' costs are based on the assumed equation described below.
#'
#' @details
#' The equation used to calculate the heating and cooling cost goes as the following:
#' We assume the user will turn on heater if the temperature maximum (tmax) is 8 degrees C. We
#' assume the user will turn on AC if the temperature minimum (tmin) is 14 degrees.
#' Simply turning on the AC or the heater costs $10, and every additional degree above the
#' baseline (8 C or 14 C) costs a specified price_per_degree. The default is 0.8, but this can be
#' change by manipulating the appropriate param. This means that with every degree increase above
#' or below the baseline temp, the cost goes up 0.8 cents.
#' So, you will spend $10.8 if the average outdoor temp is 15 degrees.
#'
#' @param climatedata This is where you put in a climate dataset.Clim tfrom this package is the
#' recommended dataset to use, as the dataset must have the same variables as clim
#' @param price_heating_per_degree This is the cost of every incremental increas in heating due
#' to the difference in actual heating with the baseline (8 degrees C).
#' @param price_cooling_per_degree Same as above, but for cooling (14 degrees C)
#' @param interactive_plot_selection If T, the function returns a plot of the daily temperature
#' regulation cost datapoints. The AC costs are in blue, the heating costs are in red.
#' @return List with the following items
#' \describe{
#' \item{Dataframe}{Data frame of month names and the average heating and AC costs for that month
#' based on the daily historical climate data}
#' \item{mean_heating_cost}{1 number representing the mean daily heating cost}
#' \item{mean_AC_cost}{1 number representing the mean daily AC cost}
#' \item{Plot}{Plot of costs of heating and cooling across months}
#' }
#' @author Sofie McComb & Margaux Sleckman
#'

ComputeHeatingAC_Cost = function(climatedata,
                                 price_heating_per_degree = 0.8, price_cooling_per_degree = 0.8,
                                 interactive_plot_selection = F){
  clim_cost <- climatedata %>%
    dplyr::mutate(average_temp = ((tmin+tmax)/2)) %>%
    dplyr::mutate(AC_cost_daily = ifelse(tmin >= 14, (10 + abs(14 - average_temp * price_cooling_per_degree)), 0)) %>%
    dplyr::mutate(heating_cost_daily = ifelse(tmax <= 8, (10 + abs(8 - average_temp * price_heating_per_degree)), 0)) %>%
    dplyr::mutate(month_name = month.name[month])


  daily_AC_heating <- clim_cost %>%
    dplyr::select(date, year, month, AC_cost_daily, heating_cost_daily, month_name) %>%
    dplyr::mutate(month_name = month.abb[month])


  AC_heating_per_month <- clim_cost %>%
    dplyr::group_by(month_name) %>%
    dplyr::summarise(average_monthly_AC_cost = mean(AC_cost_daily),
                     average_monthly_heating_cost = mean(heating_cost_daily))

  meanHeating_cost <- mean(AC_heating_per_month$average_monthly_heating_cost)
  meanAC_cost <- mean(AC_heating_per_month$average_monthly_AC_cost)

  AC_heating_per_month_for_plot <- subset(daily_AC_heating, AC_cost_daily != 0 | heating_cost_daily != 0)

  plot <- ggplot2::ggplot(AC_heating_per_month_for_plot)+
    ggiraph::geom_point_interactive(ggplot2::aes(x=month_name,  y = AC_cost_daily,
                                                 tooltip = date), col="blue", alpha = 0.3)+
    ggiraph::geom_point_interactive(ggplot2::aes(x=month_name,  y = heating_cost_daily, tooltip = date), col = "red", alpha = 0.3)+
    ggplot2::theme_classic()+
    ggplot2::xlab("Month")+
    ggplot2::xlim(month.abb)+
    ggplot2::ylab("Average cost of temperature regulation ($)")+
    ggplot2::labs(title = "Daily temperature regulation cost between 1942-2015",
                  subtitle = "Hover over point for date\n")

  interactive_plot <- ggiraph::girafe(code = print(plot))

  if(interactive_plot_selection == T){

    return(list(MonthlyDataframe = AC_heating_per_month,
                mean_heating_cost = meanHeating_cost,
                mean_AC_cost = meanAC_cost,
                Plot = interactive_plot
    ))

  }

  else{

    return(list(Dataframe = AC_heating_per_month,
                mean_heating_cost = meanHeating_cost,
                mean_AC_cost = meanAC_cost))
  }

}
