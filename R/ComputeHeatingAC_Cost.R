#' Heating and Cooling Cost Function
#'
#' @description
#' This function takes historical climate data with daily minimum and maximum temperatures
#' and returns a summary of the daily cost of air conditioning (AC) and heating. The function computes
#' the average monthly cost of heating and AC, the mean daily cost of AC and Heating, and an
#' optional interactive graph of daily heating and AC costs across the months of the year. The
#' costs are based on the assumed equation described in the details section.
#'
#' @details
#' The equation used to calculate the heating and cooling cost goes as the following:
#' We assume the user will turn on the heater if the temperature maximum (tmax) is 8 degrees C. We
#' assume the user will turn on the AC if the temperature minimum (tmin) is 14 degrees.
#' Simply turning on the AC or the heater costs $10, and every additional degree change from the
#' baseline (8 C or 14 C) costs a specified price_per_degree. The default is 0.8, but this can be
#' changed by manipulating the appropriate function argument. Therefore, with a default of 0.8,
#' with every degree increase above or below the baseline temp, the cost goes up 0.8 cents.
#' For example, you will spend $10.8 if the average outdoor temp is 15 degrees.
#'
#' Function can be used to assess how changes in long-term climate can impact the cost of human
#' well-being through the change in costs of AC and heating.
#'
#' @param climatedata Data frame of climate data, including temperature, precipitation, water year, and dates
#' @param price_heating_per_degree Cost of every incremental change in heating from the baseline (8 degrees C)
#' @param price_cooling_per_degree Cost of every incremental change in cooling from the baseline (14 degrees C)
#' @param interactive_plot_selection If equals TRUE, the function returns an interactive plot of the daily temperature
#' regulation cost datapoints. The AC costs are in blue, the heating costs are in red.
#' @return List with the following items:
#' \describe{
#' \item{MonthlyDataframe}{Data frame of the average heating and AC costs per month based on daily climate data}
#' \item{mean_heating_cost}{Value representing the mean daily heating cost}
#' \item{mean_AC_cost}{Value representing the mean daily AC cost}
#' \item{Plot}{Interactive plot of costs of heating and cooling by month}
#' }
#' @author Sofie McComb & Margaux Sleckman
#'

ComputeHeatingAC_Cost = function(climatedata,
                                 price_heating_per_degree = 0.8,
                                 price_cooling_per_degree = 0.8,
                                 interactive_plot_selection = F){

  #Compute the daily heating and cooling costs in relation to daily average temperature
   clim_cost <- climatedata %>%
    dplyr::mutate(average_temp = ((tmin+tmax)/2)) %>%
    dplyr::mutate(AC_cost_daily = ifelse(tmin >= 14, (10 + abs(14 - average_temp * price_cooling_per_degree)), 0)) %>%
    dplyr::mutate(heating_cost_daily = ifelse(tmax <= 8, (10 + abs(8 - average_temp * price_heating_per_degree)), 0)) %>%
    dplyr::mutate(month_name = month.name[month])

  #Create df of daily AC and heating values
  daily_AC_heating <- clim_cost %>%
    dplyr::select(date, year, month,
                  AC_cost_daily, heating_cost_daily,
                  month_name) %>%
    dplyr::mutate(month_name = month.abb[month])

  #Summarize average daily heating and cooling cost by month (averaged across years)
  AC_heating_per_month <- clim_cost %>%
    dplyr::group_by(month_name) %>%
    dplyr::summarise(average_daily_AC_cost = mean(AC_cost_daily),
                     average_daily_heating_cost = mean(heating_cost_daily))

  #Summarize Average heating and cooling cost overall (average across months and years)
  meanHeating_cost <- mean(AC_heating_per_month$average_daily_heating_cost)
  meanAC_cost <- mean(AC_heating_per_month$average_daily_AC_cost)

  #Subset dataset for graphing, where costs cannot equal zero
  AC_heating_per_month_for_plot <- subset(daily_AC_heating,
                                          AC_cost_daily != 0 | heating_cost_daily != 0)

  #Graph the monthly heating and cooling costs
  plot <- ggplot2::ggplot(AC_heating_per_month_for_plot)+
    ggiraph::geom_point_interactive(ggplot2::aes(x=month_name,  y = AC_cost_daily,
                                                 tooltip = date), col="blue", alpha = 0.3)+
    ggiraph::geom_point_interactive(ggplot2::aes(x=month_name,  y = heating_cost_daily, tooltip = date), col = "red", alpha = 0.3)+
    ggplot2::theme_classic()+
    ggplot2::xlab("Month")+
    ggplot2::xlim(month.abb)+
    ggplot2::ylab("Average cost of temperature regulation ($)")+
    ggplot2::labs(title = "Daily temperature regulation cost",
                  subtitle = "Hover over point for date\n")

  #Make plot interactive
  interactive_plot <- ggiraph::girafe(code = print(plot))

  #If user requests interactive plot, return the plot along with the dataframe and average costs of cooling and costs
    #Otherwise return everything but the plot (plot default=F as plot takes a significant amount of time to load)
  if(interactive_plot_selection == T){

    return(list(MonthlyDataframe = AC_heating_per_month,
                mean_heating_cost = meanHeating_cost,
                mean_AC_cost = meanAC_cost,
                Plot = interactive_plot
    ))

  }else{

    return(list(MonthlyDataframe = AC_heating_per_month,
                mean_heating_cost = meanHeating_cost,
                mean_AC_cost = meanAC_cost))
  }

}

