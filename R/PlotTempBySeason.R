#' Calculate and Plot Temperature by Season
#'
#' @description
#' Function to calculate and plot average maximum and minimum temperature by season across the years.
#'
#' @details
#' Seasons were defined as: Spring (March, April, May), Summer (June, July, August),
#' Fall (September, October, November), and Winter (December, January, February).
#' Temperature in is degrees Celsius.
#'
#' Function can be used to assess how average seasonal temperature extremes have changed annually,
#' which can provide important information for assessing how climate has potentially shifted in the region over time.
#'
#' @param climdata Data frame of climate data, including temperature, precipitation, water year, and dates
#' @return List with the following items
#' \describe{
#' \item{ClimateDF}{Data frame of maximum and minimum temperature by season over the years}
#' \item{ClimatePlot}{Plot of maximum and minimum temperature over the years facet-wrapped by season}
#' }
#'
#' @author
#' Sofie McComb & Margaux Sleckman

PlotTempBySeason=function(climdata){

#Remove the last year so to make sure dataframe ends with a full year of data
climdata<-climdata[climdata$year!=max(climdata$year), ]

# Add column with seasons Spring, Summer, Fall, and Winter based on Month
climdata$season<-""
for (i in 1: nrow(climdata)){
  if(climdata$month[i]==3|climdata$month[i]==4|climdata$month[i]==5){
    climdata$season[i]<-"Spring"
  }else if (climdata$month[i]==6|climdata$month[i]==7|climdata$month[i]==8){
    climdata$season[i]<-"Summer"
  }else if (climdata$month[i]==9|climdata$month[i]==10|climdata$month[i]==11){
    climdata$season[i]<-"Fall"
  }else if (climdata$month[i]==12|climdata$month[i]==1|climdata$month[i]==2){
    climdata$season[i]<-"Winter"
  }else
    return("Error with setting seasonality")
}#end of for loop

#Get the mean max and min temp per season per year
meanclim <- climdata %>%
  dplyr::group_by(year, season) %>%
  dplyr::summarise(avgmaxtemp = mean(tmax, na.rm = T),
                   avgmintemp=mean(tmin, na.rm=T))

#Plot the max and min seasonal temp per year
climplot<-ggplot2::ggplot(meanclim, ggplot2::aes(x=year)) +
  ggplot2::geom_line(ggplot2::aes(y=avgmaxtemp),colour="red") +
  ggplot2::geom_line(ggplot2::aes(y=avgmintemp),colour="skyblue") +
  ggplot2::facet_grid(~season)+
  ggplot2::theme_light()+
  ggplot2::labs(title="Average Maximum and Minimum Seasonal Temperatures",
                x = "Years", y = "Temperature (Celsius)")

#Return dataframe and ggplot
return(list(ClimateDF=meanclim, ClimatePlot=climplot))
}
