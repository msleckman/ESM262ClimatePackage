#' Plot Temperature by Season
#'
#' @description
#' Function to calculate and plot maximum and minimum temperature by season across the years.
#'
#' @details
#' Seasons were defined as: Spring (March, April, May), Summer (June, July, August),
#' Fall (September, October, November), and Winter (December, January, February).
#'
#' @param climdata Data frame of climate data
#' @return List with the following items
#' \describe{
#' \item{ClimateDF}{Data frame of maximum and minimum temperature by season over the years}
#' \item{ClimatePlot}{Plot of maximum and minimum temperature over the years facet-wrapped by season}
#' }
#'
#' @examples
#' PlotTempBySeason(climdata=clim)
#'
#' @author
#' Sofie McComb & Margaux Sleckman

PlotTempBySeason=function(climdata){

#Remove the last year so to make sure dataframe ends with a full year of data
climdata<-climdata[climdata$year!=max(clim$year), ]

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
climplot<-ggplot2::ggplot(meanclim, aes(x=year)) +
  geom_line(aes(y=avgmaxtemp),colour="red") +
  geom_line(aes(y=avgmintemp),colour="skyblue") +
  facet_grid(~season)+
  theme_light()+
  labs(x = "Years",
       y = "Temperature (Celsius)")+
  theme(text = element_text(size=12),
        plot.title = element_text(size=14, face="bold"),
        plot.subtitle = element_text(size=12),
        plot.caption = element_text(size=10,hjust = 0))

#Return dataframe and ggplot
return(list(ClimateDF=meanclim, ClimatePlot=climplot))
}
