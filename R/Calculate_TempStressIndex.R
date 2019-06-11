#' Function to calculate temperature stress index for different species on the basis of air temperature
#'
#' Calculate Temperature Stress Index per Species.
#' Compute climate stress days, or the number of days per year where tmax >86 deg F (>30 deg C).
#' If species stress days <= climate stress days, than the species stressed column equals yes, otherwise it equals no.
#' @param climdata data frame of climate data
#' @param speciesdf data frame of species temperature stress data
#' @return dataframe of species temperature stress status by year and the number of high temperature stress days that year
#'
#' @references
#' http://agron-www.agron.iastate.edu/courses/Agron541/classes/541/lesson04a/4a.2.html
#' Air temperatures of >86 deg F (>30 deg C) are considered high stress temperatures for most plant species

###################################################################

Calculate_TempStressIndex=function(climdata, speciesdf){

  #Remove the last year so to make sure dataframe ends with a full year of data
  climdata<-climdata[climdata$year!=max(climdata$year), ]

  #Sum the number of days each year with max temp >86F (30C)
  sumtemp<- climdata %>%
    group_by(year) %>%
    summarize(stressdays=sum(tmax >= 30,na.rm = T))

  #Format species dataframe in wideformat to combine with climate data
  species_wide <- species %>%
    select(Species, StressDays) %>%
    spread(Species, StressDays)

  #Combine with climate data
  species_clim<-cbind(sumtemp, species_wide)

  #Determine if species is stressed in each year based on number of stress days tolerated vs number of high stress days that year
  for (y in 1:nrow(species_clim)){
    for(s in 3:ncol(species_clim)){
      #If species stress days <= climate stress days, than that column equals yes, otherwise equals no
      ifelse(species_clim[y,s]<=species_clim[y,2], species_clim[y,s]<-"yes", species_clim[y,s]<-"no")
    }
  }

  #Return dataframe of species temperature stress status by year
  return(StressDF=species_clim)

}
