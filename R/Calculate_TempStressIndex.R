#' Evaluate Species-specific Temperature Stress
#'
#'#'@description
#' Function to calculate temperature stress status for each individual species on the basis of air temperature values.
#'
#' @details
#' Temperature stress evaluted by computing the number of climate stress days,
#' or the number of days per year where tmax >86 deg F (>30 deg C).
#' Air temperatures of >86 deg F (>30 deg C) are considered high stress temperatures for most plant species (see references).
#' If the number of species tolerable stress days <= climate stress days,
#' than the species stressed column equals yes (species faces temperature stress), otherwise it equals no.
#' Plant characteristic information and climate tolerance sourced from the USDA.
#'
#' Function can be used to assess which species face greater temperature stress in the region,
#' and may need additional support.
#'
#' @param climdata Data frame of climate data
#' @param speciesdf Data frame of species temperature and precipitation characteristics
#' @return
#' \describe{
#' \item{StressDF}{Data frame including the number of high temperature stress days that year and species temperature stress status by year}
#'}
#' @references
#' http://agron-www.agron.iastate.edu/courses/Agron541/classes/541/lesson04a/4a.2.html;
#' https://plants.usda.gov/
#'
#'
#' @author
#' Sofie McComb & Margaux Sleckman

###################################################################

Calculate_TempStressIndex=function(climdata, speciesdf){

  #Remove the last year so to make sure dataframe ends with a full year of data
  climdata<-climdata[climdata$year!=max(climdata$year), ]

  #Sum the number of days each year with max temp >86F (30C)
  sumtemp<- climdata %>%
    dplyr::group_by(~year) %>%
    dplyr::summarize(stressdays=sum(~tmax >= 30,na.rm = T))

  #Format species dataframe in wideformat to combine with climate data
  species_wide <- speciesdf %>%
    dplyr::select(~Species, ~StressDays) %>%
    tidyr::spread(~Species, ~StressDays)

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
