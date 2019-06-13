#' Evaluate Species Specific Temperature Stress
#'
#' @description
#' Function to calculate temperature stress status for each individual species on the basis of air temperature values.
#'
#' @details
#' Temperature stress evaluted by computing the number of annual climate stress days,
#' or the number of days per year where tmax >86 deg F (>30 deg C).
#' Air temperatures of >86 deg F (>30 deg C) are considered high stress temperatures for most plant species (see references).
#' If the number of species tolerable temperature stress days <= climate stress days,
#' than the species stressed column equals yes (species faces temperature stress), otherwise it equals no.
#' There is a unique stressed column for each species, with the species identifier as the column name header.
#' Plant characteristic information and climate tolerance information is sourced from the USDA.
#'
#' This function can be used to assess which species face reoccurring temperature stress in the region,
#' and may need additional support.
#'
#' @param climdata Data frame of climate data, including temperature, precipitation, water year, and dates
#' @param speciesdf Data frame of species temperature and precipitation characteristics
#' @return
#' \describe{
#' \item{StressDF}{Data frame including the number of annual high temperature stress days and species temperature stress status by year}
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
    dplyr::group_by(year) %>%
    dplyr::summarize(stressdays=sum(tmax >= 30,na.rm = T))

  #Format species dataframe in wideformat to combine with climate data
  species_wide <- speciesdf %>%
    dplyr::select(Species, StressDays) %>%
    tidyr::spread(Species, StressDays)

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
