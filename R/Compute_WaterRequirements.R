#' Compute Species Annual Water Status
#'
#'@description
#' Function to compute water requirement status for different species on the basis of species
#' minimum and maximum precipitation characteristics.
#'
#'@details
#' Water requirement status defined per species as insufficient (annual precipitation < species precip min),
#' adequate (annual precipitation between species precip min and max), and oversaturated (annual precipitation >
#' species precip max).
#' Plant characteristic information from the USDA (establish minimum and maximum tolerable precipitation levels per species in mm).
#'
#' Function can be used to assess how well the water requirements of regional species are being met,
#' and the status can serve as a flag to any potential water use issues for maintaining species health and biodiversity.
#'
#' @param climdata Data frame of climate data
#' @param speciesdf Data frame of species temperature and precipitation characteristic data
#' @return Dataframe of annual precipitation levels (mm) and species annual water requirement status
#'
#' @references
#' https://plants.usda.gov/
#'
#' @example
#' Compute_WaterRequirements(climdata=clim, speciesdf=species)
#'
#' @author
#' Sofie McComb & Margaux Sleckman

###################################################################

Compute_WaterRequirements=function(climdata, speciesdf){

  #Remove the last year so to make sure dataframe ends with a full year of data
  climdata<-climdata[climdata$year!=max(climdata$year), ]

  #Sum the annual precipitation
  sumprecip<- climdata %>%
    group_by(year) %>%
    summarize(annualprecip=sum(rain,na.rm = T))

  #Add columns for species with NA values to fill in and cbind with climate data
  speciesnam<-setNames(data.frame(matrix(ncol = length(speciesdf$Species), nrow = nrow(sumprecip))), speciesdf$Species)
  species_climate<-cbind(sumprecip,speciesnam)

  #Compare annual precip to species min/max values to set water requirements status
  for (y in 1:nrow(species_climate)){
    annprecip<-as.numeric(species_climate[y,2])
    for (s in 1:nrow(speciesdf)){
      sp<-speciesdf[s, c("PrecipMin", "PrecipMax")]
      ifelse(annprecip<sp$PrecipMin, species_climate[y,s+2]<-"Insufficient",
             ifelse(annprecip>sp$PrecipMax, species_climate[y,s+2]<-"Saturated",
                    species_climate[y,s+2]<-"Adequate"))
    }#End species for loop
  }#End year for loop

  #Return dataframe of annual species water requirements status
  return(WaterReqDF=species_climate)
}
