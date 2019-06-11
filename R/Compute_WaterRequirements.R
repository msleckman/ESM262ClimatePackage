#' Function to compute water requirement status for different species on the basis of species minimum and maximum precipitation characteristics
#'
#' Compute Annual Water Requirements per Species.
#' Water requirement status defined per species as insufficient (< precip min),
#' adequate (btwn precip min and max), and oversaturated (> precip max)
#' @param climdata data frame of climate data
#' @param speciesdf data frame of species temperature and precipitation characteristic data
#' @return dataframe of annual precipitation levels (mm) and species annual water requirement status
#'
#' @references
#' https://plants.usda.gov/
#' Plant characteristic information from the USDA (establish minimum and maximum tolerable precipitation levels per species in mm)

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
