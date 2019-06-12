#' Sample Plant Characteristic Data
#'
#' Sample dataset for various Santa Barbara plant species providing climate and growth-based plant characteristics.
#' Species information given by species identifier, scientific name, and common name.
#' Dataset provides drought tolerance capability and the number of days species can tolerate high heat stress days per year.
#' Data also reports minimum and maximum amount of precipitation the species can tolerate.
#' URLs provide source for using >86 degF (>30 degC) as the baseline temperature for stressful temperature levels
#' and for USDA plant characteristic information.
#' Species heat stress days derived from drought tolerance level.
#'
#' @format A data frame with 3 rows and 7 variables:
#' \itemize{
#' \item Species is the species identifier
#' \item Scientific is the species scientific name
#' \item Common is the species common name
#' \item DroughtTolerance is the drought tolerance capability (low, medium, high)
#' \item StressDays is the number of days species can tolerate high temperatures per year before becoming stressed
#' \item PrecipMin is the minimum precipitation tolerated by the species
#' \item PrecipMax is the maximum precipitation tolerated by the species
#' }
#'
#' @source \url{http://agron-www.agron.iastate.edu/courses/Agron541/classes/541/lesson04a/4a.2.html}
#' @source \url{https://plants.usda.gov/}
#'
"species"
