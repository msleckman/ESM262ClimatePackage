#' Calculate Average Monthly and Annual Precipitation
#'
#' @description
#' This function transforms a climate dataset input into a multidimentional array
#' and then performs calculations on the array to
#' output the average weekly precipitation per month across the years (summarize by months)
#' and the average weekly precipitation across the years (summarize by years).
#'
#' @details
#' To perform these calculations, a weeks column was added to the climate dataframe, as the number of days varies across the months.
#' Four weeks were defined per month, with the first three weeks each having 7 days, and the last week containing the rest of the
#' days in the month, often approximately 10 days. The difference in the number of days per week was determined to not have a
#' significant impact on the average weekly precipitation. Precipitation given in mm.
#'
#' This function can be used to increase understanding of changes in mean precipitation levels over time,
#' both monthly and over the years, in order to inform regional hydrologic decion-making, particularly water availability.
#'
#' @param clim_data Data frame of climate data, including temperature, precipitation, water year, and dates
#' @param full_clim_dataset If equals TRUE, then the function accepts the full sample clim dataset
#' and will print dimnames appropriate for the size of that dataframe
#' @return List with the following items:
#' \describe{
#' \item{avge_precipitation_month}{Dataframe of average weekly precipitation (mm) per month across all years}
#' \item{avge_precipitation_year}{Dataframe of average weekly precipitaiton (mm) for every year}
#' }
#'
#' @author Sofie McComb & Margaux Sleckman
#'

CalculateMonthlyPrecip <- function(clim_data, full_clim_dataset = T){

# Clim_data modification to add column for weeks

  clim_w_week <- clim_data %>%
  dplyr::mutate(week = ifelse(day <= 7, 1,
                       ifelse(day > 7 & day <= 14, 2,
                              ifelse(day > 14 & day <= 21, 3,
                                     ifelse(day > 21, 4, NA)
                                     )))) %>% transform(id=match(year, unique(year)))

years_working <-  length(unique(clim_w_week$year))-1
months_working <- length(unique(clim_w_week$month))
weeks_working <- length(unique(clim_w_week$week))

#create empty multidimensional array of climate dataframe
clim_array = array(dim = c(weeks_working, months_working, years_working))

## Populate the array with numeric mean weekly values drawn from the climate dataframe

for (weeks in 1:weeks_working){
  for (months in 1:months_working){
    for (years in 1:years_working){

        value <- clim_w_week %>%
          dplyr::filter(week == weeks & month == months & id == years) %>%
          dplyr::summarise(weekrain=mean(rain, na.rm=T))

        clim_array[weeks, months, years]  = as.numeric(value)

      }
    }
  }

# Add dimension names if the dataframe is the same dimensions as the example clim dataset
  #Functionality added as an example of how the dataframe should formatted by the user for other datasets
if(full_clim_dataset == T){

dimnames(clim_array) = list(c("week 1","week 2","week 3","week 4"),
                       c("Jan","Feb", "Mar", "Apr", "May", "Jun",
                          "Jul","Aug", "Sep", "Oct","Nov", "Dec"),
                       c(seq(from = 1942,to = 2015, by = 1))
                         )

}

#Perform array calculations

## average precip by month
average_precip_by_month = apply(clim_array, c(2), mean)
average_precip_by_month <- as.data.frame(average_precip_by_month)

## average precip by year
average_precip_by_year = apply(clim_array, c(3), mean)
average_precip_by_year <- as.data.frame(average_precip_by_year)


return(list(avge_precipitation_month = average_precip_by_month,
            avge_precipitation_year = average_precip_by_year))

}

