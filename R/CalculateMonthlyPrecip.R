#' Calculate Monthly Precipitation
#'
#' @description
#' This function takes the climate dataset clim and turns it into an multidimentional array to then
#' output the average weekly precip per month across the years and the average weekly precipitation
#' across the years.
#' @param clim_data parameter to addthe climate dataset. Input the clim dataframe from this package
#' @return List with the following items:
#' \describe{
#' \item{Dataframe}{dataframe of average weekly precipitation per month across all years since 1942}
#' \item{Dataframe}{dataframe of average weekly precipitaiton for every year since 1942}
#' }
#' @author Sofie McComb & Margaux Sleckman
#'

CalculateMonthlyPrecip <- function(clim_data, full_clim_dataset = T){

# Clim dataframe modification

  clim_w_week <- clim_data %>%
  dplyr::mutate(week = ifelse(day <= 7, 1,
                       ifelse(day > 7 & day <= 14, 2,
                              ifelse(day > 14 & day <= 21, 3,
                                     ifelse(day > 21, 4, NA)
                                     )))) %>% transform(id=match(year, unique(year)))

years_working <-  length(unique(clim_w_week$year))-1
months_working <- length(unique(clim_w_week$month))
weeks_working <- length(unique(clim_w_week$week))

clim_array = array(dim = c(weeks_working, months_working, years_working))

## Populate with mean weekly values

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

# add  names if size is apprepriate
if(full_clim_dataset == T){

dimnames(clim_array) = list(c("week 1","week 2","week 3","week 4"),
                       c("Jan","Feb", "Mar", "Apr", "May", "Jun",
                          "Jul","Aug", "Sep", "Oct","Nov", "Dec"),
                       c(seq(from = 1942,to = 2015, by = 1))
                         )

}

## average precip by year
average_precip_by_year = apply(clim_array, c(3), mean)
average_precip_by_year <- as.data.frame(average_precip_by_year)


## average precip by month
average_precip_by_month = apply(clim_array, c(2), mean)
average_precip_by_month <- as.data.frame(average_precip_by_month)


return(list(avge_precipitation_month = average_precip_by_month,
            avge_precipitation_year = average_precip_by_year))

}

