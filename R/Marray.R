#' Function 2: m-array
#' @title M-array function for mean weekly rainfall (across months, across years)
#' @description this function takes the climate dataset clim and turns it into an multidimentional array to then output the average weekly precip per month across the years and the average weekly precipitation across the years.
#' @param clim_data the climate dataset. Input the clim dataframe from this package
#' @author Sofie McComb & Margaux Sleckman
#' @example Marray_function(clim_data = clim)
#' @return a list that includes a dataframe of average weekly precipitation per month across all years and a dataframe of average weekly precipitaiton per year


Marray_function <-function(clim_data){

# Clim dataframe modification

  clim_w_week <- clim_data %>%
  mutate(week = ifelse(day <= 7, 1,
                       ifelse(day > 7 & day <= 14, 2,
                              ifelse(day > 14 & day <= 21, 3,
                                     ifelse(day > 21, 4, NA)
                                     )))) %>% transform(id=match(year, unique(year)))

clim_array = array(dim = c(4, 12, 74))

## Populate with mean weekly values

for (weeks in 1:4){
  for (months in 1:12){
    for (years in 1:74){

        value <- clim_w_week %>%
          filter(week == weeks & month == months & id == years) %>%
          summarise(weekrain=mean(rain, na.rm=T))

        clim_array[weeks, months, years]  = as.numeric(value)

      }
    }
  }

# add  names
dimnames(clim_array) = list(c("week 1","week 2","week 3","week 4"),
                       c("Jan","Feb", "Mar", "Apr", "May", "Jun",
                          "Jul","Aug", "Sep", "Oct","Nov", "Dec"),
                       c(seq(1942,2015))
                         )

## average precip by year
average_precip_by_year = apply(clim_array, c(3), mean)
average_precip_by_year <- as.data.frame(average_precip_by_year)


## average precip by month
average_precip_by_month = apply(clim_array, c(2), mean)
average_precip_by_month <- as.data.frame(average_precip_by_month)



return(list(`average precipitation by year` = average_precip_by_month,
            `average precipitation per year` = average_precip_by_year))


}

