#' Function 2: m-array
#' @title
#' @description
#' @param
#' @param
#' @param
#' @author Sofie McComb & Margaux Sleckman
#' @example
#' @return
#'
#'

library(dplyr)

Marray_function <-function(clim){ }


clim$week <- NA

clim <- clim %>%
  mutate(week = ifelse(day <= 7, 1,
                       ifelse(day > 7 & day <= 14, 2,
                              ifelse(day > 14 & day <= 21, 3,
                                     ifelse(day > 21, 4, NA)
                                     ))))



clim_array = array(dim = c(4, 12, 75))
dim(clim_array)
clim_array[1,2,3]

# populate with values (we are just guessing, ideally this is where measuring would occur)

for (week in 1:4){
  for (month in 1:12){
    # for(year in 1:75){

      # value = runif(min=0.2,max=0.5, n=75)
      value = mean(clim$rain)

      clim_array[week,month,]=value

      # value = mean(clim$rain[week])
      # clim_array[]= value
  #  }
}
}

head(clim_array)

clim_array

# add useful names
dimnames(soilm) = list(c("Farm1","Farm2","Farm3","Farm4","Farm5"),
                       c("apple","avocado","orange","almond"),
                       c("shallow","deep"))

length(unique(clim$year))


