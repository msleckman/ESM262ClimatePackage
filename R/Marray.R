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



Marray_function <-function(clim){ }


clim <- clim %>% mutate(week = )

clim$week <- for(i in nrow(clim)){

  if(clim$day <= 7){
    clim$week[i] == 1
    }

  if(clim$day > 7 & <= 14){
    clim$week[i] == 2
  }

  if(clim$day > 14 & <= 21){
    clim$week[i] == 3
  }

  if (clim$day > 21){
  clim$week[i] == 4
  }
}

values

clim_array = array(dim = c(4, 12, 75))
dim(clim_array)
clim_array

# populate with values (we are just guessing, ideally this is where measuring would occur)

for (year in 1:75){
  for (month in 1:12){
    for (week in 1:4) {

      value = clim$rain[]
      clim_array[i,j,]=value

    }
  }
}

head(clim_array[,,1])

clim_array[,,1]

# add useful names
dimnames(soilm) = list(c("Farm1","Farm2","Farm3","Farm4","Farm5"),
                       c("apple","avocado","orange","almond"),
                       c("shallow","deep"))

length(unique(clim$year))


