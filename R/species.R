#' Example Species Temperature Stress Data from Santa Barbara County
#'
#' Sample dataset giving the number of days species can tolerate high heat stress days per year.
#' Species information given by species identifier, scientific name, and common name.
#' URL provides source for using >86 degF (>30 degC) as the baseline temperature for stressful temperature levels.
#' Species heat stress days derived from drought tolerance level defined by USDA plant characterisitcs.
#' Example dataset: BAPI has high drought tolerance, QUAG has medium tolerance, and NODE has low tolerance.
#' @format A data frame with 3 rows and 4 variables:
#' \itemize{
#' \item Species species identifier
#' \item Scientific species scientific name
#' \item Common species common name
#' \item StressDays # of days species can tolerate high temperatures per year before becoming stressed
#' }
#'
#' @source \url{#http://agron-www.agron.iastate.edu/courses/Agron541/classes/541/lesson04a/4a.2.html}
#'
"species"
