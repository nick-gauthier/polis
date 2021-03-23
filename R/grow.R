#' Grow population
#'
#' @param net
#' @param epsilon
#' @param rmax
#'
#' @return
#' @export
#'
#' @examples
grow <- function(net, epsilon = .0001, rmax = .02){
  net %N>%
    mutate(population_new = population + population * pmin(epsilon * (harvest - population), rmax) - migrant_production + immigrants,
           population_new = if_else(population_new > .0001, population_new, .0001),
           eq = if_else(abs((population_new - population) / population) > 0.001, 0, eq + 1),
           population = population_new)
}
