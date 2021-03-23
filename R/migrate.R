#' Migrate
#'
#' @param net
#' @param alpha1
#' @param alpha2
#' @param beta
#' @param nu
#'
#' @return
#' @export
#'
#' @examples
migrate <- function(net, alpha1, alpha2, beta, nu = .05){
  net %E>%
    mutate(migrant_utility =.N()$population[to] ^ alpha1 * (.N()$harvest[to] / .N()$population[to]) ^ alpha2 * exp(-distance / beta)) %N>%
    mutate(migrant_balance = centrality_degree(weights = migrant_utility, mode = 'out', loops = TRUE),
           migrant_production = population * nu) %E>%
    mutate(migrant_flow = .N()$migrant_production[from] * migrant_utility / .N()$migrant_balance[from])  %N>%
    mutate(immigrants = centrality_degree(weights = migrant_flow, mode = 'in', loops = TRUE))
}

#this errors if beta is 0, would be nice not to
