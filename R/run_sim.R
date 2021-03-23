#' Title
#'
#' @param net
#' @param alpha1
#' @param alpha2
#' @param beta1
#' @param beta2
#' @param nu
#'
#' @return
#' @export
#'
#' @examples
run_sim <- function(net, alpha1 = 1.15, alpha2 = 0, beta1 = 5, beta2 = 10, nu = .05){
  n_nodes <- igraph::gorder(net)

  new_net <- net %>%
    trade(alpha = alpha1, beta = beta1) %>%
    migrate(alpha1 = alpha1, alpha2 = alpha2, beta = beta2, nu = nu) %>%
    grow %>%
    select(-c(trade_balance:trade_production, migrant_balance, population_new)) %E>%
    select(-c(trade_utility, migrant_utility)) %>%
    activate('nodes')

  # stop the simulation if all hexes have been at equilibrium for more than 100 years
  if(sum(pull(new_net, eq) >= 100) < n_nodes){
    return(new_net)
  } else return(done(new_net))
}
