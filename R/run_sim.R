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
run_sim <- function(net, nsim, alpha1 = 1.15, alpha2 = 0, beta1 = 5, beta2 = 10, nu = .05, rmax = 0.02, taper = 0.05, keep_intermediate = FALSE){
  n_nodes <- igraph::gorder(net)
  net_pruned <- taper(net, beta = max(beta1, beta2), tolerance = taper)

  if(keep_intermediate){
    purrr::accumulate(seq_len(nsim), ~run_sim_1(.x,
                      alpha1 = alpha1, alpha2 = alpha2,
                      beta1 = beta1, beta2 = beta2,
                      nu = nu, rmax = rmax,
                      n_nodes = n_nodes, step = .y),
                      .init = net_pruned)
  } else {
    purrr::reduce(seq_len(nsim), ~run_sim_1(.x, alpha1 = alpha1, alpha2 = alpha2,
                      beta1 = beta1, beta2 = beta2,
                      nu = nu, rmax = rmax,
                      n_nodes = n_nodes, step = .y),
                  .init = net_pruned)
  }
}

run_sim_1 <- function(net, alpha1, alpha2, beta1, beta2, nu, rmax, n_nodes, step) {
  new_net <- net |>
    trade(alpha = alpha1, beta = beta1) |>
    migrate(alpha1 = alpha1, alpha2 = alpha2, beta = beta2, nu = nu) |>
    grow(rmax = rmax) |>
    dplyr::select(-c(trade_balance:trade_production, migrant_balance, population_new)) |>
    tidygraph::activate(edges) |>
    dplyr::select(-c(trade_utility, migrant_utility)) |>
    tidygraph::activate('nodes')

  # stop the simulation if all hexes have been at equilibrium for more than 10 years
  if(sum(pull(new_net, eq) >= 10) < n_nodes){
    return(new_net)
  } else {
    message(paste0('The simulation has reached equilibrium at time step ', step - 10))
    return(done(new_net))
  }
}
