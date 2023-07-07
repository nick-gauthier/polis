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
  tidygraph::activate(net, 'edges') |>
    dplyr::mutate(migrant_utility = .N()$population[to] ^ alpha1 * (.N()$harvest[to] / .N()$population[to]) ^ alpha2 * exp(-distance / beta)) |>
    tidygraph::activate('nodes') |>
    dplyr::mutate(
      migrant_balance = tidygraph::centrality_degree(weights = migrant_utility, mode = 'out', loops = TRUE),
      migrant_production = population * nu) |>
    tidygraph::activate('edges') |>
    dplyr::mutate(migrant_flow = .N()$migrant_production[from] * migrant_utility / .N()$migrant_balance[from]) |>
    tidygraph::activate('nodes') |>
    dplyr::mutate(immigrants = tidygraph::centrality_degree(weights = migrant_flow, mode = 'in', loops = TRUE))
}

#this errors if beta is 0, would be nice not to
