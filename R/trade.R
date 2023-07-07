#' Trade
#'
#' @param net
#' @param alpha
#' @param beta
#' @param phi
#'
#' @return
#' @export
#'
#' @examples
trade <- function(net, alpha, beta){
  tidygraph::activate(net, 'edges') |>
    dplyr::mutate(trade_utility = .N()$population[to] ^ alpha * exp(-distance / beta)) |>
    tidygraph::activate('nodes') |>
    dplyr::mutate(
      trade_balance = tidygraph::centrality_degree(weights = trade_utility, mode = 'out', loops = TRUE),
      trade_production = food) |>
    tidygraph::activate('edges') |>
    dplyr::mutate(trade_flow = .N()$trade_production[from] * trade_utility / .N()$trade_balance[from]) |>
    tidygraph::activate('nodes') |>
    dplyr::mutate(harvest = tidygraph::centrality_degree(weights = trade_flow, mode = 'in', loops = TRUE))
}
